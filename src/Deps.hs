{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Deps where

import AST
import Control.Monad (unless, when)
import Data.Bifunctor (bimap)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Files (overwrite)
import GHC.Clock (getMonotonicTime)
import Logger (logDebug, logInfo)
import Matcher
import Printer (printAttribute, printFunction)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.IO (Handle, hPutStrLn)
import Text.Printf (printf)
import XMIR (escapeXML, escapeXMLText)
import Yaml (ExtraArgument)

data Term
  = TeExpression Expression
  | TeAttribute Attribute
  | TeBytes Bytes
  | TeBindings [Binding]

type BuildTermMethod = [ExtraArgument] -> Subst -> IO Term

data State = State
  { _manufactured :: Maybe Int
  , _stuck :: Maybe T.Text
  }

type BuildTermMethodS = [ExtraArgument] -> Subst -> IO (Term, State)

type BuildTermFunc = String -> BuildTermMethod

type SaveStepFunc = Expression -> IO ()

saveStep :: Maybe FilePath -> String -> (Expression -> IO String) -> Int -> SaveStepFunc
saveStep Nothing _ _ _ _ = pure ()
saveStep (Just dir) ext render step expr = do
  createDirectoryIfMissing True dir
  let path = dir </> printf "%05d.%s" step ext
  content <- render expr
  overwrite path content
  logDebug (printf "Saved step '%d' to '%s'" step path)

dontSaveStep :: SaveStepFunc
dontSaveStep = saveStep Nothing "" (\_ -> pure "") 0

data Judgment
  = Normalization
  | Morphing
  | Dataization
  | Evaluation
  | Contextualization
  deriving (Eq, Show)

letter :: Judgment -> String
letter Normalization = "𝒩"
letter Morphing = "𝕄"
letter Dataization = "𝔻"
letter Evaluation = "𝔼"
letter Contextualization = "𝒞"

opened :: Judgment -> String
opened Normalization = "normalize"
opened Morphing = "morph"
opened Dataization = "dataize"
opened Evaluation = "evaluate"
opened Contextualization = "contextualize"

data Acyclic
  = Proven
  | Plausible
  deriving (Bounded, Enum, Eq, Show)

certainty :: Acyclic -> String
certainty Proven = "proven"
certainty Plausible = "plausible"

data Evaluation
  = EvRun Judgment T.Text
  | EvFiring Int T.Text Judgment Expression
  | EvFormation Int Expression Expression
  | EvLooped Int Judgment Acyclic Expression Expression (Maybe (Int, Maybe Expression))
  | EvStuck Int T.Text Judgment Expression
  | EvStall Int T.Text
  | EvStuckOn Int T.Text
  | EvStarved Int Int Judgment Expression
  | EvTimeout Int Int Judgment Expression
  | EvSpent Int Int Judgment Expression
  | EvData Int T.Text Expression (Either Int Bytes)
  | EvTerm Int T.Text Expression Expression
  | EvSymbolize Int T.Text Expression Expression
  | EvKnown Int Int Bytes
  | EvJoin Int T.Text (T.Text, T.Text) Expression
  | EvJoined Int Int (Int, Int)
  | EvTerminate Int (Maybe (Either Int Bytes)) T.Text T.Text
  | EvMinted Int Int [Either Int Bytes]
  | EvDeferred Int Int Judgment Expression (Maybe Expression) Expression
  | EvBuilt Int Expression
  | EvAnswer Int Expression

type SaveEvalFunc = Evaluation -> IO ()

renumbered :: Int -> Int -> Evaluation -> Evaluation
renumbered floor' offset = record
  where
    record :: Evaluation -> Evaluation
    record (EvFiring depth key judgment site) = EvFiring depth key judgment (term site)
    record (EvFormation depth self site) = EvFormation depth (term self) (term site)
    record (EvLooped depth judgment mode self site answered) = EvLooped depth judgment mode (term self) (term site) (fmap (bimap symbol (fmap term)) answered)
    record (EvStuck depth key judgment self) = EvStuck depth key judgment (term self)
    record (EvStarved depth limit judgment site) = EvStarved depth limit judgment (term site)
    record (EvTimeout depth limit judgment site) = EvTimeout depth limit judgment (term site)
    record (EvSpent depth limit judgment site) = EvSpent depth limit judgment (term site)
    record (EvData depth spelling operand value) = EvData depth spelling (term operand) (datum value)
    record (EvTerm depth spelling operand value) = EvTerm depth spelling (term operand) (term value)
    record (EvSymbolize depth spelling source value) = EvSymbolize depth spelling (term source) (term value)
    record (EvKnown depth sym bytes) = EvKnown depth (symbol sym) bytes
    record (EvJoin depth spelling pair value) = EvJoin depth spelling pair (term value)
    record (EvJoined depth fresh (one, two)) = EvJoined depth (symbol fresh) (symbol one, symbol two)
    record (EvTerminate depth condition side raising) = EvTerminate depth (fmap datum condition) side raising
    record (EvMinted depth sym operands) = EvMinted depth (symbol sym) (map datum operands)
    record (EvDeferred depth sym judgment copy call site) = EvDeferred depth (symbol sym) judgment (term copy) (fmap term call) (term site)
    record (EvBuilt depth value) = EvBuilt depth (term value)
    record (EvAnswer depth value) = EvAnswer depth (term value)
    record other = other
    term :: Expression -> Expression
    term = lifted floor' offset
    symbol :: Int -> Int
    symbol idx
      | idx > floor' = idx + offset
      | otherwise = idx
    datum :: Either Int Bytes -> Either Int Bytes
    datum = either (Left . symbol) Right

type Named = Map.Map Int [(Expression, T.Text)]

namedLookup :: Expression -> Named -> Maybe T.Text
namedLookup term names = lookup term (Map.findWithDefault [] (hashExpression term) names)

namedInsert :: Expression -> T.Text -> Named -> Named
namedInsert term naming = Map.alter renamed (hashExpression term)
  where
    renamed :: Maybe [(Expression, T.Text)] -> Maybe [(Expression, T.Text)]
    renamed entries = Just ((term, naming) : filter ((/= term) . fst) (fromMaybe [] entries))

data Protocol = Protocol
  { _fired :: Int
  , _named :: Named
  , _open :: Map.Map Int Int
  , _begun :: Bool
  }

emptyProtocol :: Protocol
emptyProtocol = Protocol 0 Map.empty Map.empty False

data Nesting = Nesting
  { _fires :: Int
  , _openedAt :: Map.Map Int Int
  , _closing :: [(Int, String)]
  }

emptyNesting :: Nesting
emptyNesting = Nesting 0 Map.empty []

saveEval :: Handle -> IORef Protocol -> (Expression -> IO String) -> (Expression -> IO String) -> SaveEvalFunc
saveEval handle cursor render salted report = do
  line <- atomicModify cursor (written report)
  mapM_ saved line
  where
    saved :: String -> IO ()
    saved line = do
      hPutStrLn handle line
      logDebug (printf "Saved one line of the protocol: %s" (dropWhile (== ' ') line))
    written :: Evaluation -> Protocol -> IO (Protocol, Maybe String)
    written (EvRun judgment locator) protocol =
      pure (protocol{_begun = True}, Just (printf "%s(%s)" (letter judgment) (T.unpack locator)))
    written (EvFiring depth key judgment site) protocol = do
      locator <- render site
      pure
        ( protocol
            { _fired = firings
            , _open = Map.insert depth firings protocol._open
            }
        , Just (indented depth (printf "𝔼(%s)  # %s(%s)" (T.unpack key) (letter judgment) locator))
        )
      where
        firings :: Int
        firings = protocol._fired + 1
    written (EvFormation depth self site) protocol = do
      form <- render self
      locator <- render site
      pure (protocol, Just (indented depth (printf "formation(%s)  # %s(%s)" form (letter Dataization) locator)))
    written (EvLooped depth judgment mode self site answered) protocol = do
      form <- render self
      locator <- render site
      pure (protocol, Just (indented depth (printf "looped(%s)%s  # %s(%s), %s" form (maybe "" symbolized answered) (letter judgment) locator (certainty mode))))
      where
        symbolized :: (Int, Maybe Expression) -> String
        symbolized (symbol, _) = printf " := %s" (printFunction (FnSymbol symbol))
    written (EvStuck depth key judgment self) protocol = do
      form <- render self
      pure (protocol, Just (indented depth (printf "unanswered(%s)  # %s(%s)" (T.unpack key) (letter judgment) form)))
    written (EvStall depth key) protocol =
      pure (protocol, Just (indented depth (printf "stall(%s)" (T.unpack key))))
    written (EvStuckOn depth key) protocol =
      pure (protocol, Just (indented depth (printf "stuck(%s)" (T.unpack key))))
    written (EvStarved depth limit judgment site) protocol = do
      locator <- render site
      pure (protocol, Just (indented depth (printf "starved(%d)  # %s(%s)" limit (letter judgment) locator)))
    written (EvTimeout depth limit judgment site) protocol = do
      locator <- render site
      pure (protocol, Just (indented depth (printf "timeout(%d)  # %s(%s)" limit (letter judgment) locator)))
    written (EvSpent depth limit judgment site) protocol = do
      locator <- render site
      pure (protocol, Just (indented depth (printf "spent(%d)  # %s(%s)" limit (letter judgment) locator)))
    written (EvData depth spelling operand value) protocol = do
      datum <- spelled value
      line <- commented (printf "%s := %s" (labelled protocol depth spelling) datum) Dataization operand
      pure (protocol, Just (indented depth line))
      where
        spelled :: Either Int Bytes -> IO String
        spelled (Left symbol) = printf "𝔻(%s)" <$> render (standing symbol)
        spelled (Right bytes) = render (ExBytes bytes)
    written (EvTerm depth spelling operand term) protocol = do
      let naming = labelled protocol depth spelling
      (protocol', value) <- valued protocol naming term
      line <- commented (printf "%s := %s" naming value) Morphing operand
      pure (protocol', Just (indented depth line))
    written (EvSymbolize depth spelling source term) protocol = do
      let naming = labelled protocol depth spelling
      (protocol', value) <- valued protocol naming term
      line <- commented' (printf "%s := %s" naming value) source
      pure (protocol', Just (indented depth line))
    written (EvKnown depth symbol bytes) protocol = do
      form <- render (standing symbol)
      value <- render (ExBytes bytes)
      pure (protocol, Just (indented depth (printf "𝔻(%s) == %s" form value)))
    written (EvJoin depth spelling (left, right) term) protocol = do
      let naming = labelled protocol depth spelling
      (protocol', value) <- valued protocol naming term
      pure (protocol', Just (indented depth (printf "%s := %s  # [%s, %s]" naming value (T.unpack left) (T.unpack right))))
    written (EvJoined depth fresh (one, two)) protocol = do
      form <- render (standing fresh)
      left <- render (standing one)
      right <- render (standing two)
      pure (protocol, Just (indented depth (printf "𝔻(%s) ∈ { 𝔻(%s), 𝔻(%s) }" form left right)))
    written (EvTerminate depth condition side raised) protocol = do
      cond <- maybe (pure []) (fmap pure . spelled) condition
      pure (protocol, Just (indented depth (printf "terminate(%s)  # %s" (intercalate ", " (cond ++ [T.unpack side])) (T.unpack raised))))
      where
        spelled :: Either Int Bytes -> IO String
        spelled (Left symbol) = printf "𝔻(%s)" <$> render (standing symbol)
        spelled (Right bytes) = render (ExBytes bytes)
    written EvMinted{} protocol = pure (protocol, Nothing)
    written (EvDeferred depth symbol judgment copy call site) protocol = do
      form <- render (fromMaybe copy call)
      locator <- render site
      pure (protocol, Just (indented depth (printf "deferred(%s) := %s  # %s(%s)" (printFunction (FnSymbol symbol)) form (letter judgment) locator)))
    written (EvBuilt depth term) protocol = do
      value <- borrowed protocol term
      pure (protocol, Just (indented depth (printf "%s.1 := %s  # %s" (labelled protocol depth answer) value (T.unpack answer))))
    written (EvAnswer depth term) protocol = do
      let stem :: String
          stem = labelled protocol depth answer
          naming :: String
          naming = printf "%s.2" stem
      (protocol', value) <- valued protocol naming term
      pure (protocol', Just (indented depth (printf "%s := %s  # 𝕄(%s.1)" naming value stem)))
    valued :: Protocol -> String -> Expression -> IO (Protocol, String)
    valued protocol naming term = case denoted term of
      Nothing -> (,) protocol <$> render term
      Just _ -> do
        value <- maybe (render term) (pure . T.unpack) (namedLookup term protocol._named)
        pure (protocol{_named = namedInsert term (T.pack naming) protocol._named}, value)
    borrowed :: Protocol -> Expression -> IO String
    borrowed protocol term = case namedLookup term protocol._named of
      Nothing -> render term
      Just naming -> pure (T.unpack naming)
    commented :: String -> Judgment -> Expression -> IO String
    commented line judgment operand = printf "%s  # %s(%s)" line (letter judgment) <$> salted operand
    commented' :: String -> Expression -> IO String
    commented' line source = printf "%s  # %s" line <$> salted source
    labelled :: Protocol -> Int -> T.Text -> String
    labelled protocol depth spelling =
      printf "%s.%d" (T.unpack spelling) (fromMaybe 0 (Map.lookup (depth - 1) protocol._open))

endEval :: Handle -> IORef Protocol -> Double -> IO ()
endEval handle cursor began = do
  protocol <- readIORef cursor
  when protocol._begun $ do
    now <- getMonotonicTime
    let taken = milliseconds began now
    hPutStrLn handle (printf "msec(%d)" taken)
    hPutStrLn handle (printf "firings(%d)" protocol._fired)
    hPutStrLn handle (printf "fps(%d)" (perSecond protocol._fired taken))

milliseconds :: Double -> Double -> Int
milliseconds began now = round ((now - began) * 1000)

perSecond :: Int -> Int -> Int
perSecond firings taken = round (fromIntegral firings * 1000 / fromIntegral (max 1 taken) :: Double)

saveEvalXml :: Handle -> IORef Nesting -> (Expression -> IO String) -> SaveEvalFunc
saveEvalXml handle cursor render report = do
  written <- atomicModify cursor (elements report)
  mapM_ (hPutStrLn handle) written
  logDebug (printf "Saved %d line(s) of the XML protocol" (length written))
  where
    elements :: Evaluation -> Nesting -> IO (Nesting, [String])
    elements (EvRun judgment locator) nesting =
      pure
        ( nesting{_closing = (0, opened judgment) : (-1, "protocol") : nesting._closing}
        ,
          [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          , "<protocol>"
          , indentedXml 0 (printf "<%s at=\"%s\">" (opened judgment) (quoted locator))
          ]
        )
    elements (EvFiring depth key judgment site) nesting = do
      locator <- render site
      pure
        ( nesting
            { _fires = fires
            , _openedAt = Map.insert depth fires nesting._openedAt
            , _closing = (depth, "evaluate") : kept
            }
        , closers ++ [indentedXml depth (printf "<evaluate λ=\"%s\" by=\"%s\" at=\"%s\">" (quoted key) (opened judgment) (escapeXML locator))]
        )
      where
        (kept, closers) = closed depth nesting._closing
        fires :: Int
        fires = nesting._fires + 1
    elements (EvFormation depth self site) nesting = do
      form <- render self
      locator <- render site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = (depth, "formation") : kept}, closers ++ [indentedXml depth (printf "<formation at=\"%s\" term=\"%s\">" (escapeXML locator) (escapeXML form))])
    elements (EvLooped depth judgment mode self site answered) nesting = do
      form <- render self
      locator <- render site
      (origin, given) <- maybe (pure ("", "")) called (answered >>= snd)
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<looped%s by=\"%s\" match=\"%s\" at=\"%s\"%s>%s<e>%s</e></looped>" (maybe "" symbolized answered) (opened judgment) (certainty mode) (escapeXML locator) origin given (escapeXMLText form))])
      where
        symbolized :: (Int, Maybe Expression) -> String
        symbolized (symbol, _) = printf " symbol=\"%s\"" (sigma symbol)
    elements (EvStuck depth key judgment self) nesting = do
      form <- render self
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<unanswered λ=\"%s\" by=\"%s\">%s</unanswered>" (quoted key) (opened judgment) (escapeXMLText form))])
    elements (EvStall depth key) nesting = do
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<stall λ=\"%s\"/>" (quoted key))])
    elements (EvStuckOn depth key) nesting = do
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<unfinished λ=\"%s\"/>" (quoted key))])
    elements (EvStarved depth limit judgment site) nesting = do
      locator <- render site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<starved limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) (escapeXML locator))])
    elements (EvTimeout depth limit judgment site) nesting = do
      locator <- render site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<timeout limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) (escapeXML locator))])
    elements (EvSpent depth limit judgment site) nesting = do
      locator <- render site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<spent limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) (escapeXML locator))])
    elements (EvData depth spelling _ value) nesting = do
      record <- stood value
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth record])
      where
        (kept, closers) = closed depth nesting._closing
        stood :: Either Int Bytes -> IO String
        stood (Left symbol) = do
          form <- render (standing symbol)
          pure (printf "<dataize meta=\"%s\">%s</dataize>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText form))
        stood (Right bytes) = do
          form <- render (ExBytes bytes)
          pure (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText form))
    elements (EvTerm depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText body))])
    elements (EvSymbolize depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText body))])
    elements (EvKnown depth symbol bytes) nesting = do
      value <- render (ExBytes bytes)
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<known symbol=\"%s\">%s</known>" (sigma symbol) (escapeXMLText value))])
    elements (EvJoin depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText body))])
    elements (EvJoined depth fresh (one, two)) nesting =
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth joint])
      where
        (kept, closers) = closed depth nesting._closing
        joint :: String
        joint = printf "<joined symbol=\"%s\">%s %s</joined>" (sigma fresh) (sigma one) (sigma two)
    elements (EvTerminate depth condition side _) nesting = do
      terminal <- case condition of
        Just (Left symbol) -> pure (printf "<terminate symbol=\"%s\" branch=\"%s\"/>" (sigma symbol) (quoted side))
        Just (Right bytes) -> printf "<terminate branch=\"%s\">%s</terminate>" (quoted side) . escapeXMLText <$> render (ExBytes bytes)
        Nothing -> pure (printf "<terminate branch=\"%s\"/>" (quoted side))
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth terminal])
    elements (EvMinted depth symbol operands) nesting = do
      spelledOperands <- mapM spelled operands
      let (kept, closers) = closed depth nesting._closing
          mint :: String
          mint
            | null operands = printf "<minted symbol=\"%s\"/>" (sigma symbol)
            | otherwise = printf "<minted symbol=\"%s\">%s</minted>" (sigma symbol) (escapeXMLText (unwords spelledOperands))
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth mint])
      where
        spelled :: Either Int Bytes -> IO String
        spelled (Left fresh) = pure (sigma fresh)
        spelled (Right bytes) = render (ExBytes bytes)
    elements (EvDeferred depth symbol judgment copy call site) nesting = do
      form <- render copy
      locator <- render site
      (origin, given) <- maybe (pure ("", "")) called call
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<deferred symbol=\"%s\" by=\"%s\" at=\"%s\"%s>%s<e>%s</e></deferred>" (sigma symbol) (opened judgment) (escapeXML locator) origin given (escapeXMLText form))])
    elements (EvBuilt depth term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
          naming :: String
          naming = printf "%s.1" (labelled nesting depth answer)
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<built meta=\"%s\">%s</built>" (escapeXML naming) (escapeXMLText body))])
    elements (EvAnswer depth term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
          naming :: String
          naming = printf "%s.2" (labelled nesting depth answer)
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<answer meta=\"%s\">%s</answer>" (escapeXML naming) (escapeXMLText body))])
    labelled :: Nesting -> Int -> T.Text -> String
    labelled nesting depth spelling =
      printf "%s.%d" (T.unpack spelling) (fromMaybe 0 (Map.lookup (depth - 1) nesting._openedAt))
    called :: Expression -> IO (String, String)
    called term = do
      let (object, arguments) = invoked term
      path <- render object
      pure (printf " of=\"%s\"" (escapeXML path), printf "<with>%s</with>" (concat arguments))
    invoked :: Expression -> (Expression, [String])
    invoked (ExApplication term (ArTau attr value)) =
      let (object, arguments) = invoked term
       in (object, arguments ++ [printf "<attr name=\"%s\">%s</attr>" (escapeXML (printAttribute attr)) (escapeXMLText (valued value))])
    invoked term = (term, [])
    valued :: Expression -> String
    valued (ExFormation [BiLambda (FnSymbol idx)]) = sigma idx
    valued (ExFormation bds) = maybe "?" valued (listToMaybe [body | BiTau AtPhi body <- bds])
    valued (ExApplication _ (ArTau AtPhi body)) = valued body
    valued _ = "?"
    sigma :: Int -> String
    sigma = printFunction . FnSymbol
    quoted :: T.Text -> String
    quoted = escapeXML . T.unpack

endEvalXml :: Handle -> IORef Nesting -> Double -> IO ()
endEvalXml handle cursor began = do
  nesting <- readIORef cursor
  mapM_ (hPutStrLn handle) (snd (closed 0 nesting._closing))
  unless (null nesting._closing) $ do
    now <- getMonotonicTime
    let taken = milliseconds began now
    hPutStrLn handle (indentedXml 0 (printf "<msec>%d</msec>" taken))
    hPutStrLn handle (indentedXml 0 (printf "<firings>%d</firings>" nesting._fires))
    hPutStrLn handle (indentedXml 0 (printf "<fps>%d</fps>" (perSecond nesting._fires taken)))
    hPutStrLn handle "</protocol>"
  writeIORef cursor nesting{_closing = []}

closed :: Int -> [(Int, String)] -> ([(Int, String)], [String])
closed depth open = (kept, [indentedXml level (printf "</%s>" element) | (level, element) <- shut])
  where
    (shut, kept) = span ((>= depth) . fst) open

indented :: Int -> String -> String
indented depth line = replicate (2 * depth) ' ' ++ line

indentedXml :: Int -> String -> String
indentedXml depth = indented (depth + 1)

atomicModify :: IORef a -> (a -> IO (a, b)) -> IO b
atomicModify ref action = readIORef ref >>= action >>= \(value, made) -> writeIORef ref value >> pure made

standing :: Int -> Expression
standing symbol = ExFormation [BiLambda (FnSymbol symbol)]

answer :: T.Text
answer = "𝑛"

dontSaveEval :: SaveEvalFunc
dontSaveEval _ = pure ()

data Progress = Progress
  { _began :: Double
  , _told :: Maybe Double
  , _formations :: Int
  , _firings :: Int
  }

emptyProgress :: Double -> Progress
emptyProgress began = Progress began Nothing 0 0

progressed :: IORef Progress -> Double -> (Expression -> IO String) -> SaveEvalFunc -> SaveEvalFunc
progressed cursor interval render record evaluation = do
  record evaluation
  mapM_ reported (sited evaluation)
  where
    sited :: Evaluation -> Maybe (Progress -> Progress, Expression)
    sited (EvFiring _ _ _ site) = Just (\progress -> progress{_firings = progress._firings + 1}, site)
    sited (EvFormation _ _ site) = Just (\progress -> progress{_formations = progress._formations + 1}, site)
    sited _ = Nothing
    reported :: (Progress -> Progress, Expression) -> IO ()
    reported (counted, site) = do
      now <- getMonotonicTime
      progress <- counted <$> readIORef cursor
      if maybe True (\told -> now - told >= interval) progress._told
        then do
          locator <- render site
          logInfo (printf "Entered %d formations and fired %d λ functions in %.0fs, now at %s" progress._formations progress._firings (now - progress._began) locator)
          writeIORef cursor progress{_told = Just now}
        else writeIORef cursor progress
