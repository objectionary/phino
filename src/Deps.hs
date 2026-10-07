{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Deps where

import AST
import Bytes (btsSize, btsToNum)
import Control.Monad (unless, when)
import Data.Bifunctor (bimap, first)
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

type SaveMadeFunc = Expression -> Expression -> IO ()

dontSaveMade :: SaveMadeFunc
dontSaveMade _ _ = pure ()

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
  | EvStarted Int Judgment Expression
  | EvDelta Int Bytes
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
  | EvApplied Int Judgment Expression Expression Expression
  | EvComputed Int Expression Expression
  | EvBuilt Int Expression
  | EvAnswer Int Expression

type SaveEvalFunc = Evaluation -> IO ()

renumbered :: Int -> Int -> Evaluation -> Evaluation
renumbered floor' offset = record
  where
    record :: Evaluation -> Evaluation
    record (EvFiring depth key judgment site) = EvFiring depth key judgment (term site)
    record (EvStarted depth judgment site) = EvStarted depth judgment (term site)
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
    record (EvApplied depth judgment call object site) = EvApplied depth judgment (term call) (term object) (term site)
    record (EvComputed depth before after) = EvComputed depth (term before) (term after)
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

resited :: Expression -> Expression -> Evaluation -> Evaluation
resited from to = record
  where
    record :: Evaluation -> Evaluation
    record (EvFiring depth key judgment site) = EvFiring depth key judgment (moved site)
    record (EvStarted depth judgment site) = EvStarted depth judgment (moved site)
    record (EvLooped depth judgment mode self site answered) = EvLooped depth judgment mode self (moved site) answered
    record (EvStarved depth limit judgment site) = EvStarved depth limit judgment (moved site)
    record (EvTimeout depth limit judgment site) = EvTimeout depth limit judgment (moved site)
    record (EvSpent depth limit judgment site) = EvSpent depth limit judgment (moved site)
    record (EvDeferred depth sym judgment copy call site) = EvDeferred depth sym judgment copy call (moved site)
    record (EvApplied depth judgment call object site) = EvApplied depth judgment call object (moved site)
    record other = other
    moved :: Expression -> Expression
    moved site
      | site == from = to
      | otherwise = site

ieee :: Bytes -> Maybe String
ieee bytes
  | btsSize bytes == 8 = Just (show (either fromIntegral id (btsToNum bytes) :: Double))
  | otherwise = Nothing

tier :: Evaluation -> Int
tier EvRun{} = 0
tier (EvFiring depth _ _ _) = depth
tier (EvStarted depth _ _) = depth
tier (EvDelta depth _) = depth
tier (EvLooped depth _ _ _ _ _) = depth
tier (EvStuck depth _ _ _) = depth
tier (EvStall depth _) = depth
tier (EvStuckOn depth _) = depth
tier (EvStarved depth _ _ _) = depth
tier (EvTimeout depth _ _ _) = depth
tier (EvSpent depth _ _ _) = depth
tier (EvData depth _ _ _) = depth
tier (EvTerm depth _ _ _) = depth
tier (EvSymbolize depth _ _ _) = depth
tier (EvKnown depth _ _) = depth
tier (EvJoin depth _ _ _) = depth
tier (EvJoined depth _ _) = depth
tier (EvTerminate depth _ _ _) = depth
tier (EvMinted depth _ _) = depth
tier (EvDeferred depth _ _ _ _ _) = depth
tier (EvApplied depth _ _ _ _) = depth
tier (EvComputed depth _ _) = depth
tier (EvBuilt depth _) = depth
tier (EvAnswer depth _) = depth

type Named a = Map.Map Int [(Expression, a)]

namedLookup :: Expression -> Named a -> Maybe a
namedLookup term names = lookup term (Map.findWithDefault [] (hashExpression term) names)

namedInsert :: forall a. Expression -> a -> Named a -> Named a
namedInsert term naming = Map.alter renamed (hashExpression term)
  where
    renamed :: Maybe [(Expression, a)] -> Maybe [(Expression, a)]
    renamed entries = Just ((term, naming) : filter ((/= term) . fst) (fromMaybe [] entries))

namedCarry :: Expression -> Expression -> Named a -> Named a
namedCarry before after names = maybe names (\naming -> namedInsert after naming names) (namedLookup before names)

abbreviated :: Named Expression -> Expression -> Expression
abbreviated names term
  | Map.null names = term
  | otherwise = fromMaybe (abbreviatedInside names term) (namedLookup term names)

abbreviatedInside :: Named Expression -> Expression -> Expression
abbreviatedInside names (ExFormation bds) = ExFormation (map binding bds)
  where
    binding :: Binding -> Binding
    binding (BiTau attr expr) = BiTau attr (abbreviated names expr)
    binding bd = bd
abbreviatedInside names (ExApplication expr (ArTau attr arg)) = ExApplication (abbreviated names expr) (ArTau attr (abbreviated names arg))
abbreviatedInside names (ExApplication expr (ArAlpha alpha arg)) = ExApplication (abbreviated names expr) (ArAlpha alpha (abbreviated names arg))
abbreviatedInside names (ExDispatch expr attr) = ExDispatch (abbreviated names expr) attr
abbreviatedInside _ term = term

data Protocol = Protocol
  { _fired :: Int
  , _named :: Named T.Text
  , _open :: [(Int, Int)]
  , _begun :: Bool
  , _made :: Named Expression
  , _counted :: Map.Map Int Int
  , _built :: Map.Map Int Int
  , _deltas :: Map.Map Int Int
  , _found :: Maybe (Bytes, String)
  , _blocks :: [(Int, Maybe String, Bool)]
  , _answered :: [(Int, Expression, String)]
  }

emptyProtocol :: Protocol
emptyProtocol = Protocol 0 Map.empty [] False Map.empty Map.empty Map.empty Map.empty Nothing [] []

data Nesting = Nesting
  { _fires :: Int
  , _openedAt :: [(Int, Int)]
  , _closing :: [(Int, String)]
  , _objects :: Named Expression
  , _numbered :: Map.Map Int Int
  , _datums :: Map.Map Int Int
  , _held :: Maybe (Bytes, String)
  , _heads :: [(Int, String)]
  , _pending :: [(Int, String, String)]
  , _sources :: [(Int, Expression, String)]
  }

emptyNesting :: Nesting
emptyNesting = Nesting 0 [] [] Map.empty Map.empty Map.empty Nothing [] [] []

saveEval :: Handle -> IORef Protocol -> (Expression -> IO String) -> (Expression -> IO String) -> SaveEvalFunc
saveEval handle cursor printed printed' report = do
  made <- atomicModify cursor (fmap (first (forgotten report) . headed) . written report . beside report . outer (tier report))
  mapM_ saved made
  where
    forgotten :: Evaluation -> Protocol -> Protocol
    forgotten EvDelta{} protocol = protocol
    forgotten _ protocol = protocol{_found = Nothing}
    beside :: Evaluation -> Protocol -> Protocol
    beside EvStarted{} protocol = protocol
    beside EvComputed{} protocol = protocol
    beside EvMinted{} protocol = protocol
    beside report' protocol = protocol{_blocks = dropWhile (\(level, _, _) -> level >= tier report') protocol._blocks}
    headed :: (Protocol, Maybe String) -> (Protocol, [String])
    headed (protocol, Nothing) = (protocol, [])
    headed (protocol, Just line) =
      ( protocol{_blocks = [(level, heading, True) | (level, heading, _) <- protocol._blocks]}
      , [indented level (heading ++ ":") | (level, Just heading, False) <- reverse protocol._blocks] ++ [line]
      )
    render :: Expression -> IO String
    render term = do
      protocol <- readIORef cursor
      printed (abbreviated protocol._made term)
    located :: Protocol -> Expression -> IO String
    located protocol site = case [naming | (_, built, naming) <- protocol._answered, built == site] of
      naming : _ -> pure naming
      [] -> render site
    context :: Protocol -> Judgment -> Expression -> IO [String]
    context protocol judgment site = do
      locator <- located protocol site
      let said = printf "%s(%s)" (letter judgment) locator
      pure [said | innermost protocol /= Just said]
    innermost :: Protocol -> Maybe String
    innermost protocol = case protocol._blocks of
      (_, heading, _) : _ -> heading
      [] -> Nothing
    remarked :: String -> [String] -> String
    remarked line [] = line
    remarked line remarks = printf "%s  # %s" line (intercalate ", " remarks)
    salted :: Expression -> IO String
    salted term = do
      protocol <- readIORef cursor
      printed' (abbreviated protocol._made term)
    saved :: String -> IO ()
    saved line = do
      hPutStrLn handle line
      logDebug (printf "Saved one line of the protocol: %s" (dropWhile (== ' ') line))
    written :: Evaluation -> Protocol -> IO (Protocol, Maybe String)
    written (EvRun judgment locator) protocol =
      pure (protocol{_begun = True, _blocks = [(0, Just heading, True)]}, Just (heading ++ ":"))
      where
        heading :: String
        heading = printf "%s(%s)" (letter judgment) (T.unpack locator)
    written (EvFiring depth key judgment site) protocol = do
      remarks <- context protocol judgment site
      pure
        ( protocol
            { _fired = firings
            , _open = (depth, firings) : protocol._open
            , _blocks = (depth, Nothing, True) : protocol._blocks
            }
        , Just (indented depth (remarked (printf "𝔼(%s):" (T.unpack key)) remarks))
        )
      where
        firings :: Int
        firings = protocol._fired + 1
    written (EvStarted depth judgment site) protocol = do
      locator <- located protocol site
      let heading = printf "%s(%s)" (letter judgment) locator
          inner = dropWhile (\(level, _, _) -> level > depth) protocol._blocks
      pure $ case inner of
        (level, Just heading', _) : _ | level == depth && heading' == heading -> (protocol{_blocks = inner}, Nothing)
        _ -> (protocol{_blocks = (depth, Just heading, False) : dropWhile (\(level, _, _) -> level >= depth) inner}, Nothing)
    written (EvDelta depth bytes) protocol = do
      datum <- render (ExBytes bytes)
      let index = maybe 1 (+ 1) (Map.lookup (opener protocol) protocol._deltas)
          naming :: String
          naming = printf "%s.%d" (labelled protocol sigil) index
      pure (protocol{_deltas = Map.insert (opener protocol) index protocol._deltas, _found = Just (bytes, naming)}, Just (indented depth (remarked (printf "%s := %s" naming datum) (maybe [] pure (ieee bytes)))))
    written (EvLooped depth judgment mode self site answered) protocol = do
      form <- render self
      remarks <- context protocol judgment site
      pure (protocol, Just (indented depth (remarked (printf "looped(%s)%s" form (maybe "" symbolized answered)) (remarks ++ [certainty mode]))))
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
      remarks <- context protocol judgment site
      pure (protocol, Just (indented depth (remarked (printf "starved(%d)" limit) remarks)))
    written (EvTimeout depth limit judgment site) protocol = do
      remarks <- context protocol judgment site
      pure (protocol, Just (indented depth (remarked (printf "timeout(%d)" limit) remarks)))
    written (EvSpent depth limit judgment site) protocol = do
      remarks <- context protocol judgment site
      pure (protocol, Just (indented depth (remarked (printf "spent(%d)" limit) remarks)))
    written (EvData depth spelling operand value) protocol = do
      datum <- maybe (spelled value) pure (found value protocol._found)
      line <- commented (printf "%s := %s" (labelled protocol spelling) datum) Dataization operand
      pure (protocol, Just (indented depth line))
      where
        spelled :: Either Int Bytes -> IO String
        spelled (Left symbol) = printf "𝔻(%s)" <$> render (standing symbol)
        spelled (Right bytes) = render (ExBytes bytes)
    written (EvTerm depth spelling operand term) protocol = do
      let naming = labelled protocol spelling
      (protocol', value) <- valued protocol naming term
      line <- commented (printf "%s := %s" naming value) Morphing operand
      pure (protocol', Just (indented depth line))
    written (EvSymbolize depth spelling source term) protocol = do
      let naming = labelled protocol spelling
      (protocol', value) <- valued protocol naming term
      line <- commented' (printf "%s := %s" naming value) source
      pure (protocol', Just (indented depth line))
    written (EvKnown depth symbol bytes) protocol = do
      form <- render (standing symbol)
      value <- render (ExBytes bytes)
      pure (protocol, Just (indented depth (printf "𝔻(%s) == %s" form value)))
    written (EvJoin depth spelling (left, right) term) protocol = do
      let naming = labelled protocol spelling
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
      form <- maybe (render copy) (printed . abbreviatedInside protocol._made) call
      remarks <- context protocol judgment site
      pure (protocol, Just (indented depth (remarked (printf "deferred(%s) := %s" (printFunction (FnSymbol symbol)) form) remarks)))
    written (EvApplied depth judgment call object site) protocol = do
      let (index, counted) = numbered protocol
          aliased :: Expression
          aliased = alias (opener protocol) index
      form <- printed (abbreviatedInside protocol._made call)
      remarks <- context protocol judgment site
      pure (counted{_made = namedInsert call aliased (namedInsert object aliased counted._made)}, Just (indented depth (remarked (printf "%s.%d := %s" (labelled protocol answer) index form) remarks)))
    written (EvComputed _ before after) protocol = pure (protocol{_made = namedCarry before after protocol._made}, Nothing)
    written (EvBuilt depth term) protocol = do
      let (index, counted) = numbered protocol
      value <- borrowed protocol term
      let naming = printf "%s.%d" (labelled protocol answer) index
      pure (counted{_built = Map.insert (opener protocol) index counted._built, _answered = (depth, term, naming) : counted._answered}, Just (indented depth (printf "%s := %s  # %s" naming value (T.unpack answer))))
    written (EvAnswer depth term) protocol = do
      let (index, counted) = numbered protocol
          stem :: String
          stem = labelled protocol answer
          naming :: String
          naming = printf "%s.%d" stem index
      (protocol', value) <- valued counted naming term
      pure (protocol', Just (indented depth (printf "%s := %s  # 𝕄(%s.%d)" naming value stem (Map.findWithDefault 1 (opener protocol) protocol._built))))
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
    outer :: Int -> Protocol -> Protocol
    outer depth protocol = protocol{_open = dropWhile ((>= depth) . fst) protocol._open, _answered = dropWhile (\(level, _, _) -> level > depth) protocol._answered}
    labelled :: Protocol -> T.Text -> String
    labelled protocol spelling = printf "%s.%d" (T.unpack spelling) (opener protocol)
    opener :: Protocol -> Int
    opener protocol = maybe 0 snd (listToMaybe protocol._open)
    numbered :: Protocol -> (Int, Protocol)
    numbered protocol = (index, protocol{_counted = Map.insert (opener protocol) index protocol._counted})
      where
        index :: Int
        index = maybe 1 (+ 1) (Map.lookup (opener protocol) protocol._counted)

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
saveEvalXml handle cursor printed report = do
  written <- atomicModify cursor (opened' . flushed . beside report . outer (tier report))
  mapM_ (hPutStrLn handle) written
  logDebug (printf "Saved %d line(s) of the XML protocol" (length written))
  where
    render :: Expression -> IO String
    render term = do
      nesting <- readIORef cursor
      printed (abbreviated nesting._objects term)
    located :: Nesting -> Expression -> IO String
    located nesting site = case [naming | (_, built, naming) <- nesting._sources, built == site] of
      naming : _ -> pure naming
      [] -> render site
    opened' :: (Nesting, [String]) -> IO (Nesting, [String])
    opened' (nesting, openings) = bimap (forgotten report) (openings ++) <$> elements report nesting
    beside :: Evaluation -> Nesting -> Nesting
    beside EvStarted{} nesting = nesting
    beside EvComputed{} nesting = nesting
    beside report' nesting = nesting{_heads = dropWhile ((>= tier report') . fst) nesting._heads, _pending = dropWhile (\(level, _, _) -> level >= tier report') nesting._pending}
    flushed :: Nesting -> (Nesting, [String])
    flushed nesting
      | silent report = (nesting, [])
      | otherwise = (nesting{_pending = [], _closing = [(level, element) | (level, element, _) <- nesting._pending] ++ nesting._closing}, [opening | (_, _, opening) <- reverse nesting._pending])
    silent :: Evaluation -> Bool
    silent EvStarted{} = True
    silent EvComputed{} = True
    silent _ = False
    forgotten :: Evaluation -> Nesting -> Nesting
    forgotten EvDelta{} nesting = nesting
    forgotten _ nesting = nesting{_held = Nothing}
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
      locator <- located nesting site
      pure
        ( nesting
            { _fires = fires
            , _openedAt = (depth, fires) : nesting._openedAt
            , _closing = (depth, "evaluate") : kept
            }
        , closers ++ [indentedXml depth (printf "<evaluate λ=\"%s\" by=\"%s\" at=\"%s\">" (quoted key) (opened judgment) (escapeXML locator))]
        )
      where
        (kept, closers) = closed depth nesting._closing
        fires :: Int
        fires = nesting._fires + 1
    elements (EvStarted depth judgment site) nesting = do
      locator <- located nesting site
      let opening = indentedXml depth (printf "<%s at=\"%s\">" (opened judgment) (escapeXML locator))
          inner = dropWhile ((> depth) . fst) nesting._heads
          pending = dropWhile (\(level, _, _) -> level > depth) nesting._pending
          (kept, closers) = closed depth nesting._closing
      pure $ case inner of
        (level, opening') : _ | level == depth && opening' == opening -> (nesting{_heads = inner, _pending = pending}, [])
        _ -> (nesting{_heads = (depth, opening) : dropWhile ((>= depth) . fst) inner, _pending = (depth, opened judgment, opening) : dropWhile (\(level, _, _) -> level >= depth) pending, _closing = kept}, closers)
    elements (EvLooped depth judgment mode self site answered) nesting = do
      form <- render self
      locator <- located nesting site
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
      locator <- located nesting site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<starved limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) (escapeXML locator))])
    elements (EvTimeout depth limit judgment site) nesting = do
      locator <- located nesting site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<timeout limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) (escapeXML locator))])
    elements (EvSpent depth limit judgment site) nesting = do
      locator <- located nesting site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<spent limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) (escapeXML locator))])
    elements (EvDelta depth bytes) nesting = do
      datum <- render (ExBytes bytes)
      let index = maybe 1 (+ 1) (Map.lookup (opener nesting) nesting._datums)
          naming :: String
          naming = printf "%s.%d" (labelled nesting sigil) index
          (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept, _datums = Map.insert (opener nesting) index nesting._datums, _held = Just (bytes, naming)}, closers ++ [indentedXml depth (printf "<delta meta=\"%s\"%s>%s</delta>" (escapeXML naming) (maybe "" (printf " number=\"%s\"" . escapeXML) (ieee bytes) :: String) (escapeXMLText datum))])
    elements (EvData depth spelling _ value) nesting = do
      record <- maybe (stood value) (pure . printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting spelling)) . escapeXMLText) (found value nesting._held)
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth record])
      where
        (kept, closers) = closed depth nesting._closing
        stood :: Either Int Bytes -> IO String
        stood (Left symbol) = do
          form <- render (standing symbol)
          pure (printf "<dataize meta=\"%s\">%s</dataize>" (escapeXML (labelled nesting spelling)) (escapeXMLText form))
        stood (Right bytes) = do
          form <- render (ExBytes bytes)
          pure (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting spelling)) (escapeXMLText form))
    elements (EvTerm depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting spelling)) (escapeXMLText body))])
    elements (EvSymbolize depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting spelling)) (escapeXMLText body))])
    elements (EvKnown depth symbol bytes) nesting = do
      value <- render (ExBytes bytes)
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<known symbol=\"%s\">%s</known>" (sigma symbol) (escapeXMLText value))])
    elements (EvJoin depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting spelling)) (escapeXMLText body))])
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
      locator <- located nesting site
      (origin, given) <- maybe (pure ("", "")) called call
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indentedXml depth (printf "<deferred symbol=\"%s\" by=\"%s\" at=\"%s\"%s>%s<e>%s</e></deferred>" (sigma symbol) (opened judgment) (escapeXML locator) origin given (escapeXMLText form))])
    elements (EvApplied depth judgment call object site) nesting = do
      (origin, given) <- parted (abbreviatedInside nesting._objects call)
      locator <- located nesting site
      let (index, counted) = numbered nesting
          (kept, closers) = closed depth nesting._closing
          naming :: String
          naming = printf "%s.%d" (labelled nesting answer) index
          aliased :: Expression
          aliased = alias (opener nesting) index
      pure (counted{_closing = kept, _objects = namedInsert call aliased (namedInsert object aliased counted._objects)}, closers ++ [indentedXml depth (printf "<applied meta=\"%s\" by=\"%s\" at=\"%s\" of=\"%s\">%s</applied>" (escapeXML naming) (opened judgment) (escapeXML locator) (escapeXML origin) given)])
    elements (EvComputed _ before after) nesting = pure (nesting{_objects = namedCarry before after nesting._objects}, [])
    elements (EvBuilt depth term) nesting = do
      body <- render term
      let (index, counted) = numbered nesting
          (kept, closers) = closed depth nesting._closing
          naming :: String
          naming = printf "%s.%d" (labelled nesting answer) index
      pure (counted{_closing = kept, _sources = (depth, term, naming) : counted._sources}, closers ++ [indentedXml depth (printf "<built meta=\"%s\">%s</built>" (escapeXML naming) (escapeXMLText body))])
    elements (EvAnswer depth term) nesting = do
      body <- render term
      let (index, counted) = numbered nesting
          (kept, closers) = closed depth nesting._closing
          naming :: String
          naming = printf "%s.%d" (labelled nesting answer) index
      pure (counted{_closing = kept}, closers ++ [indentedXml depth (printf "<answer meta=\"%s\">%s</answer>" (escapeXML naming) (escapeXMLText body))])
    outer :: Int -> Nesting -> Nesting
    outer depth nesting = nesting{_openedAt = dropWhile ((>= depth) . fst) nesting._openedAt, _sources = dropWhile (\(level, _, _) -> level > depth) nesting._sources}
    labelled :: Nesting -> T.Text -> String
    labelled nesting spelling = printf "%s.%d" (T.unpack spelling) (opener nesting)
    opener :: Nesting -> Int
    opener nesting = maybe 0 snd (listToMaybe nesting._openedAt)
    numbered :: Nesting -> (Int, Nesting)
    numbered nesting = (index, nesting{_numbered = Map.insert (opener nesting) index nesting._numbered})
      where
        index :: Int
        index = maybe 1 (+ 1) (Map.lookup (opener nesting) nesting._numbered)
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
    parted :: Expression -> IO (String, String)
    parted (ExApplication term (ArTau attr value)) = do
      origin <- printed term
      given <- argued value
      pure (origin, printf "<attr name=\"%s\">%s</attr>" (escapeXML (printAttribute attr)) (escapeXMLText given))
    parted term = (,"") <$> printed term
    argued :: Expression -> IO String
    argued (ExFormation [BiLambda (FnSymbol idx)]) = pure (sigma idx)
    argued term = printed term
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

sigil :: T.Text
sigil = "𝛿"

found :: Either Int Bytes -> Maybe (Bytes, String) -> Maybe String
found (Right bytes) (Just (held, naming))
  | bytes == held = Just naming
found _ _ = Nothing

alias :: Int -> Int -> Expression
alias firing index = ExMeta (T.pack (printf "n.%d.%d" firing index))

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
    sited (EvStarted _ Dataization site) = Just (\progress -> progress{_formations = progress._formations + 1}, site)
    sited _ = Nothing
    reported :: (Progress -> Progress, Expression) -> IO ()
    reported (counted, site) = do
      now <- getMonotonicTime
      progress <- counted <$> readIORef cursor
      if maybe True (\told -> now - told >= interval) progress._told
        then do
          locator <- render site
          logInfo (printf "Started %d dataizations and fired %d λ functions in %.0fs, now at %s" progress._formations progress._firings (now - progress._began) locator)
          writeIORef cursor progress{_told = Just now}
        else writeIORef cursor progress
