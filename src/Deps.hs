{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Deps where

import AST
import Bytes (btsSize, btsToNum)
import Control.Monad (foldM, guard, unless, when, zipWithM, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify, put, runStateT)
import Data.Bifunctor (bimap, first, second)
import Data.Either (lefts, rights)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Files (overwrite)
import GHC.Clock (getMonotonicTime)
import Logger (logDebug, logInfo)
import Matcher
import Printer (printAlpha, printAttribute, printFunction)
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

data Side
  = LeftSide
  | RightSide
  deriving (Eq, Ord, Show)

sided :: Side -> String
sided LeftSide = "left"
sided RightSide = "right"

data Evaluation
  = EvRun Judgment T.Text
  | EvFiring Int T.Text Judgment Expression
  | EvStarted Int Judgment Expression
  | EvDelta Int Bytes
  | EvManufactured Int Int
  | EvLooped Int Judgment Acyclic Expression Expression
  | EvStuck Int T.Text Judgment Expression
  | EvStall Int T.Text
  | EvStuckOn Int T.Text
  | EvStarved Int Int Judgment Expression
  | EvTimeout Int Int Judgment Expression
  | EvSpent Int Int Judgment Expression
  | EvData Int T.Text Expression (Either Int Bytes)
  | EvTerm Int T.Text Judgment Expression Expression
  | EvBound Int T.Text Expression Expression
  | EvReshaped Int T.Text T.Text Expression Expression
  | EvSymbolized Int Int Bytes
  | EvMinted Int Int [Either Int Bytes]
  | EvDeferred Int Int Judgment Expression (Maybe Expression) Expression
  | EvApplied Int Judgment Expression Expression Expression
  | EvBuilt Int Expression
  | EvAnswer Int Expression
  | EvMorphed Int Expression
  | EvForked Int (Either Int Bytes) Side Expression
  | EvJoined Int (Either Int Bytes) Int (Int, Int)
  | EvTaken Int [Expression] Expression
  | EvWorld Expression

type SaveEvalFunc = Evaluation -> IO ()

type RewindEvalFunc = IO (IO ())

resited :: Expression -> Expression -> Evaluation -> Evaluation
resited from to = record
  where
    record :: Evaluation -> Evaluation
    record (EvFiring depth key judgment site) = EvFiring depth key judgment (moved site)
    record (EvStarted depth judgment site) = EvStarted depth judgment (moved site)
    record (EvLooped depth judgment mode self site) = EvLooped depth judgment mode self (moved site)
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
tier (EvManufactured depth _) = depth
tier (EvLooped depth _ _ _ _) = depth
tier (EvStuck depth _ _ _) = depth
tier (EvStall depth _) = depth
tier (EvStuckOn depth _) = depth
tier (EvStarved depth _ _ _) = depth
tier (EvTimeout depth _ _ _) = depth
tier (EvSpent depth _ _ _) = depth
tier (EvData depth _ _ _) = depth
tier (EvTerm depth _ _ _ _) = depth
tier (EvBound depth _ _ _) = depth
tier (EvReshaped depth _ _ _ _) = depth
tier (EvSymbolized depth _ _) = depth
tier (EvMinted depth _ _) = depth
tier (EvDeferred depth _ _ _ _ _) = depth
tier (EvApplied depth _ _ _ _) = depth
tier (EvBuilt depth _) = depth
tier (EvAnswer depth _) = depth
tier (EvMorphed depth _) = depth
tier (EvForked depth _ _ _) = depth
tier (EvJoined depth _ _ _) = depth
tier (EvTaken depth _ _) = depth
tier EvWorld{} = maxBound

type Named a = Map.Map Int [(Expression, a)]

namedLookup :: Expression -> Named a -> Maybe a
namedLookup term names = lookup term (Map.findWithDefault [] (hashExpression term) names)

namedInsert :: forall a. Expression -> a -> Named a -> Named a
namedInsert term naming = Map.alter renamed (hashExpression term)
  where
    renamed :: Maybe [(Expression, a)] -> Maybe [(Expression, a)]
    renamed entries = Just ((term, naming) : filter ((/= term) . fst) (fromMaybe [] entries))

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
  , _found :: Maybe (Either Int Bytes, String)
  , _blocks :: [(Int, Maybe String, String, Bool)]
  , _answered :: [(Int, Expression, String)]
  }

emptyProtocol :: Protocol
emptyProtocol = Protocol 0 Map.empty [] False Map.empty Map.empty Map.empty Map.empty Nothing [] []

data Nesting = Nesting
  { _fires :: Int
  , _openedAt :: [(Int, Int)]
  , _closing :: [(Int, String, String, Bool)]
  , _objects :: Named Expression
  , _numbered :: Map.Map Int Int
  , _datums :: Map.Map Int Int
  , _held :: Maybe (Either Int Bytes, String)
  , _heads :: [(Int, (Judgment, Expression))]
  , _pending :: [(Int, Judgment, Expression)]
  , _sources :: [(Int, Expression, String)]
  , _world :: Map.Map Attribute Expression
  , _shapes :: Map.Map [Maybe Attribute] [(Expression, [Binding])]
  , _opaque :: Int
  }

type Naming = StateT (Nesting, [String]) IO

emptyNesting :: Nesting
emptyNesting = Nesting 0 [] [] Map.empty Map.empty Map.empty Nothing [] [] [] Map.empty Map.empty 0

saveEval :: Handle -> IORef Protocol -> (Expression -> IO String) -> (Expression -> IO String) -> SaveEvalFunc
saveEval handle cursor printed printed' report = do
  made <- atomicModify cursor (fmap (first (forgotten report) . headed) . written report . beside report . outer (tier report))
  mapM_ saved made
  where
    forgotten :: Evaluation -> Protocol -> Protocol
    forgotten EvDelta{} protocol = protocol
    forgotten EvWorld{} protocol = protocol
    forgotten _ protocol = protocol{_found = Nothing}
    beside :: Evaluation -> Protocol -> Protocol
    beside EvStarted{} protocol = protocol
    beside EvMinted{} protocol = protocol
    beside report' protocol = protocol{_blocks = dropWhile (\(level, _, _, _) -> level >= tier report') protocol._blocks}
    headed :: (Protocol, Maybe String) -> (Protocol, [String])
    headed (protocol, Nothing) = (protocol, [])
    headed (protocol, Just line) =
      ( protocol{_blocks = [(level, heading, opening, True) | (level, heading, opening, _) <- protocol._blocks]}
      , [opening | (_, _, opening, False) <- reverse protocol._blocks] ++ lines line
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
      (_, heading, _, _) : _ -> heading
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
      pure (protocol{_begun = True, _blocks = [(0, Just heading, heading ++ ":", True)]}, Just (heading ++ ":"))
      where
        heading :: String
        heading = printf "%s(%s)" (letter judgment) (T.unpack locator)
    written (EvFiring depth key judgment site) protocol = do
      opening <- indented depth . remarked (printf "𝔼(%s):" (T.unpack key)) <$> context protocol judgment site
      pure
        ( protocol
            { _fired = firings
            , _open = (depth, firings) : protocol._open
            , _blocks = (depth, Nothing, opening, True) : protocol._blocks
            }
        , Just opening
        )
      where
        firings :: Int
        firings = protocol._fired + 1
    written (EvStarted depth judgment site) protocol = do
      locator <- located protocol site
      let heading = printf "%s(%s)" (letter judgment) locator
          inner = dropWhile (\(level, _, _, _) -> level > depth) protocol._blocks
      pure $ case inner of
        (level, Just heading', _, _) : _ | level == depth && heading' == heading -> (protocol{_blocks = inner}, Nothing)
        _ -> (protocol{_blocks = (depth, Just heading, indented depth (heading ++ ":"), False) : dropWhile (\(level, _, _, _) -> level >= depth) inner}, Nothing)
    written (EvDelta depth bytes) protocol = do
      datum <- render (ExBytes bytes)
      let index = maybe 1 (+ 1) (Map.lookup (opener protocol) protocol._deltas)
          naming :: String
          naming = printf "%s·%d" (labelled protocol sigil) index
      pure (protocol{_deltas = Map.insert (opener protocol) index protocol._deltas, _found = Just (Right bytes, naming)}, Just (indented depth (remarked (printf "%s := %s" naming datum) (maybe [] pure (ieee bytes)))))
    written EvManufactured{} protocol = pure (protocol, Nothing)
    written (EvLooped depth judgment mode self site) protocol = do
      form <- render self
      remarks <- context protocol judgment site
      pure (protocol, Just (indented depth (remarked (printf "looped(%s)" form) (remarks ++ [certainty mode]))))
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
      datum <- maybe (spelt value) pure (found value protocol._found)
      line <- commented (printf "%s := %s" (labelled protocol spelling) datum) Dataization operand
      pure (protocol, Just (indented depth line))
    written (EvTerm depth spelling judgment operand term) protocol = do
      let naming = labelled protocol spelling
      (protocol', value) <- valued protocol naming term
      line <- commented (printf "%s := %s" naming value) judgment operand
      pure (protocol', Just (indented depth line))
    written (EvBound depth spelling source term) protocol = do
      let naming = labelled protocol spelling
      (protocol', value) <- valued protocol naming term
      line <- commented' (printf "%s := %s" naming value) source
      pure (protocol', Just (indented depth line))
    written (EvReshaped depth spelling block source term) protocol = do
      let naming = labelled protocol spelling
      (protocol', value) <- valued protocol naming term
      line <- printf "%s := %s  # %s(%s)" naming value (T.unpack block) <$> salted source
      pure (protocol', Just (indented depth line))
    written (EvSymbolized depth symbol bytes) protocol = do
      datum <- render (ExBytes bytes)
      pure (protocol, Just (indented depth (remarked (printf "symbolized(%s) := %s" (printFunction (FnSymbol symbol)) datum) (maybe [] pure (ieee bytes)))))
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
      pure (counted{_made = namedInsert call aliased (namedInsert object aliased counted._made)}, Just (indented depth (remarked (printf "%s·%d := %s" (labelled protocol answer) index form) remarks)))
    written (EvBuilt depth term) protocol = do
      let (index, counted) = numbered protocol
      value <- borrowed protocol term
      let naming = printf "%s·%d" (labelled protocol answer) index
      pure (counted{_built = Map.insert (opener protocol) index counted._built, _answered = (depth, term, naming) : counted._answered}, Just (indented depth (printf "%s := %s  # %s" naming value (T.unpack answer))))
    written (EvAnswer depth term) protocol = do
      let (index, counted) = numbered protocol
          stem :: String
          stem = labelled protocol answer
          naming :: String
          naming = printf "%s·%d" stem index
      (protocol', value) <- valued counted naming term
      pure (protocol', Just (indented depth (printf "%s := %s  # 𝕄(%s·%d)" naming value stem (Map.findWithDefault 1 (opener protocol) protocol._built))))
    written EvMorphed{} protocol = pure (protocol, Nothing)
    written (EvForked depth condition side chosen) protocol = do
      cond <- spelt condition
      line <- commented' (printf "forked(%s) := %s" cond (sided side)) chosen
      pure (protocol, Just (indented depth line))
    written (EvJoined depth condition symbol (left, right)) protocol = do
      cond <- spelt condition
      pure (protocol, Just (indented depth (printf "joined(%s) := %s ? %s : %s" (printFunction (FnSymbol symbol)) cond (printFunction (FnSymbol left)) (printFunction (FnSymbol right)))))
    written (EvTaken depth sides term) protocol = do
      let (index, counted) = numbered protocol
          naming :: String
          naming = printf "%s·%d" (labelled protocol answer) index
      (protocol', value) <- valued counted naming term
      remarks <- mapM sourced sides
      pure (protocol', Just (indented depth (remarked (printf "%s := %s" naming value) remarks)))
    written EvWorld{} protocol = pure (protocol, Nothing)
    spelt :: Either Int Bytes -> IO String
    spelt (Left symbol) = printf "𝔻(%s)" <$> render (standing symbol)
    spelt (Right bytes) = render (ExBytes bytes)
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
    sourced :: Expression -> IO String
    sourced side@(ExMeta _) = salted side
    sourced side = printf "%s(%s)" (letter Morphing) <$> salted side
    outer :: Int -> Protocol -> Protocol
    outer depth protocol = protocol{_open = dropWhile ((>= depth) . fst) protocol._open, _answered = dropWhile (\(level, _, _) -> level > depth) protocol._answered}
    labelled :: Protocol -> T.Text -> String
    labelled protocol spelling = printf "%s·%d" (T.unpack spelling) (opener protocol)
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

rewindEval :: IORef Protocol -> RewindEvalFunc
rewindEval cursor = do
  marked <- readIORef cursor
  pure $
    modifyIORef' cursor $ \protocol ->
      marked
        { _fired = protocol._fired
        , _counted = protocol._counted
        , _deltas = protocol._deltas
        , _blocks = [(level, heading, opening, False) | (level, heading, opening, _) <- marked._blocks]
        }

milliseconds :: Double -> Double -> Int
milliseconds began now = round ((now - began) * 1000)

perSecond :: Int -> Int -> Int
perSecond firings taken = round (fromIntegral firings * 1000 / fromIntegral (max 1 taken) :: Double)

saveEvalXml :: Handle -> IORef Nesting -> (Expression -> IO String) -> SaveEvalFunc
saveEvalXml handle cursor printed report = do
  written <- atomicModify cursor (flushed . beside report . outer (tier report) >=> opened' >=> reopened)
  mapM_ (hPutStrLn handle) written
  logDebug (printf "Saved %d line(s) of the XML protocol" (length written))
  where
    opened' :: (Nesting, [String]) -> IO (Nesting, [String])
    opened' (nesting, openings) = bimap (forgotten report) (openings ++) <$> elements report nesting
    reopened :: (Nesting, [String]) -> IO (Nesting, [String])
    reopened (nesting, []) = pure (nesting, [])
    reopened (nesting, lines') =
      pure
        ( nesting{_closing = [(level, element, opening, True) | (level, element, opening, _) <- nesting._closing]}
        , [opening | (_, _, opening, False) <- reverse nesting._closing] ++ lines'
        )
    beside :: Evaluation -> Nesting -> Nesting
    beside EvStarted{} nesting = nesting
    beside report' nesting = nesting{_heads = dropWhile ((>= tier report') . fst) nesting._heads, _pending = dropWhile (\(level, _, _) -> level >= tier report') nesting._pending}
    flushed :: Nesting -> IO (Nesting, [String])
    flushed nesting
      | silent report = pure (nesting, [])
      | otherwise = foldM pended (nesting{_pending = []}, []) (reverse nesting._pending)
    pended :: (Nesting, [String]) -> (Int, Judgment, Expression) -> IO (Nesting, [String])
    pended (nesting, openings) (level, judgment, site) = do
      (nesting', lines') <- staged level nesting (printf "<%s at=\"%s\">" (opened judgment) . escapeXML <$> located level site)
      pure (nesting'{_closing = (level, opened judgment, last lines', True) : nesting'._closing}, openings ++ lines')
    silent :: Evaluation -> Bool
    silent EvStarted{} = True
    silent EvWorld{} = True
    silent _ = False
    forgotten :: Evaluation -> Nesting -> Nesting
    forgotten EvDelta{} nesting = nesting
    forgotten EvManufactured{} nesting = nesting
    forgotten EvWorld{} nesting = nesting
    forgotten _ nesting = nesting{_held = Nothing}
    staged :: Int -> Nesting -> Naming String -> IO (Nesting, [String])
    staged depth nesting naming = do
      (element, (nesting', records)) <- runStateT naming (nesting, [])
      pure (nesting', map (indentedXml depth) (reverse (element : records)))
    placed :: Int -> Nesting -> Naming String -> IO (Nesting, [String])
    placed depth nesting naming = second (closers ++) <$> staged depth nesting{_closing = kept} naming
      where
        (kept, closers) = closed depth nesting._closing
    elements :: Evaluation -> Nesting -> IO (Nesting, [String])
    elements (EvRun judgment locator) nesting =
      pure
        ( nesting{_closing = (0, opened judgment, run, True) : (-1, "path", path, True) : (if null nesting._closing then [(-2, "protocol", "<protocol>", True)] else kept)}
        , ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>" | null nesting._closing]
            ++ ["<protocol>" | null nesting._closing]
            ++ closers
            ++ [path, run]
        )
      where
        (kept, closers) = closed (-1) nesting._closing
        path :: String
        path = indentedXml (-1) "<path>"
        run :: String
        run = indentedXml 0 (printf "<%s at=\"%s\">" (opened judgment) (quoted locator))
    elements (EvFiring depth key judgment site) nesting = do
      (fired, lines') <- placed depth nesting (printf "<evaluate λ=\"%s\" by=\"%s\" at=\"%s\">" (quoted key) (opened judgment) . escapeXML <$> located depth site)
      pure (fired{_fires = fired._fires + 1, _openedAt = (depth, fired._fires + 1) : fired._openedAt, _closing = (depth, "evaluate", last lines', True) : fired._closing}, lines')
    elements (EvStarted depth judgment site) nesting =
      pure $ case inner of
        (level, opening) : _ | level == depth && opening == (judgment, site) -> (nesting{_heads = inner, _pending = pending}, [])
        _ -> (nesting{_heads = (depth, (judgment, site)) : dropWhile ((>= depth) . fst) inner, _pending = (depth, judgment, site) : dropWhile (\(level, _, _) -> level >= depth) pending, _closing = kept}, closers)
      where
        inner :: [(Int, (Judgment, Expression))]
        inner = dropWhile ((> depth) . fst) nesting._heads
        pending :: [(Int, Judgment, Expression)]
        pending = dropWhile (\(level, _, _) -> level > depth) nesting._pending
        (kept, closers) = closed depth nesting._closing
    elements (EvLooped depth judgment mode self site) nesting =
      placed depth nesting $ do
        locator <- located depth site
        origin <- sole depth self
        pure (enclosed "looped" (printf "<looped by=\"%s\" match=\"%s\" at=\"%s\"" (opened judgment) (certainty mode) (escapeXML locator)) origin)
    elements (EvStuck depth key judgment self) nesting =
      placed depth nesting (printf "<unanswered λ=\"%s\" by=\"%s\">%s</unanswered>" (quoted key) (opened judgment) . escapeXMLText <$> leaf depth self)
    elements (EvStall depth key) nesting =
      placed depth nesting (pure (printf "<stalled λ=\"%s\"/>" (quoted key)))
    elements (EvStuckOn depth key) nesting =
      placed depth nesting (pure (printf "<stuck λ=\"%s\"/>" (quoted key)))
    elements (EvStarved depth limit judgment site) nesting =
      placed depth nesting (printf "<starved limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) . escapeXML <$> located depth site)
    elements (EvTimeout depth limit judgment site) nesting =
      placed depth nesting (printf "<timeout limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) . escapeXML <$> located depth site)
    elements (EvSpent depth limit judgment site) nesting =
      placed depth nesting (printf "<spent limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) . escapeXML <$> located depth site)
    elements (EvDelta depth bytes) nesting = do
      datum <- printed (ExBytes bytes)
      let (naming, nesting') = stored (Right bytes) nesting
      placed depth nesting' (pure (printf "<dataized meta=\"%s\"%s>%s</dataized>" (escapeXML naming) (maybe "" (printf " number=\"%s\"" . escapeXML) (ieee bytes) :: String) (escapeXMLText datum)))
    elements (EvManufactured depth symbol) nesting = placed depth nesting' (pure (printf "<manufactured meta=\"%s\">%s</manufactured>" (escapeXML naming) (sigma symbol)))
      where
        (naming, nesting') = stored (Left symbol) nesting
    elements (EvData depth spelling _ value) nesting =
      placed depth nesting $ case (found value nesting._held, value) of
        (Just naming, _) -> pure (bound (escapeXMLText naming))
        (Nothing, Left symbol) -> pure (bound (sigma symbol))
        (Nothing, Right bytes) -> bound . escapeXMLText <$> lift (printed (ExBytes bytes))
      where
        bound :: String -> String
        bound = printf "<bound meta=\"%s\">%s</bound>" (escapeXML (labelled nesting spelling))
    elements (EvTerm depth spelling _ _ term) nesting = placed depth nesting (bind depth nesting spelling term)
    elements (EvBound depth spelling _ term) nesting = placed depth nesting (bind depth nesting spelling term)
    elements (EvReshaped depth spelling _ _ term) nesting = placed depth nesting (bind depth nesting spelling term)
    elements (EvSymbolized depth symbol bytes) nesting = do
      datum <- printed (ExBytes bytes)
      placed depth nesting (pure (printf "<symbolized symbol=\"%s\"%s>%s</symbolized>" (sigma symbol) (maybe "" (printf " number=\"%s\"" . escapeXML) (ieee bytes) :: String) (escapeXMLText datum)))
    elements (EvMinted depth symbol operands) nesting = do
      spelledOperands <- mapM spelled' operands
      placed depth nesting (pure (minted spelledOperands))
      where
        spelled' :: Either Int Bytes -> IO String
        spelled' (Left fresh') = pure (sigma fresh')
        spelled' (Right bytes) = printed (ExBytes bytes)
        minted :: [String] -> String
        minted [] = printf "<minted symbol=\"%s\"/>" (sigma symbol)
        minted spelt = printf "<minted symbol=\"%s\">%s</minted>" (sigma symbol) (escapeXMLText (unwords spelt))
    elements (EvDeferred depth symbol judgment copy call site) nesting =
      placed depth nesting $ do
        locator <- located depth site
        origin <- maybe (sole depth copy) (called depth) call
        pure (enclosed "deferred" (printf "<deferred symbol=\"%s\" by=\"%s\" at=\"%s\"" (sigma symbol) (opened judgment) (escapeXML locator)) origin)
    elements (EvApplied depth judgment call object site) nesting =
      placed depth nesting $ do
        locator <- located depth site
        (origin, given) <- parted depth call
        (naming, aliased) <- counted
        modify (first (\nesting' -> nesting'{_objects = namedInsert call aliased (namedInsert object aliased nesting'._objects)}))
        pure (printf "<applied meta=\"%s\" by=\"%s\" at=\"%s\" of=\"%s\">%s</applied>" (escapeXML naming) (opened judgment) (escapeXML locator) (escapeXML origin) given)
    elements (EvBuilt depth term) nesting =
      placed depth nesting $ do
        body <- leaf depth term
        (naming, _) <- counted
        modify (first (\nesting' -> nesting'{_sources = (depth, term, naming) : nesting'._sources}))
        pure (printf "<built meta=\"%s\">%s</built>" (escapeXML naming) (escapeXMLText body))
    elements (EvAnswer depth term) nesting = placed depth nesting (concluded "answered" depth term)
    elements (EvMorphed depth term) nesting = placed depth nesting (concluded "morphed" depth term)
    elements (EvForked depth condition side _) nesting =
      placed depth nesting $ case condition of
        Left symbol -> pure (forked (sigma symbol))
        Right bytes -> forked . escapeXMLText <$> lift (printed (ExBytes bytes))
      where
        forked :: String -> String
        forked = printf "<forked side=\"%s\">%s</forked>" (sided side)
    elements (EvJoined depth condition symbol (left, right)) nesting =
      placed depth nesting $ case condition of
        Left symbol' -> pure (joined (sigma symbol'))
        Right bytes -> joined . escapeXMLText <$> lift (printed (ExBytes bytes))
      where
        joined :: String -> String
        joined = printf "<joined symbol=\"%s\" left=\"%s\" right=\"%s\">%s</joined>" (sigma symbol) (sigma left) (sigma right)
    elements (EvTaken depth _ term) nesting = placed depth nesting (concluded "answered" depth term)
    elements (EvWorld world) nesting = pure (known world nesting, [])
    outer :: Int -> Nesting -> Nesting
    outer depth nesting = nesting{_openedAt = dropWhile ((>= depth) . fst) nesting._openedAt, _sources = dropWhile (\(level, _, _) -> level > depth) nesting._sources}
    labelled :: Nesting -> T.Text -> String
    labelled nesting spelling = printf "%s·%d" (T.unpack spelling) (opener nesting)
    stored :: Either Int Bytes -> Nesting -> (String, Nesting)
    stored value nesting = (naming, nesting{_datums = Map.insert (opener nesting) index nesting._datums, _held = Just (value, naming)})
      where
        index :: Int
        index = maybe 1 (+ 1) (Map.lookup (opener nesting) nesting._datums)
        naming :: String
        naming = printf "%s·%d" (labelled nesting sigil) index
    opener :: Nesting -> Int
    opener nesting = maybe 0 snd (listToMaybe nesting._openedAt)
    numbered :: Nesting -> (Int, Nesting)
    numbered nesting = (index, nesting{_numbered = Map.insert (opener nesting) index nesting._numbered})
      where
        index :: Int
        index = maybe 1 (+ 1) (Map.lookup (opener nesting) nesting._numbered)
    counted :: Naming (String, Expression)
    counted = do
      (nesting, records) <- get
      let (index, nesting') = numbered nesting
      put (nesting', records)
      pure (printf "%s·%d" (labelled nesting answer) index, alias (opener nesting) index)
    recorded :: Expression -> (String -> String) -> Naming Expression
    recorded term element = do
      (naming, aliased) <- counted
      modify (bimap (\nesting -> nesting{_objects = namedInsert term aliased nesting._objects}) (element (escapeXML naming) :))
      pure aliased
    opaque :: Expression -> Naming Expression
    opaque term = do
      (nesting, records) <- get
      let meta = ExMeta (T.pack (printf "e%d" (nesting._opaque + 1)))
      put (nesting{_opaque = nesting._opaque + 1, _objects = namedInsert term meta nesting._objects}, records)
      pure meta
    located :: Int -> Expression -> Naming String
    located depth site = do
      (nesting, _) <- get
      maybe (leaf depth site) pure (listToMaybe [naming | (_, built, naming) <- nesting._sources, built == site])
    concluded :: String -> Int -> Expression -> Naming String
    concluded tag depth term = do
      body <- leaf depth term
      (naming, _) <- counted
      pure (printf "<%s meta=\"%s\">%s</%s>" tag (escapeXML naming) (escapeXMLText body) tag)
    bind :: Int -> Nesting -> T.Text -> Expression -> Naming String
    bind depth nesting spelling term = printf "<bound meta=\"%s\">%s</bound>" (escapeXML (labelled nesting spelling)) . escapeXMLText <$> leaf depth term
    leaf :: Int -> Expression -> Naming String
    leaf depth term = titled depth term >>= lift . spelled
    titled :: Int -> Expression -> Naming Expression
    titled depth term = do
      (nesting, _) <- get
      maybe (coined depth term) pure (namedLookup term nesting._objects)
    coined :: Int -> Expression -> Naming Expression
    coined _ term@(ExFormation [BiLambda (FnSymbol _)]) = pure term
    coined _ term@(ExFormation [BiDelta _]) = pure term
    coined depth term@(ExFormation bds) = do
      (nesting, _) <- get
      maybe (anonymous depth term bds) (cloned depth term) (copied nesting bds)
    coined depth term@(ExDispatch inner attr) = do
      head' <- titled depth inner
      if pathed head' then pure (ExDispatch head' attr) else opaque term
    coined depth term@(ExApplication _ _) = do
      (nesting, _) <- get
      let (head', arguments) = chained nesting._objects term
      origin <- leaf depth head'
      given <- mapM (argument depth) arguments
      recorded term (\naming -> printf "<applied meta=\"%s\" of=\"%s\">%s</applied>" naming (escapeXML origin) (concat given))
    coined _ term
      | plain term = pure term
      | otherwise = opaque term
    cloned :: Int -> Expression -> (Expression, [(Attribute, Expression)]) -> Naming Expression
    cloned depth term (origin, []) = do
      path <- titled depth origin
      modify (first (\nesting -> nesting{_objects = namedInsert term path nesting._objects}))
      pure path
    cloned depth term (origin, bound) = do
      path <- leaf depth origin
      given <- mapM (\(attr, value) -> titled depth value >>= lift . attributed (printAttribute attr)) bound
      recorded term (\naming -> printf "<formed meta=\"%s\" of=\"%s\">%s</formed>" naming (escapeXML path) (concat given))
    anonymous :: Int -> Expression -> [Binding] -> Naming Expression
    anonymous depth term bds = do
      parts <- mapM (part depth) bds
      let marks = concat (lefts parts)
          inner = concat (rights parts)
      recorded term (\naming -> if null inner then printf "<formed meta=\"%s\"%s/>" naming marks else printf "<formed meta=\"%s\"%s>%s</formed>" naming marks inner)
    part :: Int -> Binding -> Naming (Either String String)
    part depth (BiTau attr value) = Right <$> (titled depth value >>= lift . attributed (printAttribute attr))
    part _ (BiVoid attr) = pure (Right (printf "<void attr=\"%s\"/>" (escapeXML (printAttribute attr))))
    part _ (BiLambda func) = pure (Left (printf " λ=\"%s\"" (escapeXML (printFunction func))))
    part _ (BiDelta bytes) = Left . printf " Δ=\"%s\"" . escapeXML <$> lift (printed (ExBytes bytes))
    part _ _ = pure (Right "")
    argument :: Int -> Argument -> Naming String
    argument depth (ArTau attr value) = titled depth value >>= lift . attributed (printAttribute attr)
    argument depth (ArAlpha alpha value) = titled depth value >>= lift . attributed (printAlpha alpha)
    attributed :: String -> Expression -> IO String
    attributed name ExTermination = pure (printf "<bottom attr=\"%s\"/>" (escapeXML name))
    attributed name value = tagged (kind value) . escapeXMLText <$> spelled value
      where
        tagged :: String -> String -> String
        tagged tag text = printf "<%s attr=\"%s\">%s</%s>" tag (escapeXML name) text tag
    kind :: Expression -> String
    kind (ExFormation [BiLambda (FnSymbol _)]) = "symbol"
    kind (ExFormation [BiDelta _]) = "datum"
    kind (ExBytes _) = "datum"
    kind (ExMeta _) = "meta"
    kind _ = "ref"
    chained :: Named Expression -> Expression -> (Expression, [Argument])
    chained objects (ExApplication inner arg) = case inner of
      ExApplication _ _ | Nothing <- namedLookup inner objects -> second (++ [arg]) (chained objects inner)
      _ -> (inner, [arg])
    chained _ term = (term, [])
    parted :: Int -> Expression -> Naming (String, String)
    parted depth (ExApplication term arg) = (,) <$> leaf depth term <*> argument depth arg
    parted depth term = (,"") <$> leaf depth term
    called :: Int -> Expression -> Naming (String, String)
    called depth term = do
      path <- leaf depth object
      given <- mapM passed arguments
      pure (path, printf "<with>%s</with>" (concat given))
      where
        object :: Expression
        arguments :: [(Attribute, Expression)]
        (object, arguments) = invoked term
    sole :: Int -> Expression -> Naming (String, String)
    sole depth term = (,"") <$> leaf depth term
    enclosed :: String -> String -> (String, String) -> String
    enclosed _ opening (origin, "") = printf "%s of=\"%s\"/>" opening (escapeXML origin)
    enclosed tag opening (origin, inner) = printf "%s of=\"%s\">%s</%s>" opening (escapeXML origin) inner tag
    invoked :: Expression -> (Expression, [(Attribute, Expression)])
    invoked (ExApplication term (ArTau attr value)) = second (++ [(attr, value)]) (invoked term)
    invoked term = (term, [])
    passed :: (Attribute, Expression) -> Naming String
    passed (attr, value) = valued value >>= lift . attributed (printAttribute attr)
    valued :: Expression -> Naming Expression
    valued value = do
      (nesting, _) <- get
      case (namedLookup value nesting._objects, symbolic value) of
        (Just aliased, _) -> pure aliased
        (Nothing, Just symbol) -> pure (standing symbol)
        (Nothing, Nothing) -> opaque value
    symbolic :: Expression -> Maybe Int
    symbolic (ExFormation [BiLambda (FnSymbol idx)]) = Just idx
    symbolic (ExFormation bds) = listToMaybe [body | BiTau AtPhi body <- bds] >>= symbolic
    symbolic (ExApplication _ (ArTau AtPhi body)) = symbolic body
    symbolic _ = Nothing
    spelled :: Expression -> IO String
    spelled (ExFormation [BiLambda (FnSymbol idx)]) = pure (sigma idx)
    spelled (ExFormation [BiDelta bytes]) = printed (ExBytes bytes)
    spelled (ExDispatch inner attr)
      | sigmatic inner = printf "%s.%s" <$> spelled inner <*> pure (printAttribute attr)
    spelled term = printed term
    sigmatic :: Expression -> Bool
    sigmatic (ExFormation [BiLambda (FnSymbol _)]) = True
    sigmatic (ExDispatch inner _) = sigmatic inner
    sigmatic _ = False
    known :: Expression -> Nesting -> Nesting
    known world@(ExFormation bds) nesting = nesting{_world = world', _shapes = Map.fromListWith (flip (++)) [(shape kept, [(path, kept)]) | (path, kept) <- breadth [(ExDispatch ExRoot attr, inner) | (attr, ExFormation inner) <- Map.toList world']], _objects = namedInsert world ExRoot nesting._objects}
      where
        world' :: Map.Map Attribute Expression
        world' = Map.union (Map.fromList [(attr, value) | BiTau attr value <- bds]) nesting._world
        breadth :: [(Expression, [Binding])] -> [(Expression, [Binding])]
        breadth [] = []
        breadth level = [(path, filter (not . rho) inner) | (path, inner) <- level] ++ breadth [(ExDispatch path attr, inner') | (path, inner) <- level, BiTau attr (ExFormation inner') <- inner, attr /= AtRho]
    known _ nesting = nesting
    shape :: [Binding] -> [Maybe Attribute]
    shape = map label
      where
        label :: Binding -> Maybe Attribute
        label (BiTau attr _) = Just attr
        label (BiVoid attr) = Just attr
        label _ = Nothing
    rho :: Binding -> Bool
    rho (BiTau AtRho _) = True
    rho (BiVoid AtRho) = True
    rho _ = False
    copied :: Nesting -> [Binding] -> Maybe (Expression, [(Attribute, Expression)])
    copied nesting bds = snd <$> listToMaybe (sortOn fst (mapMaybe matched candidates))
      where
        own :: [Binding]
        own = filter (not . rho) bds
        parents :: [Expression]
        parents = [value | BiTau AtRho value <- bds]
        candidates :: [(Expression, [Binding])]
        candidates = case parents of
          [] -> Map.findWithDefault [] (shape own) nesting._shapes
          value : _ -> [(ExDispatch value attr, filter (not . rho) origin) | BiTau attr (ExFormation origin) <- fromMaybe [] (declared value), attr /= AtRho]
        matched :: (Expression, [Binding]) -> Maybe (Int, (Expression, [(Attribute, Expression)]))
        matched (path, kept) = do
          guard (length kept == length own)
          given <- concat <$> zipWithM bound kept own
          guard (or (zipWith (==) kept own) || not (null parents) && length given == length [() | BiVoid _ <- kept])
          pure (length given, (path, given))
        declared :: Expression -> Maybe [Binding]
        declared ExRoot = Just [BiTau attr value | (attr, value) <- Map.toList nesting._world]
        declared (ExFormation inner) = Just inner
        declared (ExApplication target _) = declared target
        declared (ExDispatch target attr) = do
          outer' <- declared target
          listToMaybe [inner | BiTau attr' (ExFormation inner) <- outer', attr' == attr]
        declared _ = Nothing
        bound :: Binding -> Binding -> Maybe [(Attribute, Expression)]
        bound (BiVoid attr) (BiTau attr' value)
          | attr == attr' = Just [(attr, value)]
        bound (BiTau attr origin) (BiTau attr' value)
          | attr == attr' = Just [(attr, value) | origin /= value]
        bound origin binding
          | origin == binding = Just []
          | otherwise = Nothing
    pathed :: Expression -> Bool
    pathed ExRoot = True
    pathed ExXi = True
    pathed (ExMeta _) = True
    pathed (ExDispatch inner _) = pathed inner
    pathed term = sigmatic term
    plain :: Expression -> Bool
    plain ExRoot = True
    plain ExXi = True
    plain ExTermination = True
    plain (ExMeta _) = True
    plain (ExBytes _) = True
    plain _ = False
    sigma :: Int -> String
    sigma = printFunction . FnSymbol
    quoted :: T.Text -> String
    quoted = escapeXML . T.unpack

endEvalXml :: Handle -> IORef Nesting -> Double -> IO ()
endEvalXml handle cursor began = do
  nesting <- readIORef cursor
  mapM_ (hPutStrLn handle) (snd (closed (-1) nesting._closing))
  unless (null nesting._closing) $ do
    now <- getMonotonicTime
    let taken = milliseconds began now
    hPutStrLn handle (indentedXml (-1) (printf "<msec>%d</msec>" taken))
    hPutStrLn handle (indentedXml (-1) (printf "<firings>%d</firings>" nesting._fires))
    hPutStrLn handle (indentedXml (-1) (printf "<fps>%d</fps>" (perSecond nesting._fires taken)))
    hPutStrLn handle "</protocol>"
  writeIORef cursor nesting{_closing = []}

rewindEvalXml :: Handle -> IORef Nesting -> RewindEvalFunc
rewindEvalXml handle cursor = do
  marked <- readIORef cursor
  pure $ do
    nesting <- readIORef cursor
    mapM_ (hPutStrLn handle) (snd (closed (-1) nesting._closing))
    writeIORef
      cursor
      marked
        { _fires = nesting._fires
        , _numbered = nesting._numbered
        , _datums = nesting._datums
        , _opaque = nesting._opaque
        , _closing = [(level, element, opening, level < -1) | (level, element, opening, _) <- marked._closing]
        }

closed :: Int -> [(Int, String, String, Bool)] -> ([(Int, String, String, Bool)], [String])
closed depth open = (kept, [indentedXml level (printf "</%s>" element) | (level, element, _, True) <- shut])
  where
    (shut, kept) = span (\(level, _, _, _) -> level >= depth) open

indented :: Int -> String -> String
indented depth line = replicate (2 * depth) ' ' ++ line

indentedXml :: Int -> String -> String
indentedXml depth = indented (depth + 2)

atomicModify :: IORef a -> (a -> IO (a, b)) -> IO b
atomicModify ref action = readIORef ref >>= action >>= \(value, made) -> writeIORef ref value >> pure made

standing :: Int -> Expression
standing symbol = ExFormation [BiLambda (FnSymbol symbol)]

answer :: T.Text
answer = "𝑛"

sigil :: T.Text
sigil = "𝛿"

found :: Either Int Bytes -> Maybe (Either Int Bytes, String) -> Maybe String
found value (Just (held, naming))
  | value == held = Just naming
found _ _ = Nothing

alias :: Int -> Int -> Expression
alias firing index = ExMeta (T.pack (printf "n·%d·%d" firing index))

dontSaveEval :: SaveEvalFunc
dontSaveEval _ = pure ()

dontRewindEval :: RewindEvalFunc
dontRewindEval = pure (pure ())

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
