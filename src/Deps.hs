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
import Data.Char (isAlphaNum)
import Data.Either (lefts, rights)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (intercalate, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Text as T
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)
import Files (overwrite)
import Forks (Refusal (..), Side (..))
import GHC.Clock (getMonotonicTime)
import Logger (logDebug, logInfo)
import Matcher
import Printer (printAlpha, printAttribute, printBinding, printFunction)
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

refused :: Refusal -> String
refused refusal = case refusal of
  Unnamed -> "the join names no otherwise"
  Unformed side -> printf "the %s side is no formation" (handed side)
  Holding side AtLambda -> printf "the %s side is an atom" (handed side)
  Holding side AtDelta -> printf "the %s side is a datum" (handed side)
  Holding side attr -> printf "the %s side has %s" (handed side) (printAttribute attr)
  Unripe Lefthand attr -> printf "%s is void on the left and attached on the right" (printAttribute attr)
  Unripe Righthand attr -> printf "%s is void on the right and attached on the left" (printAttribute attr)
  where
    handed :: Side -> String
    handed Lefthand = "left"
    handed Righthand = "right"

dotted :: [Attribute] -> String
dotted = concatMap (('.' :) . printAttribute)

lodged :: [Attribute] -> Expression -> Maybe Binding
lodged [attr] (ExFormation bds) = listToMaybe [bd | bd <- bds, attributeFromBinding bd == Just attr]
lodged (attr : path) (ExFormation bds) = listToMaybe [inner | BiTau attr' inner <- bds, attr' == attr] >>= lodged path
lodged _ _ = Nothing

data Evaluation
  = EvRun Judgment T.Text
  | EvFiring Int T.Text Judgment Expression
  | EvStarted Int Judgment Expression
  | EvDelta Int Bytes
  | EvManufactured Int Int
  | EvBoxed Int Expression
  | EvUndressed Int Expression
  | EvLooped Int Judgment Acyclic Expression Expression (Maybe (Int, Expression))
  | EvStuck Int T.Text Judgment Expression
  | EvStall Int T.Text
  | EvStuckOn Int T.Text
  | EvStarved Int Int Judgment Expression
  | EvTimeout Int Int Judgment Expression
  | EvSpent Int Int Judgment Expression
  | EvData Int T.Text Expression (Either Int Bytes)
  | EvTerm Int T.Text Judgment Expression Expression
  | EvBound Int Expression Expression
  | EvReshaped Int T.Text T.Text Expression Expression
  | EvSymbolized Int Int Bytes
  | EvCast Int T.Text Expression Expression Expression
  | EvMinted Int Int [Either Int Bytes]
  | EvDeferred Int Int Judgment Expression (Maybe Expression) Expression
  | EvApplied Int Judgment Expression Expression Expression
  | EvBuilt Int Expression
  | EvAnswer Int Expression
  | EvMorphed Int Expression
  | EvJoined Int (Either T.Text Expression) Int (Int, Int) (Maybe Int)
  | EvDiverged Int (Expression, Expression) (Expression, Expression) [Attribute] Refusal
  | EvUnripe Int (Expression, Expression) (Expression, Expression) Side Attribute
  | EvFabricated Int (Expression, Expression) (Expression, Expression) Expression
  | EvTaken Int Expression
  | EvWorld Expression
  | EvHeader Header Expression Expression

data Header = Header String (Maybe String) ZonedTime [String] World (Maybe (FilePath, Int))

data World = World (Maybe FilePath) Int Int Int Int

type SaveEvalFunc = Evaluation -> IO ()

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
  | btsSize bytes == 8 = Just (show (btsToNum bytes))
  | otherwise = Nothing

tier :: Evaluation -> Int
tier EvRun{} = 0
tier (EvFiring depth _ _ _) = depth
tier (EvStarted depth _ _) = depth
tier (EvDelta depth _) = depth
tier (EvManufactured depth _) = depth
tier (EvBoxed depth _) = depth
tier (EvUndressed depth _) = depth
tier (EvLooped depth _ _ _ _ _) = depth
tier (EvStuck depth _ _ _) = depth
tier (EvStall depth _) = depth
tier (EvStuckOn depth _) = depth
tier (EvStarved depth _ _ _) = depth
tier (EvTimeout depth _ _ _) = depth
tier (EvSpent depth _ _ _) = depth
tier (EvData depth _ _ _) = depth
tier (EvTerm depth _ _ _ _) = depth
tier (EvBound depth _ _) = depth
tier (EvReshaped depth _ _ _ _) = depth
tier (EvSymbolized depth _ _) = depth
tier (EvCast depth _ _ _ _) = depth
tier (EvMinted depth _ _) = depth
tier (EvDeferred depth _ _ _ _ _) = depth
tier (EvApplied depth _ _ _ _) = depth
tier (EvBuilt depth _) = depth
tier (EvAnswer depth _) = depth
tier (EvMorphed depth _) = depth
tier (EvJoined depth _ _ _ _) = depth
tier (EvDiverged depth _ _ _ _) = depth
tier (EvUnripe depth _ _ _ _) = depth
tier (EvFabricated depth _ _ _) = depth
tier (EvTaken depth _) = depth
tier EvWorld{} = maxBound
tier EvHeader{} = 0

type Named a = Map.Map Int [(Expression, a)]

namedLookup :: Expression -> Named a -> Maybe a
namedLookup term names = lookup term (Map.findWithDefault [] (hashExpression term) names)

namedInsert :: forall a. Expression -> a -> Named a -> Named a
namedInsert term naming = Map.alter renamed (hashExpression term)
  where
    renamed :: Maybe [(Expression, a)] -> Maybe [(Expression, a)]
    renamed entries = Just ((term, naming) : filter ((/= term) . fst) (fromMaybe [] entries))

namedFormation :: Expression -> a -> Named a -> Named a
namedFormation term@(ExFormation bds) naming names
  | all ((`notElem` [Just AtLambda, Just AtDelta]) . attributeFromBinding) bds = namedInsert term naming names
namedFormation _ _ names = names

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
  , _blocks :: [(Int, Maybe (Judgment, String), String, Bool)]
  , _answered :: [(Int, Expression, String)]
  , _unwritten :: Map.Map Int (Named Expression)
  }

emptyProtocol :: Protocol
emptyProtocol = Protocol 0 Map.empty [] False Map.empty Map.empty Map.empty Map.empty Nothing [] [] Map.empty

data Nesting = Nesting
  { _fires :: Int
  , _openedAt :: [(Int, Int)]
  , _closing :: [(Int, String)]
  , _objects :: Named Expression
  , _numbered :: Map.Map Int Int
  , _parts :: Map.Map Int Int
  , _datums :: Map.Map Int Int
  , _held :: Maybe (Either Int Bytes, String)
  , _heads :: [(Int, (Judgment, Expression))]
  , _pending :: [(Int, Judgment, Expression)]
  , _sources :: [(Int, Expression, String)]
  , _world :: Map.Map Attribute Expression
  , _shapes :: Map.Map [Maybe Attribute] [(Expression, [Binding])]
  , _opaque :: Int
  , _unmarked :: Map.Map Int (Named Expression)
  }

type Naming = StateT (Nesting, [String]) IO

emptyNesting :: Nesting
emptyNesting = Nesting 0 [] [] Map.empty Map.empty Map.empty Map.empty Nothing [] [] [] Map.empty Map.empty 0 Map.empty

saveEval :: Handle -> IORef Protocol -> (Expression -> IO String) -> (Expression -> IO String) -> SaveEvalFunc
saveEval handle cursor printed printed' report = do
  made <- atomicModify cursor (fmap (first (forgotten report) . headed) . written (tier report) report . beside (tier report) report . outer (tier report))
  mapM_ saved made
  where
    forgotten :: Evaluation -> Protocol -> Protocol
    forgotten EvBound{} protocol = protocol
    forgotten EvDelta{} protocol = protocol
    forgotten EvManufactured{} protocol = protocol
    forgotten EvWorld{} protocol = protocol
    forgotten _ protocol = protocol{_found = Nothing}
    beside :: Int -> Evaluation -> Protocol -> Protocol
    beside _ EvStarted{} protocol = protocol
    beside _ EvMinted{} protocol = protocol
    beside _ EvBound{} protocol = protocol
    beside at _ protocol = protocol{_blocks = dropWhile (\(level, _, _, _) -> level >= at) protocol._blocks}
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
      [] -> salted (unwritten protocol site)
    context :: Protocol -> Judgment -> Expression -> IO [String]
    context protocol judgment site = do
      locator <- located protocol site
      let said = printf "%s(%s)" (letter judgment) locator
      pure [said | innermost protocol /= Just said]
    innermost :: Protocol -> Maybe String
    innermost protocol = case protocol._blocks of
      (_, heading, _, _) : _ -> snd <$> heading
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
    written :: Int -> Evaluation -> Protocol -> IO (Protocol, Maybe String)
    written _ (EvRun judgment locator) protocol =
      pure (protocol{_begun = True, _blocks = [(0, Just (judgment, heading), heading ++ ":", True)]}, Just (heading ++ ":"))
      where
        heading :: String
        heading = printf "%s(%s)" (letter judgment) (T.unpack locator)
    written at (EvFiring depth key judgment site) protocol = do
      opening <- indented at . remarked (printf "𝔼(%s):" (T.unpack key)) <$> context protocol judgment site
      pure
        ( protocol
            { _fired = firings
            , _open = (depth, firings) : protocol._open
            , _blocks = (at, Nothing, opening, True) : protocol._blocks
            }
        , Just opening
        )
      where
        firings :: Int
        firings = protocol._fired + 1
    written at (EvStarted _ judgment site) protocol = do
      locator <- located protocol site
      let heading = printf "%s(%s)" (letter judgment) locator
          inner = dropWhile (\(level, _, _, _) -> level > at) protocol._blocks
      pure $ case inner of
        (level, Just heading', _, _) : _ | level == at && heading' == (judgment, heading) -> (protocol{_blocks = inner}, Nothing)
        _ -> (protocol{_blocks = (at, Just (judgment, heading), indented at (heading ++ ":"), False) : dropWhile (\(level, _, _, _) -> level >= at) inner}, Nothing)
    written at (EvDelta _ bytes) protocol = do
      datum <- render (ExBytes bytes)
      let (naming, protocol') = stored (Right bytes) protocol
      pure (protocol', Just (indented at (remarked (printf "%s := %s" naming datum) (maybe [] pure (ieee bytes)))))
    written at (EvManufactured _ symbol) protocol = do
      datum <- spelt (Left symbol)
      let (naming, protocol') = stored (Left symbol) protocol
      pure (protocol', Just (indented at (printf "%s := %s" naming datum)))
    written at (EvBoxed _ term) protocol = taking at "boxed" term protocol
    written at (EvUndressed _ term) protocol = taking at "undressed" term protocol
    written at (EvLooped _ judgment mode self site answered) protocol = do
      form <- render self
      remarks <- context protocol judgment site
      (line, notes) <- maybe (pure (printf "looped %s" form, [])) (severed form) answered
      pure (protocol, Just (indented at (remarked line (remarks ++ certainty mode : notes))))
      where
        severed :: String -> (Int, Expression) -> IO (String, [String])
        severed form (symbol, call) = do
          made <- printed (abbreviatedInside protocol._made call)
          pure (printf "looped %s := %s" (printFunction (FnSymbol symbol)) made, [printf "repeats %s" form])
    written at (EvStuck _ key judgment self) protocol = do
      form <- render self
      pure (protocol, Just (indented at (printf "unanswered %s  # %s(%s)" (T.unpack key) (letter judgment) form)))
    written at (EvStall _ key) protocol =
      pure (protocol, Just (indented at (printf "stall %s" (T.unpack key))))
    written at (EvStuckOn _ key) protocol =
      pure (protocol, Just (indented at (printf "stuck %s" (T.unpack key))))
    written at (EvStarved _ limit judgment site) protocol = do
      remarks <- context protocol judgment site
      pure (protocol, Just (indented at (remarked (printf "starved %d" limit) remarks)))
    written at (EvTimeout _ limit judgment site) protocol = do
      remarks <- context protocol judgment site
      pure (protocol, Just (indented at (remarked (printf "timeout %d" limit) remarks)))
    written at (EvSpent _ limit judgment site) protocol = do
      remarks <- context protocol judgment site
      pure (protocol, Just (indented at (remarked (printf "spent %d" limit) remarks)))
    written at (EvData _ spelling operand value) protocol = do
      datum <- maybe (spelt value) pure (found value protocol._found)
      line <- commented protocol (printf "%s := %s" (labelled protocol spelling) datum) Dataization operand
      pure (protocol, Just (indented at line))
    written at (EvTerm _ spelling judgment operand term) protocol = do
      let naming = labelled protocol spelling
      (protocol', value) <- valued protocol naming term
      line <- commented protocol (printf "%s := %s" naming value) judgment operand
      pure (protocol', Just (indented at line))
    written _ (EvBound _ meta term) protocol =
      pure (protocol{_unwritten = Map.alter (Just . namedInsert meta term . fromMaybe Map.empty) (opener protocol) protocol._unwritten}, Nothing)
    written at (EvReshaped _ spelling block source term) protocol = do
      let naming = labelled protocol spelling
      (protocol', value) <- valued protocol naming term
      line <- printf "%s := %s  # %s(%s)" naming value (T.unpack block) <$> salted (unwritten protocol source)
      pure (protocol', Just (indented at line))
    written at (EvSymbolized _ symbol bytes) protocol = do
      datum <- render (ExBytes bytes)
      pure (protocol, Just (indented at (remarked (printf "symbolized %s := %s" (printFunction (FnSymbol symbol)) datum) (maybe [] pure (ieee bytes)))))
    written at (EvCast _ spelling source model term) protocol = do
      let naming = labelled protocol spelling
      (protocol', value) <- valued protocol naming term
      one <- spoken protocol source
      two <- spoken protocol model
      pure (protocol', Just (indented at (printf "cast %s := %s  # %s up to %s" naming value one two)))
    written _ EvMinted{} protocol = pure (protocol, Nothing)
    written at (EvDeferred _ symbol judgment copy call site) protocol = do
      form <- maybe (render copy) (printed . abbreviatedInside protocol._made) call
      remarks <- context protocol judgment site
      pure (protocol, Just (indented at (remarked (printf "deferred %s := %s" (printFunction (FnSymbol symbol)) form) remarks)))
    written at (EvApplied _ judgment call object site) protocol = do
      let (index, counted) = numbered protocol
          (index', counted') = numbered counted
          stem :: String
          stem = labelled protocol answer
      form <- printed (abbreviatedInside protocol._made call)
      body <- printed (abbreviatedInside protocol._made object)
      remarks <- context protocol judgment site
      pure
        ( counted'{_made = namedInsert call (alias (opener protocol) index) (namedInsert object (alias (opener protocol) index') counted'._made)}
        , Just (unlines [indented at (remarked (printf "%s·%d := %s" stem index form) remarks), indented at (printf "%s·%d := %s" stem index' body)])
        )
    written at (EvBuilt depth term) protocol = do
      let (index, counted) = numbered protocol
      value <- borrowed protocol term
      let naming = printf "%s·%d" (labelled protocol answer) index
      pure (counted{_built = Map.insert (opener protocol) index counted._built, _answered = (depth, term, naming) : counted._answered}, Just (indented at (printf "%s := %s  # %s" naming value (T.unpack answer))))
    written at (EvAnswer _ term) protocol = do
      let (index, counted) = numbered protocol
          stem :: String
          stem = labelled protocol answer
          naming :: String
          naming = printf "%s·%d" stem index
      (protocol', value) <- valued counted naming term
      pure (protocol'{_made = namedFormation term (alias (opener protocol) index) protocol'._made}, Just (indented at (printf "%s := %s  # 𝕄(%s·%d)" naming value stem (Map.findWithDefault 1 (opener protocol) protocol._built))))
    written at (EvMorphed _ term) protocol = do
      let (index, counted) = numbered protocol
          naming :: String
          naming = printf "%s·%d" (labelled protocol answer) index
      (protocol', value) <- case (namedLookup term protocol._made, namedLookup term protocol._named) of
        (Nothing, Nothing) -> valued counted naming term
        _ -> (,) counted <$> borrowed protocol term
      pure (protocol'{_made = namedFormation term (alias (opener protocol) index) protocol'._made}, Just (indented at (printf "morphed %s := %s" naming value)))
    written at (EvJoined _ condition symbol (left, right) bottom) protocol = do
      on <- either (pure . labelled protocol) salted condition
      termination <- render ExTermination
      pure (protocol, Just (indented at (remarked (printf "joined %s := %s ? %s : %s" (printFunction (FnSymbol symbol)) on (printFunction (FnSymbol left)) (printFunction (FnSymbol right))) [printf "where %s is %s" (printFunction (FnSymbol idx)) termination | Just idx <- [bottom], idx `elem` [left, right]])))
    written at (EvDiverged _ left right path refusal) protocol = do
      one <- sided protocol left
      two <- sided protocol right
      differed <- explained protocol path (snd left) (snd right)
      pure (protocol, Just (indented at (printf "diverged %s vs %s  # cannot join: %s; cannot fabricate: %s" one two differed (refused refusal))))
    written at (EvUnripe _ left right side attr) protocol = do
      one <- sided protocol left
      two <- sided protocol right
      pure (protocol, Just (indented at (printf "unripe %s vs %s  # %s" one two (refused (Unripe side attr)))))
    written at (EvFabricated _ left right term) protocol = do
      one <- sided protocol left
      two <- sided protocol right
      body <- printed (abbreviatedInside protocol._made term)
      let (index, counted) = numbered protocol
      pure (counted{_made = namedInsert term (alias (opener protocol) index) counted._made}, Just (indented at (printf "fabricated %s·%d := %s and %s  # %s" (labelled protocol answer) index one two body)))
    written at (EvTaken _ term) protocol = do
      let (index, counted) = numbered protocol
          naming :: String
          naming = printf "%s·%d" (labelled protocol answer) index
      (protocol', value) <- valued counted naming term
      pure (protocol'{_made = namedFormation term (alias (opener protocol) index) protocol'._made}, Just (indented at (printf "%s := %s" naming value)))
    written _ (EvWorld world) protocol = pure (protocol{_made = namedInsert world ExRoot protocol._made}, Nothing)
    written _ (EvHeader (Header release revision dated command (World file count size tops bindings) table) locator expression) protocol = do
      place <- printed locator
      body <- printed expression
      pure (protocol, Just (unlines (map ("# " ++) (told ++ ["Locator: " ++ place, "Expression: " ++ body]) ++ [""])))
      where
        told :: [String]
        told =
          [ printf "Written by phino %s on %s" (maybe release (printf "%s (%s)" release) revision) (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Ez" dated)
          , "Command: " ++ commanded command
          , printf "World: %s, %s" (fromMaybe "stdin" file) (intercalate ", " [amount count "line" "lines", amount size "byte" "bytes", amount tops "object" "objects" ++ " at the top", amount bindings "binding" "bindings"])
          ]
            ++ ["Symbolic: " ++ path ++ ", " ++ amount entries "entry" "entries" | Just (path, entries) <- [table]]
        amount :: Int -> String -> String -> String
        amount 1 one _ = "1 " ++ one
        amount many _ several = show many ++ " " ++ several
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
    explained :: Protocol -> [Attribute] -> Expression -> Expression -> IO String
    explained protocol path one two = case reverse path of
      [] -> printf "the left side is %s and the right side is %s" <$> borrowed protocol one <*> borrowed protocol two
      attr : _ -> printf "at %s the left side %s and the right side %s" (dotted path) <$> had one <*> had two
        where
          had :: Expression -> IO String
          had side = maybe (pure (printf "has no %s" (printAttribute attr))) (fmap ("has " ++) . bound) (lodged path side)
          bound :: Binding -> IO String
          bound (BiTau attr' term) = printf "%s ↦ %s" (printAttribute attr') <$> borrowed protocol term
          bound (BiVoid attr') = pure (printf "%s ↦ ∅" (printAttribute attr'))
          bound (BiDelta bytes) = printf "Δ ⤍ %s" <$> render (ExBytes bytes)
          bound (BiLambda func) = pure (printf "λ ⤍ %s" (printFunction func))
          bound bd = pure (printBinding bd)
    sided :: Protocol -> (Expression, Expression) -> IO String
    sided _ (_, term@(ExFormation [BiLambda (FnSymbol _)])) = render term
    sided protocol (side@(ExMeta _), _) = spoken protocol side
    sided protocol (_, term) = borrowed protocol term
    spoken :: Protocol -> Expression -> IO String
    spoken protocol meta = maybe (labelled protocol . T.pack <$> salted meta) (borrowed protocol) (namedLookup meta (held protocol))
    commented :: Protocol -> String -> Judgment -> Expression -> IO String
    commented protocol line judgment operand = printf "%s  # %s(%s)" line (letter judgment) <$> salted (unwritten protocol operand)
    unwritten :: Protocol -> Expression -> Expression
    unwritten protocol = abbreviated (held protocol)
    held :: Protocol -> Named Expression
    held protocol = Map.findWithDefault Map.empty (opener protocol) protocol._unwritten
    outer :: Int -> Protocol -> Protocol
    outer depth protocol = protocol{_open = dropWhile ((>= depth) . fst) protocol._open, _answered = dropWhile (\(level, _, _) -> level > depth) protocol._answered, _unwritten = foldr (Map.delete . snd) protocol._unwritten (takeWhile ((>= depth) . fst) protocol._open)}
    labelled :: Protocol -> T.Text -> String
    labelled protocol spelling = printf "%s·%d" (T.unpack spelling) (opener protocol)
    opener :: Protocol -> Int
    opener protocol = maybe 0 snd (listToMaybe protocol._open)
    stored :: Either Int Bytes -> Protocol -> (String, Protocol)
    stored value protocol = (naming, protocol{_deltas = Map.insert (opener protocol) index protocol._deltas, _found = Just (value, naming)})
      where
        index :: Int
        index = maybe 1 (+ 1) (Map.lookup (opener protocol) protocol._deltas)
        naming :: String
        naming = printf "%s·%d" (labelled protocol sigil) index
    numbered :: Protocol -> (Int, Protocol)
    numbered protocol = (index, protocol{_counted = Map.insert (opener protocol) index protocol._counted})
      where
        index :: Int
        index = maybe 1 (+ 1) (Map.lookup (opener protocol) protocol._counted)
    taking :: Int -> String -> Expression -> Protocol -> IO (Protocol, Maybe String)
    taking at word term protocol = do
      let (index, counted) = numbered protocol
      taken <- printed (abbreviatedInside protocol._made term)
      pure (counted{_made = namedInsert term (alias (opener protocol) index) counted._made}, Just (indented at (printf "%s %s·%d := %s" word (labelled protocol answer) index taken)))

endEval :: Handle -> IORef Protocol -> Double -> IO ()
endEval handle cursor began = do
  protocol <- readIORef cursor
  when protocol._begun $ do
    now <- getMonotonicTime
    let taken = milliseconds began now
    hPutStrLn handle (printf "msec %d" taken)
    hPutStrLn handle (printf "firings %d" protocol._fired)
    hPutStrLn handle (printf "fps %d" (perSecond protocol._fired taken))

milliseconds :: Double -> Double -> Int
milliseconds began now = round ((now - began) * 1000)

perSecond :: Int -> Int -> Int
perSecond firings taken = round (fromIntegral firings * 1000 / fromIntegral (max 1 taken) :: Double)

saveEvalXml :: Handle -> IORef Nesting -> (Expression -> IO String) -> SaveEvalFunc
saveEvalXml handle cursor printed report = do
  written <- atomicModify cursor (flushed . beside (tier report) report . outer (tier report) >=> opened' (tier report))
  mapM_ (hPutStrLn handle) written
  logDebug (printf "Saved %d line(s) of the XML protocol" (length written))
  where
    opened' :: Int -> (Nesting, [String]) -> IO (Nesting, [String])
    opened' at (nesting, openings) = bimap (forgotten report) (openings ++) <$> elements at report nesting
    beside :: Int -> Evaluation -> Nesting -> Nesting
    beside _ EvStarted{} nesting = nesting
    beside _ EvBound{} nesting = nesting
    beside at _ nesting = nesting{_heads = dropWhile ((>= at) . fst) nesting._heads, _pending = dropWhile (\(level, _, _) -> level >= at) nesting._pending}
    flushed :: Nesting -> IO (Nesting, [String])
    flushed nesting
      | silent report = pure (nesting, [])
      | otherwise = foldM pended (nesting{_pending = []}, []) (reverse nesting._pending)
    pended :: (Nesting, [String]) -> (Int, Judgment, Expression) -> IO (Nesting, [String])
    pended (nesting, openings) (level, judgment, site) = do
      (nesting', lines') <- staged level nesting (printf "<%s at=\"%s\">" (opened judgment) . escapeXML <$> located level site)
      pure (nesting'{_closing = (level, opened judgment) : nesting'._closing}, openings ++ lines')
    silent :: Evaluation -> Bool
    silent EvStarted{} = True
    silent EvWorld{} = True
    silent EvBound{} = True
    silent _ = False
    forgotten :: Evaluation -> Nesting -> Nesting
    forgotten EvBound{} nesting = nesting
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
    elements :: Int -> Evaluation -> Nesting -> IO (Nesting, [String])
    elements _ (EvRun judgment locator) nesting =
      pure
        ( nesting{_closing = (0, opened judgment) : rooted kept}
        , prologue nesting ++ closers ++ [run]
        )
      where
        (kept, closers) = closed 0 nesting._closing
        run :: String
        run = indentedXml 0 (printf "<%s at=\"%s\">" (opened judgment) (quoted locator))
    elements _ (EvHeader (Header release revision dated command (World file count size tops bindings) table) locator _) nesting = do
      place <- printed locator
      pure
        ( nesting{_closing = rooted nesting._closing}
        , prologue nesting
            ++ map
              (indentedXml 0)
              ( [ printf "<written by=\"phino\" version=\"%s\"%s on=\"%s\"/>" (escapeXML release) (maybe "" (printf " revision=\"%s\"" . escapeXML) revision :: String) (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" dated)
                , printf "<command>%s</command>" (escapeXMLText (commanded command))
                , printf "<world%s lines=\"%d\" bytes=\"%d\" objects=\"%d\" bindings=\"%d\"/>" (maybe "" (printf " file=\"%s\"" . escapeXML) file :: String) count size tops bindings
                ]
                  ++ [printf "<symbolic file=\"%s\" entries=\"%d\"/>" (escapeXML path) entries | Just (path, entries) <- [table]]
                  ++ [printf "<locator>%s</locator>" (escapeXMLText place)]
              )
        )
    elements at (EvFiring depth key judgment site) nesting = do
      (fired, lines') <- placed at nesting (printf "<evaluate λ=\"%s\" by=\"%s\" at=\"%s\">" (quoted key) (opened judgment) . escapeXML <$> located depth site)
      pure (fired{_fires = fired._fires + 1, _openedAt = (depth, fired._fires + 1) : fired._openedAt, _closing = (at, "evaluate") : fired._closing}, lines')
    elements at (EvStarted _ judgment site) nesting =
      pure $ case inner of
        (level, opening) : _ | level == at && opening == (judgment, site) -> (nesting{_heads = inner, _pending = pending}, [])
        _ -> (nesting{_heads = (at, (judgment, site)) : dropWhile ((>= at) . fst) inner, _pending = (at, judgment, site) : dropWhile (\(level, _, _) -> level >= at) pending, _closing = kept}, closers)
      where
        inner :: [(Int, (Judgment, Expression))]
        inner = dropWhile ((> at) . fst) nesting._heads
        pending :: [(Int, Judgment, Expression)]
        pending = dropWhile (\(level, _, _) -> level > at) nesting._pending
        (kept, closers) = closed at nesting._closing
    elements at (EvLooped depth judgment mode self site Nothing) nesting =
      placed at nesting $ do
        locator <- located depth site
        origin <- sole depth self
        pure (enclosed "looped" (printf "<looped by=\"%s\" match=\"%s\" at=\"%s\"" (opened judgment) (certainty mode) (escapeXML locator)) origin)
    elements at (EvLooped depth judgment mode self site (Just (symbol, call))) nesting =
      placed at nesting $ do
        locator <- located depth site
        origin <- leaf depth self
        (object, given) <- called depth call
        pure (printf "<looped symbol=\"%s\" by=\"%s\" match=\"%s\" at=\"%s\" of=\"%s\" object=\"%s\">%s</looped>" (sigma symbol) (opened judgment) (certainty mode) (escapeXML locator) (escapeXML origin) (escapeXML object) given)
    elements at (EvStuck depth key judgment self) nesting =
      placed at nesting (printf "<unanswered λ=\"%s\" by=\"%s\">%s</unanswered>" (quoted key) (opened judgment) . escapeXMLText <$> leaf depth self)
    elements at (EvStall _ key) nesting =
      placed at nesting (pure (printf "<stalled λ=\"%s\"/>" (quoted key)))
    elements at (EvStuckOn _ key) nesting =
      placed at nesting (pure (printf "<stuck λ=\"%s\"/>" (quoted key)))
    elements at (EvStarved depth limit judgment site) nesting =
      placed at nesting (printf "<starved limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) . escapeXML <$> located depth site)
    elements at (EvTimeout depth limit judgment site) nesting =
      placed at nesting (printf "<timeout limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) . escapeXML <$> located depth site)
    elements at (EvSpent depth limit judgment site) nesting =
      placed at nesting (printf "<spent limit=\"%d\" by=\"%s\" at=\"%s\"/>" limit (opened judgment) . escapeXML <$> located depth site)
    elements at (EvDelta _ bytes) nesting = do
      datum <- printed (ExBytes bytes)
      let (naming, nesting') = stored (Right bytes) nesting
      placed at nesting' (pure (printf "<dataized meta=\"%s\"%s>%s</dataized>" (escapeXML naming) (maybe "" (printf " number=\"%s\"" . escapeXML) (ieee bytes) :: String) (escapeXMLText datum)))
    elements at (EvManufactured _ symbol) nesting = placed at nesting' (pure (printf "<manufactured meta=\"%s\">%s</manufactured>" (escapeXML naming) (sigma symbol)))
      where
        (naming, nesting') = stored (Left symbol) nesting
    elements at (EvBoxed depth term) nesting = taken at "boxed" depth term nesting
    elements at (EvUndressed depth term) nesting = taken at "undressed" depth term nesting
    elements at (EvData _ spelling _ value) nesting =
      placed at nesting $ case (found value nesting._held, value) of
        (Just naming, _) -> pure (bound (escapeXMLText naming))
        (Nothing, Left symbol) -> pure (bound (sigma symbol))
        (Nothing, Right bytes) -> bound . escapeXMLText <$> lift (printed (ExBytes bytes))
      where
        bound :: String -> String
        bound = printf "<bound meta=\"%s\">%s</bound>" (escapeXML (labelled nesting spelling))
    elements at (EvTerm depth spelling _ _ term) nesting = placed at nesting (bind depth nesting spelling term)
    elements _ (EvBound _ meta term) nesting = pure (nesting{_unmarked = Map.alter (Just . namedInsert meta term . fromMaybe Map.empty) (opener nesting) nesting._unmarked}, [])
    elements at (EvReshaped depth spelling _ _ term) nesting = placed at nesting (bind depth nesting spelling term)
    elements at (EvSymbolized _ symbol bytes) nesting = do
      datum <- printed (ExBytes bytes)
      placed at nesting (pure (printf "<symbolized symbol=\"%s\"%s>%s</symbolized>" (sigma symbol) (maybe "" (printf " number=\"%s\"" . escapeXML) (ieee bytes) :: String) (escapeXMLText datum)))
    elements at (EvCast depth spelling source model term) nesting =
      placed at nesting $ do
        one <- spoken depth nesting source
        two <- spoken depth nesting model
        printf "<cast meta=\"%s\" of=\"%s\" up-to=\"%s\">%s</cast>" (escapeXML (labelled nesting spelling)) (escapeXML one) (escapeXML two) . escapeXMLText <$> leaf depth term
    elements at (EvMinted _ symbol operands) nesting = do
      spelledOperands <- mapM spelled' operands
      placed at nesting (pure (minted spelledOperands))
      where
        spelled' :: Either Int Bytes -> IO String
        spelled' (Left fresh') = pure (sigma fresh')
        spelled' (Right bytes) = printed (ExBytes bytes)
        minted :: [String] -> String
        minted [] = printf "<minted symbol=\"%s\"/>" (sigma symbol)
        minted spelt = printf "<minted symbol=\"%s\">%s</minted>" (sigma symbol) (escapeXMLText (unwords spelt))
    elements at (EvDeferred depth symbol judgment copy call site) nesting =
      placed at nesting $ do
        locator <- located depth site
        origin <- maybe (sole depth copy) (called depth) call
        pure (enclosed "deferred" (printf "<deferred symbol=\"%s\" by=\"%s\" at=\"%s\"" (sigma symbol) (opened judgment) (escapeXML locator)) origin)
    elements at (EvApplied depth judgment call object site) nesting =
      placed at nesting $ do
        locator <- located depth site
        (origin, given) <- parted depth call
        (naming, aliased) <- counted
        modify (bimap (\nesting' -> nesting'{_objects = namedInsert call aliased nesting'._objects}) (printf "<applied meta=\"%s\" by=\"%s\" at=\"%s\" of=\"%s\">%s</applied>" (escapeXML naming) (opened judgment) (escapeXML locator) (escapeXML origin) given :))
        element <- made depth object
        (naming', aliased') <- counted
        modify (first (\nesting' -> nesting'{_objects = namedInsert object aliased' nesting'._objects}))
        pure (element (escapeXML naming'))
    elements at (EvBuilt depth term) nesting =
      placed at nesting $ do
        body <- leaf depth term
        (naming, _) <- counted
        modify (first (\nesting' -> nesting'{_sources = (depth, term, naming) : nesting'._sources}))
        pure (printf "<built meta=\"%s\">%s</built>" (escapeXML naming) (escapeXMLText body))
    elements at (EvAnswer depth term) nesting = placed at nesting (concluded "answered" depth term)
    elements at (EvMorphed depth term) nesting = placed at nesting (concluded "morphed" depth term)
    elements at (EvJoined _ condition symbol (left, right) bottom) nesting =
      placed at nesting $ do
        on <- either (pure . labelled nesting) (lift . traced) condition
        pure (printf "<joined symbol=\"%s\" on=\"%s\" left=\"%s\" right=\"%s\"%s/>" (sigma symbol) (escapeXML on) (sigma left) (sigma right) (concat [printf " terminator=\"%s\"" (sigma idx) :: String | Just idx <- [bottom], idx `elem` [left, right]]))
    elements at (EvDiverged depth left right path refusal) nesting =
      placed at nesting $ do
        one <- sided depth nesting left
        two <- sided depth nesting right
        places <- mapM (uncurry (lodging depth path)) [("left", snd left), ("right", snd right)]
        pure (printf "<diverged left=\"%s\" right=\"%s\"%s reason=\"%s\">%s</diverged>" (escapeXML one) (escapeXML two) (if null path then "" else printf " at=\"%s\"" (escapeXML (dotted path)) :: String) (escapeXML (refused refusal)) (concat places))
    elements at (EvUnripe depth left right _ attr) nesting =
      placed at nesting $ do
        one <- sided depth nesting left
        two <- sided depth nesting right
        pure (printf "<unripe left=\"%s\" right=\"%s\" at=\"%s\"/>" (escapeXML one) (escapeXML two) (escapeXML (printAttribute attr)))
    elements at (EvFabricated depth left right term) nesting =
      placed at nesting $ do
        one <- sided depth nesting left
        two <- sided depth nesting right
        body <- leaf depth term
        (naming, _) <- counted
        pure (printf "<fabricated meta=\"%s\" left=\"%s\" right=\"%s\">%s</fabricated>" (escapeXML naming) (escapeXML one) (escapeXML two) (escapeXMLText body))
    elements at (EvTaken depth term) nesting = placed at nesting (concluded "answered" depth term)
    elements _ (EvWorld world) nesting = pure (known world nesting, [])
    prologue :: Nesting -> [String]
    prologue nesting
      | null nesting._closing = ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<protocol>"]
      | otherwise = []
    rooted :: [(Int, String)] -> [(Int, String)]
    rooted [] = [(-1, "protocol")]
    rooted open = open
    sided :: Int -> Nesting -> (Expression, Expression) -> Naming String
    sided depth _ (_, term@(ExFormation [BiLambda (FnSymbol _)])) = leaf depth term
    sided depth nesting (side@(ExMeta _), _) = spoken depth nesting side
    sided depth _ (_, term) = leaf depth term
    spoken :: Int -> Nesting -> Expression -> Naming String
    spoken depth nesting meta = maybe (labelled nesting . T.pack <$> lift (spelled meta)) (leaf depth) (namedLookup meta (held nesting))
    held :: Nesting -> Named Expression
    held nesting = Map.findWithDefault Map.empty (opener nesting) nesting._unmarked
    outer :: Int -> Nesting -> Nesting
    outer depth nesting = nesting{_openedAt = dropWhile ((>= depth) . fst) nesting._openedAt, _sources = dropWhile (\(level, _, _) -> level > depth) nesting._sources, _unmarked = foldr (Map.delete . snd) nesting._unmarked (takeWhile ((>= depth) . fst) nesting._openedAt)}
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
      (nesting, records) <- get
      let index = maybe 1 (+ 1) (Map.lookup (opener nesting) nesting._parts)
          aliased = ExMeta (T.pack (printf "e·%d·%d" (opener nesting) index))
      put (nesting{_parts = Map.insert (opener nesting) index nesting._parts, _objects = namedInsert term aliased nesting._objects}, element (escapeXML (printf "%s·%d" (labelled nesting fragment) index)) : records)
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
      maybe (titled depth (abbreviated (held nesting) site) >>= lift . traced) pure (listToMaybe [naming | (_, built, naming) <- nesting._sources, built == site])
    taken :: Int -> String -> Int -> Expression -> Nesting -> IO (Nesting, [String])
    taken at tag depth term nesting =
      placed at nesting $ do
        body <- coined depth term >>= lift . spelled
        (naming, aliased) <- counted
        modify (first (\nesting' -> nesting'{_objects = namedInsert term aliased nesting'._objects}))
        pure (printf "<%s meta=\"%s\">%s</%s>" tag (escapeXML naming) (escapeXMLText body) tag)
    concluded :: String -> Int -> Expression -> Naming String
    concluded tag depth term = do
      body <- leaf depth term
      (naming, aliased) <- counted
      modify (first (\nesting -> nesting{_objects = namedFormation term aliased nesting._objects}))
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
      case copied nesting bds of
        Just (origin, []) -> do
          path <- titled depth origin
          modify (first (\nesting' -> nesting'{_objects = namedInsert term path nesting'._objects}))
          pure path
        copy -> maybe (anonymous depth bds) (cloned depth) copy >>= recorded term
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
    made :: Int -> Expression -> Naming (String -> String)
    made depth (ExFormation bds) = do
      (nesting, _) <- get
      maybe (anonymous depth bds) (cloned depth) (copied nesting bds)
    made depth term = cloned depth (term, [])
    cloned :: Int -> (Expression, [(Attribute, Expression)]) -> Naming (String -> String)
    cloned depth (origin, bound) = do
      path <- leaf depth origin
      given <- mapM (\(attr, value) -> titled depth value >>= lift . attributed (printAttribute attr)) bound
      pure (\naming -> enclosed "formed" (printf "<formed meta=\"%s\"" naming) (path, concat given))
    anonymous :: Int -> [Binding] -> Naming (String -> String)
    anonymous depth bds = do
      parts <- mapM (part depth) bds
      pure (\naming -> gathered "formed" (printf "formed meta=\"%s\"" naming) parts)
    lodging :: Int -> [Attribute] -> String -> Expression -> Naming String
    lodging depth [] tag side = (\body -> printf "<%s>%s</%s>" tag (escapeXMLText body) tag) <$> leaf depth side
    lodging depth path tag side = gathered tag tag <$> mapM (part depth) (maybeToList (lodged path side))
    gathered :: String -> String -> [Either String String] -> String
    gathered tag opening parts
      | null inner = printf "<%s%s/>" opening marks
      | otherwise = printf "<%s%s>%s</%s>" opening marks inner tag
      where
        marks :: String
        marks = concat (lefts parts)
        inner :: String
        inner = concat (rights parts)
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
    traced :: Expression -> IO String
    traced ExXi = pure "ξ"
    traced (ExDispatch inner attr) = printf "%s.%s" <$> traced inner <*> pure (printAttribute attr)
    traced term = spelled term
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
  mapM_ (hPutStrLn handle) (snd (closed 0 nesting._closing))
  unless (null nesting._closing) $ do
    now <- getMonotonicTime
    let taken = milliseconds began now
    hPutStrLn handle (indentedXml 0 (printf "<msec>%d</msec>" taken))
    hPutStrLn handle (indentedXml 0 (printf "<firings>%d</firings>" nesting._fires))
    hPutStrLn handle (indentedXml 0 (printf "<fps>%d</fps>" (perSecond nesting._fires taken)))
    hPutStrLn handle "</protocol>"
  writeIORef cursor nesting{_closing = []}

commanded :: [String] -> String
commanded command = unwords (map shelled ("phino" : command))
  where
    shelled :: String -> String
    shelled word
      | not (null word) && all safe word = word
      | otherwise = "'" ++ concatMap escaped word ++ "'"
    safe :: Char -> Bool
    safe char = isAlphaNum char || char `elem` ("-_./=:,+@%" :: String)
    escaped :: Char -> String
    escaped '\'' = "'\\''"
    escaped char = [char]

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

fragment :: T.Text
fragment = "𝑒"

found :: Either Int Bytes -> Maybe (Either Int Bytes, String) -> Maybe String
found value (Just (held, naming))
  | value == held = Just naming
found _ _ = Nothing

alias :: Int -> Int -> Expression
alias firing index = ExMeta (T.pack (printf "n·%d·%d" firing index))

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
