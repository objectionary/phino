{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Morph (Answer (..), Budget (..), Deadline (..), Firing (..), Facts, Kept (..), ReduceContext (..), ReduceException (..), EvaluationFunc, Memo (..), Kindred, ReductionFunc, adopted, kindle, kindred, minting, placed, Morphed, Steps (..), Tally (..), admitting, argued, awaited, called, carried, charged, coined, counted, declared, deeper, emptyState, ended, enter, execBuildTerm, expired, fired, inferred, insideUniverse, isLambda, lambda, leadsTo, factual, memoized, morph, noted, remembering, morph', morphing, normalized, onward, opening, parking, recalled, refused, remember, remembered, repeats, retained, settled, severing, starved, tallied, timed, universed, unparked, vetted, voided) where

import AST
import Builder (buildExpressionThrows, nameIn)
import Control.Applicative ((<|>))
import Control.Exception (Exception, catch, evaluate, throwIO, try)
import Control.Monad (forM_, guard, unless, when)
import Data.Bifunctor (first, second)
import Data.Either (lefts)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, partition, sortOn)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import Deps (Acyclic (..), BuildTermFunc, BuildTermMethod, Evaluation (..), Judgment (..), SaveEvalFunc, SaveStepFunc, State (..), Term (..), dispatched, dontSaveStep, sized)
import Engine (Engine (..))
import GHC.Clock (getMonotonicTime)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import qualified Inference as In
import Lambdas (Lambdas, stayed, thrown)
import Locator (locatedExpression, withLocatedExpression)
import Matcher (substEmpty)
import Must (Must (..))
import Printer (printExpression)
import Random (shuffle)
import Rewriter (RewriteContext (RewriteContext), Rewritten, Seen, rewrite, seenInsert)
import Rule (RuleContext (RuleContext), xiFree)
import Sides (Sides, named, opened, resolver)
import System.Timeout (timeout)
import Text.Printf (printf)
import Yaml (ExtraArgument (..))

type Morphed = (Expression, NonEmpty Rewritten)

type ReductionFunc = Expression -> ReduceContext -> Expression -> State -> IO (Maybe Bytes, State)

type EvaluationFunc = ReduceContext -> State -> Expression -> Expression -> IO (Expression, State)

emptyState :: State
emptyState = State Nothing Nothing []

data Steps = Steps
  { _limit :: Int
  , _spent :: Int
  }

data Tally = Tally
  { _ceiling :: Int
  , _count :: IORef Int
  }

data Deadline = Deadline
  { _seconds :: Int
  , _until :: Double
  }

data Memo = Memo (IORef (Store (Int, Kept))) (IORef (Map.Map Firing (Expression, Answer))) (IORef Int) (IORef Int) Kin

data Kin = Kin (IORef Spots) (IORef (Map.Map Int [Kindred])) (IORef (Map.Map Int Firing))

data Spots = Spots (Store Int) (Map.Map Int Expression)

data Kindred = Kindred Expression Expression Firing Answer

data Renaming = Renaming (IORef (Map.Map Int Int)) Spots (IORef (Set.Set Firing))

data Answer
  = Answer Expression Expression
  | Joined Expression
  | Raised

ended :: Answer -> Expression
ended (Answer _ normal) = normal
ended (Joined normal) = normal
ended Raised = ExTermination

data Firing = Firing T.Text [Either Int Bytes] [Expression]
  deriving (Eq, Ord)

data Kept
  = Answered Answer
  | Looped Expression
  | Stalled T.Text Int

type Store answer = Map.Map Int [(Expression, answer)]

data Facts = Facts (IORef (Store Expression)) (IORef Lodgings)

data Lodging = Lodging [(Expression, Attribute)] [(Attribute, Expression)]

data Lodgings = Lodgings !(Store Lodging) !(Map.Map Int [Expression])

data ReduceContext = ReduceContext
  { _locator :: Expression
  , _site :: Expression
  , _universe :: Maybe Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _maxNodes :: Maybe Int
  , _maxMemory :: Maybe Int
  , _steps :: Steps
  , _tally :: Maybe Tally
  , _minted :: IORef Int
  , _sides :: Sides
  , _deadline :: Maybe Deadline
  , _memo :: Maybe Memo
  , _nesting :: Int
  , _opened :: Maybe (Int, Judgment, Expression)
  , _depthSensitive :: Bool
  , _shuffle :: Bool
  , _partial :: Bool
  , _acyclic :: Maybe Acyclic
  , _maxLoops :: Int
  , _judgment :: Judgment
  , _parked :: [T.Text]
  , _entered :: Seen
  , _facts :: Maybe Facts
  , _symbolic :: Lambdas
  , _raising :: [T.Text]
  , _reckoning :: Bool
  , _buildTerm :: BuildTermFunc
  , _reduce :: ReductionFunc
  , _evaluate :: EvaluationFunc
  , _saveStep :: SaveStepFunc
  , _saveEval :: SaveEvalFunc
  , _engine :: Engine
  }

data Budget
  = Depth Judgment Int
  | Firings Int
  | Cycles Int
  | Nodes Int
  | Memory Int
  | Seconds Int

data ReduceException
  = OutOfSteps Budget
  | Stuck T.Text
  | StuckAt T.Text (NonEmpty Rewritten) State
  | OutOfStepsAt Budget (NonEmpty Rewritten) State
  | Diverged T.Text
  | Looping Expression
  | LoopingAt Expression (NonEmpty Rewritten) State
  | Undataizable Expression (NonEmpty Rewritten) State
  | Unmorphable Expression
  | Abnormal Judgment Expression
  deriving anyclass (Exception)

instance Show ReduceException where
  show (OutOfSteps (Depth judgment limit)) =
    printf "%s did not finish before reaching the limit of steps: --max-steps=%d" (show judgment) limit
  show (OutOfSteps (Firings limit)) =
    printf "Evaluation did not finish before reaching the limit of firings: --max-firings=%d" limit
  show (OutOfSteps (Cycles limit)) =
    printf "Normalization did not finish before reaching the limit of cycles: --max-cycles=%d" limit
  show (OutOfSteps (Nodes limit)) =
    printf "Evaluation did not finish before reaching the limit of nodes: --max-nodes=%d" limit
  show (OutOfSteps (Memory limit)) =
    printf "Evaluation did not finish before reaching the limit of memory: --max-memory=%s" (sized limit)
  show (OutOfSteps (Seconds limit)) =
    printf "Evaluation did not finish before reaching the limit of seconds: --max-seconds=%d" limit
  show (OutOfStepsAt budget _ _) = show (OutOfSteps budget)
  show (Stuck func) = printf "No entry of --symbolic answers the λ function '%s'" (T.unpack func)
  show (StuckAt func _ _) = show (Stuck func)
  show (Diverged func) = printf "The λ function '%s' could not join its two sides" (T.unpack func)
  show (Looping term) = printf "Reduction entered a formation it is already inside: %s" (printExpression term)
  show (LoopingAt term _ _) = show (Looping term)
  show (Undataizable ExTermination _ _) = "dataization reached the terminator ⊥, which signals an error and cannot be dataized"
  show (Undataizable term _ _) = printf "no dataization rule matched, while dataization expects a normal form: %s" (printExpression term)
  show (Unmorphable term) = printf "Morphing expects a normal form, but no morphing rule matches: %s" (printExpression term)
  show (Abnormal Morphing term) = printf "Morphing expects a normal form, but the term at the locator is not one: %s" (printExpression term)
  show (Abnormal _ term) = printf "Dataization expects a normal form, but the term at the locator is not one: %s" (printExpression term)

deeper :: ReduceContext -> IO ReduceContext
deeper ctx@ReduceContext{_steps = Steps limit spent} = do
  clocked ctx
  when (spent >= limit) $ do
    starve ctx._memo
    ctx._saveEval (EvStarved ctx._nesting limit ctx._judgment ctx._site)
    throwIO (OutOfSteps (Depth ctx._judgment limit))
  pure ctx{_steps = Steps limit (spent + 1)}
  where
    starve :: Maybe Memo -> IO ()
    starve Nothing = pure ()
    starve (Just (Memo _ _ _ exhausted _)) = modifyIORef' exhausted (+ 1)

measured :: ReduceContext -> Expression -> IO ()
measured ctx term = do
  forM_ ctx._maxNodes $ \cap ->
    when (countNodes term > cap) (overflown (EvOversized ctx._nesting cap ctx._judgment ctx._site) (Nodes cap))
  forM_ ctx._maxMemory $ \cap -> do
    enabled <- getRTSStatsEnabled
    unless enabled (ioError (userError "--max-memory needs the statistics of the runtime, which this binary was linked without"))
    stats <- getRTSStats
    when (toInteger (gcdetails_live_bytes (gc stats)) > toInteger cap) (overflown (EvBlown ctx._nesting cap ctx._judgment ctx._site) (Memory cap))
  where
    overflown :: Evaluation -> Budget -> IO ()
    overflown record budget = do
      ctx._saveEval record
      throwIO (OutOfSteps budget)

tallied :: Maybe Int -> IO (Maybe Tally)
tallied = traverse (\cap -> Tally cap <$> newIORef 0)

timed :: Maybe Int -> IO (Maybe Deadline)
timed = traverse (\cap -> Deadline cap . (+ fromIntegral cap) <$> getMonotonicTime)

charged :: ReduceContext -> IO ()
charged ctx = do
  clocked ctx
  mapM_ billed ctx._tally
  where
    billed :: Tally -> IO ()
    billed (Tally cap count) = do
      fired <- readIORef count
      when (fired >= cap) $ do
        ctx._saveEval (EvSpent ctx._nesting cap ctx._judgment ctx._site)
        throwIO (OutOfSteps (Firings cap))
      writeIORef count (fired + 1)

clocked :: ReduceContext -> IO ()
clocked ctx = mapM_ clock ctx._deadline
  where
    clock :: Deadline -> IO ()
    clock (Deadline cap due) = do
      now <- getMonotonicTime
      when (now >= due) (expired ctx cap)

expired :: ReduceContext -> Int -> IO a
expired ctx cap = do
  ctx._saveEval (EvTimeout ctx._nesting cap ctx._judgment ctx._site)
  throwIO (OutOfSteps (Seconds cap))

awaited :: ReduceContext -> IO a -> IO a
awaited ctx action = case ctx._deadline of
  Nothing -> action
  Just (Deadline cap due) -> do
    now <- getMonotonicTime
    maybe (expired ctx cap) pure =<< timeout (ceiling (max 0 (due - now) * 1000000)) action

memoized :: Maybe Acyclic -> IO (Maybe Memo)
memoized (Just Plausible) = Just <$> (Memo <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef 0 <*> newIORef 0 <*> (Kin <$> newIORef (Spots Map.empty Map.empty) <*> newIORef Map.empty <*> newIORef Map.empty))
memoized _ = pure Nothing

recalled :: Maybe Memo -> Expression -> Int -> IO (Maybe Kept)
recalled Nothing _ _ = pure Nothing
recalled (Just (Memo store _ answers _ _)) form spent = do
  kept <- readIORef store
  count <- readIORef answers
  let live = [known | (term, (stamp, known)) <- Map.findWithDefault [] (hashExpression form) kept, term == form, current count stamp known]
  pure (find answered live <|> listToMaybe live)
  where
    current :: Int -> Int -> Kept -> Bool
    current count stamp (Stalled _ least) = stamp == count && spent >= least
    current _ _ _ = True
    answered :: Kept -> Bool
    answered (Answered _) = True
    answered _ = False

counted :: Maybe Memo -> IO Int
counted Nothing = pure 0
counted (Just (Memo _ _ answers _ _)) = readIORef answers

starved :: Maybe Memo -> IO Int
starved Nothing = pure 0
starved (Just (Memo _ _ _ exhausted _)) = readIORef exhausted

retained :: Maybe Memo -> Expression -> Int -> Kept -> IO ()
retained Nothing _ _ _ = pure ()
retained (Just (Memo store _ _ _ _)) form stamp kept =
  modifyIORef' store (Map.insertWith (++) (hashExpression form) [(form, (stamp, kept))])

remembered :: Maybe Memo -> Firing -> IO (Maybe (Expression, Answer))
remembered Nothing _ = pure Nothing
remembered (Just (Memo _ firings _ _ _)) firing = Map.lookup firing <$> readIORef firings

remember :: Maybe Memo -> Firing -> Expression -> Answer -> IO ()
remember Nothing _ _ _ = pure ()
remember (Just (Memo _ firings answers _ _)) firing form answer = do
  modifyIORef' firings (Map.insert firing (form, answer))
  modifyIORef' answers (+ 1)

placed :: Maybe Memo -> Expression -> Int -> IO ()
placed (Just (Memo _ _ _ _ (Kin places _ _))) term idx
  | not (bare term) = modifyIORef' places (\spots -> List.foldl' spot spots (List.nub [term, stayed term]))
  where
    spot :: Spots -> Expression -> Spots
    spot spots@(Spots known spelt) spelling = maybe (Spots (Map.insertWith (++) (hashExpression spelling) [(spelling, idx)] known) (Map.insertWith (\_ old -> old) idx spelling spelt)) (const spots) (spotted spots spelling)
    bare :: Expression -> Bool
    bare (ExFormation [BiLambda (FnSymbol _)]) = True
    bare _ = False
placed _ _ _ = pure ()

spotted :: Spots -> Expression -> Maybe Int
spotted (Spots known _) term = lookup term (Map.findWithDefault [] (hashExpression term) known)

abstracted :: Sides -> Spots -> Expression -> IO (Expression, [(Int, Expression)])
abstracted store spots form = do
  cache <- newIORef Map.empty
  goExpr cache form
  where
    goExpr :: IORef (Map.Map T.Text (Expression, [(Int, Expression)])) -> Expression -> IO (Expression, [(Int, Expression)])
    goExpr cache term = maybe (nested cache term) (\idx -> pure (ExFormation [BiLambda (FnSymbol idx)], [(idx, term)])) (spotted spots term)
    nested :: IORef (Map.Map T.Text (Expression, [(Int, Expression)])) -> Expression -> IO (Expression, [(Int, Expression)])
    nested cache term@(ExMeta name)
      | isName name = do
          done <- readIORef cache
          case Map.lookup name done of
            Just again -> pure again
            Nothing -> do
              side <- opened store term
              made <-
                if side == term
                  then pure (term, [])
                  else do
                    (side', posed) <- goExpr cache side
                    if side' == side then pure (term, posed) else (,posed) <$> named store side'
              made <$ modifyIORef' cache (Map.insert name made)
    nested cache (ExFormation bds) = do
      parts <- mapM (goBinding cache) bds
      pure (ExFormation (map fst parts), concatMap snd parts)
    nested cache (ExApplication expr arg) = do
      (expr', one) <- goExpr cache expr
      (arg', two) <- goArgument cache arg
      pure (ExApplication expr' arg', one ++ two)
    nested cache (ExDispatch expr attr) = first (`ExDispatch` attr) <$> goExpr cache expr
    nested _ term = pure (term, [])
    goBinding :: IORef (Map.Map T.Text (Expression, [(Int, Expression)])) -> Binding -> IO (Binding, [(Int, Expression)])
    goBinding cache (BiTau attr expr) = first (BiTau attr) <$> goExpr cache expr
    goBinding _ bd = pure (bd, [])
    goArgument :: IORef (Map.Map T.Text (Expression, [(Int, Expression)])) -> Argument -> IO (Argument, [(Int, Expression)])
    goArgument cache (ArTau attr expr) = first (ArTau attr) <$> goExpr cache expr
    goArgument cache (ArAlpha alpha expr) = first (ArAlpha alpha) <$> goExpr cache expr

kindle :: Sides -> Maybe Memo -> Expression -> Firing -> Answer -> IO ()
kindle store (Just (Memo _ _ _ _ (Kin places likes _))) form firing answer = do
  spots <- readIORef places
  key <- fst <$> abstracted store spots form
  modifyIORef' likes (Map.insertWith (++) (hashShape key) [Kindred key form firing answer])
kindle _ _ _ _ _ = pure ()

minting :: Maybe Memo -> Firing -> Answer -> IO ()
minting (Just (Memo _ _ _ _ (Kin _ _ minters))) firing@(Firing _ operands normals) answer =
  modifyIORef' minters (\known -> List.foldl' (\held idx -> Map.insertWith (\_ old -> old) idx firing held) known [idx | idx <- answered answer, idx `notElem` inputs])
  where
    inputs :: [Int]
    inputs = lefts operands ++ concatMap symbols normals
minting _ _ _ = pure ()

answered :: Answer -> [Int]
answered (Answer built normal) = List.nub (symbols built ++ symbols normal)
answered (Joined normal) = List.nub (symbols normal)
answered Raised = []

kindred :: ReduceContext -> Expression -> IO (Maybe (Kindred, Map.Map Int Int, [(Int, Expression)]))
kindred ctx@ReduceContext{_memo = Just (Memo _ _ _ _ (Kin places likes _))} form = do
  spots <- readIORef places
  known <- readIORef likes
  (key, posed) <- abstracted ctx._sides spots form
  look <- resolver ctx._sides
  let found = listToMaybe [(kin, pairing, posed) | kin@(Kindred before _ _ _) <- Map.findWithDefault [] (hashShape key) known, Just pairing <- [bijection' look before key]]
  awaited ctx (evaluate (isJust found `seq` found))
kindred _ _ = pure Nothing

adopted :: ReduceContext -> (Kindred, Map.Map Int Int, [(Int, Expression)]) -> Expression -> IO Answer
adopted ctx@ReduceContext{_memo = Just (Memo _ _ _ _ (Kin places _ _))} (Kindred key form firing answer, pairing, posed) target = do
  table <- newIORef pairing
  visiting <- newIORef Set.empty
  Spots known _ <- readIORef places
  adopt ctx (Renaming table (Spots known (Map.fromList [(idx, fromMaybe (ExFormation [BiLambda (FnSymbol idx')]) (lookup idx' posed)) | idx <- symbols key, Just idx' <- [Map.lookup idx pairing]])) visiting) firing key form answer (Just target)
adopted _ _ _ = throwIO (userError "A renamed answer needs the memo of --acyclic=plausible")

adopt :: ReduceContext -> Renaming -> Firing -> Expression -> Expression -> Answer -> Maybe Expression -> IO Answer
adopt ctx renaming@(Renaming table _ visiting) firing key form answer target = do
  modifyIORef' visiting (Set.insert firing)
  firing' <- refired ctx renaming firing
  existing <- remembered ctx._memo firing'
  look <- resolver ctx._sides
  case existing >>= \(first', answer') -> (first',answer',) <$> bijection' look (ended answer) (ended answer') of
    Just (first', answer', pairing) -> do
      modifyIORef' table (`Map.union` pairing)
      forM_ target $ \form' -> do
        ctx._saveEval (EvRecalled (ctx._nesting + 1) first' (ended answer'))
        stamp <- counted ctx._memo
        retained ctx._memo form' stamp (Answered answer')
        ctx._saveEval (EvMemoized (ctx._nesting + 1) form' (ended answer'))
      pure answer'
    Nothing -> do
      minters <- held ctx._memo
      let own = [idx | idx <- answered answer, Map.lookup idx minters == Just firing]
      known <- readIORef table
      forM_ [idx | idx <- own, Map.notMember idx known] $ \idx -> do
        fresh <- coined ctx
        modifyIORef' table (Map.insert idx fresh)
      answer' <- reanswered answer
      form' <- maybe (renamedTerm ctx renaming form) pure target
      remember ctx._memo firing' form' answer'
      table' <- readIORef table
      minted' ctx._memo firing' [fresh | idx <- own, Just fresh <- [Map.lookup idx table']]
      stamp <- counted ctx._memo
      retained ctx._memo form' stamp (Answered answer')
      ctx._saveEval (EvRenamed (ctx._nesting + 1) form' (ended answer') form [(idx, idx') | idx <- List.nub (symbols key ++ answered answer), Just idx' <- [Map.lookup idx table'], idx' /= idx])
      pure answer'
  where
    reanswered :: Answer -> IO Answer
    reanswered (Answer built normal) = Answer <$> renamedTerm ctx renaming built <*> renamedTerm ctx renaming normal
    reanswered (Joined normal) = Joined <$> renamedTerm ctx renaming normal
    reanswered Raised = pure Raised
    held :: Maybe Memo -> IO (Map.Map Int Firing)
    held (Just (Memo _ _ _ _ (Kin _ _ minters))) = readIORef minters
    held Nothing = pure Map.empty
    minted' :: Maybe Memo -> Firing -> [Int] -> IO ()
    minted' (Just (Memo _ _ _ _ (Kin _ _ minters))) firing' fresh = modifyIORef' minters (\known -> List.foldl' (\kept idx -> Map.insert idx firing' kept) known fresh)
    minted' Nothing _ _ = pure ()

refired :: ReduceContext -> Renaming -> Firing -> IO Firing
refired ctx renaming (Firing func operands normals) = Firing func <$> mapM operand operands <*> mapM (renamedTerm ctx renaming) normals
  where
    operand :: Either Int Bytes -> IO (Either Int Bytes)
    operand (Left idx) = Left <$> renamedSymbol ctx renaming idx
    operand bytes = pure bytes

renamedTerm :: ReduceContext -> Renaming -> Expression -> IO Expression
renamedTerm ctx renaming@(Renaming _ spots@(Spots _ spelt) _) term = do
  renamed <- newIORef Map.empty
  goExpr renamed term
  where
    goExpr :: IORef (Map.Map T.Text Expression) -> Expression -> IO Expression
    goExpr renamed expr = maybe (nested renamed expr) (spot renamed expr) (spotted spots expr)
    spot :: IORef (Map.Map T.Text Expression) -> Expression -> Int -> IO Expression
    spot renamed expr idx = maybe (nested renamed expr) pure (Map.lookup idx spelt)
    nested :: IORef (Map.Map T.Text Expression) -> Expression -> IO Expression
    nested renamed expr@(ExMeta name)
      | isName name = do
          done <- readIORef renamed
          case Map.lookup name done of
            Just again -> pure again
            Nothing -> do
              side <- opened ctx._sides expr
              made <- if side == expr then pure expr else named ctx._sides =<< goExpr renamed side
              made <$ modifyIORef' renamed (Map.insert name made)
    nested renamed expr = walked renamed expr
    walked :: IORef (Map.Map T.Text Expression) -> Expression -> IO Expression
    walked renamed (ExFormation bds) = ExFormation <$> mapM (goBinding renamed) bds
    walked renamed (ExApplication expr arg) = ExApplication <$> goExpr renamed expr <*> goArgument renamed arg
    walked renamed (ExDispatch expr attr) = (`ExDispatch` attr) <$> goExpr renamed expr
    walked _ expr = pure expr
    goBinding :: IORef (Map.Map T.Text Expression) -> Binding -> IO Binding
    goBinding renamed (BiTau attr expr) = BiTau attr <$> goExpr renamed expr
    goBinding _ (BiLambda (FnSymbol idx)) = BiLambda . FnSymbol <$> renamedSymbol ctx renaming idx
    goBinding _ bd = pure bd
    goArgument :: IORef (Map.Map T.Text Expression) -> Argument -> IO Argument
    goArgument renamed (ArTau attr expr) = ArTau attr <$> goExpr renamed expr
    goArgument renamed (ArAlpha alpha expr) = ArAlpha alpha <$> goExpr renamed expr

renamedSymbol :: ReduceContext -> Renaming -> Int -> IO Int
renamedSymbol ctx renaming@(Renaming table _ visiting) idx = do
  known <- readIORef table
  maybe unknown pure (Map.lookup idx known)
  where
    unknown :: IO Int
    unknown = do
      minter <- case ctx._memo of
        Just (Memo _ _ _ _ (Kin _ _ minters)) -> Map.lookup idx <$> readIORef minters
        Nothing -> pure Nothing
      seen <- readIORef visiting
      case minter of
        Just firing | Set.notMember firing seen -> do
          kept <- remembered ctx._memo firing
          forM_ kept (\(form, answer) -> adopt ctx renaming firing form form answer Nothing)
          maybe fresh pure . Map.lookup idx =<< readIORef table
        Just _ -> fresh
        Nothing -> idx <$ modifyIORef' table (Map.insert idx idx)
    fresh :: IO Int
    fresh = do
      made <- coined ctx
      made <$ modifyIORef' table (Map.insert idx made)

parking :: NonEmpty Rewritten -> State -> IO a -> IO a
parking seq state action = action `catch` rethrow
  where
    rethrow :: ReduceException -> IO a
    rethrow (Stuck func) = throwIO (StuckAt func seq state)
    rethrow (OutOfSteps budget) = throwIO (OutOfStepsAt budget seq state)
    rethrow (Looping term) = throwIO (LoopingAt term seq state)
    rethrow failure = throwIO failure

unparked :: IO a -> IO a
unparked action = action `catch` rethrow
  where
    rethrow :: ReduceException -> IO a
    rethrow (StuckAt func _ _) = throwIO (Stuck func)
    rethrow (OutOfStepsAt budget _ _) = throwIO (OutOfSteps budget)
    rethrow (LoopingAt term _ _) = throwIO (Looping term)
    rethrow failure = throwIO failure

admitting :: Expression -> ReduceContext -> IO (Either (Acyclic, Expression) ReduceContext)
admitting term ctx = maybe (pure (Right ctx)) (`admitted` ctx) (entrance ctx._judgment term)

enter :: Expression -> ReduceContext -> IO ReduceContext
enter form ctx = admitted form ctx >>= either (refused form ctx) pure

refused :: Expression -> ReduceContext -> (Acyclic, Expression) -> IO a
refused form ctx (mode, before) = do
  ctx._saveEval (EvLooped ctx._nesting ctx._judgment mode before ctx._site Nothing)
  throwIO (Looping form)

severing :: Expression -> Expression -> State -> ReduceContext -> IO (Expression, ReduceContext)
severing term univ state ctx = case entrance ctx._judgment term of
  Just form -> either (,ctx) (term,) <$> sever (over term) form univ state ctx
  Nothing -> pure (term, ctx)
  where
    over :: Expression -> Maybe Attribute
    over (ExDispatch _ attr) = Just attr
    over _ = Nothing

sever :: Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Either Expression ReduceContext)
sever taken form univ state ctx = admitted form ctx >>= either cut (pure . Right)
  where
    cut :: (Acyclic, Expression) -> IO (Either Expression ReduceContext)
    cut loop = case form of
      ExFormation bds -> Left <$> severed taken univ ctx state form bds loop
      _ -> refused form ctx loop

severed :: Maybe Attribute -> Expression -> ReduceContext -> State -> Expression -> [Binding] -> (Acyclic, Expression) -> IO Expression
severed taken univ ctx state form bds (mode, before) = do
  call <- awaited ctx (evaluate =<< ((<|>) <$> traced ctx._facts form <*> traced ctx._facts before))
  forM_ call (recited ctx . snd)
  reckoned <- traverse (spelled taken . fst) call
  fresh <- coined ctx
  ctx._saveEval (EvLooped ctx._nesting ctx._judgment mode before ctx._site (Just (fresh, reckoned)))
  pure (ExFormation [BiLambda (FnSymbol fresh)])
  where
    spelled :: Maybe Attribute -> Expression -> IO Expression
    spelled (Just attr) found = pure (ExDispatch found attr)
    spelled Nothing found
      | ctx._reckoning = pure found
      | otherwise = argued univ ctx{_reckoning = True} state form bds found

recited :: ReduceContext -> [(Expression, Expression)] -> IO ()
recited ctx = mapM_ (\(term, source) -> ctx._saveEval (EvRecalled ctx._nesting term source))

factual :: IO Facts
factual = Facts <$> newIORef Map.empty <*> newIORef (Lodgings Map.empty Map.empty)

noted :: ReduceContext -> Expression -> Expression -> IO ()
noted ctx object source = forM_ ctx._facts $ \(Facts facts lodgings) -> do
  known <- (Just source ==) . lookup object . Map.findWithDefault [] (hashExpression object) <$> readIORef facts
  unless known $ do
    modifyIORef' facts (Map.insertWith (++) (hashExpression object) [(object, source)])
    modifyIORef' lodgings (\index -> fst (lodged Nothing index object))
    ctx._saveEval (EvRemembered ctx._nesting object source)
  where
    lodged :: Maybe (Expression, Attribute) -> Lodgings -> Expression -> (Lodgings, Expression)
    lodged edge index@(Lodgings nodes shapes) term@(ExFormation bds) = case [node | (node, _) <- Map.findWithDefault [] (hashExpression term) nodes, node == term] of
      node : _ -> (maybe index (\parent -> Lodgings (Map.adjust (map (adopted node parent)) (hashExpression node) nodes) shapes) edge, node)
      [] -> case List.foldl' (lodging term) (index, []) [(attr, body) | BiTau attr body@(ExFormation _) <- bds, attr /= AtRho] of
        (Lodgings inner shapes', kids) -> (Lodgings (Map.insertWith (++) (hashExpression term) [(term, Lodging (maybeToList edge) (reverse kids))] inner) (Map.insertWith (++) (shaped bds) [term] shapes'), term)
    lodged _ index term = (index, term)
    lodging :: Expression -> (Lodgings, [(Attribute, Expression)]) -> (Attribute, Expression) -> (Lodgings, [(Attribute, Expression)])
    lodging parent (index, kids) (attr, body) = case lodged (Just (parent, attr)) index body of
      (index', kid) -> index' `seq` (index', (attr, kid) : kids)
    adopted :: Expression -> (Expression, Attribute) -> (Expression, Lodging) -> (Expression, Lodging)
    adopted node parent (term, Lodging parents kids)
      | term == node = (term, Lodging (parent : parents) kids)
      | otherwise = (term, Lodging parents kids)

shaped :: [Binding] -> Int
shaped bds = hashExpression (ExFormation (rholess bds))

rholess :: [Binding] -> [Binding]
rholess = filter ((/= Just AtRho) . attributeFromBinding)

remembering :: Maybe Facts -> Expression -> IO Bool
remembering Nothing _ = pure False
remembering (Just (Facts facts _)) term = isJust . lookup term . Map.findWithDefault [] (hashExpression term) <$> readIORef facts

traced :: Maybe Facts -> Expression -> IO (Maybe (Expression, [(Expression, Expression)]))
traced Nothing _ = pure Nothing
traced (Just (Facts facts lodgings)) form = do
  known <- readIORef facts
  index <- readIORef lodgings
  pure (go index known [] form)
  where
    go :: Lodgings -> Store Expression -> [Expression] -> Expression -> Maybe (Expression, [(Expression, Expression)])
    go index known passed term = listToMaybe [(named, (object, source) : hops) | (path, object, source) <- lodged index known term, object `notElem` passed, let (base, hops) = resolved index known (object : passed) source, Just named <- [rooted (foldl ExDispatch base path)]]
    resolved :: Lodgings -> Store Expression -> [Expression] -> Expression -> (Expression, [(Expression, Expression)])
    resolved index known passed source = case lookup source (Map.findWithDefault [] (hashExpression source) known) of
      Just _ -> fromMaybe (source, []) (go index known passed source)
      Nothing -> case source of
        ExApplication target arg -> first (`ExApplication` arg) (resolved index known passed target)
        ExDispatch target attr -> first (`ExDispatch` attr) (resolved index known passed target)
        _ -> (source, [])
    rooted :: Expression -> Maybe Expression
    rooted term = term <$ guard (grounded term)
    grounded :: Expression -> Bool
    grounded ExRoot = True
    grounded (ExDispatch target _) = grounded target
    grounded (ExApplication target _) = grounded target
    grounded _ = False
    lodged :: Lodgings -> Store Expression -> Expression -> [([Attribute], Expression, Expression)]
    lodged (Lodgings nodes shapes) known (ExFormation bds) = sortOn (\(path, _, _) -> length path) [(descended distances object, object, source) | digest <- Set.toAscList (Set.fromList (map hashExpression (concatMap (map fst) (Map.elems distances)))), (object, source) <- Map.findWithDefault [] digest known, isJust (lookup object (Map.findWithDefault [] digest distances))]
      where
        distances :: Store Int
        distances = spread 0 matched (Map.fromListWith (++) [(hashExpression node, [(node, 0)]) | node <- matched])
        matched :: [Expression]
        matched = [node | node@(ExFormation inner) <- Map.findWithDefault [] (shaped bds) shapes, rholess inner == rholess bds]
        spread :: Int -> [Expression] -> Store Int -> Store Int
        spread _ [] reached = reached
        spread depth frontier reached = uncurry (spread (succ depth)) (List.foldl' (climbed (succ depth)) ([], reached) [parent | node <- frontier, (parent, _) <- lodging node])
        climbed :: Int -> ([Expression], Store Int) -> Expression -> ([Expression], Store Int)
        climbed depth (next, reached) parent
          | isJust (lookup parent (Map.findWithDefault [] (hashExpression parent) reached)) = (next, reached)
          | otherwise = (parent : next, Map.insertWith (++) (hashExpression parent) [(parent, depth)] reached)
        descended :: Store Int -> Expression -> [Attribute]
        descended reached node = case lookup node (Map.findWithDefault [] (hashExpression node) reached) of
          Just depth | depth > 0 -> concat (take 1 [attr : descended reached kid | (attr, kid) <- children node, lookup kid (Map.findWithDefault [] (hashExpression kid) reached) == Just (pred depth)])
          _ -> []
        lodging :: Expression -> [(Expression, Attribute)]
        lodging node = maybe [] (\(Lodging parents _) -> parents) (lookup node (Map.findWithDefault [] (hashExpression node) nodes))
        children :: Expression -> [(Attribute, Expression)]
        children node = maybe [] (\(Lodging _ kids) -> kids) (lookup node (Map.findWithDefault [] (hashExpression node) nodes))
    lodged _ _ _ = []

coined :: ReduceContext -> IO Int
coined ctx = atomicModifyIORef' ctx._minted (\count -> (succ count, succ count))

fired :: Expression -> ReduceContext -> Expression -> State -> Expression -> IO (Expression, State)
fired univ ctx site state form = sever Nothing form univ state ctx{_judgment = Evaluation, _site = site} >>= either (pure . (,state)) kindled
  where
    kindled :: ReduceContext -> IO (Expression, State)
    kindled entered = do
      deep <- deeper entered
      deep._evaluate deep state form univ

voided :: ReduceContext -> Expression -> IO (Maybe [Attribute])
voided ReduceContext{_facts = Nothing} _ = pure Nothing
voided ctx@ReduceContext{_facts = Just (Facts facts _)} form@(ExFormation bds) = do
  known <- readIORef facts
  case lookup form (Map.findWithDefault [] (hashExpression form) known) of
    Just source -> do
      let (voids, hops) = placed known (Map.size known) bds source
      recited ctx ((form, source) : hops)
      pure voids
    Nothing -> pure Nothing
  where
    placed :: Store Expression -> Int -> [Binding] -> Expression -> (Maybe [Attribute], [(Expression, Expression)])
    placed known budget _ (ExApplication head' _) = headed known budget head'
    placed _ _ own (ExDispatch _ _) = (Just [attr | BiVoid attr <- own], [])
    placed _ _ _ _ = (Nothing, [])
    headed :: Store Expression -> Int -> Expression -> (Maybe [Attribute], [(Expression, Expression)])
    headed known budget (ExApplication head' _) = headed known budget head'
    headed known budget head'@(ExFormation inner) = case lookup head' (Map.findWithDefault [] (hashExpression head') known) of
      Just source | budget > 0 -> second ((head', source) :) (placed known (budget - 1) inner source)
      _ -> (Just [attr | BiVoid attr <- inner], [])
    headed _ _ _ = (Nothing, [])
voided _ _ = pure Nothing

called :: Expression -> ReduceContext -> Expression -> [Binding] -> IO (Maybe Expression)
called univ ctx form known = fmap (uncurry applied) <$> declared univ ctx form
  where
    applied :: Expression -> [Binding] -> Expression
    applied path voids = foldl ExApplication path [ArTau attr value | BiTau attr value <- known, attr /= AtRho, BiVoid attr `elem` voids]

declared :: Expression -> ReduceContext -> Expression -> IO (Maybe (Expression, [Binding]))
declared univ ctx form = traced ctx._facts form >>= maybe (pure Nothing) placed
  where
    placed :: (Expression, [(Expression, Expression)]) -> IO (Maybe (Expression, [Binding]))
    placed (call, hops) = do
      recited ctx hops
      pure ((erased call,) <$> located (erased call) (fromMaybe univ ctx._universe))
    located :: Expression -> Expression -> Maybe [Binding]
    located ExRoot (ExFormation bds) = Just bds
    located (ExDispatch path attr) whole = case located path whole of
      Just siblings -> listToMaybe [inner | BiTau attr' (ExFormation inner) <- siblings, attr' == attr]
      Nothing -> Nothing
    located _ _ = Nothing
    erased :: Expression -> Expression
    erased (ExApplication target _) = erased target
    erased (ExDispatch target attr) = ExDispatch (erased target) attr
    erased other = other

argued :: Expression -> ReduceContext -> State -> Expression -> [Binding] -> Expression -> IO Expression
argued univ ctx state site bds call = foldl ExDispatch <$> go inner <*> pure attrs
  where
    attrs :: [Attribute]
    inner :: Expression
    (attrs, inner) = dispatched call
    go :: Expression -> IO Expression
    go (ExApplication target (ArTau attr body)) = do
      target' <- go target
      ExApplication target' . ArTau attr <$> reckoned attr body
    go term = pure term
    reckoned :: Attribute -> Expression -> IO Expression
    reckoned _ body@(ExFormation [BiLambda (FnSymbol _)]) = pure body
    reckoned attr body = do
      placed <- ctx._engine._contextualize body (ExFormation (filter (not . named attr) bds))
      figured (ExDispatch site attr) placed
    named :: Attribute -> Binding -> Bool
    named attr (BiTau attr' _) = attr == attr'
    named _ _ = False
    figured :: Expression -> Expression -> IO Expression
    figured place placed = unparked $ do
      (answer, state') <- worked place placed state
      if hanging answer
        then fst <$> worked (ExDispatch place AtPhi) (ExDispatch answer AtPhi) state'
        else pure answer
      where
        worked :: Expression -> Expression -> State -> IO (Expression, State)
        worked place' placed' state' = do
          (normal, morphed) <- settled placed' univ state' =<< opening Morphing ctx{_site = place'}
          case normal of
            ExFormation _
              | thrown ctx._raising normal -> pure (normal, morphed)
              | otherwise -> fired univ ctx place' morphed normal
            _ -> pure (normal, morphed)
        hanging :: Expression -> Bool
        hanging (ExFormation bds) = not (any isLambda bds) && or [not (xiFree body) | BiTau AtPhi body <- bds]
        hanging _ = False

admitted :: Expression -> ReduceContext -> IO (Either (Acyclic, Expression) ReduceContext)
admitted form ctx = maybe (pure (Right ctx)) remembered ctx._acyclic
  where
    remembered :: Acyclic -> IO (Either (Acyclic, Expression) ReduceContext)
    remembered mode = maybe (Right ctx{_entered = seenInsert (digest mode form) form ctx._entered}) (\before -> Left (mode, before)) . listToMaybe . drop ctx._maxLoops <$> repeats mode form ctx

repeats :: Acyclic -> Expression -> ReduceContext -> IO [Expression]
repeats mode form ctx = do
  look <- resolver ctx._sides
  let hits = take (succ ctx._maxLoops) (filter (repeated look mode) (Map.findWithDefault [] (digest mode form) ctx._entered))
  awaited ctx (evaluate (length hits `seq` hits))
  where
    repeated :: (T.Text -> Maybe Expression) -> Acyclic -> Expression -> Bool
    repeated look Proven before = alike' look form before
    repeated look Plausible before
      | atomic form = akin' look before form
      | otherwise = within' look before form
    atomic :: Expression -> Bool
    atomic (ExFormation bds) = any isLambda bds
    atomic _ = False

digest :: Acyclic -> Expression -> Int
digest Proven = hashShape
digest Plausible = hashSkeleton

entrance :: Judgment -> Expression -> Maybe Expression
entrance Dataization term@(ExFormation bds)
  | boxed bds || isJust (lambda bds) = Just term
entrance Morphing (ExDispatch form@(ExFormation bds) _)
  | isJust (lambda bds) = Just form
entrance Evaluation term@(ExFormation bds)
  | isJust (lambda bds) = Just term
entrance _ _ = Nothing

lambda :: [Binding] -> Maybe (T.Text, Expression)
lambda bds = case partition isLambda bds of
  ([BiLambda (Function func)], rest) -> Just (func, ExFormation rest)
  _ -> Nothing

isLambda :: Binding -> Bool
isLambda (BiLambda _) = True
isLambda _ = False

morph' :: Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
morph' = morph'' True

morph'' :: Bool -> Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
morph'' owner start univ state entry = go start univ state entry
  where
    go :: Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
    go (expr@(ExMeta name), seq) univ state caller
      | isName name = do
          side <- opened caller._sides expr
          if side == expr then throwIO (Unmorphable expr) else go (side, seq) univ state caller
    go (expr, seq) univ state caller = do
      (term, entered) <- severing expr univ state =<< parking seq state (universed univ caller{_judgment = Morphing})
      ctx <- opening Morphing =<< deeper entered
      parking seq state $ do
        reached <- maybe (inferred term univ state ctx ctx._engine._morphing) (\raise -> pure (Just (In.Answered (Morphing, "raise") raise, state))) (raised term)
        case reached of
          Just (In.Answered step built, state') -> do
            when owner (ctx._saveEval (EvMorphed ctx._nesting built))
            seq' <- leadsTo seq step built ctx
            pure ((built, seq'), state')
          Just (In.Onward way built world, state') -> do
            (morphed, state'') <- onward seq state' way univ built ctx
            go morphed world state'' ctx
          Nothing -> throwIO (Unmorphable term)
      where
        raised :: Expression -> Maybe Expression
        raised (ExDispatch target@(ExFormation _) _)
          | thrown caller._raising target = Just target
        raised _ = Nothing

morph :: Expression -> State -> ReduceContext -> IO (Expression, [Rewritten], State)
morph universe state caller@ReduceContext{..} = do
  expr <- locatedExpression _locator universe
  vetted Morphing expr caller
  prepared <- try (universed universe caller)
  case prepared of
    Left (OutOfSteps _) | _partial -> pure (expr, [(universe, Nothing)], state)
    Left failure -> throwIO (failure :: ReduceException)
    Right univCtx -> reduced expr =<< opening Morphing univCtx
  where
    reduced :: Expression -> ReduceContext -> IO (Expression, [Rewritten], State)
    reduced expr ctx = do
      result <- try (morph' (expr, (universe, Nothing) :| []) universe state ctx)
      case result of
        Right ((morphed, seq), state') -> pure (morphed, reverse (NE.toList seq), state')
        Left (StuckAt func seq parked) | _partial -> residue seq parked{_stuck = Just func}
        Left (OutOfStepsAt _ seq parked) | _partial -> residue seq parked
        Left (OutOfSteps _) | _partial -> pure (expr, [(universe, Nothing)], state)
        Left (LoopingAt _ seq parked) -> residue seq parked
        Left failure -> throwIO (failure :: ReduceException)
    residue :: NonEmpty Rewritten -> State -> IO (Expression, [Rewritten], State)
    residue seq parked = do
      expr <- locatedExpression _locator (fst (NE.head seq))
      pure (expr, reverse (NE.toList seq), parked)

vetted :: Judgment -> Expression -> ReduceContext -> IO ()
vetted judgment expr ctx = unless (ctx._engine._normal expr) (throwIO (Abnormal judgment expr))

inferred :: Expression -> Expression -> State -> ReduceContext -> [In.Inference value] -> IO (Maybe (In.Conclusion value, State))
inferred expr univ state ctx rules = do
  ordered <- if ctx._shuffle then shuffle rules else pure rules
  matched <- go ordered
  traverse (premised state) matched
  where
    go :: [In.Inference value] -> IO (Maybe (In.Premises value))
    go [] = pure Nothing
    go (rule : rest) = rule (RuleContext (execBuildTerm univ ctx) (Just univ) ctx._engine._normal) expr univ >>= maybe (go rest) (pure . Just)
    premised :: State -> In.Premises value -> IO (In.Conclusion value, State)
    premised state' (In.Concludes conclusion) = pure (conclusion, state')
    premised state' (In.Morphs term world next) = do
      (morphed, state'') <- detached term world state' ctx
      next morphed >>= premised state''
    premised state' (In.Evaluates form world next) = do
      (answer, state'') <- ctx._evaluate ctx state' form world
      next answer >>= premised state''
    premised state' (In.Contextualizes term context next) = ctx._engine._contextualize term context >>= next >>= premised state'

onward :: NonEmpty Rewritten -> State -> In.Way -> Expression -> Expression -> ReduceContext -> IO (Morphed, State)
onward seq state (In.Taken step) _ expr ctx = do
  seq' <- leadsTo seq step expr ctx
  pure ((expr, seq'), state)
onward seq state (In.Normalized step) univ expr ctx = do
  labelled <- leadsTo seq step expr ctx
  normal@(term, _) <- normalized expr labelled ctx
  unless (term `elem` [expr, bare expr]) (ctx._saveEval (EvNormalized ctx._nesting (rooted expr) term))
  pure (normal, state)
  where
    bare :: Expression -> Expression
    bare (ExApplication inner _) = bare inner
    bare inner = inner
    rooted :: Expression -> Expression
    rooted inner
      | inner == univ = ExRoot
    rooted (ExDispatch inner attr) = ExDispatch (rooted inner) attr
    rooted (ExApplication inner arg) = ExApplication (rooted inner) arg
    rooted inner = inner
onward seq state (In.Named step) univ expr ctx = case ctx._universe of
  Just world -> onward seq state (In.Taken step) univ world ctx
  Nothing -> onward seq state (In.Normalized step) univ expr ctx
onward seq state (In.Staged stage) _ expr ctx = morph' (expr, seq) stage state ctx

leadsTo :: NonEmpty Rewritten -> (Judgment, String) -> Expression -> ReduceContext -> IO (NonEmpty Rewritten)
leadsTo ((current, _) :| rest) rule expr ReduceContext{..} = do
  updated <- withLocatedExpression _locator expr current
  pure ((updated, Nothing) :| (current, Just rule) : rest)

normalized :: Expression -> NonEmpty Rewritten -> ReduceContext -> IO (Expression, NonEmpty Rewritten)
normalized expr seq ctx@ReduceContext{..} = do
  measured ctx expr
  taken <- newIORef Map.empty
  peeled <- newIORef Map.empty
  copies <- newIORef Map.empty
  go taken peeled copies expr seq
  where
    go :: IORef (Store Expression) -> IORef (Store Expression) -> IORef (Store Expression) -> Expression -> NonEmpty Rewritten -> IO (Expression, NonEmpty Rewritten)
    go taken peeled copies term chain = do
      whole <- withLocatedExpression _locator term (fst (NE.head chain))
      (rewrittens, exceeded) <- rewrite whole _engine._normalization (rewriteContext taken peeled copies ctx)
      when exceeded $ do
        _saveEval (EvExceeded _nesting _maxCycles _judgment _site)
        throwIO (OutOfSteps (Cycles _maxCycles))
      let (rw :| rws) = NE.reverse rewrittens
          chain' = rw :| rws <> NE.tail chain
      term' <- locatedExpression _locator (fst rw)
      look <- resolver _sides
      let unfolded = spined look False term'
      if unfolded == term' then pure (term', chain') else go taken peeled copies unfolded chain'
    spined :: (T.Text -> Maybe Expression) -> Bool -> Expression -> Expression
    spined look _ (ExDispatch term attr) = ExDispatch (spined look True term) attr
    spined look _ (ExApplication term arg) = ExApplication (spined look True term) arg
    spined look True term@(ExMeta name) = fromMaybe term (look name)
    spined _ _ term = term
    rewriteContext :: IORef (Store Expression) -> IORef (Store Expression) -> IORef (Store Expression) -> ReduceContext -> RewriteContext
    rewriteContext taken peeled copies whole@ReduceContext{..} =
      RewriteContext _locator _maxDepth _maxCycles _depthSensitive _universe _buildTerm _engine._normal _engine._matching MtDisabled Nothing (\term -> (measured whole =<< locatedExpression _locator term) >> _saveStep term) (made taken peeled copies whole)
    made :: IORef (Store Expression) -> IORef (Store Expression) -> IORef (Store Expression) -> ReduceContext -> Expression -> Expression -> IO ()
    made _ _ copies whole redex@(ExApplication head' (ArTau attr _)) object@(ExFormation bds)
      | attr /= AtRho && object /= head' = do
          made' <- readIORef copies
          if lookup redex (Map.findWithDefault [] (hashExpression redex) made') == Just object
            then whole._saveEval (EvRecalled whole._nesting (called whole._universe redex) object)
            else do
              modifyIORef' copies (Map.insertWith (++) (hashExpression redex) [(redex, object)])
              whole._saveEval (EvApplied whole._nesting whole._judgment (called whole._universe redex) object whole._site)
              known <- remembering whole._facts head'
              when (known && copied bds) (noted whole object redex)
    made taken _ _ whole redex@(ExApplication _ (ArTau AtRho _)) object@(ExFormation bds)
      | copied bds = do
          pending <- readIORef taken
          forM_ (lookup redex (Map.findWithDefault [] (hashExpression redex) pending)) (noted whole object)
    made taken peeled _ whole redex@(ExDispatch target attr) object
      | attr /= AtRho = do
          stripped peeled whole target attr
          dispatched taken peeled whole redex object
    made _ _ _ _ _ _ = pure ()
    stripped :: IORef (Store Expression) -> ReduceContext -> Expression -> Attribute -> IO ()
    stripped peeled whole target@(ExFormation bds) attr = do
      known <- remembering whole._facts target
      when known (modifyIORef' peeled (Map.insertWith (++) (hashExpression (left bds)) [(left bds, target)]))
      where
        left :: [Binding] -> Expression
        left inner = ExFormation [bd | bd <- inner, attributeFromBinding bd /= Just attr]
    stripped _ _ _ _ = pure ()
    dispatched :: IORef (Store Expression) -> IORef (Store Expression) -> ReduceContext -> Expression -> Expression -> IO ()
    dispatched taken peeled whole redex object@(ExApplication (ExFormation bds) (ArTau AtRho _))
      | copied bds = sourced peeled whole redex >>= mapM_ (\path -> modifyIORef' taken (Map.insertWith (++) (hashExpression object) [(object, path)]))
    dispatched _ peeled whole redex object@(ExFormation bds)
      | copied bds = sourced peeled whole redex >>= mapM_ (noted whole object)
    dispatched taken peeled whole redex (ExApplication name@(ExMeta _) rho@(ArTau AtRho _)) = do
      side <- opened whole._sides name
      case side of
        ExFormation bds
          | copied bds && side /= name ->
              let object = ExApplication side rho
               in sourced peeled whole redex >>= mapM_ (\path -> modifyIORef' taken (Map.insertWith (++) (hashExpression object) [(object, path)]))
        _ -> pure ()
    dispatched _ _ _ _ _ = pure ()
    sourced :: IORef (Store Expression) -> ReduceContext -> Expression -> IO (Maybe Expression)
    sourced peeled whole redex@(ExDispatch target attr) = case worldly whole._universe redex of
      Just path -> pure (Just path)
      Nothing -> do
        known <- remembering whole._facts target
        held <- readIORef peeled
        pure (if known then Just redex else (`ExDispatch` attr) <$> lookup target (Map.findWithDefault [] (hashExpression target) held))
    sourced _ _ _ = pure Nothing
    copied :: [Binding] -> Bool
    copied bds = boxed bds || isJust (lambda bds)
    worldly :: Maybe Expression -> Expression -> Maybe Expression
    worldly universe (ExDispatch target attr) = (`ExDispatch` attr) <$> worldly universe target
    worldly _ ExRoot = Just ExRoot
    worldly universe target = ExRoot <$ guard (Just target == universe)
    called :: Maybe Expression -> Expression -> Expression
    called universe (ExApplication head' arg) = ExApplication (nameIn universe head') arg
    called _ redex = redex

universed :: Expression -> ReduceContext -> IO ReduceContext
universed _ ctx@ReduceContext{_universe = Just _} = pure ctx
universed univ ctx = do
  (normal, _) <- normalized univ ((univ, Nothing) :| []) ctx{_locator = ExRoot, _maxNodes = Nothing, _saveStep = dontSaveStep}
  ctx._saveEval (EvWorld normal)
  pure ctx{_universe = Just normal}

insideUniverse :: Expression -> Expression -> ReduceContext -> IO (Expression, ReduceContext)
insideUniverse expr univ ctx@ReduceContext{_buildTerm = buildTerm} = case univ of
  ExFormation bds -> do
    (TeAttribute attr) <- buildTerm "random-tau" [] substEmpty
    let aiming = ctx{_locator = ExDispatch ExRoot attr}
        synthetic = ExFormation (BiTau attr expr : bds)
    (normal, _) <- normalized expr ((synthetic, Nothing) :| []) aiming
    pure (ExFormation (BiTau attr normal : bds), aiming{_universe = extended attr normal})
  _ -> throwIO (userError "Can't reduce an expression inside a universe which is not a formation")
  where
    extended :: Attribute -> Expression -> Maybe Expression
    extended attr normal = case ctx._universe of
      Just (ExFormation bds) -> Just (ExFormation (BiTau attr normal : bds))
      _ -> Nothing

opening :: Judgment -> ReduceContext -> IO ReduceContext
opening judgment ctx
  | ctx._opened == Just (ctx._nesting, judgment, ctx._site) = pure ctx{_judgment = judgment}
  | otherwise = do
      ctx._saveEval (EvStarted ctx._nesting judgment ctx._site)
      pure ctx{_judgment = judgment, _nesting = ctx._nesting + 1, _opened = Just (ctx._nesting + 1, judgment, ctx._site)}

settled :: Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
settled term univ state ctx = do
  (normal, _) <- normalized term ((univ, Nothing) :| []) ctx
  ((morphed, _), state') <- morph' (normal, (univ, Nothing) :| []) univ state ctx
  pure (morphed, state')

morphing :: Expression -> ReduceContext -> Expression -> State -> IO (Expression, State)
morphing univ ctx expr state = do
  (universe, aiming) <- insideUniverse expr univ ctx
  normal <- locatedExpression aiming._locator universe
  unparked $ do
    opened <- opening Morphing =<< universed universe aiming
    ((morphed, _), state') <- morph' (normal, (universe, Nothing) :| []) universe state{_copies = []} opened
    recurring morphed universe state'{_copies = state._copies ++ state'._copies} opened
  where
    recurring :: Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
    recurring term@(ExFormation bds) world state' opened
      | Just mode <- opened._acyclic
      , boxed bds = do
          hits <- repeats mode term opened
          case drop opened._maxLoops hits of
            before : _ -> (,state') <$> severed Nothing world opened state' term bds (mode, before)
            []
              | null hits -> pure (term, state')
              | otherwise -> pure (term, state'{_copies = state'._copies ++ [term]})
    recurring term _ state' _ = pure (term, state')

carried :: State -> ReduceContext -> (State, ReduceContext)
carried state ctx = (state{_copies = []}, maybe ctx (\mode -> ctx{_entered = foldr (\form -> seenInsert (digest mode form) form) ctx._entered state._copies}) ctx._acyclic)

execBuildTerm :: Expression -> ReduceContext -> BuildTermFunc
execBuildTerm _ ctx "evaluate" = evaluated ctx
execBuildTerm univ ctx "morph" = _morph univ ctx
execBuildTerm _ ctx func = _buildTerm ctx func

evaluated :: ReduceContext -> BuildTermMethod
evaluated ctx [ArgExpression expr, ArgExpression universe] subst = do
  form <- buildExpressionThrows expr subst
  world <- buildExpressionThrows universe subst
  TeExpression . fst <$> ctx._evaluate ctx emptyState form world
evaluated _ _ _ = throwIO (userError "Function evaluate() requires exactly 2 expression arguments")

_morph :: Expression -> ReduceContext -> BuildTermMethod
_morph univ ctx [ArgExpression expr] subst = do
  built <- buildExpressionThrows expr subst
  TeExpression . fst <$> detached built univ emptyState ctx
_morph _ _ _ _ = throwIO (userError "Function morph() requires exactly 1 expression argument")

detached :: Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
detached expr univ state ctx = unparked $ do
  ((morphed, _), state') <- morph'' (ctx._judgment /= Morphing) (expr, (univ, Nothing) :| []) univ state ctx
  pure (morphed, state')
