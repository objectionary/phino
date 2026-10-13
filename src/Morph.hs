{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Morph (Answer, Deadline (..), Firing (..), Kept (..), ReduceContext (..), ReduceException (..), EvaluationFunc, Memo (..), ReductionFunc, Morphed, Steps (..), Tally (..), boxed, charged, counted, deeper, emptyState, enter, entering, execBuildTerm, inferred, insideUniverse, isLambda, lambda, leadsTo, memoized, morph, morph', morphing, normalized, onward, opening, parking, recalled, remember, remembered, retained, settled, starved, tallied, timed, universed, unparked) where

import AST
import Builder (buildExpressionThrows, nameIn)
import Control.Applicative ((<|>))
import Control.Exception (Exception, catch, evaluate, throwIO, try)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Text as T
import Deps (Acyclic (..), BuildTermFunc, BuildTermMethod, Evaluation (..), Judgment (..), SaveEvalFunc, SaveStepFunc, State (..), Term (..), dontSaveStep)
import Engine (Engine (..))
import GHC.Clock (getMonotonicTime)
import qualified Inference as In
import Lambdas (Lambdas)
import Locator (locatedExpression, withLocatedExpression)
import Matcher (substEmpty)
import Must (Must (..))
import Printer (printExpression)
import Random (shuffle)
import Rewriter (RewriteContext (RewriteContext), Rewritten, Seen, rewrite, seenInsert)
import Rule (RuleContext (RuleContext))
import System.Timeout (timeout)
import Text.Printf (printf)
import Yaml (ExtraArgument (..))

type Morphed = (Expression, NonEmpty Rewritten)

type ReductionFunc = Expression -> ReduceContext -> Expression -> State -> IO (Maybe Bytes, State)

type EvaluationFunc = ReduceContext -> State -> Expression -> Expression -> IO (Expression, State)

emptyState :: State
emptyState = State Nothing Nothing

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

data Memo = Memo (IORef (Store (Int, Kept))) (IORef (Map.Map Firing Answer)) (IORef Int) (IORef Int)

type Answer = (Expression, Expression)

data Firing = Firing T.Text [Either Int Bytes] [Expression]
  deriving (Eq, Ord)

data Kept
  = Answered Answer
  | Looped Expression
  | Stalled T.Text Int

type Store answer = Map.Map Int [(Expression, answer)]

data ReduceContext = ReduceContext
  { _locator :: Expression
  , _site :: Expression
  , _universe :: Maybe Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _steps :: Steps
  , _tally :: Maybe Tally
  , _minted :: IORef Int
  , _deadline :: Maybe Deadline
  , _memo :: Maybe Memo
  , _nesting :: Int
  , _opened :: Maybe (Int, Judgment, Expression)
  , _depthSensitive :: Bool
  , _shuffle :: Bool
  , _partial :: Bool
  , _acyclic :: Maybe Acyclic
  , _judgment :: Judgment
  , _parked :: [T.Text]
  , _entered :: Seen
  , _symbolic :: Lambdas
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

data ReduceException
  = OutOfSteps Budget
  | OutOfTime Int
  | Stuck T.Text
  | StuckAt T.Text (NonEmpty Rewritten) State
  | OutOfStepsAt Budget (NonEmpty Rewritten) State
  | Looping Expression
  | LoopingAt Expression (NonEmpty Rewritten) State
  | Undataizable Expression State
  | Unmorphable Expression
  deriving anyclass (Exception)

instance Show ReduceException where
  show (OutOfSteps (Depth judgment limit)) =
    printf "%s did not finish before reaching the limit of steps: --max-steps=%d" (show judgment) limit
  show (OutOfSteps (Firings limit)) =
    printf "Evaluation did not finish before reaching the limit of firings: --max-firings=%d" limit
  show (OutOfSteps (Cycles limit)) =
    printf "Normalization did not finish before reaching the limit of cycles: --max-cycles=%d" limit
  show (OutOfStepsAt budget _ _) = show (OutOfSteps budget)
  show (OutOfTime limit) =
    printf "Evaluation did not finish before reaching the limit of seconds: --max-seconds=%d" limit
  show (Stuck func) = printf "No entry of --symbolic answers the λ function '%s'" (T.unpack func)
  show (StuckAt func _ _) = show (Stuck func)
  show (Looping term) = printf "Reduction entered a formation it is already inside: %s" (printExpression term)
  show (LoopingAt term _ _) = show (Looping term)
  show (Undataizable ExTermination _) = "dataization reached the terminator ⊥, which signals an error and cannot be dataized"
  show (Undataizable term _) = printf "no dataization rule matched, while dataization expects a normal form: %s" (printExpression term)
  show (Unmorphable term) = printf "Morphing expects a normal form, but no morphing rule matches: %s" (printExpression term)

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
    starve (Just (Memo _ _ _ exhausted)) = modifyIORef' exhausted (+ 1)

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
  throwIO (OutOfTime cap)

memoized :: Maybe Acyclic -> IO (Maybe Memo)
memoized (Just Plausible) = Just <$> (Memo <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef 0 <*> newIORef 0)
memoized _ = pure Nothing

recalled :: Maybe Memo -> Expression -> Int -> IO (Maybe Kept)
recalled Nothing _ _ = pure Nothing
recalled (Just (Memo store _ answers _)) form spent = do
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
counted (Just (Memo _ _ answers _)) = readIORef answers

starved :: Maybe Memo -> IO Int
starved Nothing = pure 0
starved (Just (Memo _ _ _ exhausted)) = readIORef exhausted

retained :: Maybe Memo -> Expression -> Int -> Kept -> IO ()
retained Nothing _ _ _ = pure ()
retained (Just (Memo store _ _ _)) form stamp kept =
  modifyIORef' store (Map.insertWith (++) (hashExpression form) [(form, (stamp, kept))])

remembered :: Maybe Memo -> Firing -> IO (Maybe Answer)
remembered Nothing _ = pure Nothing
remembered (Just (Memo _ firings _ _)) firing = Map.lookup firing <$> readIORef firings

remember :: Maybe Memo -> Firing -> Answer -> IO ()
remember Nothing _ _ = pure ()
remember (Just (Memo _ firings answers _)) firing answer = do
  modifyIORef' firings (Map.insert firing answer)
  modifyIORef' answers (+ 1)

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

entering :: Expression -> ReduceContext -> IO ReduceContext
entering term ctx = maybe (pure ctx) (`enter` ctx) (entrance ctx._judgment term)

enter :: Expression -> ReduceContext -> IO ReduceContext
enter form ctx = maybe (pure ctx) remembered ctx._acyclic
  where
    remembered :: Acyclic -> IO ReduceContext
    remembered mode =
      awaited (find (repeated mode form) (Map.findWithDefault [] (digest mode form) ctx._entered)) >>= \case
        Just before -> do
          ctx._saveEval (EvLooped ctx._nesting ctx._judgment mode before ctx._site)
          throwIO (Looping form)
        Nothing -> pure ctx{_entered = seenInsert (digest mode form) form ctx._entered}
    awaited :: Maybe Expression -> IO (Maybe Expression)
    awaited found = case ctx._deadline of
      Nothing -> pure found
      Just (Deadline cap due) -> do
        now <- getMonotonicTime
        maybe (expired ctx cap) pure =<< timeout (ceiling (max 0 (due - now) * 1000000)) (evaluate found)
    digest :: Acyclic -> Expression -> Int
    digest Proven = hashShape
    digest Plausible = hashSkeleton
    repeated :: Acyclic -> Expression -> Expression -> Bool
    repeated Proven form before = alike form before
    repeated Plausible form before = within before form

entrance :: Judgment -> Expression -> Maybe Expression
entrance Dataization term@(ExFormation bds)
  | boxed bds || isJust (lambda bds) = Just term
entrance Morphing (ExDispatch form@(ExFormation bds) _)
  | isJust (lambda bds) = Just form
entrance Evaluation term@(ExFormation bds)
  | isJust (lambda bds) = Just term
entrance _ _ = Nothing

boxed :: [Binding] -> Bool
boxed bds = any phi bds && not (any isLambda bds) && not (any delta bds)
  where
    phi :: Binding -> Bool
    phi (BiTau AtPhi _) = True
    phi _ = False
    delta :: Binding -> Bool
    delta (BiDelta _) = True
    delta _ = False

lambda :: [Binding] -> Maybe (T.Text, Expression)
lambda bds = case partition isLambda bds of
  ([BiLambda (Function func)], rest) -> Just (func, ExFormation rest)
  _ -> Nothing

isLambda :: Binding -> Bool
isLambda (BiLambda _) = True
isLambda _ = False

morph' :: Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
morph' start univ state entry = go start univ state entry
  where
    go :: Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
    go (expr, seq) univ state caller = do
      ctx <- opening Morphing =<< deeper =<< entering expr =<< universed univ caller{_judgment = Morphing}
      parking seq state $ do
        reached <- inferred expr univ state ctx ctx._engine._morphing
        case reached of
          Just (In.Answered step built, state') -> do
            seq' <- leadsTo seq step built ctx
            pure ((built, seq'), state')
          Just (In.Onward way built world, state') -> do
            (morphed, state'') <- onward seq state' way built ctx
            go morphed world state'' ctx
          Nothing -> throwIO (Unmorphable expr)

morph :: Expression -> State -> ReduceContext -> IO (Expression, [Rewritten], State)
morph universe state caller@ReduceContext{..} = do
  ctx <- opening Morphing =<< universed universe caller
  expr <- locatedExpression _locator universe
  result <- try (morph' (expr, (universe, Nothing) :| []) universe state ctx)
  case result of
    Right ((morphed, seq), state') -> pure (morphed, reverse (NE.toList seq), state')
    Left (StuckAt func seq parked) | _partial -> residue seq parked{_stuck = Just func}
    Left (OutOfStepsAt _ seq parked) | _partial -> residue seq parked
    Left (LoopingAt _ seq parked) -> residue seq parked
    Left failure -> throwIO (failure :: ReduceException)
  where
    residue :: NonEmpty Rewritten -> State -> IO (Expression, [Rewritten], State)
    residue seq parked = do
      expr <- locatedExpression _locator (fst (NE.head seq))
      pure (expr, reverse (NE.toList seq), parked)

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

onward :: NonEmpty Rewritten -> State -> In.Way -> Expression -> ReduceContext -> IO (Morphed, State)
onward seq state (In.Taken step) expr ctx = do
  seq' <- leadsTo seq step expr ctx
  pure ((expr, seq'), state)
onward seq state (In.Normalized step) expr ctx = do
  labelled <- leadsTo seq step expr ctx
  normal <- normalized expr labelled ctx
  pure (normal, state)
onward seq state (In.Named step) expr ctx = case ctx._universe of
  Just world -> onward seq state (In.Taken step) world ctx
  Nothing -> onward seq state (In.Normalized step) expr ctx
onward seq state (In.Staged stage) expr ctx = morph' (expr, seq) stage state ctx

leadsTo :: NonEmpty Rewritten -> (Judgment, String) -> Expression -> ReduceContext -> IO (NonEmpty Rewritten)
leadsTo ((current, _) :| rest) rule expr ReduceContext{..} = do
  updated <- withLocatedExpression _locator expr current
  pure ((updated, Nothing) :| (current, Just rule) : rest)

normalized :: Expression -> NonEmpty Rewritten -> ReduceContext -> IO (Expression, NonEmpty Rewritten)
normalized expr seq ctx@ReduceContext{..} = do
  whole <- withLocatedExpression _locator expr (fst (NE.head seq))
  (rewrittens, exceeded) <- rewrite whole _engine._normalization (rewriteContext ctx)
  when exceeded (throwIO (OutOfSteps (Cycles _maxCycles)))
  let (rw :| rws) = NE.reverse rewrittens
      seq' = rw :| rws <> NE.tail seq
  expr' <- locatedExpression _locator (fst rw)
  pure (expr', seq')
  where
    rewriteContext :: ReduceContext -> RewriteContext
    rewriteContext ReduceContext{..} =
      RewriteContext _locator _maxDepth _maxCycles _depthSensitive _universe _buildTerm _engine._normal _engine._matching MtDisabled Nothing _saveStep (\redex object -> _saveEval (EvApplied _nesting _judgment (called _universe redex) object _site))
    called :: Maybe Expression -> Expression -> Expression
    called universe (ExApplication head' arg) = ExApplication (nameIn universe head') arg
    called _ redex = redex

universed :: Expression -> ReduceContext -> IO ReduceContext
universed _ ctx@ReduceContext{_universe = Just _} = pure ctx
universed univ ctx = do
  (normal, _) <- normalized univ ((univ, Nothing) :| []) ctx{_locator = ExRoot, _saveStep = dontSaveStep}
  pure ctx{_universe = Just normal}

insideUniverse :: Expression -> Expression -> ReduceContext -> IO (Expression, ReduceContext)
insideUniverse expr univ ctx@ReduceContext{_buildTerm = buildTerm} = case univ of
  ExFormation bds -> do
    (TeAttribute attr) <- buildTerm "random-tau" [] substEmpty
    let aiming = ctx{_locator = ExDispatch ExRoot attr, _site = ExDispatch ExRoot attr}
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
  (morphed, _, state') <- morph universe state aiming
  pure (morphed, state')

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
  ((morphed, _), state') <- morph' (expr, (univ, Nothing) :| []) univ state ctx
  pure (morphed, state')
