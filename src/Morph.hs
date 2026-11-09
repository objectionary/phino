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

module Morph (Answer (..), Deadline (..), Firing (..), Facts, Kept (..), ReduceContext (..), ReduceException (..), EvaluationFunc, Memo (..), ReductionFunc, Morphed, Steps (..), Tally (..), admitting, argued, awaited, boxed, called, carried, charged, coined, counted, declared, deeper, emptyState, ended, enter, execBuildTerm, expired, fired, inferred, insideUniverse, isLambda, lambda, leadsTo, factual, memoized, morph, noted, remembering, morph', morphing, normalized, onward, opening, parking, recalled, refused, remember, remembered, retained, settled, severing, starved, tallied, timed, universed, unparked, voided) where

import AST
import Builder (buildExpressionThrows, nameIn)
import Control.Applicative ((<|>))
import Control.Exception (Exception, catch, evaluate, throwIO, try)
import Control.Monad (forM_, guard, unless, when)
import Data.Bifunctor (first, second)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Text as T
import Deps (Acyclic (..), BuildTermFunc, BuildTermMethod, Evaluation (..), Judgment (..), SaveEvalFunc, SaveStepFunc, State (..), Term (..), dispatched, dontSaveStep)
import Engine (Engine (..))
import GHC.Clock (getMonotonicTime)
import qualified Inference as In
import Lambdas (Lambdas, thrown)
import Locator (locatedExpression, withLocatedExpression)
import Matcher (substEmpty)
import Must (Must (..))
import Printer (printExpression)
import Random (shuffle)
import Rewriter (RewriteContext (RewriteContext), Rewritten, Seen, rewrite, seenInsert)
import Rule (RuleContext (RuleContext), xiFree)
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

data Memo = Memo (IORef (Store (Int, Kept))) (IORef (Map.Map Firing (Expression, Answer))) (IORef Int) (IORef Int)

data Answer
  = Answer Expression Expression
  | Joined Expression

ended :: Answer -> Expression
ended (Answer _ normal) = normal
ended (Joined normal) = normal

data Firing = Firing T.Text [Either Int Bytes] [Expression]
  deriving (Eq, Ord)

data Kept
  = Answered Answer
  | Looped Expression
  | Stalled T.Text Int

type Store answer = Map.Map Int [(Expression, answer)]

type Facts = IORef (Store Expression)

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
  , _maxLoops :: Int
  , _judgment :: Judgment
  , _parked :: [T.Text]
  , _entered :: Seen
  , _facts :: Maybe Facts
  , _symbolic :: Lambdas
  , _raising :: [T.Text]
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
  | Diverged T.Text
  | Looping Expression
  | LoopingAt Expression (NonEmpty Rewritten) State
  | Undataizable Expression (NonEmpty Rewritten) State
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
  show (Diverged func) = printf "The λ function '%s' could not join its two sides" (T.unpack func)
  show (Looping term) = printf "Reduction entered a formation it is already inside: %s" (printExpression term)
  show (LoopingAt term _ _) = show (Looping term)
  show (Undataizable ExTermination _ _) = "dataization reached the terminator ⊥, which signals an error and cannot be dataized"
  show (Undataizable term _ _) = printf "no dataization rule matched, while dataization expects a normal form: %s" (printExpression term)
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

awaited :: ReduceContext -> IO a -> IO a
awaited ctx action = case ctx._deadline of
  Nothing -> action
  Just (Deadline cap due) -> do
    now <- getMonotonicTime
    maybe (expired ctx cap) pure =<< timeout (ceiling (max 0 (due - now) * 1000000)) action

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

remembered :: Maybe Memo -> Firing -> IO (Maybe (Expression, Answer))
remembered Nothing _ = pure Nothing
remembered (Just (Memo _ firings _ _)) firing = Map.lookup firing <$> readIORef firings

remember :: Maybe Memo -> Firing -> Expression -> Answer -> IO ()
remember Nothing _ _ _ = pure ()
remember (Just (Memo _ firings answers _)) firing form answer = do
  modifyIORef' firings (Map.insert firing (form, answer))
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
  call <- (<|>) <$> traced ctx._facts form <*> traced ctx._facts before
  forM_ call (recited ctx . snd)
  reckoned <- traverse (spelled taken . fst) call
  fresh <- coined ctx
  ctx._saveEval (EvLooped ctx._nesting ctx._judgment mode before ctx._site (Just (fresh, reckoned)))
  pure (ExFormation [BiLambda (FnSymbol fresh)])
  where
    spelled :: Maybe Attribute -> Expression -> IO Expression
    spelled (Just attr) found = pure (ExDispatch found attr)
    spelled Nothing found = argued univ ctx state form bds found

recited :: ReduceContext -> [(Expression, Expression)] -> IO ()
recited ctx = mapM_ (\(term, source) -> ctx._saveEval (EvRecalled ctx._nesting term source))

factual :: IO Facts
factual = newIORef Map.empty

noted :: ReduceContext -> Expression -> Expression -> IO ()
noted ctx object source = forM_ ctx._facts $ \facts -> do
  known <- (Just source ==) . lookup object . Map.findWithDefault [] (hashExpression object) <$> readIORef facts
  unless known $ do
    modifyIORef' facts (Map.insertWith (++) (hashExpression object) [(object, source)])
    ctx._saveEval (EvRemembered ctx._nesting object source)

remembering :: Maybe Facts -> Expression -> IO Bool
remembering Nothing _ = pure False
remembering (Just facts) term = isJust . lookup term . Map.findWithDefault [] (hashExpression term) <$> readIORef facts

traced :: Maybe Facts -> Expression -> IO (Maybe (Expression, [(Expression, Expression)]))
traced Nothing _ = pure Nothing
traced (Just facts) form = do
  known <- readIORef facts
  pure (go known (Map.size known) form)
  where
    go :: Store Expression -> Int -> Expression -> Maybe (Expression, [(Expression, Expression)])
    go _ 0 _ = Nothing
    go known budget term = listToMaybe [(named, (object, source) : hops) | (path, object, source) <- sortOn (\(path, _, _) -> length path) [(path, object, source) | (object, source) <- concat (Map.elems known), path <- take 1 (lodged term object)], let (base, hops) = resolved known budget source, Just named <- [rooted (foldl ExDispatch base path)]]
    resolved :: Store Expression -> Int -> Expression -> (Expression, [(Expression, Expression)])
    resolved known budget source = case lookup source (Map.findWithDefault [] (hashExpression source) known) of
      Just _ -> fromMaybe (source, []) (go known (budget - 1) source)
      Nothing -> case source of
        ExApplication target arg -> first (`ExApplication` arg) (resolved known budget target)
        ExDispatch target attr -> first (`ExDispatch` attr) (resolved known budget target)
        _ -> (source, [])
    rooted :: Expression -> Maybe Expression
    rooted term = term <$ guard (grounded term)
    grounded :: Expression -> Bool
    grounded ExRoot = True
    grounded (ExDispatch target _) = grounded target
    grounded (ExApplication target _) = grounded target
    grounded _ = False
    lodged :: Expression -> Expression -> [[Attribute]]
    lodged (ExFormation bds) whole = searched (filter (not . rho) bds) [([], whole)] Map.empty
    lodged _ _ = []
    searched :: [Binding] -> [([Attribute], Expression)] -> Store () -> [[Attribute]]
    searched _ [] _ = []
    searched own ((path, term@(ExFormation inner)) : rest) seen
      | isJust (lookup term (Map.findWithDefault [] (hashExpression term) seen)) = searched own rest seen
      | filter (not . rho) inner == own = reverse path : searched own rest seen
      | otherwise = searched own (rest ++ [(attr : path, body) | BiTau attr body <- inner, attr /= AtRho]) (Map.insertWith (++) (hashExpression term) [(term, ())] seen)
    searched own (_ : rest) seen = searched own rest seen
    rho :: Binding -> Bool
    rho bd = attributeFromBinding bd == Just AtRho

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
voided ctx@ReduceContext{_facts = Just facts} form@(ExFormation bds) = do
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
            ExFormation _ -> fired univ ctx place' morphed normal
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
repeats mode form ctx = awaited ctx (evaluate (length hits `seq` hits))
  where
    hits :: [Expression]
    hits = take (succ ctx._maxLoops) (filter (repeated mode) (Map.findWithDefault [] (digest mode form) ctx._entered))
    repeated :: Acyclic -> Expression -> Bool
    repeated Proven before = alike form before
    repeated Plausible before = within before form

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
morph' = morph'' True

morph'' :: Bool -> Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
morph'' owner start univ state entry = go start univ state entry
  where
    go :: Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
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
            (morphed, state'') <- onward seq state' way built ctx
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
        Left (LoopingAt _ seq parked) -> residue seq parked
        Left failure -> throwIO (failure :: ReduceException)
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
  taken <- newIORef Map.empty
  peeled <- newIORef Map.empty
  (rewrittens, exceeded) <- rewrite whole _engine._normalization (rewriteContext taken peeled ctx)
  when exceeded $ do
    _saveEval (EvExceeded _nesting _maxCycles _judgment _site)
    throwIO (OutOfSteps (Cycles _maxCycles))
  let (rw :| rws) = NE.reverse rewrittens
      seq' = rw :| rws <> NE.tail seq
  expr' <- locatedExpression _locator (fst rw)
  pure (expr', seq')
  where
    rewriteContext :: IORef (Store Expression) -> IORef (Store Expression) -> ReduceContext -> RewriteContext
    rewriteContext taken peeled whole@ReduceContext{..} =
      RewriteContext _locator _maxDepth _maxCycles _depthSensitive _universe _buildTerm _engine._normal _engine._matching MtDisabled Nothing _saveStep (made taken peeled whole)
    made :: IORef (Store Expression) -> IORef (Store Expression) -> ReduceContext -> Expression -> Expression -> IO ()
    made _ _ whole redex@(ExApplication head' (ArTau attr _)) object@(ExFormation bds)
      | attr /= AtRho && object /= head' = do
          whole._saveEval (EvApplied whole._nesting whole._judgment (called whole._universe redex) object whole._site)
          known <- remembering whole._facts head'
          when (known && copied bds) (noted whole object redex)
    made taken _ whole redex@(ExApplication _ (ArTau AtRho _)) object@(ExFormation bds)
      | copied bds = do
          pending <- readIORef taken
          forM_ (lookup redex (Map.findWithDefault [] (hashExpression redex) pending)) (noted whole object)
    made taken peeled whole redex@(ExDispatch target attr) object
      | attr /= AtRho = do
          stripped peeled whole target attr
          dispatched taken peeled whole redex object
    made _ _ _ _ _ = pure ()
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
  (normal, _) <- normalized univ ((univ, Nothing) :| []) ctx{_locator = ExRoot, _saveStep = dontSaveStep}
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
