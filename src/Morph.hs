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

module Morph (Answer (..), Deadline (..), Firing (..), Kept (..), ReduceContext (..), ReduceException (..), EvaluationFunc, Memo (..), ReductionFunc, Morphed, Steps (..), Tally (..), admitting, argued, awaited, boxed, called, carried, charged, coined, counted, deeper, emptyState, ended, enter, entering, execBuildTerm, expired, fired, inferred, insideUniverse, isLambda, lambda, leadsTo, memoized, morph, morph', morphing, normalized, onward, opening, parking, recalled, refused, remember, remembered, retained, settled, severing, starved, tallied, timed, universed, unparked) where

import AST
import Builder (buildExpressionThrows, nameIn)
import Control.Applicative ((<|>))
import Control.Exception (Exception, catch, evaluate, throwIO, try)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
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

data Memo = Memo (IORef (Store (Int, Kept))) (IORef (Map.Map Firing Answer)) (IORef Int) (IORef Int)

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
  Just form -> either (,ctx) (term,) <$> sever form univ state ctx
  Nothing -> pure (term, ctx)

sever :: Expression -> Expression -> State -> ReduceContext -> IO (Either Expression ReduceContext)
sever form univ state ctx = admitted form ctx >>= either cut (pure . Right)
  where
    cut :: (Acyclic, Expression) -> IO (Either Expression ReduceContext)
    cut loop = case form of
      ExFormation bds
        | boxed bds
        , Just call <- called univ ctx bds ->
            Left <$> severed univ ctx state form bds call loop
      _ -> refused form ctx loop

severed :: Expression -> ReduceContext -> State -> Expression -> [Binding] -> Expression -> (Acyclic, Expression) -> IO Expression
severed univ ctx state form bds call (mode, before) = do
  reckoned <- argued univ ctx state form bds call
  fresh <- coined ctx
  ctx._saveEval (EvLooped ctx._nesting ctx._judgment mode before ctx._site (Just (fresh, reckoned)))
  pure (ExFormation [BiLambda (FnSymbol fresh)])

coined :: ReduceContext -> IO Int
coined ctx = atomicModifyIORef' ctx._minted (\count -> (succ count, succ count))

fired :: Expression -> ReduceContext -> Expression -> State -> Expression -> IO (Expression, State)
fired univ ctx site state form = sever form univ state ctx{_judgment = Evaluation, _site = site} >>= either (pure . (,state)) kindled
  where
    kindled :: ReduceContext -> IO (Expression, State)
    kindled entered = do
      deep <- deeper entered
      deep._evaluate deep state form univ

called :: Expression -> ReduceContext -> [Binding] -> Maybe Expression
called univ ctx known = uncurry applied <$> origin known
  where
    applied :: Expression -> [Binding] -> Expression
    applied path declared = foldl ExApplication path [ArTau attr value | BiTau attr value <- known, attr /= AtRho, BiVoid attr `elem` declared]
    origin :: [Binding] -> Maybe (Expression, [Binding])
    origin bds = do
      parent <- case filter ((== Just AtRho) . attributeFromBinding) bds of
        [] -> Just ExRoot
        [BiTau AtRho (ExFormation outer)] -> fst <$> origin outer
        [BiTau AtRho path] -> Just (erased path)
        _ -> Nothing
      ExFormation siblings <- located parent (fromMaybe univ ctx._universe)
      chosen [(ExDispatch parent attr, declared) | BiTau attr (ExFormation declared) <- siblings, attr /= AtRho, fits declared]
      where
        fits :: [Binding] -> Bool
        fits declared = all (`elem` map attributeFromBinding declared) [attributeFromBinding bd | bd <- bds, attributeFromBinding bd /= Just AtRho]
        chosen :: [(Expression, [Binding])] -> Maybe (Expression, [Binding])
        chosen [] = Nothing
        chosen candidates = case [candidate | candidate <- candidates, agreed candidate == maximum (map agreed candidates)] of
          [one] -> Just one
          _ -> Nothing
        agreed :: (Expression, [Binding]) -> (Int, Int)
        agreed (_, declared) = (length [attr | BiTau attr _ <- bds, BiVoid attr `elem` declared], length (filter (`elem` declared) bds))
    located :: Expression -> Expression -> Maybe Expression
    located ExRoot whole = Just whole
    located (ExDispatch path attr) whole = case located path whole of
      Just (ExFormation siblings) -> listToMaybe [body | BiTau attr' body <- siblings, attr' == attr]
      _ -> Nothing
    located _ _ = Nothing
    erased :: Expression -> Expression
    erased (ExApplication target _) = erased target
    erased (ExDispatch target attr) = ExDispatch (erased target) attr
    erased other = other

argued :: Expression -> ReduceContext -> State -> Expression -> [Binding] -> Expression -> IO Expression
argued univ ctx state site bds = go
  where
    go :: Expression -> IO Expression
    go (ExApplication target (ArTau attr body)) = do
      target' <- go target
      ExApplication target' . ArTau attr <$> reckoned attr body
    go call = pure call
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
      ctx <- opening Morphing =<< deeper =<< entering expr =<< parking seq state (universed univ caller{_judgment = Morphing})
      parking seq state $ do
        reached <- inferred expr univ state ctx ctx._engine._morphing
        case reached of
          Just (In.Answered step built, state') -> do
            when owner (ctx._saveEval (EvMorphed ctx._nesting built))
            seq' <- leadsTo seq step built ctx
            pure ((built, seq'), state')
          Just (In.Onward way built world, state') -> do
            (morphed, state'') <- onward seq state' way built ctx
            go morphed world state'' ctx
          Nothing -> throwIO (Unmorphable expr)

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
      , boxed bds
      , Just call <- called world opened bds = do
          hits <- repeats mode term opened
          case drop opened._maxLoops hits of
            before : _ -> (,state') <$> severed world opened state' term bds call (mode, before)
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
