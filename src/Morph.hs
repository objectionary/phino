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

module Morph (Answer, Deadline (..), Firing (..), Kept (..), ReduceContext (..), ReduceException (..), EvaluationFunc, FiringFunc, Memo (..), ReductionFunc, Morphed, Refused (..), Steps (..), Tally (..), admitted, boxed, charged, counted, deeper, emptyState, enter, entering, execBuildTerm, inferred, insideUniverse, isLambda, lambda, leadsTo, memoized, morph, morph', morphing, normalized, onward, parking, recalled, refused, remember, remembered, retained, settled, starved, tallied, timed, universed, unparked) where

import AST
import Builder (buildExpressionThrows, nameIn, pathOf)
import Control.Applicative ((<|>))
import Control.Exception (Exception, SomeException, catch, evaluate, throwIO, try)
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Deps (Acyclic (..), BuildTermFunc, BuildTermMethod, Evaluation (..), Judgment (..), SaveEvalFunc, SaveStepFunc, State (..), Term (..), dontSaveEval, dontSaveStep, renumbered)
import Engine (Engine (..))
import GHC.Clock (getMonotonicTime)
import qualified Inference as In
import Lambdas (Lambdas, emptyLambdas)
import Locator (locatedExpression, withLocatedExpression)
import Matcher (substEmpty)
import Must (Must (..))
import Pool (pooled)
import Printer (printExpression)
import Random (shuffle)
import Rewriter (RewriteContext (RewriteContext), Rewritten, Seen, rewrite, seenInsert)
import Rule (RuleContext (RuleContext))
import System.Timeout (timeout)
import Tau (tausOf)
import Text.Printf (printf)
import Yaml (ExtraArgument (..))

type Morphed = (Expression, NonEmpty Rewritten)

type ReductionFunc = Expression -> ReduceContext -> Expression -> State -> IO (Maybe Bytes, State)

type EvaluationFunc = ReduceContext -> State -> Expression -> Expression -> IO (Expression, State)

type FiringFunc = Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Maybe Expression, State)

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

data Memo = Memo (IORef (Store (Int, Kept))) (IORef (Map.Map Firing Answer)) (IORef Int) (IORef Int) (IORef (Set.Set (Expression, Attribute)))

type Answer = (Expression, Expression)

data Firing = Firing T.Text [Either Int Bytes] [Expression]
  deriving (Eq, Ord)

data Kept
  = Answered Answer
  | Looped Expression
  | Stalled T.Text Int

type Store answer = Map.Map Int [(Expression, answer)]

data Frame = Frame (IORef Expression) (IORef Expression) Expression (Maybe Attribute)

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
  , _opened :: Maybe (Int, Expression)
  , _depthSensitive :: Bool
  , _shuffle :: Bool
  , _partial :: Bool
  , _deep :: Bool
  , _jobs :: Int
  , _acyclic :: Maybe Acyclic
  , _judgment :: Judgment
  , _parked :: [T.Text]
  , _entered :: Seen
  , _symbolic :: Lambdas
  , _buildTerm :: BuildTermFunc
  , _reduce :: ReductionFunc
  , _evaluate :: EvaluationFunc
  , _fire :: FiringFunc
  , _saveStep :: SaveStepFunc
  , _saveEval :: SaveEvalFunc
  , _engine :: Engine
  }

data Budget
  = Depth Int
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
  show (OutOfSteps (Depth limit)) =
    printf "Dataization did not finish before reaching the limit of steps: --max-steps=%d" limit
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
  show (Undataizable _ _) = "no dataization rule matched"
  show (Unmorphable term) = printf "Morphing expects a normal form, but no morphing rule matches: %s" (printExpression term)

data Refused = Refused Acyclic Expression State
  deriving anyclass (Exception)

instance Show Refused where
  show (Refused _ before _) = show (Looping before)

data Severed = Severed Expression State
  deriving anyclass (Exception)

instance Show Severed where
  show (Severed answer _) = printf "The deep walk answered a copy it cut with %s" (printExpression answer)

deeper :: ReduceContext -> IO ReduceContext
deeper ctx@ReduceContext{_steps = Steps limit spent} = do
  clocked ctx
  when (spent >= limit) $ do
    starve ctx._memo
    ctx._saveEval (EvStarved ctx._nesting limit ctx._judgment ctx._site)
    throwIO (OutOfSteps (Depth limit))
  pure ctx{_steps = Steps limit (spent + 1)}
  where
    starve :: Maybe Memo -> IO ()
    starve Nothing = pure ()
    starve (Just (Memo _ _ _ exhausted _)) = modifyIORef' exhausted (+ 1)

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
memoized (Just Plausible) = Just <$> (Memo <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef 0 <*> newIORef 0 <*> newIORef Set.empty)
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

remembered :: Maybe Memo -> Firing -> IO (Maybe Answer)
remembered Nothing _ = pure Nothing
remembered (Just (Memo _ firings _ _ _)) firing = Map.lookup firing <$> readIORef firings

remember :: Maybe Memo -> Firing -> Answer -> IO ()
remember Nothing _ _ = pure ()
remember (Just (Memo _ firings answers _ _)) firing answer = do
  modifyIORef' firings (Map.insert firing answer)
  modifyIORef' answers (+ 1)

visited :: Maybe Memo -> Expression -> Attribute -> IO Bool
visited Nothing _ _ = pure False
visited (Just (Memo _ _ _ _ walked)) object attr = Set.member (object, attr) <$> readIORef walked

visit :: Maybe Memo -> Expression -> Attribute -> IO ()
visit Nothing _ _ = pure ()
visit (Just (Memo _ _ _ _ walked)) object attr = modifyIORef' walked (Set.insert (object, attr))

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
enter form ctx = admitted form ctx >>= either refuse pure
  where
    refuse :: (Acyclic, Expression) -> IO ReduceContext
    refuse (mode, before) = do
      looped ctx mode before Nothing
      throwIO (Looping form)

admitted :: Expression -> ReduceContext -> IO (Either (Acyclic, Expression) ReduceContext)
admitted form ctx = maybe (pure (Right ctx)) remembered ctx._acyclic
  where
    remembered :: Acyclic -> IO (Either (Acyclic, Expression) ReduceContext)
    remembered mode =
      maybe (Right ctx{_entered = seenInsert (digest mode form) form ctx._entered}) (Left . (mode,))
        <$> awaited (find (repeated mode form) (Map.findWithDefault [] (digest mode form) ctx._entered))
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

refused :: ReduceContext -> Refused -> IO (Maybe Expression, State)
refused ctx (Refused mode before reached) = (Nothing, reached) <$ looped ctx mode before Nothing

looped :: ReduceContext -> Acyclic -> Expression -> Maybe (Int, Maybe Expression) -> IO ()
looped ctx mode before answer = ctx._saveEval (EvLooped ctx._nesting ctx._judgment mode before ctx._site answer)

entrance :: Judgment -> Expression -> Maybe Expression
entrance Dataization term@(ExFormation bds)
  | boxed bds || isJust (lambda bds) = Just term
entrance Morphing (ExDispatch form@(ExFormation bds) _)
  | isJust (lambda bds) = Just form
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
      ctx <- deeper =<< entering expr =<< universed univ caller{_judgment = Morphing}
      parking seq state $ do
        reached <- inferred expr univ state ctx ctx._engine._morphing
        case reached of
          Just (In.Answered step built, state') -> do
            seq' <- leadsTo seq step built ctx
            pure ((built, seq'), state')
          Just (In.Onward way built world, state') -> do
            (walked, state'') <- prewalked way built univ state' ctx{_steps = entry._steps}
            (morphed, state''') <- onward seq state'' way walked ctx
            go morphed world state''' ctx
          Nothing -> throwIO (Unmorphable expr)

prewalked :: In.Way -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
prewalked (In.Normalized _) expr univ state ctx
  | ctx._deep = go expr
  where
    go :: Expression -> IO (Expression, State)
    go (ExDispatch target@(ExDispatch _ _) attr) = first (`ExDispatch` attr) <$> go target
    go (ExDispatch form@(ExFormation bds) attr)
      | attr /= AtRho && reading attr bds && all tau bds = first (`ExDispatch` attr) <$> deepened (Just attr) form univ state ctx
    go term = pure (term, state)
    tau :: Binding -> Bool
    tau (BiTau _ _) = True
    tau _ = False
    reading :: Attribute -> [Binding] -> Bool
    reading attr bds = maybe False (not . closed) (listToMaybe [body | BiTau attr' body <- bds, attr' == attr])
prewalked _ expr _ state _ = pure (expr, state)

morph :: Expression -> State -> ReduceContext -> IO (Expression, [Rewritten], State)
morph universe state caller@ReduceContext{..} = do
  ctx <- universed universe caller
  expr <- locatedExpression _locator universe
  result <- try (morph' (expr, (universe, Nothing) :| []) universe state ctx)
  case result of
    Right ((morphed, seq), state') -> walked (walking ctx) morphed seq state'
    Left (StuckAt func seq parked) | _partial -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked (marked ctx func) residue seq parked{_stuck = Just func}
    Left (OutOfStepsAt _ seq parked) | _partial -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked (walking ctx) residue seq parked
    Left (LoopingAt _ seq parked) -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked (walking ctx) residue seq parked
    Left failure -> throwIO (failure :: ReduceException)
  where
    walking :: ReduceContext -> ReduceContext
    walking ctx = ctx{_judgment = Morphing}
    marked :: ReduceContext -> T.Text -> ReduceContext
    marked ctx func = (walking ctx){_parked = func : _parked}
    walked :: ReduceContext -> Expression -> NonEmpty Rewritten -> State -> IO (Expression, [Rewritten], State)
    walked walker morphed seq state'
      | not _deep = pure (morphed, reverse (NE.toList seq), state')
      | otherwise = do
          (deep, state'') <- deepened Nothing morphed universe state' walker
          seq' <- leadsTo seq (Morphing, "deep") deep walker
          pure (deep, reverse (NE.toList seq'), state'')

deepened :: Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
deepened focus expr univ state ctx = do
  world <- newIORef (fromMaybe univ ctx._universe)
  case focus of
    Just attr -> do
      (store, path) <- home Nothing world expr
      body <- held (ExDispatch path attr) store
      (entered, state') <- sibling Nothing (Frame world store path (Just attr)) body state ctx
      when (entered /= body) (stored store (ExDispatch path attr) entered)
      (,state') <$> held path store
    Nothing -> step (if ctx._jobs > 1 then spread else parts) (Just ctx._site) Nothing (Frame world world ctx._site Nothing) expr state ctx
  where
    go :: Maybe Expression -> Maybe Attribute -> Frame -> Expression -> State -> ReduceContext -> IO (Expression, State)
    go = step parts
    step :: (Maybe Expression -> Frame -> Expression -> State -> ReduceContext -> IO (Expression, State)) -> Maybe Expression -> Maybe Attribute -> Frame -> Expression -> State -> ReduceContext -> IO (Expression, State)
    step walk standing dispatched frame@(Frame world _ _ _) term state' caller = do
      let here = sited standing caller
      ctx' <- deeper here
      copy <- deferrable dispatched world term state' here
      case copy of
        Just form -> deferred form state' here
        Nothing -> do
          outcome <- try (walk standing frame term state' here)
          case outcome of
            Left (Severed answer reached) -> pure (answer, reached)
            Right (walked, walkedState) -> do
              when (walked /= term) (here._saveEval (EvComputed here._nesting term walked))
              placed <- ctx._engine._contextualize walked =<< context frame
              current <- readIORef world
              (answer, answered) <- ctx'._fire dispatched placed current walkedState ctx'{_universe = Just current} `catch` cut standing frame ctx'
              mapM_ (noted here._site frame walked) answer
              pure (fromMaybe walked answer, answered)
    cut :: Maybe Expression -> Frame -> ReduceContext -> Refused -> IO (Maybe Expression, State)
    cut (Just _) (Frame _ store path (Just AtPhi)) caller refusal@(Refused mode before reached) = do
      form <- held path store
      if copied form
        then do
          fresh <- coined caller
          looped caller mode before (Just (fresh, called form))
          throwIO (Severed (ExFormation [BiLambda (FnSymbol fresh)]) reached)
        else refused caller refusal
    cut _ _ caller refusal = refused caller refusal
    copied :: Expression -> Bool
    copied (ExFormation bds) = boxed bds && not (any abstract bds) && any code bds
    copied _ = False
    deferrable :: Maybe Attribute -> IORef Expression -> Expression -> State -> ReduceContext -> IO (Maybe Expression)
    deferrable dispatched world form@(ExFormation bds) state' caller
      | copied form && maybe True (\attr -> not (any (named attr) bds)) dispatched = do
          current <- readIORef world
          known <- mapM (resolved current form state' caller) bds
          pure (if any bare known then Just (ExFormation known) else Nothing)
    deferrable _ _ _ _ _ = pure Nothing
    code :: Binding -> Bool
    code (BiTau AtPhi (ExFormation _)) = False
    code (BiTau AtPhi _) = True
    code _ = False
    bare :: Binding -> Bool
    bare (BiTau attr (ExFormation [BiLambda (FnSymbol _)])) = attr /= AtPhi && attr /= AtRho
    bare _ = False
    resolved :: Expression -> Expression -> State -> ReduceContext -> Binding -> IO Binding
    resolved current form state' caller bd@(BiTau attr body@(ExDispatch _ _))
      | attr /= AtPhi && attr /= AtRho = do
          placed <- ctx._engine._contextualize body (scope attr form)
          outcome <- try (settled placed current state' (reading current caller))
          case outcome of
            Right (made@(ExFormation [BiLambda (FnSymbol _)]), _) -> pure (BiTau attr made)
            Left (OutOfTime cap) -> expired caller cap
            _ -> pure bd
    resolved _ _ _ _ bd = pure bd
    reading :: Expression -> ReduceContext -> ReduceContext
    reading current caller =
      caller
        { _universe = Just current
        , _symbolic = emptyLambdas
        , _memo = Nothing
        , _tally = Nothing
        , _acyclic = Nothing
        , _deep = False
        , _saveStep = dontSaveStep
        , _saveEval = dontSaveEval
        }
    deferred :: Expression -> State -> ReduceContext -> IO (Expression, State)
    deferred copy state' caller = do
      fresh <- coined caller
      caller._saveEval (EvDeferred caller._nesting fresh caller._judgment copy (called copy) caller._site)
      pure (ExFormation [BiLambda (FnSymbol fresh)], state')
    coined :: ReduceContext -> IO Int
    coined caller = atomicModifyIORef' caller._minted (\count -> (count + 1, count + 1))
    called :: Expression -> Maybe Expression
    called copy@(ExFormation bds) = do
      (path, declared) <- origin copy
      pure (foldl ExApplication path [ArTau attr value | BiTau attr value <- bds, attr /= AtRho, BiVoid attr `elem` declared])
    called _ = Nothing
    origin :: Expression -> Maybe (Expression, [Binding])
    origin (ExFormation bds) = do
      parent <- case filter ((== Just AtRho) . attributeFromBinding) bds of
        [] -> Just ExRoot
        [BiTau AtRho form@(ExFormation _)] -> fst <$> origin form
        [BiTau AtRho path] -> Just (erased path)
        _ -> Nothing
      ExFormation siblings <- located parent (fromMaybe univ ctx._universe)
      chosen bds [(ExDispatch parent attr, declared) | BiTau attr (ExFormation declared) <- siblings, attr /= AtRho, fits declared]
      where
        fits :: [Binding] -> Bool
        fits declared = all (`elem` map attributeFromBinding declared) [attributeFromBinding bd | bd <- bds, attributeFromBinding bd /= Just AtRho]
    origin _ = Nothing
    chosen :: [Binding] -> [(Expression, [Binding])] -> Maybe (Expression, [Binding])
    chosen _ [] = Nothing
    chosen bds candidates = case [candidate | candidate <- candidates, agreed candidate == maximum (map agreed candidates)] of
      [one] -> Just one
      _ -> Nothing
      where
        agreed :: (Expression, [Binding]) -> (Int, Int)
        agreed (_, declared) = (length [attr | BiTau attr _ <- bds, BiVoid attr `elem` declared], length (filter (`elem` declared) bds))
    sited :: Maybe Expression -> ReduceContext -> ReduceContext
    sited Nothing caller = caller
    sited (Just loc) caller = caller{_site = loc}
    parts :: Maybe Expression -> Frame -> Expression -> State -> ReduceContext -> IO (Expression, State)
    parts _ _ term@(ExFormation bds) state' _
      | any abstract bds = pure (term, state')
    parts standing (Frame world _ _ _) form@(ExFormation bds) state' caller = do
      (store, path) <- home standing world form
      state'' <- bindings standing (synonym caller._universe form) (Frame world store path Nothing) [attr | BiTau attr _ <- bds, attr /= AtRho] state' caller
      entered <- held path store
      pure (entered, state'')
    parts _ frame (ExDispatch target attr) state' caller = do
      (entered, state'') <- go Nothing (Just attr) frame target state' caller
      pure (ExDispatch entered attr, state'')
    parts _ frame (ExApplication target arg) state' caller = do
      (entered, state'') <- go Nothing Nothing frame target state' caller
      (applied, state''') <- argument (go Nothing Nothing) frame arg state'' caller
      pure (ExApplication entered applied, state''')
    parts _ _ term state' _ = pure (term, state')
    sibling :: Maybe Attribute -> Frame -> Expression -> State -> ReduceContext -> IO (Expression, State)
    sibling dispatched frame term@(ExDispatch ExXi attr) state' caller
      | attr /= AtRho = go Nothing dispatched frame term state' caller
    sibling _ frame (ExDispatch target attr) state' caller = first (`ExDispatch` attr) <$> sibling (Just attr) frame target state' caller
    sibling _ frame (ExApplication target arg) state' caller = do
      (entered, state'') <- sibling Nothing frame target state' caller
      first (ExApplication entered) <$> argument (sibling Nothing) frame arg state'' caller
    sibling _ _ term state' _ = pure (term, state')
    abstract :: Binding -> Bool
    abstract (BiVoid _) = True
    abstract _ = False
    spread :: Maybe Expression -> Frame -> Expression -> State -> ReduceContext -> IO (Expression, State)
    spread standing (Frame world _ _ _) form@(ExFormation bds) state' caller
      | not (any abstract bds) = do
          floor' <- readIORef caller._minted
          jobs <- mapM (planned floor' (synonym caller._universe form)) (zip [1 ..] bds)
          (entered, state'') <- pooled caller._jobs jobs (gathered floor') ([], state')
          pure (ExFormation (reverse entered), state'')
      where
        planned :: Int -> Maybe (Expression, [Attribute]) -> (Int, Binding) -> IO (IO ([Evaluation], Int, Either SomeException (Int -> IO (Binding, Maybe State))))
        planned floor' alias (idx, BiTau attr body)
          | attr /= AtRho = do
              new <- if closed body then fresh alias attr caller else pure True
              pure (if new then worker floor' idx attr body else kept (BiTau attr body))
        planned _ _ (_, bd) = pure (kept bd)
        kept :: Binding -> IO ([Evaluation], Int, Either SomeException (Int -> IO (Binding, Maybe State)))
        kept bd = pure ([], 0, Right (const (pure (bd, Nothing))))
        worker :: Int -> Int -> Attribute -> Expression -> IO ([Evaluation], Int, Either SomeException (Int -> IO (Binding, Maybe State)))
        worker floor' idx attr body = do
          buffer <- newIORef []
          tau <- tausOf idx
          tally <- tallied (fmap (\(Tally cap _) -> cap) caller._tally)
          minted <- newIORef floor'
          memo <- memoized caller._acyclic
          copy <- newIORef =<< readIORef world
          (store, path) <- home standing copy form
          let own = caller{_jobs = 1, _tally = tally, _minted = minted, _memo = memo, _saveEval = modifyIORef' buffer . (:), _buildTerm = minting tau caller._buildTerm}
          outcome <- try (try (go (fmap (`ExDispatch` attr) standing) Nothing (Frame copy store path (Just attr)) body state' own))
          records <- reverse <$> readIORef buffer
          spent <- subtract floor' <$> readIORef minted
          pure (records, spent, fmap (either (severed floor') (\(term, walked) offset -> pure (BiTau attr (lifted floor' offset term), Just (moved floor' offset walked)))) outcome)
        severed :: Int -> Severed -> Int -> IO (Binding, Maybe State)
        severed floor' (Severed answer reached) offset = throwIO (Severed (lifted floor' offset answer) (moved floor' offset reached))
        moved :: Int -> Int -> State -> State
        moved floor' offset walked = walked{_manufactured = fmap (\sym -> if sym > floor' then sym + offset else sym) walked._manufactured}
        gathered :: Int -> ([Binding], State) -> ([Evaluation], Int, Either SomeException (Int -> IO (Binding, Maybe State))) -> IO ([Binding], State)
        gathered floor' (done, current) (records, spent, outcome) = do
          offset <- subtract floor' <$> readIORef caller._minted
          mapM_ (caller._saveEval . renumbered floor' offset) records
          modifyIORef' caller._minted (+ spent)
          (bd, walked) <- either throwIO ($ offset) outcome
          pure (bd : done, fromMaybe current walked)
    spread standing frame term state' caller = parts standing frame term state' caller
    minting :: IO T.Text -> BuildTermFunc -> BuildTermFunc
    minting tau build func
      | func == "random-tau" = \args subst -> if null args then TeAttribute . AtLabel <$> tau else build func args subst
      | otherwise = build func
    bindings :: Maybe Expression -> Maybe (Expression, [Attribute]) -> Frame -> [Attribute] -> State -> ReduceContext -> IO State
    bindings _ _ _ [] state' _ = pure state'
    bindings standing alias frame@(Frame world store path _) (attr : rest) state' caller = do
      body <- held (ExDispatch path attr) store
      new <- if closed body then fresh alias attr caller else pure True
      state'' <-
        if new
          then do
            (entered, walked) <- go (fmap (`ExDispatch` attr) standing) Nothing (Frame world store path (Just attr)) body state' caller
            walked <$ when (entered /= body) (stored store (ExDispatch path attr) entered)
          else pure state'
      bindings standing alias frame rest state'' caller
    home :: Maybe Expression -> IORef Expression -> Expression -> IO (IORef Expression, Expression)
    home (Just path) world form = do
      placed <- put path form <$> readIORef world
      case placed of
        Just whole -> (world, path) <$ writeIORef world whole
        Nothing -> home Nothing world form
    home Nothing _ form = (,ExRoot) <$> newIORef form
    context :: Frame -> IO Expression
    context (Frame _ _ _ Nothing) = pure ExXi
    context (Frame _ store path (Just attr)) = scope attr <$> held path store
    noted :: Expression -> Frame -> Expression -> Expression -> IO ()
    noted site frame@(Frame world _ _ _) walked answer = case address frame walked of
      Just (store, path@(ExDispatch _ _))
        | store /= world || not (above path site) -> stored store path answer
      _ -> pure ()
    address :: Frame -> Expression -> Maybe (IORef Expression, Expression)
    address (Frame world _ _ _) ExRoot = Just (world, ExRoot)
    address (Frame _ store path (Just _)) ExXi = Just (store, path)
    address frame (ExDispatch target attr) = fmap (`ExDispatch` attr) <$> address frame target
    address _ _ = Nothing
    above :: Expression -> Expression -> Bool
    above path (ExDispatch target _) = path == target || above path target
    above _ _ = False
    held :: Expression -> IORef Expression -> IO Expression
    held path store = readIORef store >>= maybe (throwIO (userError (printf "The deep walk lost the object at %s" (printExpression path)))) pure . located path
    stored :: IORef Expression -> Expression -> Expression -> IO ()
    stored store path value = modifyIORef' store (\whole -> fromMaybe whole (put path value whole))
    put :: Expression -> Expression -> Expression -> Maybe Expression
    put ExRoot value _ = Just value
    put (ExDispatch path attr) value whole = case located path whole of
      Just (ExFormation bds)
        | attr /= AtRho && not (any abstract bds) && any (named attr) bds ->
            put path (ExFormation (map (\bd -> if named attr bd then BiTau attr value else bd) bds)) whole
      _ -> Nothing
    put _ _ _ = Nothing
    located :: Expression -> Expression -> Maybe Expression
    located ExRoot whole = Just whole
    located (ExDispatch path attr) whole = case located path whole of
      Just (ExFormation bds) -> listToMaybe [body | BiTau attr' body <- bds, attr' == attr]
      _ -> Nothing
    located _ _ = Nothing
    synonym :: Maybe Expression -> Expression -> Maybe (Expression, [Attribute])
    synonym Nothing _ = Nothing
    synonym (Just world) form = case pathOf world form of
      ExRoot -> Nothing
      ExFormation _ -> Nothing
      name -> Just (erased name, supplied name)
      where
        supplied :: Expression -> [Attribute]
        supplied (ExApplication target (ArTau attr _)) = attr : supplied target
        supplied _ = []
    erased :: Expression -> Expression
    erased (ExApplication target _) = erased target
    erased (ExDispatch target attr) = ExDispatch (erased target) attr
    erased other = other
    fresh :: Maybe (Expression, [Attribute]) -> Attribute -> ReduceContext -> IO Bool
    fresh (Just (object, filled)) attr caller
      | attr `notElem` filled = do
          seen <- visited caller._memo object attr
          unless seen (visit caller._memo object attr)
          pure (not seen)
    fresh _ _ _ = pure True
    scope :: Attribute -> Expression -> Expression
    scope attr (ExFormation bds) = ExFormation (filter (not . named attr) bds)
    scope _ other = other
    named :: Attribute -> Binding -> Bool
    named attr (BiTau attr' _) = attr' == attr
    named _ _ = False
    argument :: (Frame -> Expression -> State -> ReduceContext -> IO (Expression, State)) -> Frame -> Argument -> State -> ReduceContext -> IO (Argument, State)
    argument walk frame (ArTau attr arg) state' caller = do
      (entered, state'') <- walk frame arg state' caller
      pure (ArTau attr entered, state'')
    argument walk frame (ArAlpha alpha arg) state' caller = do
      (entered, state'') <- walk frame arg state' caller
      pure (ArAlpha alpha entered, state'')

closed :: Expression -> Bool
closed ExXi = False
closed (ExDispatch target _) = closed target
closed (ExApplication target (ArTau _ arg)) = closed target && closed arg
closed (ExApplication target (ArAlpha _ arg)) = closed target && closed arg
closed _ = True

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
