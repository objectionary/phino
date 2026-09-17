{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The Morphing function 𝕄 and the machinery every reduction of the calculus is
-- threaded with: the context, the step budget, the signals a stuck run raises
-- and the plumbing that reads a rule's premises. 𝔻 lives in 'Dataize' and 𝔼 in
-- 'Evaluate', both of which import this module; the edges pointing back — the
-- 'dataize' operand of a λ function, which is a dataization, and the firing of
-- a λ function itself, which is an evaluation — are injected as '_reduce',
-- '_evaluate' and '_fire' rather than imported (see 'ReductionFunc' and
-- 'EvaluationFunc').
module Morph (ReduceContext (..), ReduceException (..), EvaluationFunc, FiringFunc, ReductionFunc, Morphed, Steps (..), deeper, emptyState, excluding, execBuildTerm, insideUniverse, leadsTo, morph, morph', morphing, normalized, parking, producer, sidePremise, unparked, verb) where

import AST
import Builder (buildExpressionThrows, contextualize)
import Control.Exception (Exception, catch, throwIO, try)
import Control.Monad (foldM)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Deps (BuildTermFunc, BuildTermMethodS, SaveEvalFunc, SaveStepFunc, State (..), Term (..))
import Lambdas (Lambdas)
import Locator (locatedExpression, withLocatedExpression)
import Matcher (MetaValue (..), Subst (..), combine, matchExpression', substEmpty, substSingle)
import Must (Must (..))
import Printer (printExpression)
import Random (shuffle)
import Rewriter (RewriteContext (RewriteContext), Rewritten, Seen, rewrite, seenInsert, seenMember)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Text.Printf (printf)
import Yaml (ExtraArgument (..), normalizationRules)
import qualified Yaml as Y

-- A term together with the derivation that reached it: what one frame of a
-- judgment's spine is handed and hands on.
type Morphed = (Expression, NonEmpty Rewritten)

-- How the morphing side reaches back to the dataization one. A λ function
-- brings its 'dataize' operands down through 𝔻, and that is a whole run of a
-- judgment 𝕄 has no business knowing about, since 'Dataize' imports 'Morph'
-- and not the other way round. The reduction is therefore injected into the
-- context, the way 'Deps' injects '_buildTerm', and 'Dataize' supplies its own
-- 'reduction' for it. What comes back is data or nothing at all, since an
-- operand 𝔻 could not bring down to bytes leaves the firing that asked for it
-- with nothing to bind. The state 𝑠 goes in and comes back out, so the symbols
-- a nested run mints are counted in the same sequence as the ones around it.
type ReductionFunc = Expression -> ReduceContext -> Expression -> State -> IO (Maybe Bytes, State)

-- How 𝕄 reaches the Evaluation function 𝔼, which lives in 'Evaluate' and
-- imports this module for the machinery every judgment shares. 𝔼 is what the
-- 'ml' and 'fire' rules ask for through an 'evaluate' premise, and it answers
-- with a normal form, so the rule that asked needs no 'normalize' after it. The
-- edge is injected rather than imported, exactly as 'ReductionFunc' injects the
-- 𝔻 one, and 'Evaluate' supplies its own 'evaluation' for it.
type EvaluationFunc = ReduceContext -> State -> BuildTermMethodS

-- How the deep walk reaches 𝔼. Like 'EvaluationFunc' it answers a normal form,
-- or nothing at all where nothing fired: the walk stands that answer back into
-- the program, and what a firing stands there has to look like what the program
-- itself would have morphed to, or the two cannot be compared (#1268). The
-- first argument is the attribute the term stands dispatched under, which is
-- what tells a λ the dispatch demands from one it does not.
type FiringFunc = Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Maybe Expression, State)

-- The initial, empty state a run of 𝕄 or 𝔻 starts from: nothing minted and
-- nothing manufactured yet. The 'State' type itself lives in 'Deps' next to
-- 'BuildTermMethod'.
emptyState :: State
emptyState = State 0 Nothing

-- How many steps of the 𝕄/𝔻 recursion one branch of a derivation may take
-- ('_limit', the '--max-steps' option) and how many the branch reaching this
-- point has already taken ('_spent'). 𝕄 and 𝔻 recurse into each other, into the
-- premises of their own rules and into the λ functions they fire, so a budget local to
-- one of those chains is reset by the next nested call and bounds nothing (see
-- #1052). This one rides in the context that every such path — the spine, the
-- side-premises, '_dataize' and '_morph' — already carries, so a nested call
-- inherits the count of the call that made it. It bounds depth, not total work:
-- a premise passes its count down but not back, so siblings each descend from
-- the same '_spent'. Bounding every branch is enough to terminate, since a rule
-- has finitely many premises.
data Steps = Steps
  { _limit :: Int
  , _spent :: Int
  }

-- The context every reduction of the calculus is threaded with — 𝕄 here and 𝔻 in
-- 'Dataize' — carrying the configuration plus the step budget spent so far. Nothing global is fixed here: the universe (the second argument 'e' of
-- 𝕄(n, e, s) and 𝔻(n, e, s)) is a plain expression threaded as an argument to
-- 'dataize'', 'morph'' and on to the λ functions, and the state 's' is threaded the same
-- way (see 'State'). The working expression needed for normalization is taken
-- from the head of the step chain, so no separate wrapper type is threaded
-- around.
data ReduceContext = ReduceContext
  { _locator :: Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _steps :: Steps
  , _nesting :: Int
  , _depthSensitive :: Bool
  , _shuffle :: Bool
  , _partial :: Bool
  , _deep :: Bool
  , _acyclic :: Bool
  , _seen :: Seen
  , _symbolic :: Lambdas
  , _buildTerm :: BuildTermFunc
  , _reduce :: ReductionFunc
  , _evaluate :: EvaluationFunc
  , _fire :: FiringFunc
  , _saveStep :: SaveStepFunc
  , _saveEval :: SaveEvalFunc
  }

data ReduceException
  = OutOfSteps Int
  | -- A λ function could not fire: the '--symbolic' file carries no entry
    -- answering that name, or an operand of the entry it does carry never came
    -- down to data, so there is nothing to answer with. The name is that of the
    -- function 𝔼 actually failed on, which for a chain of dispatches is the
    -- innermost one, since 'ml' reduces a head before the function above it
    -- fires.
    Stuck T.Text
  | -- A 'Stuck' caught by a frame of the 𝕄/𝔻 spine, together with the
    -- derivation and the state that frame had reached (see 'parking'). The head
    -- of the chain is the working expression with the stuck application left
    -- intact and everything reduced before it already in place: the residual
    -- program that '_partial' turns into the 'Residual' outcome. The state
    -- travels with it, so the symbols a parked run minted are never minted
    -- again.
    StuckAt T.Text (NonEmpty Rewritten) State
  | -- An 'OutOfSteps' caught by a spine frame, carrying that frame's derivation
    -- and state just like 'StuckAt': a term that never reduces is a stuck site
    -- too, so '_partial' parks it and hands back the residual instead of
    -- failing hard (#1078)
    OutOfStepsAt Int (NonEmpty Rewritten) State
  | -- Morphing was asked to reduce a term a frame above it is already reducing,
    -- which it can only ever answer by asking again. Raised under '_acyclic'
    -- alone, so the signal itself is the permission to park on it: a run that
    -- never asked for the guard never sees it.
    Looping Expression
  | -- A 'Looping' caught by a frame of the 𝕄 spine, carrying that frame's
    -- derivation and state the way 'StuckAt' does. The guard runs as a frame
    -- opens, before that frame parks anything, so the frame attaching the chain
    -- is the one the repeat was reached from and the head of the chain is its
    -- working expression — the term that came back left exactly where it stood,
    -- the way an exhausted budget stops on the last step it could afford.
    LoopingAt Expression (NonEmpty Rewritten) State
  deriving anyclass (Exception)

instance Show ReduceException where
  show (OutOfSteps limit) =
    printf "Dataization did not finish before reaching the limit of steps: --max-steps=%d" limit
  show (OutOfStepsAt limit _ _) = show (OutOfSteps limit)
  show (Stuck func) = printf "No entry of --symbolic answers the λ function '%s'" (T.unpack func)
  show (StuckAt func _ _) = show (Stuck func)
  show (Looping term) = printf "Morphing came back to a term it is already reducing: %s" (printExpression term)
  show (LoopingAt term _ _) = show (Looping term)

-- Charge one step of the 𝕄/𝔻 recursion to the budget, refusing to descend once
-- it is gone. '--max-cycles' and '--max-depth' bound only the normalization run
-- inside a single step, so before this the recursion itself was unbounded and a
-- term that never reduces to bytes kept 𝕄 and 𝔻 calling each other forever
-- (#1052). Rewriting hands back whatever it has reached when it runs out of
-- cycles; 𝔻 has no partial answer to give, so an exhausted budget always throws,
-- with or without '--depth-sensitive'.
deeper :: ReduceContext -> IO ReduceContext
deeper ctx@ReduceContext{_steps = Steps limit spent}
  | spent >= limit = throwIO (OutOfSteps limit)
  | otherwise = pure ctx{_steps = Steps limit (spent + 1)}

-- Run one frame of the 𝕄/𝔻 spine, attaching its derivation and its state to a
-- stuck λ function or an exhausted budget escaping it. 'Stuck' is raised deep
-- inside a firing, which knows nothing about the chain, so the innermost spine
-- frame it reaches is the one to record where the derivation stopped: the head
-- of that frame's chain is the working expression with the stuck application
-- intact and everything reduced before it already in place. The same holds for
-- 'OutOfSteps': a term cycling through the universe is no more a failure of the
-- chain than a missing λ function is, and under '_partial' it deserves the same
-- parked residual (#1078). Outer frames see the '…At' signals and let them
-- pass, since their chains are prefixes of that one; a side-computation running
-- on a chain of its own strips the chain off again (see 'unparked') before the
-- signal reaches the spine.
parking :: NonEmpty Rewritten -> State -> IO a -> IO a
parking seq state action = action `catch` rethrow
  where
    rethrow :: ReduceException -> IO a
    rethrow (Stuck func) = throwIO (StuckAt func seq state)
    rethrow (OutOfSteps limit) = throwIO (OutOfStepsAt limit seq state)
    rethrow (Looping term) = throwIO (LoopingAt term seq state)
    rethrow failure = throwIO failure

-- Strip the derivation off a stuck λ function escaping a side-computation that
-- ran on a chain of its own — a firing reducing an operand of its own, or a
-- 'morph' premise through '_morph'. That chain is not the spine's, so it is
-- dropped and the spine frame around the side-computation attaches its own
-- (see 'parking').
unparked :: IO a -> IO a
unparked action = action `catch` rethrow
  where
    rethrow :: ReduceException -> IO a
    rethrow (StuckAt func _ _) = throwIO (Stuck func)
    rethrow (OutOfStepsAt limit _ _) = throwIO (OutOfSteps limit)
    rethrow (LoopingAt term _ _) = throwIO (Looping term)
    rethrow failure = throwIO failure

-- The Morphing function 𝕄 maps normal forms to formations. It is ternary,
-- 𝕄(n, e, s): besides the term 'n' it takes the universe 'e' ('univ') — a plain
-- expression — and the mutable state 's', returning the morphed term together
-- with the new state. The universe is matched against the rule's 'e-match'
-- pattern (usually the '𝑒' meta, which binds 'e' so the 'universe' rule substitutes
-- it, but a rule may pin it to a literal such as 'mg' matching Φ). Its rules
-- come from 'resources/morphing': the first matching rule's premises are evaluated and
-- its conclusion 'nresult' is built, always forwarding the same universe. The
-- clauses are disjoint (see #856, #860), so their declaration order must not be
-- load-bearing; when '_shuffle' is on (the '--shuffle' flag) the rules are
-- shuffled before the 'firstMatch' walk to exercise that invariant — mirroring
-- normalization's "apply until they stop matching". A genuinely order-independent
-- step stays deterministic; a hidden overlap surfaces as a nondeterministic
-- failure rather than staying silently green.
-- The 'morph' premise that produces the conclusion is the spine: when
-- its argument comes from a 'normalize' premise, the rewriter runs over that
-- argument and its individual steps (alpha, copy, dot, …) are spliced into the
-- chain before morphing continues. Every other premise is a side-computation
-- evaluated in isolation by 'sidePremise', its own steps discarded.
morph' :: Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
morph' (expr, seq) univ state caller = do
  ctx <- deeper =<< unvisited expr caller
  parking seq state $ do
    rules <- if ctx._shuffle then shuffle Y.morphingRules else pure Y.morphingRules
    matched <- firstMatch ctx rules
    case matched of
      Just (rule, subst) -> reduce ctx rule subst
      Nothing -> throwIO (userError "no morphing rule matched")
  where
    -- The terms the frames above this one are reducing, which is what
    -- '_acyclic' answers "have I been here before" with. The context travels
    -- down the recursion and never back up, exactly as the step budget does, so
    -- what it carries is the branch from the run to this frame and not
    -- everything the run has ever touched: two sibling subterms that happen to
    -- be equal are two terms, while a term reached from itself is a loop. The
    -- store is the one the rewriter detects its own loops with, a digest map
    -- resolving a collision by an exact comparison (see 'Seen').
    unvisited :: Expression -> ReduceContext -> IO ReduceContext
    unvisited term ctx
      | not ctx._acyclic = pure ctx
      | seenMember digest term ctx._seen = throwIO (Looping term)
      | otherwise = pure ctx{_seen = seenInsert digest term ctx._seen}
      where
        digest :: Int
        digest = hashExpression term
    firstMatch :: ReduceContext -> [Y.MorphRule] -> IO (Maybe (Y.MorphRule, Subst))
    firstMatch _ [] = pure Nothing
    firstMatch ctx (rule : rest) = do
      substs <- matchExpressionWithRule' (matchExpression' rule.ematch univ) expr (asRule rule) (RuleContext (execBuildTerm univ ctx))
      case substs of
        (subst : _) -> pure (Just (rule, subst))
        [] -> firstMatch ctx rest
    -- Match the conclusion term and check the guard; premises are no longer the
    -- matcher's business, so 'where'/'having' stay empty and the guard lives in
    -- 'when'. Every morphing guard reads only meta-variables bound by 'match'
    -- and 'e-match', so it holds before any premise runs.
    asRule :: Y.MorphRule -> Y.Rule
    asRule rule = Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing
    -- Evaluate the rule's premises and build its conclusion. A literal
    -- conclusion is terminal. Otherwise the conclusion meta is produced by a
    -- trailing 'morph' premise (the spine); if that premise's argument is itself
    -- bound by a 'normalize' premise, the normalization joins the spine and its
    -- steps splice in before morphing continues.
    reduce :: ReduceContext -> Y.MorphRule -> Subst -> IO (Morphed, State)
    reduce ctx rule subst = case producer rule.nresult rule.premises of
      Nothing -> do
        (final, state') <- sides ctx rule.premises subst
        built <- buildExpressionThrows rule.nresult final
        seq' <- leadsTo seq rule.name built ctx
        pure ((built, seq'), state')
      Just concl@(Y.Premise _ (Y.OpMorph arg)) -> case producer arg rule.premises of
        Just normal@(Y.Premise _ (Y.OpNormalize inner)) -> do
          (final, state') <- sides ctx (rule.premises `excluding` [concl, normal]) subst
          built <- buildExpressionThrows inner final
          labelled <- leadsTo seq rule.name built ctx
          (normal', seq') <- normalized built labelled ctx
          morph' (normal', seq') univ state' ctx
        _ -> do
          (final, state') <- sides ctx (rule.premises `excluding` [concl]) subst
          built <- buildExpressionThrows arg final
          seq' <- leadsTo seq rule.name built ctx
          morph' (built, seq') univ state' ctx
      Just _ -> throwIO (userError (printf "morphing rule '%s' must conclude with a 'morph' premise" rule.name))
    sides :: ReduceContext -> [Y.Premise] -> Subst -> IO (Subst, State)
    sides ctx premises subst = foldM (sidePremise univ ctx) (subst, state) premises

-- Morph the expression located at '_locator' — 𝕄 asked on its own, the way
-- 'dataize' asks 𝔻. The whole input expression is itself the universe Φ (the 'e'
-- argument) threaded through 𝕄, so it is passed both as the located target and
-- as the universe; the default locator Q therefore morphs the top formation,
-- which 'mf' hands back unchanged, and '_locator' is how one aims 𝕄 at a
-- subterm. Unlike 𝔻, 𝕄 is total: it stops at the first formation it reaches
-- ('mf') and never demands bytes, and where no formation is reachable it answers
-- with the terminator ⊥ ('dead', 'xi', 'mg', 'mad', 'maad') rather than failing.
-- Only the λ functions 'ml' fires can still get stuck, and '_partial' parks
-- them just as it does under 𝔻: the answer is then the residual subterm the
-- spine had reached, taken from '_locator' of its working expression. Stopping
-- at the first formation leaves everything that formation holds as it was
-- written, which is what '_deep' walks into before the answer is handed back
-- (see 'deepened'). The state 𝑠 goes in and comes back out, so a 𝕄 asked
-- inside another judgment goes on minting symbols where that judgment left off.
morph :: Expression -> State -> ReduceContext -> IO (Expression, [Rewritten], State)
morph universe state ctx@ReduceContext{..} = do
  expr <- locatedExpression _locator universe
  result <- try (morph' (expr, (universe, Nothing) :| []) universe state ctx)
  case result of
    Right ((morphed, seq), state') -> walked morphed seq state'
    Left (StuckAt _ seq parked) | _partial -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked residue seq parked
    Left (OutOfStepsAt _ seq parked) | _partial -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked residue seq parked
    -- Unlike the two above, this one takes no '_partial' guard: a 'LoopingAt'
    -- exists only where '_acyclic' put it, so asking for the guard is already
    -- asking to be parked on what it finds.
    Left (LoopingAt _ seq parked) -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked residue seq parked
    Left failure -> throwIO (failure :: ReduceException)
  where
    -- The answer 𝕄 reached, walked by '_deep' before it is handed back (see
    -- 'deepened'), and the chain that led to both. The walk joins the chain as
    -- one step named 'deep', so '--sequence' ends on the term the command
    -- prints.
    walked :: Expression -> NonEmpty Rewritten -> State -> IO (Expression, [Rewritten], State)
    walked morphed seq state'
      | not _deep = pure (morphed, reverse (NE.toList seq), state')
      | otherwise = do
          (deep, state'') <- deepened morphed universe state' ctx
          seq' <- leadsTo seq "deep" deep ctx
          pure (deep, reverse (NE.toList seq'), state'')

-- Walk what 𝕄 answered with, entering everything it left as it was written —
-- the mechanism behind '--deep' ('_deep'). 𝕄 navigates a term to the first
-- formation it reaches and 'mf' hands that formation back with its bindings
-- untouched, since firing a bare λ is 𝔻's business; 𝔻 in turn follows the one
-- path dataization demands and ends in bytes. A part of a program that nothing
-- demands — the argument of a λ function that cannot fire, for one — is
-- therefore reduced by neither, and the object structure is lost to the one
-- that does reduce it (#1124). This walk demands nothing either. It asks 𝕄
-- about every sub-expression and, where 𝕄 lands on a formation whose λ the
-- '--symbolic' file answers, fires it and asks 𝕄 about the answer again (see
-- '_fire', which 'Evaluate' answers with its own 'fired'). A sub-expression on
-- whose way a λ function fired is replaced by the
-- answer of the last firing; where none fired it stays as it was written and
-- only its own parts are walked, so the calls no entry answers keep their names
-- and what comes back is still the same program, reduced as far as the file
-- allows. Every entry is charged to the '--max-steps' budget, which is what
-- bounds the walk.
deepened :: Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
deepened expr univ = go Nothing ExXi expr
  where
    -- A term as it was written, together with what its free ξ stands for: the
    -- formation the walk entered it from, without the binding it came from,
    -- exactly the context the 'dot' rule hands a dispatched body. At the top
    -- there is no such formation, so ξ stands for itself and contextualization
    -- leaves the term alone.
    go :: Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
    go dispatched context term state' caller = do
      ctx' <- deeper caller
      (walked, walkedState) <- parts context term state' caller
      (answer, answered) <- ctx'._fire dispatched (contextualize walked context) univ walkedState ctx'
      pure (fromMaybe walked answer, answered)
    -- The parts of a term nothing fired on, walked one by one and put back
    -- where they were, so the term keeps the shape it was written in.
    parts :: Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
    parts _ (ExFormation bds) state' caller = do
      (entered, state'') <- bindings bds bds state' caller
      pure (ExFormation entered, state'')
    parts context (ExDispatch target attr) state' caller = do
      (entered, state'') <- go (Just attr) context target state' caller
      pure (ExDispatch entered attr, state'')
    parts context (ExApplication target arg) state' caller = do
      (entered, state'') <- go Nothing context target state' caller
      (applied, state''') <- argument context arg state'' caller
      pure (ExApplication entered applied, state''')
    parts _ term state' _ = pure (term, state')
    -- Walk the bindings of a formation left to right, threading the state
    -- through them. Only what the formation itself holds is entered: ρ names
    -- the object around it rather than one inside it, and a void, Δ or λ
    -- binding carries no term to walk at all.
    bindings :: [Binding] -> [Binding] -> State -> ReduceContext -> IO ([Binding], State)
    bindings _ [] state' _ = pure ([], state')
    bindings whole (BiTau attr body : rest) state' caller
      | attr /= AtRho = do
          (entered, state'') <- go Nothing (scope attr whole) body state' caller
          (others, state''') <- bindings whole rest state'' caller
          pure (BiTau attr entered : others, state''')
    bindings whole (bd : rest) state' caller = do
      (others, state'') <- bindings whole rest state' caller
      pure (bd : others, state'')
    -- The context a binding's body is entered in: the formation without that
    -- binding, the very context 'dot' contextualizes a dispatched body in, so
    -- a body reaching back at itself through ξ collapses instead of looping.
    scope :: Attribute -> [Binding] -> Expression
    scope attr bds = ExFormation (filter (not . named) bds)
      where
        named :: Binding -> Bool
        named (BiTau attr' _) = attr' == attr
        named _ = False
    -- Both sides of an application stand in the same context: the term it
    -- applies is walked by the caller and the argument it binds is walked here.
    argument :: Expression -> Argument -> State -> ReduceContext -> IO (Argument, State)
    argument context (ArTau attr arg) state' caller = do
      (entered, state'') <- go Nothing context arg state' caller
      pure (ArTau attr entered, state'')
    argument context (ArAlpha alpha arg) state' caller = do
      (entered, state'') <- go Nothing context arg state' caller
      pure (ArAlpha alpha entered, state'')

-- The premise binding the given expression meta, if any. The conclusion of a
-- morphing rule and the argument of a continuation premise are looked up here to
-- find the premise that produces them.
producer :: Expression -> [Y.Premise] -> Maybe Y.Premise
producer (ExMeta name) = find (\premise -> premise.result == name)
producer _ = const Nothing

-- The premises whose result meta is not bound by any of the given ones — the
-- side-computations left once the spine premises are removed.
excluding :: [Y.Premise] -> [Y.Premise] -> [Y.Premise]
excluding premises removed = filter (\premise -> premise.result `notElem` map (.result) removed) premises

-- Evaluate one side-computation premise — a 'morph', 'evaluate' or 'contextualize'
-- of an earlier term — in isolation, binding its result meta. These never splice
-- steps into the trace: 'morph' and 'evaluate' reduce on a fresh chain and discard
-- it, 'contextualize' is pure. The state is threaded through: 'evaluate' (the
-- 𝔼 of the 'ml' and 'fire' rules) takes the incoming state 𝑠1 and yields a
-- new one 𝑠2, 'morph' propagates whatever its sub-reduction produced, and every
-- other operation leaves the state untouched.
sidePremise :: Expression -> ReduceContext -> (Subst, State) -> Y.Premise -> IO (Subst, State)
sidePremise univ ctx (subst, state) premise = do
  (term, state') <- runOperation
  case combine (substSingle premise.result (metaValue term)) subst of
    Just subst' -> pure (subst', state')
    Nothing -> throwIO (userError (printf "premise meta '%s' clashes with an existing binding" (T.unpack premise.result)))
  where
    -- The 𝔼 ('evaluate') and 𝕄 ('morph') operations can change the state, so they
    -- go through their state-aware builders; every other operation is stateless
    -- and the incoming state is returned unchanged.
    runOperation :: IO (Term, State)
    runOperation = case premise.operation of
      Y.OpEvaluate expr universe -> ctx._evaluate ctx state [ArgExpression expr, ArgExpression universe] subst
      Y.OpMorph expr -> _morph univ ctx state [ArgExpression expr] subst
      operation -> do
        term <- execBuildTerm univ ctx (verb operation) (verbArgs operation) subst
        pure (term, state)
    metaValue :: Term -> MetaValue
    metaValue (TeExpression value) = MvExpression value
    metaValue (TeAttribute value) = MvAttribute value
    metaValue (TeBytes value) = MvBytes value
    metaValue (TeBindings value) = MvBindings value

-- The build-term function name backing a premise operation.
verb :: Y.Operation -> String
verb (Y.OpMorph _) = "morph"
verb (Y.OpNormalize _) = "normalize"
verb (Y.OpEvaluate _ _) = "evaluate"
verb (Y.OpContextualize _ _) = "contextualize"
verb (Y.OpDataize _) = "dataize"

-- The build-term arguments backing a premise operation.
verbArgs :: Y.Operation -> [ExtraArgument]
verbArgs (Y.OpMorph expr) = [ArgExpression expr]
verbArgs (Y.OpNormalize expr) = [ArgExpression expr]
verbArgs (Y.OpEvaluate expr universe) = [ArgExpression expr, ArgExpression universe]
verbArgs (Y.OpContextualize expr context) = [ArgExpression expr, ArgExpression context]
verbArgs (Y.OpDataize expr) = [ArgExpression expr]

leadsTo :: NonEmpty Rewritten -> String -> Expression -> ReduceContext -> IO (NonEmpty Rewritten)
leadsTo ((current, _) :| rest) rule expr ReduceContext{..} = do
  updated <- withLocatedExpression _locator expr current
  pure ((updated, Nothing) :| (current, Just rule) : rest)

-- Reduce 'expr' to its normal form through the normalization rewriter, embedding
-- it at '_locator' into the working expression taken from the head of the step
-- chain so the rewriter sees the surrounding context. Splices the individual
-- steps (alpha, copy, dot, …) into the chain and returns the normalized
-- expression together with the extended sequence.
normalized :: Expression -> NonEmpty Rewritten -> ReduceContext -> IO (Expression, NonEmpty Rewritten)
normalized expr seq ctx@ReduceContext{..} = do
  whole <- withLocatedExpression _locator expr (fst (NE.head seq))
  (rewrittens, _) <- rewrite whole normalizationRules (rewriteContext ctx)
  let (rw :| rws) = NE.reverse rewrittens
      seq' = rw :| rws <> NE.tail seq
  expr' <- locatedExpression _locator (fst rw)
  pure (expr', seq')
  where
    -- Switch the reduction context to a rewriting context for normalization,
    -- disabling the must-checker and breakpoints.
    rewriteContext :: ReduceContext -> RewriteContext
    rewriteContext ReduceContext{..} =
      RewriteContext _locator _maxDepth _maxCycles _depthSensitive _buildTerm MtDisabled Nothing _saveStep

-- Bind 'expr' to a synthetic attribute of the universe and reduce it to a
-- normal form there, handing back the extended universe together with the
-- locator that aims at the binding. This is the trick phino has always played
-- to reduce a sub-expression that is not part of the program — the operand a
-- λ function names under 'dataize' or 'morph', above all — and it is also
-- the contract of the '--inside' option, so a caller asking phino to reduce a
-- part of the formation it was given does not have to splice it into the text
-- of the universe by hand. 𝔻 and 𝕄 accept normal forms only and an expression
-- handed in from outside is not necessarily one (a dispatch off a formation,
-- '⟦ x ↦ 6, ρ ↦ 5 ⟧.x', is not), so it is normalized against the extended
-- universe before either judgment sees it. The context comes back aimed at that
-- binding, so the caller hands the extended universe and the context it got
-- straight to 'dataize' or 'morph'.
insideUniverse :: Expression -> Expression -> ReduceContext -> IO (Expression, ReduceContext)
insideUniverse expr univ ctx@ReduceContext{_buildTerm = buildTerm} = case univ of
  ExFormation bds -> do
    (TeAttribute attr) <- buildTerm "random-tau" [] substEmpty
    let aiming = ctx{_locator = ExDispatch ExRoot attr}
        synthetic = ExFormation (BiTau attr expr : bds)
    (normal, _) <- normalized expr ((synthetic, Nothing) :| []) aiming
    pure (ExFormation (BiTau attr normal : bds), aiming)
  _ -> throwIO (userError "Can't reduce an expression inside a universe which is not a formation")

-- Morph a term that is not part of the program, the way 'reduction' in
-- 'Dataize' dataizes one: bound to a synthetic attribute of the universe and
-- reduced there (see 'insideUniverse'), since 𝕄 takes normal forms only and an
-- operand taken out of a formation as it was written is not necessarily one.
-- This is what a 'morph' operand of a λ function is reduced with, and the
-- dataizing sibling of it reaches 'Dataize' through '_reduce'.
morphing :: Expression -> ReduceContext -> Expression -> State -> IO (Expression, State)
morphing univ ctx expr state = do
  (universe, aiming) <- insideUniverse expr univ ctx
  (morphed, _, state') <- morph universe state aiming
  pure (morphed, state')

-- Augment the injected, context-free term builder with the dataization and
-- morphing operations that need the universe: 'evaluate' fires a λ function and
-- 'morph' morphs a sub-expression. 𝔼 ('evaluate') takes the universe as an
-- explicit second expression argument, while 𝕄 ('morph') is handed the threaded
-- 'univ'. Every other function is delegated unchanged. This is the matcher's
-- condition path (guards in 'when'/'having'), which has no state to thread, so 𝔼
-- and 𝕄 run here on a fresh, empty state whose result is discarded; the
-- state-threading callers in 'sidePremise' use '_evaluate' and '_morph' directly.
execBuildTerm :: Expression -> ReduceContext -> BuildTermFunc
execBuildTerm _ ctx "evaluate" = \args subst -> fst <$> ctx._evaluate ctx emptyState args subst
execBuildTerm univ ctx "morph" = \args subst -> fst <$> _morph univ ctx emptyState args subst
execBuildTerm _ ctx func = _buildTerm ctx func

-- The Morphing function 𝕄 exposed as a build-term function so a rule can morph
-- a sub-expression in its 'where' (the 'md' and 'ma' rules morph
-- the head before re-attaching it). The step chain is discarded: the producing
-- rule splices the surrounding normalization steps itself, and a stuck λ met
-- on the way leaves without it (see 'unparked'). The state is threaded through
-- and the new state returned alongside the morphed term.
_morph :: Expression -> ReduceContext -> State -> BuildTermMethodS
_morph univ ctx state [ArgExpression expr] subst = unparked $ do
  built <- buildExpressionThrows expr subst
  ((morphed, _), state') <- morph' (built, (univ, Nothing) :| []) univ state ctx
  pure (TeExpression morphed, state')
_morph _ _ _ _ _ = throwIO (userError "Function morph() requires exactly 1 expression argument")
