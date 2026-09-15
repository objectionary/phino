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
-- and the plumbing that reads a rule's premises. 𝔻 lives in 'Dataize', which
-- imports this module; the one edge pointing back — the 'dataize' premise of a
-- λ function reducing one of its operands — is injected as '_reduce' rather
-- than imported (see 'ReductionFunc').
module Morph (ReduceContext (..), ReduceException (..), ReductionFunc, Morphed, Steps (..), deeper, emptyState, excluding, execBuildTerm, insideUniverse, leadsTo, morph, morph', normalized, parking, producer, sidePremise, verb) where

import AST
import Builder (buildExpressionThrows, contextualize)
import Control.Exception (Exception, catch, throwIO, try)
import Control.Monad (foldM, when)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import Deps (BuildTermFunc, BuildTermMethodS, Evaluation (..), SaveEvalFunc, SaveStepFunc, State, Term (..), carried)
import Lambdas (Lambda (..), Lambdas, Meta (..), attributeOf, matched, minted)
import Locator (locatedExpression, withLocatedExpression)
import Matcher (MetaValue (..), Subst (..), combine, matchExpression', substEmpty, substSingle)
import Must (Must (..))
import Random (shuffle)
import Rewriter (RewriteContext (RewriteContext), Rewritten, rewrite)
import Rule (RuleContext (RuleContext), extraSubstitutions, matchExpressionWithRule', meetCondition)
import Text.Printf (printf)
import Yaml (ExtraArgument (..), normalizationRules)
import qualified Yaml as Y

-- A term together with the derivation that reached it: what one frame of a
-- judgment's spine is handed and hands on.
type Morphed = (Expression, NonEmpty Rewritten)

-- How the morphing side reaches back to the dataization one. The 'dataize'
-- premise of a λ function reduces one of its operands, and the answer is a
-- whole run of 𝔻 — a judgment 𝕄 has no business knowing about, since
-- 'Dataize' imports 'Morph' and not the other way round. The reduction is
-- therefore injected into the context, the way 'Deps' injects '_buildTerm',
-- and 'Dataize' supplies its own 'reduction' for it. The state 𝑠 goes in and
-- comes back out, so the symbols a nested run mints are counted in the same
-- sequence as the ones around it.
type ReductionFunc = Expression -> ReduceContext -> Expression -> State -> IO (Expression, State)

-- The initial, empty state a run of 𝕄 or 𝔻 starts from. The 'State' type itself
-- lives in 'Deps' next to 'BuildTermMethod'.
emptyState :: State
emptyState = ""

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
  , _depthSensitive :: Bool
  , _shuffle :: Bool
  , _partial :: Bool
  , _deep :: Bool
  , _functions :: Lambdas
  , _buildTerm :: BuildTermFunc
  , _reduce :: ReductionFunc
  , _saveStep :: SaveStepFunc
  , _saveEval :: SaveEvalFunc
  }

data ReduceException
  = OutOfSteps Int
  | -- A λ function could not fire: the '--functions' file carries no entry that
    -- answers that name, or every entry it carries is guarded out, so there is
    -- nothing to fire. The name is that of the function 𝔼 actually failed on,
    -- which for a chain of dispatches is the innermost one, since 'ml' reduces
    -- a head before the function above it fires.
    Stuck T.Text
  | -- A 'Stuck' caught by a frame of the 𝕄/𝔻 spine, together with the
    -- derivation and the state that frame had reached (see 'parking'). The head
    -- of the chain is the working expression with the stuck application left
    -- intact and everything reduced before it already in place: the residual
    -- program that '_partial' turns into the 'Residual' outcome. The state
    -- travels with it so the symbols a parked run minted are never minted
    -- again.
    StuckAt T.Text (NonEmpty Rewritten) State
  | -- An 'OutOfSteps' caught by a spine frame, carrying that frame's derivation
    -- and state just like 'StuckAt': a term that never reduces is a stuck site
    -- too, so '_partial' parks it and hands back the residual instead of
    -- failing hard (#1078)
    OutOfStepsAt Int (NonEmpty Rewritten) State
  deriving anyclass (Exception)

instance Show ReduceException where
  show (OutOfSteps limit) =
    printf "Dataization did not finish before reaching the limit of steps: --max-steps=%d" limit
  show (OutOfStepsAt limit _ _) = show (OutOfSteps limit)
  show (Stuck func) = printf "No entry of --functions answers the λ function '%s'" (T.unpack func)
  show (StuckAt func _ _) = show (Stuck func)

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

-- Split the λ binding off a formation for the LAMBDA morphing rule: the name of
-- the λ function to fire and the formation it fires against, the λ binding
-- removed. A formation with no λ binding, or with more than one, has nothing to
-- fire.
lambda :: [Binding] -> Maybe (T.Text, Expression)
lambda bds = case partition isLambda bds of
  ([BiLambda (Function func)], rest) -> Just (func, ExFormation rest)
  _ -> Nothing
  where
    isLambda :: Binding -> Bool
    isLambda (BiLambda _) = True
    isLambda _ = False

-- The same as 'lambda', but only for a formation that is saturated: one with
-- every binding of it filled (see 'filled'). A void is an argument the program
-- has not given yet, so such a formation is a method waiting to be applied
-- rather than an application waiting to be computed, and firing it would hand
-- the λ function a ∅ where it expects a value. 𝔻 needs no such guard, since it
-- fires only what dataization demands and nothing demands a method; the deep
-- walk meets every one a program declares — the method table of the object
-- model above all — so it asks first (see 'deepened').
saturated :: [Binding] -> Maybe (T.Text, Expression)
saturated bds = case lambda bds of
  Just (func, ExFormation rest) | all filled rest -> Just (func, ExFormation rest)
  _ -> Nothing

-- Whether a binding hands the formation something to work with. A void does
-- not: it names an argument the program has still to supply. Neither does ⊥:
-- the deep walk reduces a body in the scope of the formation around it, and a
-- formation standing unapplied still holds ρ ↦ ∅, so a ξ.ρ in that body comes
-- back as ⊥ rather than as the object the next dispatch supplies (#1196).
filled :: Binding -> Bool
filled (BiVoid _) = False
filled (BiTau _ ExTermination) = False
filled _ = True

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
  ctx <- deeper caller
  parking seq state $ do
    rules <- if ctx._shuffle then shuffle Y.morphingRules else pure Y.morphingRules
    matched <- firstMatch ctx rules
    case matched of
      Just (rule, subst) -> reduce ctx rule subst
      Nothing -> throwIO (userError "no morphing rule matched")
  where
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
-- demands — the argument of a λ function that cannot fire, for one — is therefore
-- reduced by neither, and the object structure is lost to the one that does
-- reduce it (#1124). This walk demands nothing either. It asks 𝕄 about every
-- sub-expression and, where 𝕄 lands on a formation whose λ the '--functions'
-- file answers, fires it and asks 𝕄 about the answer again (see 'fired'). A
-- sub-expression on whose way a λ function fired is replaced by the answer of
-- the last firing; where none fired it stays as it was written and only its
-- own parts are walked, so the calls no entry answers keep their names and
-- what comes back is still the same program, reduced as far as the file
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
      (answer, answerState) <- fired dispatched (contextualize walked context) univ walkedState ctx'
      pure (fromMaybe walked answer, answerState)
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

-- Ask 𝕄 about a term and fire the λ of the formation it reaches, as long as an
-- entry of the '--functions' file answers it, asking 𝕄 about every answer again:
-- what comes back is the answer of the last firing, or nothing at all where
-- nothing fired. This is the firing 'ml' makes without the dispatch that makes
-- 'ml' make it — the one 𝕄 leaves to 𝔻 — except in what it hands back: the raw
-- answer of the entry, not the normal form 𝔼 makes of it, since the deep walk
-- stands that answer back into the program, where a normal form would spell
-- the whole object out in place of the name the program called it by. A λ no
-- entry answers is left alone rather than fired and got stuck on, so what
-- phino cannot compute stays as it was written with or without '_partial'; a λ
-- function that cannot fire deeper on the spine still fails the run, exactly as
-- it does under 𝕄 alone, and '_partial' parks it. A formation still waiting for
-- its arguments is left alone too (see 'saturated'). A term standing as the
-- target of a dispatch is where 'ml' has its say: the λ is fired only where the
-- dispatched attribute is none of the formation's own (see 'demanded').
fired :: Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Maybe Expression, State)
fired dispatched term univ state caller = do
  ctx <- deeper caller
  morphed <- try (reduced ctx)
  case morphed of
    Right (ExFormation bds, state')
      | demanded bds -> maybe (pure (Nothing, state')) (evaluated ctx state') (saturated bds)
    Right (_, state') -> pure (Nothing, state')
    Left failure -> parked failure
  where
    -- Whether the dispatch the term stands under demands the λ of the formation
    -- 𝕄 reached. 'ml' fires that λ only where the dispatched attribute is none
    -- of the formation's own, since 'dot' resolves the dispatch before 'ml' is
    -- ever reached, and a walk firing it first answers a formation the dispatch
    -- no longer fits (#1187). A term standing anywhere else is demanded by
    -- nothing and the walk fires what 'mf' left bare, as it always has.
    demanded :: [Binding] -> Bool
    demanded bds = not (any bound bds)
      where
        bound :: Binding -> Bool
        bound (BiTau attr _) = Just attr == dispatched
        bound _ = False
    -- 𝕄 takes normal forms only and a term taken from the program as it was
    -- written is not necessarily one, so it is normalized against the universe
    -- first, exactly as '--inside' normalizes what it is handed. Both chains
    -- are dropped: the walk is not the spine and reports one step of its own
    -- (see 'morph'), so a stuck λ function leaves without a derivation.
    reduced :: ReduceContext -> IO (Expression, State)
    reduced ctx = do
      (normal, _) <- normalized term ((univ, Nothing) :| []) ctx
      ((morphed, _), state') <- morph' (normal, (univ, Nothing) :| []) univ state ctx
      pure (morphed, state')
    -- Fire the λ of the formation 𝕄 reached and go on from its answer, keeping
    -- the answer of the last firing. A λ no entry of the '--functions' file
    -- answers is not fired at all, which is what keeps the walk as total as 𝕄
    -- itself; one that is answered but whose every entry is guarded out is a
    -- gap in the file rather than a term phino cannot compute, so it gets stuck
    -- like any other firing. The firing reports itself to '_saveEval', so the
    -- protocol and the program agree on what was answered.
    evaluated :: ReduceContext -> State -> (T.Text, Expression) -> IO (Maybe Expression, State)
    evaluated ctx state' (func, self)
      | null (matched ctx._functions func) = pure (Nothing, state')
      | otherwise = do
          (answer, state'') <- symbol func self univ state' ctx
          (again, state''') <- fired dispatched answer univ state'' ctx
          pure (Just (fromMaybe answer again), state''')
    -- A site the walk cannot reduce — a λ function no entry answers, or one
    -- the step budget ran out on — is left as it was written and the walk goes
    -- on, which is what a partial morphing is: phino stops where it cannot
    -- decide rather than failing the whole run. Recursion ends here and nowhere
    -- else: a λ function whose answer fires it again is the object model's
    -- business, so '--max-steps' is what bounds it and the term it was called
    -- by is what stays in the program. The state the parked site had reached
    -- travels back, so the symbols it minted before it stopped are never minted
    -- again; the chain it parked on is dropped, since that chain is the walk's
    -- and not the spine's.
    parked :: ReduceException -> IO (Maybe Expression, State)
    parked (StuckAt _ _ reached) | caller._partial = pure (Nothing, reached)
    parked (OutOfStepsAt _ _ reached) | caller._partial = pure (Nothing, reached)
    parked (Stuck _) | caller._partial = pure (Nothing, state)
    parked (OutOfSteps _) | caller._partial = pure (Nothing, state)
    parked (StuckAt func _ _) = throwIO (Stuck func)
    parked (OutOfStepsAt limit _ _) = throwIO (OutOfSteps limit)
    parked failure = throwIO failure

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
      Y.OpEvaluate expr universe -> _evaluate ctx state [ArgExpression expr, ArgExpression universe] subst
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
-- λ function names under 'dataize' or 'morph', above all — and it is also the
-- contract of the '--inside' option. 𝔻 and 𝕄 accept normal forms only and an
-- expression handed in from outside is not necessarily one (a dispatch off a
-- formation, '⟦ x ↦ 6, ρ ↦ 5 ⟧.x', is not), so it is normalized against the
-- extended universe before either judgment sees it. The context comes back
-- aimed at that binding, so the caller hands the extended universe and the
-- context it got straight to 'dataize' or 'morph'.
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
-- This is what a 'morph' premise of a λ function reduces with, and the
-- dataizing sibling of it reaches 'Dataize' through '_reduce'.
morphing :: Expression -> ReduceContext -> Expression -> State -> IO (Expression, State)
morphing univ ctx expr state = do
  (universe, aiming) <- insideUniverse expr univ ctx
  (morphed, _, state') <- morph universe state aiming
  pure (morphed, state')

-- What the entries answering one λ name have reduced so far: every operand
-- already taken, under the judgment that took it and the dotted path that
-- names it, and the state the last of those reductions left behind. Entries
-- are tried in turn and each one names its own operands, so without this the
-- operands two entries share would be reduced twice — reported to the protocol
-- twice and, where the reduction mints symbols of its own, under two different
-- names.
data Reduced = Reduced
  { _already :: Map.Map (T.Text, T.Text) Expression
  , _left :: State
  }

-- phino implements no λ function of its own. Which ones exist is a property of
-- the object model being dataized, not of the calculus, so they come from the
-- '--functions' file, where each is a rule phino answers the firing with itself
-- (see 'Lambdas'). The entries whose key matches the name are tried in the
-- order the file lists them and the first one whose 'when' holds is the one
-- that answers, so one λ function may answer one term where its operands agree
-- and another where they do not. A name no entry matches, or one whose every
-- entry is guarded out, has no λ function to fire at all, and 𝔼 gets stuck on
-- it — the one behaviour left here. The formation 'self' is the one 𝔼 fired
-- against, its λ binding already removed, so an entry may name the attributes
-- of it; the universe 'univ' is what every operand of it is reduced inside.
-- What comes back is the raw term the entry answers with: normalizing it is
-- 𝔼's business, and the deep walk wants it as it was written.
symbol :: T.Text -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
symbol func self univ state ctx = go (matched ctx._functions func) (Reduced Map.empty state)
  where
    go :: [Lambda] -> Reduced -> IO (Expression, State)
    go [] _ = throwIO (Stuck func)
    go (entry : rest) known = do
      (bound, told, known') <- operands entry known
      held <- maybe (pure [bound]) (\cond -> meetCondition cond [bound] rules) entry._when
      case held of
        [] -> go rest known'
        guarded : _ -> answered entry guarded told known'._left
    rules :: RuleContext
    rules = RuleContext (execBuildTerm univ ctx)
    -- Reduce every operand the entry names, binding the meta that names it: a
    -- 'dataize' path through 𝔻, which ends in a byte formation or, where the
    -- operand is an unknown the run cannot decide, in whatever '_partial'
    -- parked at; a 'morph' path through 𝕄. A 'dataize' operand is told to the
    -- protocol next to the firing, since a byte array and a stuck λ both have a
    -- spelling of their own; a 'morph' one is a whole term and has none, so it
    -- is bracketed instead and whatever fires inside it stands between the two
    -- records.
    operands :: Lambda -> Reduced -> IO (Subst, [(T.Text, T.Text)], Reduced)
    operands entry known = do
      (bound, told, after) <- foldM dataized (substEmpty, [], known) entry._dataized
      (bound', after') <- foldM morphed (bound, after) entry._morphed
      pure (bound', told, after')
    dataized :: (Subst, [(T.Text, T.Text)], Reduced) -> (Meta, T.Text) -> IO (Subst, [(T.Text, T.Text)], Reduced)
    dataized (bound, told, known) (meta, path) = do
      (value, known') <- reduced "dataize" path known (ctx._reduce univ ctx)
      bound' <- bind meta (MvExpression value) bound
      pure (bound', told ++ [(meta._spelling, spelling) | spelling <- maybeToList (carried (MvExpression value))], known')
    morphed :: (Subst, Reduced) -> (Meta, T.Text) -> IO (Subst, Reduced)
    morphed (bound, known) (meta, path) = do
      (value, known') <- reduced "morph" path known bracketed
      bound' <- bind meta (MvExpression value) bound
      pure (bound', known')
      where
        bracketed :: Expression -> State -> IO (Expression, State)
        bracketed term current = do
          ctx._saveEval (EvOpening func meta._spelling)
          (morphed', current') <- morphing univ ctx term current
          ctx._saveEval (EvClosing func meta._spelling)
          pure (morphed', current')
    -- The operand under the dotted path, reduced by the named judgment unless
    -- an entry tried before this one has reduced it already.
    reduced :: T.Text -> T.Text -> Reduced -> (Expression -> State -> IO (Expression, State)) -> IO (Expression, Reduced)
    reduced judgment path known reduce = case Map.lookup (judgment, path) known._already of
      Just value -> pure (value, known)
      Nothing -> do
        (value, current) <- operand path >>= \term -> reduce term known._left
        pure (value, Reduced (Map.insert (judgment, path) value known._already) current)
    -- The node the formation being fired holds under the dotted path an entry
    -- names. A path nothing carries, or one that ends at a void attribute, has
    -- no operand to reduce: the entry names an attribute the object model does
    -- not declare, which is a mistake in the file and not a term phino cannot
    -- compute, so it fails the run rather than getting stuck.
    operand :: T.Text -> IO Expression
    operand path = case attributeOf path self of
      Just found -> pure found
      Nothing ->
        throwIO
          ( userError
              (printf "The λ function '%s' has no operand '%s' to reduce" (T.unpack func) (T.unpack path))
          )
    bind :: Meta -> MetaValue -> Subst -> IO Subst
    bind meta value bound = case combine (substSingle meta._name value) bound of
      Just bound' -> pure bound'
      Nothing ->
        throwIO
          ( userError
              (printf "The meta '%s' of λ function '%s' clashes with an existing binding" (T.unpack meta._spelling) (T.unpack func))
          )
    -- Mint the fresh symbols the entry asks for, build whatever its 'where'
    -- adds and answer with the term under '𝑛'. The firing is reported once,
    -- here, with the operands it dataized and the symbols it minted — every
    -- meta the protocol can spell without printing 𝜑.
    answered :: Lambda -> Subst -> [(T.Text, T.Text)] -> State -> IO (Expression, State)
    answered entry bound told current = do
      let (fresh, current') = minted entry._symbols current
      symbolic <- foldM (\acc (meta, name) -> bind meta (MvFunction name) acc) bound fresh
      extended <- extraSubstitutions [symbolic] (Just entry._extras) rules
      let named = [(meta._spelling, name) | (meta, name) <- fresh]
      case extended of
        [] ->
          throwIO
            ( userError
                (printf "The 'where' of λ function '%s' bound nothing" (T.unpack func))
            )
        final : _ -> do
          built <- buildExpressionThrows entry._answer final
          ctx._saveEval (EvFiring func (told ++ named))
          pure (built, current')

-- Augment the injected, context-free term builder with the dataization and
-- morphing operations that need the universe: 'evaluate' fires a λ function and
-- 'morph' morphs a sub-expression. 𝔼 ('evaluate') takes the universe as an
-- explicit second expression argument, while 𝕄 ('morph') is handed the threaded
-- 'univ'. Every other function is delegated unchanged. This is the matcher's
-- condition path (guards in 'when'/'having'), which has no state to thread, so 𝔼
-- and 𝕄 run here on a fresh, empty state whose result is discarded; the
-- state-threading callers in 'sidePremise' use '_evaluate' and '_morph' directly.
execBuildTerm :: Expression -> ReduceContext -> BuildTermFunc
execBuildTerm _ ctx "evaluate" = \args subst -> fst <$> _evaluate ctx emptyState args subst
execBuildTerm univ ctx "morph" = \args subst -> fst <$> _morph univ ctx emptyState args subst
execBuildTerm _ ctx func = _buildTerm ctx func

-- The Evaluation function 𝔼(b, e, s): it fires the λ function of a formation
-- 'b' against the global universe 'e', under the incoming state 𝑠, normalizes
-- its raw result 𝒩(e₁) = n, and returns that normal form together with the
-- new state. Normalizing here makes 𝔼's codomain 𝓝 (as its type demands), so
-- callers ('fire', 'ml') need no follow-up 'normalize' premise. The universe is
-- passed explicitly as the second argument (rather than threaded behind the
-- scenes), matching how the morphing 𝕄 and dataization 𝔻 functions carry it.
-- Every firing reports itself to '_saveEval', which the '--evaluations' option
-- turns into one record per line (see 'symbol'). Firings are reported in the
-- order they complete, so the λ function of a head reduced by 'ml' is reported
-- before the one dispatched on its result. A firing that gets stuck is reported
-- too, by its name alone, when the run is a partial evaluation rather than a
-- failure ('_partial'): the site is what the caller wants to learn then. The
-- report is made before the signal goes on to the spine, where 'parking'
-- attaches the derivation to it.
_evaluate :: ReduceContext -> State -> BuildTermMethodS
_evaluate ctx state [ArgExpression expr, ArgExpression universe] subst = do
  form <- buildExpressionThrows expr subst
  univ <- buildExpressionThrows universe subst
  case form of
    ExFormation bds -> case lambda bds of
      Just (func, args) -> do
        (raw, state') <- symbol func args univ state ctx `catch` parked func
        (normal, _) <- normalized raw ((univ, Nothing) :| []) ctx
        pure (TeExpression normal, state')
      Nothing -> throwIO (userError "Function evaluate() expects a formation with a λ binding")
    _ -> throwIO (userError "Function evaluate() expects a formation")
  where
    parked :: T.Text -> ReduceException -> IO a
    parked func failure@(Stuck _) = do
      when ctx._partial (ctx._saveEval (EvStuck func))
      throwIO failure
    parked _ failure = throwIO failure
_evaluate _ _ _ _ = throwIO (userError "Function evaluate() requires exactly 2 expression arguments")

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
