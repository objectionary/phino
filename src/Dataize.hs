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

module Dataize (morph, morph', dataize, dataize', insideUniverse, DataizeContext (..), DataizeException (..), Outcome (..), Steps (..), State, emptyState, execBuildTerm) where

import AST
import Atoms (ReduceFunc, Registry, fireAtom, registeredAtom)
import Builder (buildBytesThrows, buildExpressionThrows, contextualize)
import Control.Exception (Exception, catch, throwIO, try)
import Control.Monad (foldM, when)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Deps (BuildTermFunc, BuildTermMethodS, Evaluation (..), SaveEvalFunc, SaveStepFunc, State, Term (..))
import Locator (locatedExpression, withLocatedExpression)
import Matcher (MetaValue (..), Subst (..), combine, matchExpression', substEmpty, substSingle)
import Must (Must (..))
import Random (shuffle)
import Rewriter (RewriteContext (RewriteContext), Rewritten, rewrite)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Text.Printf (printf)
import Yaml (ExtraArgument (..), normalizationRules)
import qualified Yaml as Y

type Dataized = (Bytes, [Rewritten])

type Dataizable = (Expression, NonEmpty Rewritten)

type Morphed = Dataizable

-- The initial, empty state used when dataization starts. The 'State' type itself
-- lives in 'Deps' next to 'BuildTermMethod'.
emptyState :: State
emptyState = ""

-- How many steps of the 𝕄/𝔻 recursion one branch of a derivation may take
-- ('_limit', the '--max-steps' option) and how many the branch reaching this
-- point has already taken ('_spent'). 𝕄 and 𝔻 recurse into each other, into the
-- premises of their own rules and into the atoms they fire, so a budget local to
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

-- The evaluation context carries the configuration plus the step budget spent so
-- far. Nothing global is fixed here: the universe (the second argument 'e' of
-- 𝕄(n, e, s) and 𝔻(n, e, s)) is a plain expression threaded as an argument to
-- 'dataize'', 'morph'' and on to the atoms, and the state 's' is threaded the same
-- way (see 'State'). The working expression needed for normalization is taken
-- from the head of the step chain, so no separate wrapper type is threaded
-- around.
data DataizeContext = DataizeContext
  { _locator :: Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _steps :: Steps
  , _depthSensitive :: Bool
  , _shuffle :: Bool
  , _partial :: Bool
  , _deep :: Bool
  , _atoms :: Registry
  , _buildTerm :: BuildTermFunc
  , _saveStep :: SaveStepFunc
  , _saveEval :: SaveEvalFunc
  }

data DataizeException
  = OutOfSteps Int
  | -- An atom could not fire: the '--atoms' registry carries no λ function of
    -- that name, so there is nothing to run. The name is that of the atom 𝔼
    -- actually failed on, which for a chain of dispatches is the innermost one,
    -- since 'ml' reduces a head before the atom above it fires.
    Stuck T.Text
  | -- A 'Stuck' caught by a frame of the 𝕄/𝔻 spine, together with the
    -- derivation that frame had reached (see 'parking'). The head of the chain
    -- is the working expression with the stuck application left intact and
    -- everything reduced before it already in place: the residual program that
    -- '_partial' turns into the 'Residual' outcome.
    StuckAt T.Text (NonEmpty Rewritten)
  | -- An 'OutOfSteps' caught by a spine frame, carrying that frame's derivation
    -- just like 'StuckAt': a term that never reduces is a stuck site too, so
    -- '_partial' parks it and hands back the residual instead of failing hard
    -- (#1078)
    OutOfStepsAt Int (NonEmpty Rewritten)
  deriving anyclass (Exception)

instance Show DataizeException where
  show (OutOfSteps limit) =
    printf "Dataization did not finish before reaching the limit of steps: --max-steps=%d" limit
  show (OutOfStepsAt limit _) = show (OutOfSteps limit)
  show (Stuck func) = printf "Atom '%s' does not exist" (T.unpack func)
  show (StuckAt func _) = show (Stuck func)

-- What a run of 𝔻 ends with: the bytes it reached or, under '_partial', the
-- residual program: what the known inputs decided is computed, the stuck atom
-- and everything depending on it survive in place.
data Outcome
  = Dataized Bytes
  | Residual Expression
  deriving stock (Eq, Show)

-- Charge one step of the 𝕄/𝔻 recursion to the budget, refusing to descend once
-- it is gone. '--max-cycles' and '--max-depth' bound only the normalization run
-- inside a single step, so before this the recursion itself was unbounded and a
-- term that never reduces to bytes kept 𝕄 and 𝔻 calling each other forever
-- (#1052). Rewriting hands back whatever it has reached when it runs out of
-- cycles; 𝔻 has no partial answer to give, so an exhausted budget always throws,
-- with or without '--depth-sensitive'.
deeper :: DataizeContext -> IO DataizeContext
deeper ctx@DataizeContext{_steps = Steps limit spent}
  | spent >= limit = throwIO (OutOfSteps limit)
  | otherwise = pure ctx{_steps = Steps limit (spent + 1)}

-- Split the λ binding off a formation for the LAMBDA morphing rule: the name of
-- the atom to fire and the formation it fires against, the λ binding removed —
-- the two things 𝔼 reports besides the result. A formation with no λ binding,
-- or with more than one, has nothing to fire.
lambda :: [Binding] -> Maybe (T.Text, Expression)
lambda bds = case partition isLambda bds of
  ([BiLambda (Function func)], rest) -> Just (func, ExFormation rest)
  _ -> Nothing
  where
    isLambda :: Binding -> Bool
    isLambda (BiLambda _) = True
    isLambda _ = False

-- The same as 'lambda', but only for a formation that is saturated: one with no
-- void binding left in it. A void is an argument the program has not given yet,
-- so such a formation is a method waiting to be applied rather than an
-- application waiting to be computed, and firing it would hand the atom a ∅
-- where it expects a value. 𝔻 needs no such guard, since it fires only what
-- dataization demands and nothing demands a method; the deep walk meets every
-- one a program declares — the method table of the object model above all — so
-- it asks first (see 'deepened').
saturated :: [Binding] -> Maybe (T.Text, Expression)
saturated bds = case lambda bds of
  Just (func, ExFormation rest) | all filled rest -> Just (func, ExFormation rest)
  _ -> Nothing
  where
    filled :: Binding -> Bool
    filled (BiVoid _) = False
    filled _ = True

-- Run one frame of the 𝕄/𝔻 spine, attaching its derivation to a stuck atom or
-- an exhausted budget escaping it. 'Stuck' is raised deep inside an atom, which
-- knows nothing about the chain, so the innermost spine frame it reaches is the
-- one to record where the derivation stopped: the head of that frame's chain is
-- the working expression with the stuck application intact and everything
-- reduced before it already in place. The same holds for 'OutOfSteps': a term
-- cycling through the universe is no more a failure of the chain than a missing
-- atom is, and under '_partial' it deserves the same parked residual (#1078).
-- Outer frames see the '…At' signals and let them pass, since their chains are
-- prefixes of that one; a side-computation running on a chain of its own strips
-- the chain off again (see 'unparked') before the signal reaches the spine.
parking :: NonEmpty Rewritten -> IO a -> IO a
parking seq action = action `catch` rethrow
  where
    rethrow :: DataizeException -> IO a
    rethrow (Stuck func) = throwIO (StuckAt func seq)
    rethrow (OutOfSteps limit) = throwIO (OutOfStepsAt limit seq)
    rethrow failure = throwIO failure

-- Strip the derivation off a stuck atom escaping a side-computation that ran
-- on a chain of its own — an atom dataizing its input through '_dataize', or a
-- 'morph' premise through '_morph'. That chain is not the spine's, so it is
-- dropped and the spine frame around the side-computation attaches its own
-- (see 'parking').
unparked :: IO a -> IO a
unparked action = action `catch` rethrow
  where
    rethrow :: DataizeException -> IO a
    rethrow (StuckAt func _) = throwIO (Stuck func)
    rethrow (OutOfStepsAt limit _) = throwIO (OutOfSteps limit)
    rethrow failure = throwIO failure

-- The Morphing function 𝕄 maps normal forms to formations. It is ternary,
-- 𝕄(n, e, s): besides the term 'n' it takes the universe 'e' ('univ') — a plain
-- expression — and the mutable state 's', returning the morphed term together
-- with the new state. The universe is matched against the rule's 'e-match'
-- pattern (usually the '𝑒' meta, which binds 'e' so the 'universe' rule substitutes
-- it, but a rule may pin it to a literal such as 'mg' matching Φ). Its rules
-- come from 'morphing.yaml': the first matching rule's premises are evaluated and
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
morph' :: Morphed -> Expression -> State -> DataizeContext -> IO (Morphed, State)
morph' (expr, seq) univ state caller = do
  ctx <- deeper caller
  parking seq $ do
    rules <- if ctx._shuffle then shuffle Y.morphingRules else pure Y.morphingRules
    matched <- firstMatch ctx rules
    case matched of
      Just (rule, subst) -> reduce ctx rule subst
      Nothing -> throwIO (userError "no morphing rule matched")
  where
    firstMatch :: DataizeContext -> [Y.MorphRule] -> IO (Maybe (Y.MorphRule, Subst))
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
    reduce :: DataizeContext -> Y.MorphRule -> Subst -> IO (Morphed, State)
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
    sides :: DataizeContext -> [Y.Premise] -> Subst -> IO (Subst, State)
    sides ctx premises subst = foldM (sidePremise univ ctx) (subst, state) premises

-- Morph the expression located at '_locator' — 𝕄 asked on its own, the way
-- 'dataize' asks 𝔻. The whole input expression is itself the universe Φ (the 'e'
-- argument) threaded through 𝕄, so it is passed both as the located target and
-- as the universe; the default locator Q therefore morphs the top formation,
-- which 'mf' hands back unchanged, and '_locator' is how one aims 𝕄 at a
-- subterm. Unlike 𝔻, 𝕄 is total: it stops at the first formation it reaches
-- ('mf') and never demands bytes, and where no formation is reachable it answers
-- with the terminator ⊥ ('dead', 'xi', 'mg', 'mad', 'maad') rather than failing.
-- Only the atoms 'ml' fires can still get stuck, and '_partial' parks them just
-- as it does under 𝔻: the answer is then the residual subterm the spine had
-- reached, taken from '_locator' of its working expression. Stopping at the
-- first formation leaves everything that formation holds as it was written,
-- which is what '_deep' walks into before the answer is handed back (see
-- 'deepened').
morph :: Expression -> DataizeContext -> IO (Expression, [Rewritten])
morph universe ctx@DataizeContext{..} = do
  expr <- locatedExpression _locator universe
  result <- try (morph' (expr, (universe, Nothing) :| []) universe emptyState ctx)
  case result of
    Right ((morphed, seq), state) -> walked morphed seq state
    Left (StuckAt _ seq) | _partial -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked residue seq emptyState
    Left (OutOfStepsAt _ seq) | _partial -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked residue seq emptyState
    Left failure -> throwIO (failure :: DataizeException)
  where
    -- The answer 𝕄 reached, walked by '_deep' before it is handed back (see
    -- 'deepened'), and the chain that led to both. The walk joins the chain as
    -- one step named 'deep', so '--sequence' ends on the term the command
    -- prints. Morphing starts from the empty state and the state the walk ends
    -- on goes the way 𝕄's own goes: no caller consumes it yet.
    walked :: Expression -> NonEmpty Rewritten -> State -> IO (Expression, [Rewritten])
    walked morphed seq state
      | not _deep = pure (morphed, reverse (NE.toList seq))
      | otherwise = do
          (deep, _) <- deepened morphed universe state ctx
          seq' <- leadsTo seq "deep" deep ctx
          pure (deep, reverse (NE.toList seq'))

-- Walk what 𝕄 answered with, entering everything it left as it was written —
-- the mechanism behind '--deep' ('_deep'). 𝕄 navigates a term to the first
-- formation it reaches and 'mf' hands that formation back with its bindings
-- untouched, since firing a bare λ is 𝔻's business; 𝔻 in turn follows the one
-- path dataization demands and ends in bytes. A part of a program that nothing
-- demands — the argument of an atom that cannot fire, for one — is therefore
-- reduced by neither, and the object structure is lost to the one that does
-- reduce it (#1124). This walk demands nothing either. It asks 𝕄 about every
-- sub-expression and, where 𝕄 lands on a formation whose λ the registry
-- serves, fires it and asks 𝕄 about the answer again (see 'fired'). A
-- sub-expression on whose way an atom fired is replaced by the answer of the
-- last firing; where none fired it stays as it was written and only its own
-- parts are walked, so the calls the registry does not serve keep their names
-- and what comes back is still the same program, reduced as far as the
-- registry allows. Every entry is charged to the '--max-steps' budget, which
-- is what bounds the walk.
deepened :: Expression -> Expression -> State -> DataizeContext -> IO (Expression, State)
deepened expr univ = go ExXi expr
  where
    -- A term as it was written, together with what its free ξ stands for: the
    -- formation the walk entered it from, without the binding it came from,
    -- exactly the context the 'dot' rule hands a dispatched body. At the top
    -- there is no such formation, so ξ stands for itself and contextualization
    -- leaves the term alone.
    go :: Expression -> Expression -> State -> DataizeContext -> IO (Expression, State)
    go context term state' caller = do
      ctx' <- deeper caller
      (walked, walkedState) <- parts context term state' caller
      answer <- fired (contextualize walked context) univ walkedState ctx'
      maybe (pure (walked, walkedState)) pure answer
    -- The parts of a term nothing fired on, walked one by one and put back
    -- where they were, so the term keeps the shape it was written in.
    parts :: Expression -> Expression -> State -> DataizeContext -> IO (Expression, State)
    parts _ (ExFormation bds) state' caller = do
      (entered, state'') <- bindings bds bds state' caller
      pure (ExFormation entered, state'')
    parts context (ExDispatch target attr) state' caller = do
      (entered, state'') <- go context target state' caller
      pure (ExDispatch entered attr, state'')
    parts context (ExApplication target arg) state' caller = do
      (entered, state'') <- go context target state' caller
      (applied, state''') <- argument context arg state'' caller
      pure (ExApplication entered applied, state''')
    parts _ term state' _ = pure (term, state')
    -- Walk the bindings of a formation left to right, threading the state
    -- through them. Only what the formation itself holds is entered: ρ names
    -- the object around it rather than one inside it, and a void, Δ or λ
    -- binding carries no term to walk at all.
    bindings :: [Binding] -> [Binding] -> State -> DataizeContext -> IO ([Binding], State)
    bindings _ [] state' _ = pure ([], state')
    bindings whole (BiTau attr body : rest) state' caller
      | attr /= AtRho = do
          (entered, state'') <- go (scope attr whole) body state' caller
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
    argument :: Expression -> Argument -> State -> DataizeContext -> IO (Argument, State)
    argument context (ArTau attr arg) state' caller = do
      (entered, state'') <- go context arg state' caller
      pure (ArTau attr entered, state'')
    argument context (ArAlpha alpha arg) state' caller = do
      (entered, state'') <- go context arg state' caller
      pure (ArAlpha alpha entered, state'')

-- Ask 𝕄 about a term and fire the λ of the formation it reaches, as long as
-- the registry serves it, asking 𝕄 about every answer again: what comes back
-- is the answer of the last firing, or nothing at all where no atom fired. This
-- is the firing 'ml' makes without the dispatch that makes 'ml' make it — the
-- one 𝕄 leaves to 𝔻 — except in what it hands back: the atom's raw answer, not
-- the normal form 𝔼 makes of it, since the deep walk stands that answer back
-- into the program, where a normal form would spell the whole object out in
-- place of the name the program called it by. A λ the registry does not carry
-- is left alone rather than fired and got stuck on, so what phino cannot
-- compute stays as it was written with or without '_partial'; an atom that
-- cannot fire deeper on the spine still fails the run, exactly as it does
-- under 𝕄 alone, and '_partial' parks it. A formation still waiting for its
-- arguments is left alone too (see 'saturated').
fired :: Expression -> Expression -> State -> DataizeContext -> IO (Maybe (Expression, State))
fired term univ state caller = do
  ctx <- deeper caller
  morphed <- try (reduced ctx)
  case morphed of
    Right (ExFormation bds, state') -> maybe (pure Nothing) (evaluated ctx state') (saturated bds)
    Right _ -> pure Nothing
    Left failure -> parked failure
  where
    -- 𝕄 takes normal forms only and a term taken from the program as it was
    -- written is not necessarily one, so it is normalized against the universe
    -- first, exactly as '--inside' normalizes what it is handed. Both chains
    -- are dropped: the walk is not the spine and reports one step of its own
    -- (see 'morph'), so a stuck atom leaves without a derivation ('unparked').
    reduced :: DataizeContext -> IO (Expression, State)
    reduced ctx = unparked $ do
      (normal, _) <- normalized term ((univ, Nothing) :| []) ctx
      ((morphed, _), state') <- morph' (normal, (univ, Nothing) :| []) univ state ctx
      pure (morphed, state')
    -- Fire the λ of the formation 𝕄 reached and go on from its answer, keeping
    -- the answer of the last firing. The firing is reported to '_saveEval' like
    -- every other one, with the term the caller is given, so the protocol and
    -- the program agree on what the atom answered.
    evaluated :: DataizeContext -> State -> (T.Text, Expression) -> IO (Maybe (Expression, State))
    evaluated ctx state' (func, self) = case registeredAtom ctx._atoms func of
      Nothing -> pure Nothing
      Just registered -> do
        answer <- fireAtom func registered self univ (reduction univ ctx)
        ctx._saveEval (Evaluation func self (Just answer))
        again <- fired answer univ state' ctx
        pure (Just (fromMaybe (answer, state') again))
    parked :: DataizeException -> IO (Maybe a)
    parked (Stuck _) | caller._partial = pure Nothing
    parked failure = throwIO failure

-- Dataize the expression located at '_locator'. The whole input expression is
-- itself the universe Q (the 'e' argument) threaded through 𝔻 and 𝕄, so it is
-- passed both as the located target and as the universe. An atom that cannot
-- fire fails the run, unless '_partial' is on: dataization is then a partial
-- evaluation, and the run ends on the residual program the spine had reached
-- (see 'StuckAt'), with the stuck application parked in it as a normal-form
-- subterm, and the chain of steps that led there.
dataize :: Expression -> DataizeContext -> IO (Outcome, [Rewritten])
dataize universe ctx@DataizeContext{..} = do
  expr <- locatedExpression _locator universe
  -- Dataization starts from the empty state; the final state is not yet
  -- consumed by any caller, so it is discarded here.
  result <- try (dataize' (expr, (universe, Nothing) :| []) universe emptyState ctx)
  case result of
    Right ((bytes, seq), _state) -> pure (Dataized bytes, reverse seq)
    Left (StuckAt _ seq) | _partial -> pure (Residual (fst (NE.head seq)), reverse (NE.toList seq))
    Left (OutOfStepsAt _ seq) | _partial -> pure (Residual (fst (NE.head seq)), reverse (NE.toList seq))
    Left failure -> throwIO (failure :: DataizeException)

-- The Dataization function 𝔻 retrieves bytes from an expression. It is partial
-- and ternary, 𝔻(n, e, s): besides the term 'n' it takes the universe 'e' ('univ'),
-- which it forwards to 𝕄, and the mutable state 's', returning the bytes together
-- with the new state. Its rules come from 'dataization.yaml': 'delta' yields the
-- asset bytes and 'none' (a formation with no Δ/λ/φ) has nothing to dataize, so
-- it dataizes ⊥. The terminator ⊥ signals an error and lies outside 𝔻's domain,
-- so it matches no clause (there is no 'end' rule mapping it to empty bytes) and
-- dataization stops there; a data-less formation therefore fails through the
-- same path (see #955).
-- 'box' contextualizes the φ-body and keeps dataizing (its step is labelled by
-- its 'contextualize' side-computation), and 'norm' reduces through morphing,
-- splicing the morphing steps into the chain. The clauses are disjoint (see
-- #902, #905), so their declaration order must not be load-bearing; when
-- '_shuffle' is on (the '--shuffle' flag) the rules are shuffled before the
-- 'firstMatch' walk to exercise that invariant — mirroring normalization's
-- "apply until they stop matching". A genuinely order-independent step stays
-- deterministic; a hidden overlap surfaces as a nondeterministic failure rather
-- than staying silently green.
-- The conclusion bytes 'dresult' are produced by a trailing 'dataize' premise;
-- when its argument is bound by a 'morph' or 'normalize' premise, that step
-- joins the spine, otherwise the premise is an isolated side-computation.
dataize' :: Dataizable -> Expression -> State -> DataizeContext -> IO (Dataized, State)
dataize' (expr, seq) univ state caller = do
  ctx <- deeper caller
  parking seq $ do
    rules <- if ctx._shuffle then shuffle Y.dataizationRules else pure Y.dataizationRules
    matched <- firstMatch ctx rules
    case matched of
      Just (rule, subst) -> reduce ctx rule subst
      Nothing -> throwIO (userError (unmatched expr))
  where
    -- 𝔻 is partial: the terminator ⊥ signals an error and lies outside its
    -- domain (see #955), so it matches no clause and lands here. Name it in the
    -- message rather than reporting the generic "no dataization rule matched",
    -- which would otherwise hide that the computation reached a dead end.
    unmatched :: Expression -> String
    unmatched ExTermination = "dataization reached the terminator ⊥, which signals an error and cannot be dataized"
    unmatched _ = "no dataization rule matched"
    firstMatch :: DataizeContext -> [Y.DataizeRule] -> IO (Maybe (Y.DataizeRule, Subst))
    firstMatch _ [] = pure Nothing
    firstMatch ctx (rule : rest) = do
      substs <- matchExpressionWithRule' (matchExpression' rule.ematch univ) expr (asRule rule) (RuleContext (execBuildTerm univ ctx))
      case substs of
        (subst : _) -> pure (Just (rule, subst))
        [] -> firstMatch ctx rest
    asRule :: Y.DataizeRule -> Y.Rule
    asRule rule = Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing
    reduce :: DataizeContext -> Y.DataizeRule -> Subst -> IO (Dataized, State)
    reduce ctx rule subst = case bytesProducer rule.dresult rule.premises of
      Nothing -> do
        (final, state') <- sides ctx rule.premises subst
        bts <- buildBytesThrows rule.dresult final
        seq' <- leadsTo seq rule.name (ExBytes bts) ctx
        pure ((bts, NE.toList seq'), state')
      Just concl@(Y.Premise _ (Y.OpDataize arg)) -> case producer arg rule.premises of
        -- 𝔻(𝒩(e)) records the producing step (the 'box' contextualization),
        -- then normalizes its result back to a normal form before dataizing on,
        -- so 𝔻 only ever sees normal forms.
        Just normal@(Y.Premise _ (Y.OpNormalize inner)) -> do
          let side = rule.premises `excluding` [concl, normal]
          (final, state') <- sides ctx side subst
          built <- buildExpressionThrows inner final
          labelled <- leadsTo seq (labelOf side) built ctx
          (normal', seq') <- normalized built labelled ctx
          dataize' (normal', seq') univ state' ctx
        -- 𝔻(𝕄(e)) delegates to the morphing relation, splicing its steps into the
        -- chain before dataizing on.
        Just morphed@(Y.Premise _ (Y.OpMorph inner)) -> do
          (final, state') <- sides ctx (rule.premises `excluding` [concl, morphed]) subst
          built <- buildExpressionThrows inner final
          ((morphed', seq'), state'') <- morph' (built, seq) univ state' ctx
          dataize' (morphed', seq') univ state'' ctx
        -- The dataize argument is produced with no 'normalize'/'morph' spine to
        -- splice: 'fire' by its 'evaluate' side-computation (𝔼 now yields a
        -- normal form itself, so no follow-up 'normalize' is needed) and 'none'
        -- by handing the literal ⊥ straight to 𝔻. The transition is labelled by
        -- the side-computation ('evaluate') when there is one, else by the
        -- conclusion's own verb ('dataize' for 𝔻(⊥)).
        _ -> do
          let side = rule.premises `excluding` [concl]
          (final, state') <- sides ctx side subst
          built <- buildExpressionThrows arg final
          seq' <- leadsTo seq (labelOr (verb concl.operation) side) built ctx
          dataize' (built, seq') univ state' ctx
      Just _ -> throwIO (userError (printf "dataization rule '%s' must conclude with a 'dataize' premise" rule.name))
    sides :: DataizeContext -> [Y.Premise] -> Subst -> IO (Subst, State)
    sides ctx premises subst = foldM (sidePremise univ ctx) (subst, state) premises
    -- A spliced dataization step is labelled by its first side-computation —
    -- 'box' by its 'contextualize', 'fire' by its 'evaluate'; with none it is blank.
    labelOf :: [Y.Premise] -> String
    labelOf (premise : _) = verb premise.operation
    labelOf [] = ""
    -- As 'labelOf', but falls back to the given label when there is no
    -- side-computation to name the step (the 'none' rule's 𝔻(⊥) premise).
    labelOr :: String -> [Y.Premise] -> String
    labelOr _ premises@(_ : _) = labelOf premises
    labelOr fallback [] = fallback

-- The premise binding the given expression meta, if any. The conclusion of a
-- morphing rule and the argument of a continuation premise are looked up here to
-- find the premise that produces them.
producer :: Expression -> [Y.Premise] -> Maybe Y.Premise
producer (ExMeta name) = find (\premise -> premise.result == name)
producer _ = const Nothing

-- The premise binding the given bytes meta, if any — the dataization analogue of
-- 'producer' for a rule's bytes conclusion.
bytesProducer :: Bytes -> [Y.Premise] -> Maybe Y.Premise
bytesProducer (BtMeta name) = find (\premise -> premise.result == name)
bytesProducer _ = const Nothing

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
sidePremise :: Expression -> DataizeContext -> (Subst, State) -> Y.Premise -> IO (Subst, State)
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

leadsTo :: NonEmpty Rewritten -> String -> Expression -> DataizeContext -> IO (NonEmpty Rewritten)
leadsTo ((current, _) :| rest) rule expr DataizeContext{..} = do
  updated <- withLocatedExpression _locator expr current
  pure ((updated, Nothing) :| (current, Just rule) : rest)

-- Reduce 'expr' to its normal form through the normalization rewriter, embedding
-- it at '_locator' into the working expression taken from the head of the step
-- chain so the rewriter sees the surrounding context. Splices the individual
-- steps (alpha, copy, dot, …) into the chain and returns the normalized
-- expression together with the extended sequence.
normalized :: Expression -> NonEmpty Rewritten -> DataizeContext -> IO (Expression, NonEmpty Rewritten)
normalized expr seq ctx@DataizeContext{..} = do
  whole <- withLocatedExpression _locator expr (fst (NE.head seq))
  (rewrittens, _) <- rewrite whole normalizationRules (rewriteContext ctx)
  let (rw :| rws) = NE.reverse rewrittens
      seq' = rw :| rws <> NE.tail seq
  expr' <- locatedExpression _locator (fst rw)
  pure (expr', seq')
  where
    -- Switch the dataization context to a rewriting context for normalization,
    -- disabling the must-checker and breakpoints.
    rewriteContext :: DataizeContext -> RewriteContext
    rewriteContext DataizeContext{..} =
      RewriteContext _locator _maxDepth _maxCycles _depthSensitive _buildTerm MtDisabled Nothing _saveStep

-- Bind 'expr' to a synthetic attribute of the universe and reduce it to a
-- normal form there, handing back the extended universe together with the
-- locator that aims at the binding. This is the trick phino has always played
-- to reduce a sub-expression that is not part of the program — an atom's
-- operand, while the atoms still lived in the binary — and it is now the
-- contract of the '--inside' option, so an atom script asking phino to reduce
-- a part of the formation it was given does not have to splice it into the text
-- of the universe by hand. 𝔻 and 𝕄 accept normal forms only and an expression
-- handed in from outside is not necessarily one (a dispatch off a formation,
-- '⟦ x ↦ 6, ρ ↦ 5 ⟧.x', is not), so it is normalized against the extended
-- universe before either judgment sees it. The context comes back aimed at that
-- binding, so the caller hands the extended universe and the context it got
-- straight to 'dataize' or 'morph'.
insideUniverse :: Expression -> Expression -> DataizeContext -> IO (Expression, DataizeContext)
insideUniverse expr univ ctx@DataizeContext{_buildTerm = buildTerm} = case univ of
  ExFormation bds -> do
    (TeAttribute attr) <- buildTerm "random-tau" [] substEmpty
    let aiming = ctx{_locator = ExDispatch ExRoot attr}
        synthetic = ExFormation (BiTau attr expr : bds)
    (normal, _) <- normalized expr ((synthetic, Nothing) :| []) aiming
    pure (ExFormation (BiTau attr normal : bds), aiming)
  _ -> throwIO (userError "Can't reduce an expression inside a universe which is not a formation")

-- What phino answers a program that asks it to reduce a 𝜑-expression (see
-- 'ReduceFunc' in 'Atoms'): the expression is bound to a synthetic attribute
-- of the universe and dataized there, exactly the way the '--inside' option
-- does it, so the bytes come back as a Δ formation — or, where an atom on the
-- way could not fire and '_partial' parked it, the residual program instead.
-- An operand reaches a program unreduced, since reducing it may take the very
-- atom being fired, and before the channel carried questions the program had
-- no way to ask: it had to splice the operand into the text of the universe
-- and run a phino of its own on it (see #1160). The context is the one the
-- fire descended with, so the step budget of the run bounds the nesting.
reduction :: Expression -> DataizeContext -> ReduceFunc
reduction univ ctx expr = do
  (universe, aiming) <- insideUniverse expr univ ctx
  (outcome, _) <- dataize universe aiming
  pure (reduced outcome)
  where
    reduced :: Outcome -> Expression
    reduced (Dataized bytes) = ExFormation [BiDelta bytes]
    reduced (Residual residue) = residue

-- phino implements no λ function of its own. Which atoms exist is a property of
-- the object model being dataized, not of the calculus, so they come from the
-- '--atoms' registry and run as external scripts (see 'Atoms'). A name the
-- registry does not carry has no λ function to fire at all, and 𝔼 gets stuck on
-- it — the one behaviour left here. The script is handed the formation 'self'
-- (its λ binding already removed, so it may dispatch on it) and the universe
-- 'univ'; the state 𝑠 is not part of that contract yet, so it is threaded
-- through untouched.
atom :: T.Text -> Expression -> Expression -> State -> DataizeContext -> IO (Expression, State)
atom func self univ state ctx = case registeredAtom ctx._atoms func of
  Nothing -> throwIO (Stuck func)
  Just registered -> do
    raw <- fireAtom func registered self univ (reduction univ ctx)
    pure (raw, state)

-- Augment the injected, context-free term builder with the dataization and
-- morphing operations that need the universe: 'evaluate' applies an atom and
-- 'morph' morphs a sub-expression. 𝔼 ('evaluate') takes the universe as an
-- explicit second expression argument, while 𝕄 ('morph') is handed the threaded
-- 'univ'. Every other function is delegated unchanged. This is the matcher's
-- condition path (guards in 'when'/'having'), which has no state to thread, so 𝔼
-- and 𝕄 run here on a fresh, empty state whose result is discarded; the
-- state-threading callers in 'sidePremise' use '_evaluate' and '_morph' directly.
execBuildTerm :: Expression -> DataizeContext -> BuildTermFunc
execBuildTerm _ ctx "evaluate" = \args subst -> fst <$> _evaluate ctx emptyState args subst
execBuildTerm univ ctx "morph" = \args subst -> fst <$> _morph univ ctx emptyState args subst
execBuildTerm _ ctx func = _buildTerm ctx func

-- The Evaluation function 𝔼(b, e, s): it fires the λ atom of a formation 'b'
-- against the global universe 'e', under the incoming state 𝑠, normalizes the
-- atom's raw result 𝒩(e₁) = n, and returns that normal form together with the
-- new state. Normalizing here makes 𝔼's codomain 𝓝 (as its type demands), so
-- callers ('fire', 'ml') need no follow-up 'normalize' premise. The universe is
-- passed explicitly as the second argument (rather than threaded behind the
-- scenes), matching how the morphing 𝕄 and dataization 𝔻 functions carry it.
-- Every firing is reported to '_saveEval', which the '--evaluations' option
-- turns into one record per line. The reported result is the normal form 𝔼
-- returns, never the atom's raw answer, so the protocol and the caller see the
-- same term. Firings are reported in the order they complete, so the atom of a
-- head reduced by 'ml' is reported before the one dispatched on its result. A
-- firing that gets stuck is reported too, with no result, when the run is a
-- partial evaluation rather than a failure ('_partial'): the site is what the
-- caller wants to learn then. The report is made before the signal goes on to
-- the spine, where 'parking' attaches the derivation to it.
_evaluate :: DataizeContext -> State -> BuildTermMethodS
_evaluate ctx state [ArgExpression expr, ArgExpression universe] subst = do
  form <- buildExpressionThrows expr subst
  univ <- buildExpressionThrows universe subst
  case form of
    ExFormation bds -> case lambda bds of
      Just (func, args) -> do
        (raw, state') <- atom func args univ state ctx `catch` parked func args
        (normal, _) <- normalized raw ((univ, Nothing) :| []) ctx
        ctx._saveEval (Evaluation func args (Just normal))
        pure (TeExpression normal, state')
      Nothing -> throwIO (userError "Function evaluate() expects a formation with a λ binding")
    _ -> throwIO (userError "Function evaluate() expects a formation")
  where
    parked :: T.Text -> Expression -> DataizeException -> IO a
    parked func args failure@(Stuck _) = do
      when ctx._partial (ctx._saveEval (Evaluation func args Nothing))
      throwIO failure
    parked _ _ failure = throwIO failure
_evaluate _ _ _ _ = throwIO (userError "Function evaluate() requires exactly 2 expression arguments")

-- The Morphing function 𝕄 exposed as a build-term function so a rule can morph
-- a sub-expression in its 'where' (the 'md' and 'ma' rules morph
-- the head before re-attaching it). The step chain is discarded: the producing
-- rule splices the surrounding normalization steps itself, and a stuck atom met
-- on the way leaves without it (see 'unparked'). The state is threaded through
-- and the new state returned alongside the morphed term.
_morph :: Expression -> DataizeContext -> State -> BuildTermMethodS
_morph univ ctx state [ArgExpression expr] subst = unparked $ do
  built <- buildExpressionThrows expr subst
  ((morphed, _), state') <- morph' (built, (univ, Nothing) :| []) univ state ctx
  pure (TeExpression morphed, state')
_morph _ _ _ _ _ = throwIO (userError "Function morph() requires exactly 1 expression argument")
