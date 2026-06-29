{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Dataize (morph, dataize, dataize', DataizeContext (..), State, emptyState, execBuildTerm) where

import AST
import Builder (buildBytesThrows, buildExpressionThrows)
import Bytes (btsToNum, numToBts)
import Control.Exception (throwIO)
import Control.Monad (foldM)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Deps (BuildTermFunc, BuildTermMethodS, SaveStepFunc, State, Term (..))
import Locator (locatedExpression, withLocatedExpression)
import Matcher (MetaValue (..), Subst (..), combine, matchExpression', substEmpty, substSingle)
import Misc
import Must (Must (..))
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

-- The evaluation context carries only configuration. Nothing global is fixed
-- here: the universe (the second argument 'e' of 𝕄(n, e, s) and 𝔻(n, e, s)) is a
-- plain expression threaded as an argument to 'dataize'', 'morph' and on to the
-- atoms, and the state 's' is threaded the same way (see 'State'). The working
-- program needed for normalization is taken from the head of the step chain, so
-- no 'Program' is threaded around.
data DataizeContext = DataizeContext
  { _locator :: Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _depthSensitive :: Bool
  , _shuffle :: Bool
  , _buildTerm :: BuildTermFunc
  , _saveStep :: SaveStepFunc
  }

-- Resolve formation for LAMBDA Morphing rule.
-- If formation contains λ binding, the called atom result is returned. The
-- universe 'univ' is forwarded to the atom.
formation :: [Binding] -> Expression -> State -> DataizeContext -> IO (Maybe (Expression, State))
formation bds univ state ctx = do
  let (lambda, bds') = maybeLambda bds
  case lambda of
    Just (BiLambda (Function func)) -> Just <$> atom func (ExFormation bds') univ state ctx
    _ -> pure Nothing
  where
    maybeLambda :: [Binding] -> (Maybe Binding, [Binding])
    maybeLambda = maybeBinding (\case BiLambda _ -> True; _ -> False)
    maybeBinding :: (Binding -> Bool) -> [Binding] -> (Maybe Binding, [Binding])
    maybeBinding _ [] = (Nothing, [])
    maybeBinding func bds =
      let (found, rest) = partition func bds
       in case found of
            [bd] -> (Just bd, rest)
            _ -> (Nothing, bds)

-- The Morphing function 𝕄 maps normal forms to formations. It is ternary,
-- 𝕄(n, e, s): besides the term 'n' it takes the universe 'e' ('univ') — a plain
-- expression — and the mutable state 's', returning the morphed term together
-- with the new state. The universe is matched against the rule's 'e-match'
-- pattern (usually the '𝑒' meta, which binds 'e' so the 'root' rule substitutes
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
morph :: Morphed -> Expression -> State -> DataizeContext -> IO (Morphed, State)
morph (expr, seq) univ state ctx = do
  rules <- if ctx._shuffle then shuffle Y.morphingRules else pure Y.morphingRules
  matched <- firstMatch rules
  case matched of
    Just (rule, subst) -> reduce rule subst
    Nothing -> throwIO (userError "no morphing rule matched")
  where
    firstMatch :: [Y.MorphRule] -> IO (Maybe (Y.MorphRule, Subst))
    firstMatch [] = pure Nothing
    firstMatch (rule : rest) = do
      substs <- matchExpressionWithRule' (matchExpression' rule.ematch univ) expr (asRule rule) (RuleContext (execBuildTerm univ ctx))
      case substs of
        (subst : _) -> pure (Just (rule, subst))
        [] -> firstMatch rest
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
    reduce :: Y.MorphRule -> Subst -> IO (Morphed, State)
    reduce rule subst = case producer rule.nresult rule.premises of
      Nothing -> do
        (final, state') <- sides rule.premises subst
        built <- buildExpressionThrows rule.nresult final
        seq' <- leadsTo seq rule.name built ctx
        pure ((built, seq'), state')
      Just concl@(Y.Premise _ (Y.OpMorph arg)) -> case producer arg rule.premises of
        Just normal@(Y.Premise _ (Y.OpNormalize inner)) -> do
          (final, state') <- sides (rule.premises `excluding` [concl, normal]) subst
          built <- buildExpressionThrows inner final
          labelled <- leadsTo seq rule.name built ctx
          (normal', seq') <- normalized built labelled ctx
          morph (normal', seq') univ state' ctx
        _ -> do
          (final, state') <- sides (rule.premises `excluding` [concl]) subst
          built <- buildExpressionThrows arg final
          seq' <- leadsTo seq rule.name built ctx
          morph (built, seq') univ state' ctx
      Just _ -> throwIO (userError (printf "morphing rule '%s' must conclude with a 'morph' premise" rule.name))
    sides :: [Y.Premise] -> Subst -> IO (Subst, State)
    sides premises subst = foldM (sidePremise univ ctx) (subst, state) premises

-- Dataize the program located at '_locator'. The program's root is the universe
-- (the 'e' argument) passed to 𝔻 and 𝕄.
dataize :: Program -> DataizeContext -> IO Dataized
dataize program@(Program univ) ctx@DataizeContext{..} = do
  expr <- locatedExpression _locator program
  -- Dataization starts from the empty state; the final state is not yet
  -- consumed by any caller, so it is discarded here.
  ((bytes, seq), _state) <- dataize' (expr, (program, Nothing) :| []) univ emptyState ctx
  pure (bytes, reverse seq)

-- The Dataization function 𝔻 retrieves bytes from an expression. It is total and
-- ternary, 𝔻(n, e, s): besides the term 'n' it takes the universe 'e' ('univ'),
-- which it forwards to 𝕄, and the mutable state 's', returning the bytes together
-- with the new state. Its rules come from 'dataization.yaml': 'delta' yields the
-- asset bytes, 'none' (a formation) and 'end' (⊥) yield empty bytes (--),
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
dataize' (expr, seq) univ state ctx = do
  rules <- if ctx._shuffle then shuffle Y.dataizationRules else pure Y.dataizationRules
  matched <- firstMatch rules
  case matched of
    Just (rule, subst) -> reduce rule subst
    Nothing -> throwIO (userError "no dataization rule matched")
  where
    firstMatch :: [Y.DataizeRule] -> IO (Maybe (Y.DataizeRule, Subst))
    firstMatch [] = pure Nothing
    firstMatch (rule : rest) = do
      substs <- matchExpressionWithRule' (matchExpression' rule.ematch univ) expr (asRule rule) (RuleContext (execBuildTerm univ ctx))
      case substs of
        (subst : _) -> pure (Just (rule, subst))
        [] -> firstMatch rest
    asRule :: Y.DataizeRule -> Y.Rule
    asRule rule = Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing
    reduce :: Y.DataizeRule -> Subst -> IO (Dataized, State)
    reduce rule subst = case bytesProducer rule.dresult rule.premises of
      Nothing -> do
        (final, state') <- sides rule.premises subst
        bts <- buildBytesThrows rule.dresult final
        pure ((bts, NE.toList seq), state')
      Just concl@(Y.Premise _ (Y.OpDataize arg)) -> case producer arg rule.premises of
        -- 𝔻(𝒩(e)) records the producing step (the 'box' contextualization or the
        -- 'fire' λ-application), then normalizes its result back to a normal form
        -- before dataizing on, so 𝔻 only ever sees normal forms.
        Just normal@(Y.Premise _ (Y.OpNormalize inner)) -> do
          let side = rule.premises `excluding` [concl, normal]
          (final, state') <- sides side subst
          built <- buildExpressionThrows inner final
          labelled <- leadsTo seq (labelOf side) built ctx
          (normal', seq') <- normalized built labelled ctx
          dataize' (normal', seq') univ state' ctx
        -- 𝔻(𝕄(e)) delegates to the morphing relation, splicing its steps into the
        -- chain before dataizing on.
        Just morphed@(Y.Premise _ (Y.OpMorph inner)) -> do
          (final, state') <- sides (rule.premises `excluding` [concl, morphed]) subst
          built <- buildExpressionThrows inner final
          ((morphed', seq'), state'') <- morph (built, seq) univ state' ctx
          dataize' (morphed', seq') univ state'' ctx
        _ -> do
          let side = rule.premises `excluding` [concl]
          (final, state') <- sides side subst
          built <- buildExpressionThrows arg final
          seq' <- leadsTo seq (labelOf side) built ctx
          dataize' (built, seq') univ state' ctx
      Just _ -> throwIO (userError (printf "dataization rule '%s' must conclude with a 'dataize' premise" rule.name))
    sides :: [Y.Premise] -> Subst -> IO (Subst, State)
    sides premises subst = foldM (sidePremise univ ctx) (subst, state) premises
    -- A spliced dataization step is labelled by its first side-computation —
    -- 'box' by its 'contextualize', 'fire' by its 'evaluate'; with none it is blank.
    labelOf :: [Y.Premise] -> String
    labelOf (premise : _) = verb premise.operation
    labelOf [] = ""

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
leadsTo ((prog, _) :| rest) rule expr DataizeContext{..} = do
  prog' <- withLocatedExpression _locator expr prog
  pure ((prog', Nothing) :| (prog, Just rule) : rest)

-- Reduce 'expr' to its normal form through the normalization rewriter, embedding
-- it at '_locator' into the working program taken from the head of the step
-- chain so the rewriter sees the surrounding context. Splices the individual
-- steps (alpha, copy, dot, …) into the chain and returns the normalized
-- expression together with the extended sequence.
normalized :: Expression -> NonEmpty Rewritten -> DataizeContext -> IO (Expression, NonEmpty Rewritten)
normalized expr seq ctx@DataizeContext{..} = do
  prog' <- withLocatedExpression _locator expr (fst (NE.head seq))
  (rewrittens, _) <- rewrite prog' normalizationRules (rewriteContext ctx)
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

-- Synthetic dataize function for internal usage inside atoms. Here we modify the
-- universe by adding a new binding which refers to the expression we want to
-- dataize, building a local working program to reduce within. As a caller of 𝔻,
-- it first reduces the expression to a normal form, since 𝔻 only accepts normal
-- forms. The universe 'univ' itself is forwarded unchanged, so morphing Φ under
-- this context still resolves to the true universe rather than to this
-- synthetic, binding-prepended formation.
_dataize :: Expression -> Expression -> State -> DataizeContext -> IO (Bytes, State)
_dataize expr univ state ctx@DataizeContext{_buildTerm = buildTerm} = case univ of
  ExFormation bds -> do
    (TeAttribute attr) <- buildTerm "random-tau" [] substEmpty
    let prog = Program (ExFormation (BiTau attr expr : bds))
    (normal, seq) <- normalized expr ((prog, Nothing) :| []) ctx
    ((bts, _), state') <- dataize' (normal, seq) univ state ctx
    pure (bts, state')
  _ -> throwIO (userError "Can't call _dataize from atoms with non-formation universe")

-- A number atom only operates on numeric data. Empty bytes (the result of
-- dataizing a bare formation ⟦𝐵⟧ or ⊥ now that 𝔻 is total) carry no number,
-- so the operand is rejected and the atom yields ⊥.
asNumber :: Bytes -> Maybe Double
asNumber BtEmpty = Nothing
asNumber bts = Just (either toDouble id (btsToNum bts))

atom :: T.Text -> Expression -> Expression -> State -> DataizeContext -> IO (Expression, State)
atom "L_number_plus" self univ state ctx = do
  (left, lstate) <- _dataize (ExDispatch self (AtLabel "x")) univ state ctx
  (right, rstate) <- _dataize (ExDispatch self AtRho) univ lstate ctx
  case (asNumber left, asNumber right) of
    (Just first, Just second) -> pure (DataNumber (numToBts (first + second)), rstate)
    _ -> pure (ExTermination, rstate)
atom "L_number_times" self univ state ctx = do
  (left, lstate) <- _dataize (ExDispatch self (AtLabel "x")) univ state ctx
  (right, rstate) <- _dataize (ExDispatch self AtRho) univ lstate ctx
  case (asNumber left, asNumber right) of
    (Just first, Just second) -> pure (DataNumber (numToBts (first * second)), rstate)
    _ -> pure (ExTermination, rstate)
atom "L_number_eq" self univ state ctx = do
  (x, lstate) <- _dataize (ExDispatch self (AtLabel "x")) univ state ctx
  (rho, rstate) <- _dataize (ExDispatch self AtRho) univ lstate ctx
  case (asNumber x, asNumber rho) of
    (Just first, Just self') ->
      if self' == first
        then pure (DataNumber (numToBts first), rstate)
        else pure (ExDispatch self (AtLabel "y"), rstate)
    _ -> pure (ExTermination, rstate)
atom func _ _ _ _ = throwIO (userError (printf "Atom '%s' does not exist" (T.unpack func)))

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
-- against the global universe 'e', under the incoming state 𝑠, and returns the
-- atom's result together with the new state. The universe is now passed
-- explicitly as the second argument (rather than threaded behind the scenes),
-- matching how the morphing 𝕄 and dataization 𝔻 functions carry it.
_evaluate :: DataizeContext -> State -> BuildTermMethodS
_evaluate ctx state [ArgExpression expr, ArgExpression universe] subst = do
  form <- buildExpressionThrows expr subst
  univ <- buildExpressionThrows universe subst
  case form of
    ExFormation bds -> do
      resolved <- formation bds univ state ctx
      case resolved of
        Just (obj, state') -> pure (TeExpression obj, state')
        Nothing -> throwIO (userError "Function evaluate() expects a formation with a λ binding")
    _ -> throwIO (userError "Function evaluate() expects a formation")
_evaluate _ _ _ _ = throwIO (userError "Function evaluate() requires exactly 2 expression arguments")

-- The Morphing function 𝕄 exposed as a build-term function so a rule can morph
-- a sub-expression in its 'where' (the 'md' and 'ma' rules morph
-- the head before re-attaching it). The step chain is discarded: the producing
-- rule splices the surrounding normalization steps itself. The state is threaded
-- through and the new state returned alongside the morphed term.
_morph :: Expression -> DataizeContext -> State -> BuildTermMethodS
_morph univ ctx state [ArgExpression expr] subst = do
  built <- buildExpressionThrows expr subst
  ((morphed, _), state') <- morph (built, (Program univ, Nothing) :| []) univ state ctx
  pure (TeExpression morphed, state')
_morph _ _ _ _ _ = throwIO (userError "Function morph() requires exactly 1 expression argument")
