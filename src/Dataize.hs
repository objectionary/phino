{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Dataize (morph, dataize, dataize', DataizeContext (..), execBuildTerm) where

import AST
import Builder (buildBytesThrows, buildExpressionThrows)
import Control.Exception (throwIO)
import Control.Monad (foldM)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Deps (BuildTermFunc, BuildTermMethod, SaveStepFunc, Term (..))
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

-- The evaluation context carries only configuration. Nothing global is fixed
-- here: the universe (the second argument 'e' of 𝕄(n, e) and 𝔻(n, e)) is a plain
-- expression threaded as an argument to 'dataize'', 'morph' and on to the atoms.
-- The working program needed for normalization is taken from the head of the
-- step chain, so no 'Program' is threaded around.
data DataizeContext = DataizeContext
  { _locator :: Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _depthSensitive :: Bool
  , _buildTerm :: BuildTermFunc
  , _saveStep :: SaveStepFunc
  }

-- Resolve formation for LAMBDA Morphing rule.
-- If formation contains λ binding, the called atom result is returned. The
-- universe 'univ' is forwarded to the atom.
formation :: [Binding] -> Expression -> DataizeContext -> IO (Maybe Expression)
formation bds univ ctx = do
  let (lambda, bds') = maybeLambda bds
  case lambda of
    Just (BiLambda (Function func)) -> Just <$> atom func (ExFormation bds') univ ctx
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

-- The Morphing function 𝕄 maps normal forms to formations. It is binary,
-- 𝕄(n, e): besides the term 'n' it takes the universe 'e' ('univ') — a plain
-- expression — and matches it against the rule's 'e-match' pattern (always the
-- '𝑒' meta), which binds 'e' so the 'root' rule substitutes it. It is driven by
-- the ordered rules from 'morphing.yaml': the first matching rule's premises are
-- evaluated and its conclusion 'nresult' is built, always forwarding the same
-- universe. The 'morph' premise that produces the conclusion is the spine: when
-- its argument comes from a 'normalize' premise, the rewriter runs over that
-- argument and its individual steps (alpha, copy, dot, …) are spliced into the
-- chain before morphing continues. Every other premise is a side-computation
-- evaluated in isolation by 'sidePremise', its own steps discarded.
morph :: Morphed -> Expression -> DataizeContext -> IO Morphed
morph (expr, seq) univ ctx = do
  matched <- firstMatch Y.morphingRules
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
    reduce :: Y.MorphRule -> Subst -> IO Morphed
    reduce rule subst = case producer rule.nresult rule.premises of
      Nothing -> do
        final <- sides rule.premises subst
        built <- buildExpressionThrows rule.nresult final
        seq' <- leadsTo seq rule.name built ctx
        pure (built, seq')
      Just concl@(Y.Premise _ (Y.OpMorph arg)) -> case producer arg rule.premises of
        Just normal@(Y.Premise _ (Y.OpNormalize inner)) -> do
          final <- sides (rule.premises `excluding` [concl, normal]) subst
          built <- buildExpressionThrows inner final
          labelled <- leadsTo seq rule.name built ctx
          (normal', seq') <- normalized built labelled ctx
          morph (normal', seq') univ ctx
        _ -> do
          final <- sides (rule.premises `excluding` [concl]) subst
          built <- buildExpressionThrows arg final
          seq' <- leadsTo seq rule.name built ctx
          morph (built, seq') univ ctx
      Just _ -> throwIO (userError (printf "morphing rule '%s' must conclude with a 'morph' premise" rule.name))
    sides :: [Y.Premise] -> Subst -> IO Subst
    sides premises subst = foldM (sidePremise univ ctx) subst premises

-- Dataize the program located at '_locator'. The program's root is the universe
-- (the 'e' argument) passed to 𝔻 and 𝕄.
dataize :: Program -> DataizeContext -> IO Dataized
dataize program@(Program univ) ctx@DataizeContext{..} = do
  expr <- locatedExpression _locator program
  (bytes, seq) <- dataize' (expr, (program, Nothing) :| []) univ ctx
  pure (bytes, reverse seq)

-- The Dataization function 𝔻 retrieves bytes from an expression. It is total and
-- binary, 𝔻(n, e): besides the term 'n' it takes the universe 'e' ('univ'),
-- which it forwards to 𝕄. It is driven by the ordered rules from
-- 'dataization.yaml': 'delta' yields the asset bytes, 'none' (a formation) and
-- 'bott' (⊥) yield empty bytes (--), 'box' contextualizes the φ-body and keeps
-- dataizing (its step is labelled by its 'contextualize' side-computation), and
-- 'norm' reduces through morphing, splicing the morphing steps into the chain.
-- The conclusion bytes 'dresult' are produced by a trailing 'dataize' premise;
-- when its argument is bound by a 'morph' or 'normalize' premise, that step
-- joins the spine, otherwise the premise is an isolated side-computation.
dataize' :: Dataizable -> Expression -> DataizeContext -> IO Dataized
dataize' (expr, seq) univ ctx = do
  matched <- firstMatch Y.dataizationRules
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
    reduce :: Y.DataizeRule -> Subst -> IO Dataized
    reduce rule subst = case bytesProducer rule.dresult rule.premises of
      Nothing -> do
        final <- sides rule.premises subst
        bts <- buildBytesThrows rule.dresult final
        pure (bts, NE.toList seq)
      Just concl@(Y.Premise _ (Y.OpDataize arg)) -> case producer arg rule.premises of
        -- 𝔻(𝒩(e)) records the producing step (the 'box' contextualization or the
        -- 'fire' λ-application), then normalizes its result back to a normal form
        -- before dataizing on, so 𝔻 only ever sees normal forms.
        Just normal@(Y.Premise _ (Y.OpNormalize inner)) -> do
          let side = rule.premises `excluding` [concl, normal]
          final <- sides side subst
          built <- buildExpressionThrows inner final
          labelled <- leadsTo seq (labelOf side) built ctx
          (normal', seq') <- normalized built labelled ctx
          dataize' (normal', seq') univ ctx
        -- 𝔻(𝕄(e)) delegates to the morphing relation, splicing its steps into the
        -- chain before dataizing on.
        Just morphed@(Y.Premise _ (Y.OpMorph inner)) -> do
          final <- sides (rule.premises `excluding` [concl, morphed]) subst
          built <- buildExpressionThrows inner final
          (morphed', seq') <- morph (built, seq) univ ctx
          dataize' (morphed', seq') univ ctx
        _ -> do
          let side = rule.premises `excluding` [concl]
          final <- sides side subst
          built <- buildExpressionThrows arg final
          seq' <- leadsTo seq (labelOf side) built ctx
          dataize' (built, seq') univ ctx
      Just _ -> throwIO (userError (printf "dataization rule '%s' must conclude with a 'dataize' premise" rule.name))
    sides :: [Y.Premise] -> Subst -> IO Subst
    sides premises subst = foldM (sidePremise univ ctx) subst premises
    -- A spliced dataization step is labelled by its first side-computation —
    -- 'box' by its 'contextualize', 'fire' by its 'lambda'; with none it is blank.
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

-- Evaluate one side-computation premise — a 'morph', 'lambda' or 'contextualize'
-- of an earlier term — in isolation, binding its result meta. These never splice
-- steps into the trace: 'morph' and 'lambda' reduce on a fresh chain and discard
-- it, 'contextualize' is pure.
sidePremise :: Expression -> DataizeContext -> Subst -> Y.Premise -> IO Subst
sidePremise univ ctx subst premise = do
  term <- execBuildTerm univ ctx (verb premise.operation) (verbArgs premise.operation) subst
  case combine (substSingle premise.result (metaValue term)) subst of
    Just subst' -> pure subst'
    Nothing -> throwIO (userError (printf "premise meta '%s' clashes with an existing binding" (T.unpack premise.result)))
  where
    metaValue :: Term -> MetaValue
    metaValue (TeExpression value) = MvExpression value
    metaValue (TeAttribute value) = MvAttribute value
    metaValue (TeBytes value) = MvBytes value
    metaValue (TeBindings value) = MvBindings value

-- The build-term function name backing a premise operation.
verb :: Y.Operation -> String
verb (Y.OpMorph _) = "morph"
verb (Y.OpNormalize _) = "normalize"
verb (Y.OpLambda _) = "lambda"
verb (Y.OpContextualize _ _) = "contextualize"
verb (Y.OpDataize _) = "dataize"

-- The build-term arguments backing a premise operation.
verbArgs :: Y.Operation -> [ExtraArgument]
verbArgs (Y.OpMorph expr) = [ArgExpression expr]
verbArgs (Y.OpNormalize expr) = [ArgExpression expr]
verbArgs (Y.OpLambda expr) = [ArgExpression expr]
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
_dataize :: Expression -> Expression -> DataizeContext -> IO Bytes
_dataize expr univ ctx@DataizeContext{_buildTerm = buildTerm} = case univ of
  ExFormation bds -> do
    (TeAttribute attr) <- buildTerm "random-tau" [] substEmpty
    let prog = Program (ExFormation (BiTau attr expr : bds))
    (normal, seq) <- normalized expr ((prog, Nothing) :| []) ctx
    (bts, _) <- dataize' (normal, seq) univ ctx
    pure bts
  _ -> throwIO (userError "Can't call _dataize from atoms with non-formation universe")

-- A number atom only operates on numeric data. Empty bytes (the result of
-- dataizing a bare formation ⟦𝐵⟧ or ⊥ now that 𝔻 is total) carry no number,
-- so the operand is rejected and the atom yields ⊥.
asNumber :: Bytes -> Maybe Double
asNumber BtEmpty = Nothing
asNumber bts = Just (either toDouble id (btsToNum bts))

atom :: T.Text -> Expression -> Expression -> DataizeContext -> IO Expression
atom "L_number_plus" self univ ctx = do
  left <- _dataize (ExDispatch self (AtLabel "x")) univ ctx
  right <- _dataize (ExDispatch self AtRho) univ ctx
  case (asNumber left, asNumber right) of
    (Just first, Just second) -> pure (DataNumber (numToBts (first + second)))
    _ -> pure ExTermination
atom "L_number_times" self univ ctx = do
  left <- _dataize (ExDispatch self (AtLabel "x")) univ ctx
  right <- _dataize (ExDispatch self AtRho) univ ctx
  case (asNumber left, asNumber right) of
    (Just first, Just second) -> pure (DataNumber (numToBts (first * second)))
    _ -> pure ExTermination
atom "L_number_eq" self univ ctx = do
  x <- _dataize (ExDispatch self (AtLabel "x")) univ ctx
  rho <- _dataize (ExDispatch self AtRho) univ ctx
  case (asNumber x, asNumber rho) of
    (Just first, Just self') ->
      if self' == first
        then pure (DataNumber (numToBts first))
        else pure (ExDispatch self (AtLabel "y"))
    _ -> pure ExTermination
atom func _ _ _ = throwIO (userError (printf "Atom '%s' does not exist" (T.unpack func)))

-- Augment the injected, context-free term builder with the dataization and
-- morphing operations that need the universe: 'lambda' applies an atom and
-- 'morph' morphs a sub-expression. Both receive the universe 'univ'. Every other
-- function is delegated unchanged.
execBuildTerm :: Expression -> DataizeContext -> BuildTermFunc
execBuildTerm univ ctx "lambda" = _lambda univ ctx
execBuildTerm univ ctx "morph" = _morph univ ctx
execBuildTerm _ ctx func = _buildTerm ctx func

_lambda :: Expression -> DataizeContext -> BuildTermMethod
_lambda univ ctx [ArgExpression expr] subst = do
  form <- buildExpressionThrows expr subst
  case form of
    ExFormation bds -> do
      resolved <- formation bds univ ctx
      case resolved of
        Just obj -> pure (TeExpression obj)
        Nothing -> throwIO (userError "Function lambda() expects a formation with a λ binding")
    _ -> throwIO (userError "Function lambda() expects a formation")
_lambda _ _ _ _ = throwIO (userError "Function lambda() requires exactly 1 expression argument")

-- The Morphing function 𝕄 exposed as a build-term function so a rule can morph
-- a sub-expression in its 'where' (the 'dispatch' and 'application' rules morph
-- the head before re-attaching it). The step chain is discarded: the producing
-- rule splices the surrounding normalization steps itself.
_morph :: Expression -> DataizeContext -> BuildTermMethod
_morph univ ctx [ArgExpression expr] subst = do
  built <- buildExpressionThrows expr subst
  (morphed, _) <- morph (built, (Program univ, Nothing) :| []) univ ctx
  pure (TeExpression morphed)
_morph _ _ _ _ = throwIO (userError "Function morph() requires exactly 1 expression argument")
