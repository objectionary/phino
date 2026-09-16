{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The Dataization function 𝔻 and what a program asking phino to reduce one of
-- its own terms gets back. Everything 𝔻 shares with the Morphing function 𝕄 —
-- the context, the budget, the signals, the premise plumbing — lives in
-- 'Morph', which this module imports.
module Dataize (dataize, dataize', reduction, Outcome (..)) where

import AST
import Builder (buildBytesThrows, buildExpressionThrows)
import Control.Exception (throwIO, try)
import Control.Monad (foldM)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Deps (State)
import Locator (locatedExpression)
import Matcher (Subst, matchExpression')
import Morph (Morphed, ReduceContext (..), ReduceException (..), ReductionFunc, deeper, excluding, execBuildTerm, insideUniverse, leadsTo, morph', normalized, parking, producer, sidePremise, verb)
import Random (shuffle)
import Rewriter (Rewritten)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Text.Printf (printf)
import qualified Yaml as Y

type Dataized = (Bytes, [Rewritten])

-- What 𝔻 is handed: a term plus the derivation that reached it, the same pair
-- 𝕄 works on (see 'Morphed').
type Dataizable = Morphed

-- What a run of 𝔻 ends with: the bytes it reached or, under '_partial', the
-- residual program: what the known inputs decided is computed, the stuck λ
-- and everything depending on it survive in place.
data Outcome
  = Dataized Bytes
  | Residual Expression
  deriving stock (Eq, Show)

-- Dataize the expression located at '_locator'. The whole input expression is
-- itself the universe Q (the 'e' argument) threaded through 𝔻 and 𝕄, so it is
-- passed both as the located target and as the universe. A λ function that
-- cannot fire fails the run, unless '_partial' is on: dataization is then a
-- partial evaluation, and the run ends on the residual program the spine had
-- reached (see 'StuckAt'), with the stuck application parked in it as a
-- normal-form subterm, and the chain of steps that led there. The state 𝑠 goes
-- in and comes back out, so a 𝔻 asked inside another judgment goes on minting
-- symbols where that judgment left off.
dataize :: Expression -> State -> ReduceContext -> IO (Outcome, [Rewritten], State)
dataize universe state ctx@ReduceContext{..} = do
  expr <- locatedExpression _locator universe
  result <- try (dataize' (expr, (universe, Nothing) :| []) universe state ctx)
  case result of
    Right ((bytes, seq), state') -> pure (Dataized bytes, reverse seq, state')
    Left (StuckAt _ seq parked) | _partial -> pure (Residual (fst (NE.head seq)), reverse (NE.toList seq), parked)
    Left (OutOfStepsAt _ seq parked) | _partial -> pure (Residual (fst (NE.head seq)), reverse (NE.toList seq), parked)
    Left failure -> throwIO (failure :: ReduceException)

-- The Dataization function 𝔻 retrieves bytes from an expression. It is partial
-- and ternary, 𝔻(n, e, s): besides the term 'n' it takes the universe 'e' ('univ'),
-- which it forwards to 𝕄, and the mutable state 's', returning the bytes together
-- with the new state. Its rules come from 'resources/dataization': 'delta' yields the
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
dataize' :: Dataizable -> Expression -> State -> ReduceContext -> IO (Dataized, State)
dataize' (expr, seq) univ state caller = do
  ctx <- deeper caller
  parking seq state $ do
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
    firstMatch :: ReduceContext -> [Y.DataizeRule] -> IO (Maybe (Y.DataizeRule, Subst))
    firstMatch _ [] = pure Nothing
    firstMatch ctx (rule : rest) = do
      substs <- matchExpressionWithRule' (matchExpression' rule.ematch univ) expr (asRule rule) (RuleContext (execBuildTerm univ ctx))
      case substs of
        (subst : _) -> pure (Just (rule, subst))
        [] -> firstMatch ctx rest
    asRule :: Y.DataizeRule -> Y.Rule
    asRule rule = Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing
    reduce :: ReduceContext -> Y.DataizeRule -> Subst -> IO (Dataized, State)
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
    sides :: ReduceContext -> [Y.Premise] -> Subst -> IO (Subst, State)
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

-- The premise binding the given bytes meta, if any — the dataization analogue of
-- 'producer' for a rule's bytes conclusion.
bytesProducer :: Bytes -> [Y.Premise] -> Maybe Y.Premise
bytesProducer (BtMeta name) = find (\premise -> premise.result == name)
bytesProducer _ = const Nothing

-- What a 'dataize' premise of a λ function reduces one of its operands with
-- (see 'ReductionFunc' in 'Morph'): the operand is bound to a synthetic
-- attribute of the universe and dataized there, exactly the way the '--inside'
-- option does it, so what comes back is the data the operand carries. Where a
-- λ function on the way could not fire and '_partial' parked it, the operand
-- never came down to data at all and the premise asking for it is left
-- undischarged, which is the entry declining to answer rather than a failure
-- of the run. An operand reaches a firing unreduced, since reducing it may
-- take the very λ function being fired, so it is reduced here, on demand, and
-- not before. The context is the one the fire descended with, so the step
-- budget of the run bounds the nesting, and the state 𝑠 goes in and comes back
-- out, so the symbols this reduction mints are counted in the same sequence as
-- the ones around it.
reduction :: ReductionFunc
reduction univ ctx expr state = do
  (universe, aiming) <- insideUniverse expr univ ctx
  (outcome, _, state') <- dataize universe state aiming
  pure (reached outcome, state')
  where
    reached :: Outcome -> Maybe Bytes
    reached (Dataized bytes) = Just bytes
    reached (Residual _) = Nothing
