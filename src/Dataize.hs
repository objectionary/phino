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
import Control.Exception (throwIO, try)
import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Deps (Evaluation (..), Judgment (..), State (..))
import Engine (Engine (..))
import qualified Inference as In
import Locator (locatedExpression)
import Morph (Morphed, ReduceContext (..), ReduceException (..), ReductionFunc, boxed, deeper, entering, inferred, insideUniverse, leadsTo, onward, parking, universed)
import Rewriter (Rewritten)

type Dataized = (Bytes, [Rewritten])

-- What 𝔻 is handed: a term plus the derivation that reached it, the same pair
-- 𝕄 works on (see 'Morphed').
type Dataizable = Morphed

-- What a run of 𝔻 ends with: the bytes it reached or, under '_partial', the
-- residual program: what the known inputs decided is computed, the stuck λ
-- function and everything depending on it survive in place.
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
-- normal-form subterm, and the chain of steps that led there. A formation
-- '_acyclic' caught being entered from inside itself ends the run the same
-- way, since 𝔻 has no bytes to give for a question it can only ever answer by
-- asking again; where '_partial' is off the signal travels on instead, so a 𝔻
-- run reducing an operand of a firing leaves the loop to the 𝕄 spine around
-- that firing, which parks on it with no '_partial' asked for (see 'morph',
-- #1290). The state 𝑠 goes in and comes back out, so a 𝔻 asked inside another
-- judgment goes on minting symbols where that judgment left off.
dataize :: Expression -> State -> ReduceContext -> IO (Outcome, [Rewritten], State)
dataize universe state ctx@ReduceContext{..} = do
  expr <- locatedExpression _locator universe
  result <- try (dataize' (expr, (universe, Nothing) :| []) universe state ctx)
  case result of
    Right ((bytes, seq), state') -> pure (Dataized bytes, reverse seq, state')
    Left (StuckAt func seq parked) | _partial -> pure (Residual (fst (NE.head seq)), reverse (NE.toList seq), parked{_stuck = Just func})
    Left (OutOfStepsAt _ seq parked) | _partial -> pure (Residual (fst (NE.head seq)), reverse (NE.toList seq), parked)
    Left (LoopingAt _ seq parked) | _partial -> pure (Residual (fst (NE.head seq)), reverse (NE.toList seq), parked)
    Left failure -> throwIO (failure :: ReduceException)

-- The Dataization function 𝔻 retrieves bytes from an expression. It is partial
-- and ternary, 𝔻(n, e, s): besides the term 'n' it takes the universe 'e' ('univ'),
-- which it forwards to 𝕄, and the mutable state 's', returning the bytes together
-- with the new state. Its rules come from 'resources/dataization', run by the
-- engine (see '_dataization' of 'Engine'): 'delta' yields the
-- asset bytes and 'none' (a formation with no Δ/λ/φ) has nothing to dataize, so
-- it dataizes ⊥. The terminator ⊥ signals an error and lies outside 𝔻's domain,
-- so it matches no clause (there is no 'end' rule mapping it to empty bytes) and
-- dataization stops there; a data-less formation therefore fails through the
-- same path (see #955). The dead end is signalled as 'Undataizable', whose
-- message names the terminator where that is what was reached rather than
-- reporting the generic "no dataization rule matched", and which an operand of
-- a firing parks on under '_partial' (see 'reduction', #1401).
-- 'box' contextualizes the φ-body and keeps dataizing (its step is labelled by
-- its 'contextualize' side-computation), and 'norm' reduces through morphing,
-- splicing the morphing steps into the chain. The clauses are disjoint (see
-- #902, #905), so their declaration order must not be load-bearing; when
-- '_shuffle' is on (the '--shuffle' flag) the rules are shuffled before
-- 'inferred' walks them to exercise that invariant — mirroring normalization's
-- "apply until they stop matching". A genuinely order-independent step stays
-- deterministic; a hidden overlap surfaces as a nondeterministic failure rather
-- than staying silently green.
-- The conclusion bytes 'dresult' are produced by a trailing 'dataize' premise;
-- when its argument is bound by a 'morph' or 'normalize' premise, that step
-- joins the spine, otherwise the premise is an isolated side-computation (see
-- 'dataizationSpine' of 'Inference').
-- Like 𝕄, every frame asks '_acyclic' whether the formation it is about to
-- enter through 'box' or 'fire' is one a frame above it has already entered,
-- before any rule is walked: 𝔻 recurses into itself through those two rules
-- without 𝕄 ever seeing the same term twice, so a program cycling through
-- dataization alone is a loop only this guard ends (#1290, #1420). A formation
-- 'box' gets into is written to the protocol as the frame opens, whether or
-- not '_acyclic' is on, and the frame goes on one level deeper, so what the
-- φ body fires stands under the formation it was fired inside of.
dataize' :: Dataizable -> Expression -> State -> ReduceContext -> IO (Dataized, State)
dataize' (expr, seq) univ state caller = do
  guarded <- deeper =<< entering expr =<< universed univ caller{_judgment = Dataization}
  ctx <- inside guarded expr
  parking seq state $ case unknown expr of
    Just idx -> manufactured idx ctx
    Nothing -> do
      reached <- inferred expr univ state ctx ctx._engine._dataization
      case reached of
        Just (In.Answered step bts, state') -> do
          seq' <- leadsTo seq step (ExBytes bts) ctx
          pure ((bts, NE.toList seq'), state'{_manufactured = Nothing})
        Just (In.Onward way built world, state') -> do
          (dataizable, state'') <- onward seq state' way built ctx
          dataize' dataizable world state'' ctx
        Nothing -> throwIO (Undataizable expr state)
  where
    -- The context a frame opening on a formation 'box' gets into goes on
    -- with, once the formation is written to the protocol: one level deeper,
    -- so everything the φ body does stands under that record.
    inside :: ReduceContext -> Expression -> IO ReduceContext
    inside ctx (ExFormation bds)
      | boxed bds = do
          ctx._saveEval (EvFormation ctx._nesting expr ctx._site)
          pure ctx{_nesting = ctx._nesting + 1}
    inside ctx _ = pure ctx
    -- The symbol a formation carries in place of a λ name, if any. Such a
    -- formation is what a λ function answered with where it could not work the
    -- value out, so no entry of the '--symbolic' file answers it and firing it
    -- would get stuck; 𝔻 therefore takes it before the rules are ever walked,
    -- which also keeps 'fire' from matching what it cannot fire.
    unknown :: Expression -> Maybe Int
    unknown (ExFormation bds) = listToMaybe [idx | BiLambda (FnSymbol idx) <- bds]
    unknown _ = Nothing
    -- A symbol dataizes to a datum manufactured for it: dataizing an unknown
    -- never gets stuck, and the very same 42 answers every symbol, since the
    -- run is symbolic and no arithmetic of it is ever read. Which symbol the
    -- datum stands for is told to the state rather than to the term, so the
    -- protocol writes '𝔻(𝜎1)' where the term carries nothing but the 42.
    manufactured :: Int -> ReduceContext -> IO (Dataized, State)
    manufactured idx ctx = do
      seq' <- leadsTo seq (Dataization, "symbol") (ExBytes datum) ctx
      pure ((datum, NE.toList seq'), state{_manufactured = Just idx})

-- What a 'dataize' operand of a λ function is brought down with (see
-- 'ReductionFunc' in 'Morph'): the operand is bound to a synthetic attribute of
-- the universe and dataized there, exactly the way the '--inside' option does
-- it, so what comes back is the data the operand carries. Where a λ function on
-- the way could not fire and '_partial' parked it, the operand never came down
-- to data at all and nothing comes back, which leaves the firing that asked for
-- it stuck. An operand reaches a firing unreduced, since reducing it may take
-- the very λ function being fired, so it is reduced here, on demand, and not
-- before. The context is the one the fire descended with, so the step budget of
-- the run bounds the nesting, and the state 𝑠 goes in and comes back out, so
-- the symbols this reduction mints are counted in the same sequence as the ones
-- around it.
--
-- An operand reaching a term outside the domain of 𝔻 — the terminator ⊥, or a
-- term no dataization rule matches, such as a formation whose φ is a void
-- nothing filled — never comes down to data either, and under '_partial' it
-- leaves the firing stuck the same way rather than ending the run: an unfilled
-- void or an error object is as much a property of the program as a λ function
-- nobody answers (#1401). The protocol records the dead end as a stuck site
-- named '⊥', written with the term 𝔻 could not dataize, and the name travels
-- back in the state as the one the firing got stuck on (see '_stuck'). A run
-- of 𝔻 that is not an operand still fails on it, '_partial' or not (#955).
reduction :: ReductionFunc
reduction univ ctx expr state = do
  (universe, aiming) <- insideUniverse expr univ ctx
  result <- try (dataize universe state aiming)
  case result of
    Right (outcome, _, state') -> pure (reached outcome, state')
    Left (Undataizable term state') | ctx._partial -> do
      unless (dead `elem` ctx._parked) (ctx._saveEval (EvStuck ctx._nesting dead Dataization term))
      pure (Nothing, state'{_stuck = Just dead})
    Left failure -> throwIO failure
  where
    dead :: T.Text
    dead = "⊥"
    reached :: Outcome -> Maybe Bytes
    reached (Dataized bytes) = Just bytes
    reached (Residual _) = Nothing

-- The datum every symbol dataizes to: 42 as a double, the same for all of them.
-- A symbolic run computes nothing, so what the datum is carries no meaning at
-- all; what matters is that 𝔻 of an unknown answers rather than gets stuck, so
-- a λ function whose operands are unknowns still fires and still answers with
-- an unknown of its own.
datum :: Bytes
datum = BtMany ["40", "45", "00", "00", "00", "00", "00", "00"]
