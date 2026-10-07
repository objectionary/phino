{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Dataize (dataize, dataize', reduction, Outcome (..)) where

import AST
import Control.Exception (throwIO, try)
import Control.Monad (unless, when)
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

type Dataizable = Morphed

data Outcome
  = Dataized Bytes
  | Residual Expression
  deriving stock (Eq, Show)

dataize :: Expression -> State -> ReduceContext -> IO (Outcome, [Rewritten], State)
dataize universe state ctx@ReduceContext{..} = do
  expr <- locatedExpression _locator universe
  result <- try (dataize' (expr, (universe, Nothing) :| []) universe state ctx)
  case result of
    Right ((bytes, seq), state') -> pure (Dataized bytes, reverse seq, state')
    Left (StuckAt func seq parked) | _partial -> residual seq parked{_stuck = Just func}
    Left (OutOfStepsAt _ seq parked) | _partial -> residual seq parked
    Left (LoopingAt _ seq parked) | _partial -> residual seq parked
    Left failure -> throwIO (failure :: ReduceException)
  where
    residual :: NonEmpty Rewritten -> State -> IO (Outcome, [Rewritten], State)
    residual seq parked = do
      residue <- locatedExpression _locator (fst (NE.head seq))
      pure (Residual residue, reverse (NE.toList seq), parked)

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
          when (step == (Dataization, "delta")) (ctx._saveEval (EvDelta ctx._nesting bts))
          seq' <- leadsTo seq step (ExBytes bts) ctx
          pure ((bts, NE.toList seq'), state'{_manufactured = Nothing})
        Just (In.Onward way built world, state') -> do
          (dataizable, state'') <- onward seq state' way built ctx
          dataize' dataizable world state'' ctx
        Nothing -> throwIO (Undataizable expr state)
  where
    inside :: ReduceContext -> Expression -> IO ReduceContext
    inside ctx (ExFormation bds)
      | boxed bds && ctx._opened /= Just (ctx._nesting, ctx._site) = do
          ctx._saveEval (EvFormation ctx._nesting ctx._site)
          pure ctx{_nesting = ctx._nesting + 1, _opened = Just (ctx._nesting + 1, ctx._site)}
    inside ctx _ = pure ctx
    unknown :: Expression -> Maybe Int
    unknown (ExFormation bds) = listToMaybe [idx | BiLambda (FnSymbol idx) <- bds]
    unknown _ = Nothing
    manufactured :: Int -> ReduceContext -> IO (Dataized, State)
    manufactured idx ctx = do
      seq' <- leadsTo seq (Dataization, "symbol") (ExBytes datum) ctx
      pure ((datum, NE.toList seq'), state{_manufactured = Just idx})

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

datum :: Bytes
datum = BtMany ["40", "45", "00", "00", "00", "00", "00", "00"]
