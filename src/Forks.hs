{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Forks (Decision, Forks, decide, explored, joined, routed) where

import AST (Argument (..), Binding (..), Bytes, Expression (ExApplication, ExDispatch, ExFormation, ExPhiAgain, ExPhiMeet), Function (FnSymbol))
import Data.Bifunctor (first)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Deps (Side (..))

type Decision = (Either Int Bytes, Side)

type Joining = (Int, Map (Int, Int) Int, [(Int, (Int, Int))])

data Forks = Forks
  { _route :: IORef ([Side], [Side])
  , _decided :: IORef (Map (Either Int Bytes) Side)
  , _pending :: IORef [[Side]]
  }

routed :: [Side] -> IORef [[Side]] -> IO Forks
routed route pending = Forks <$> newIORef (route, []) <*> newIORef Map.empty <*> pure pending

decide :: Forks -> Either Int Bytes -> IO Side
decide forks condition = do
  decided <- readIORef forks._decided
  side <- maybe undecided pure (Map.lookup condition decided)
  modifyIORef' forks._decided (Map.insert condition side)
  pure side
  where
    undecided :: IO Side
    undecided = do
      (ahead, behind) <- readIORef forks._route
      case ahead of
        side : more -> side <$ writeIORef forks._route (more, side : behind)
        [] -> do
          writeIORef forks._route ([], LeftSide : behind)
          modifyIORef' forks._pending (reverse (RightSide : behind) :)
          pure LeftSide

explored :: forall answer. Maybe Int -> (Forks -> IO answer) -> IO [answer]
explored cap action = newIORef [] >>= go 0 [[]]
  where
    go :: Int -> [[Side]] -> IORef [[Side]] -> IO [answer]
    go _ [] _ = pure []
    go count (route : rest) pending
      | maybe False (count >=) cap = pure []
      | otherwise = do
          answer <- action =<< routed route pending
          opened <- atomicModifyIORef' pending ([],)
          (answer :) <$> go (count + 1) (rest ++ reverse opened) pending

joined :: Expression -> Expression -> Int -> Maybe (Expression, [(Int, (Int, Int))], Int)
joined left right spent = (\(term, (spent', _, made)) -> (term, reverse made, spent')) <$> goExpr left right (spent, Map.empty, [])
  where
    goExpr :: Expression -> Expression -> Joining -> Maybe (Expression, Joining)
    goExpr (ExFormation one) (ExFormation two) joining = first ExFormation <$> goList goBinding one two joining
    goExpr (ExApplication one arg) (ExApplication two arg') joining = do
      (target, joining') <- goExpr one two joining
      first (ExApplication target) <$> goArgument arg arg' joining'
    goExpr (ExDispatch one attr) (ExDispatch two attr') joining
      | attr == attr' = first (`ExDispatch` attr) <$> goExpr one two joining
    goExpr (ExPhiMeet prefix idx one) (ExPhiMeet prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = first (ExPhiMeet prefix idx) <$> goExpr one two joining
    goExpr (ExPhiAgain prefix idx one) (ExPhiAgain prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = first (ExPhiAgain prefix idx) <$> goExpr one two joining
    goExpr one two joining = same one two joining
    goBinding :: Binding -> Binding -> Joining -> Maybe (Binding, Joining)
    goBinding (BiTau attr one) (BiTau attr' two) joining
      | attr == attr' = first (BiTau attr) <$> goExpr one two joining
    goBinding (BiLambda (FnSymbol one)) (BiLambda (FnSymbol two)) joining
      | one /= two = Just (first (BiLambda . FnSymbol) (picked (one, two) joining))
    goBinding one two joining = same one two joining
    goArgument :: Argument -> Argument -> Joining -> Maybe (Argument, Joining)
    goArgument (ArTau attr one) (ArTau attr' two) joining
      | attr == attr' = first (ArTau attr) <$> goExpr one two joining
    goArgument (ArAlpha alpha one) (ArAlpha alpha' two) joining
      | alpha == alpha' = first (ArAlpha alpha) <$> goExpr one two joining
    goArgument one two joining = same one two joining
    goList :: (item -> item -> Joining -> Maybe (item, Joining)) -> [item] -> [item] -> Joining -> Maybe ([item], Joining)
    goList _ [] [] joining = Just ([], joining)
    goList walk (one : ones) (two : twos) joining = do
      (item, joining') <- walk one two joining
      first (item :) <$> goList walk ones twos joining'
    goList _ _ _ _ = Nothing
    same :: (Eq item) => item -> item -> Joining -> Maybe (item, Joining)
    same one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    picked :: (Int, Int) -> Joining -> (Int, Joining)
    picked pair joining@(spent', names, made) = case Map.lookup pair names of
      Just name -> (name, joining)
      Nothing -> (succ spent', (succ spent', Map.insert pair (succ spent') names, (succ spent', pair) : made))
