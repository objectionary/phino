{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Forks (Decision, Forks, agreed, decide, explored, marked, noted, routed, since) where

import AST (Bytes)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Deps (Side (..))

type Decision = (Either Int Bytes, Side)

-- The decisions of one path of a run: the sides it was told to take at the
-- forks it meets, the sides it has taken by the condition it took them on,
-- the decisions it has met in the order it met them, and the routes of the
-- paths it has opened for the sides it did not take.
data Forks = Forks
  { _route :: IORef ([Side], [Side])
  , _decided :: IORef (Map (Either Int Bytes) Side)
  , _met :: IORef [Decision]
  , _pending :: IORef [[Side]]
  }

routed :: [Side] -> IORef [[Side]] -> IO Forks
routed route pending = Forks <$> newIORef (route, []) <*> newIORef Map.empty <*> newIORef [] <*> pure pending

decide :: Forks -> Either Int Bytes -> IO Side
decide forks condition = do
  decided <- readIORef forks._decided
  side <- maybe undecided pure (Map.lookup condition decided)
  modifyIORef' forks._decided (Map.insert condition side)
  modifyIORef' forks._met ((condition, side) :)
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

marked :: Forks -> IO Int
marked forks = length <$> readIORef forks._met

since :: Forks -> Int -> IO [Decision]
since forks mark = (\met -> take (length met - mark) met) <$> readIORef forks._met

agreed :: Forks -> [Decision] -> IO Bool
agreed forks deps = (\decided -> all (\(condition, side) -> Map.lookup condition decided == Just side) deps) <$> readIORef forks._decided

noted :: Forks -> [Decision] -> IO ()
noted forks deps = modifyIORef' forks._met (deps ++)

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
