-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Random (randomString, shuffle) where

import Control.Exception (throwIO)
import Control.Monad (forM_, replicateM)
import Data.Char (intToDigit)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import GHC.IO (unsafePerformIO)
import System.Random (newStdGen, randomRIO)
import System.Random.Stateful (newIOGenM, uniformRM)
import Text.Printf (printf)

strings :: IORef (Set String)
{-# NOINLINE strings #-}
strings = unsafePerformIO (newIORef Set.empty)

generate :: String -> IO String
generate [] = pure []
generate ('%' : ch : rest) = do
  rep <- case ch of
    'x' -> replicateM 8 $ do
      v <- randomRIO (0, 15)
      pure (intToDigit v)
    'd' -> printf "%04d" <$> randomRIO (0 :: Int, 9999)
    _ -> pure ['%', ch]
  next <- generate rest
  pure (rep ++ next)
generate (ch : rest) = do
  rest' <- generate rest
  pure (ch : rest')

maxAttempts :: Int
maxAttempts = 100000

regenerate :: String -> IO String
regenerate pat = go maxAttempts
  where
    go :: Int -> IO String
    go 0 = throwIO (userError (printf "randomString() cannot produce a unique value for pattern '%s': the value space is exhausted" pat))
    go attempts = do
      next <- generate pat
      fresh <- atomicModifyIORef' strings $ \set ->
        if next `Set.member` set
          then (set, False)
          else (Set.insert next set, True)
      if fresh then pure next else go (attempts - 1)

randomString :: String -> IO String
randomString pat
  | randomized pat = regenerate pat
  | otherwise = generate pat
  where
    randomized :: String -> Bool
    randomized [] = False
    randomized ('%' : ch : rest) = ch == 'd' || ch == 'x' || randomized rest
    randomized (_ : rest) = randomized rest

shuffle :: [a] -> IO [a]
shuffle xs = do
  gen <- newIOGenM =<< newStdGen
  let n = length xs
  v <- V.thaw (V.fromList xs)
  forM_ [n - 1, n - 2 .. 1] $ \i -> do
    j <- uniformRM (0, i) gen
    M.swap v i j
  V.toList <$> V.freeze v
