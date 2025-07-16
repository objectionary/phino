-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Random (randomString) where

import Control.Monad (replicateM)
import Data.Char (intToDigit)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.IO (unsafePerformIO)
import System.Random (randomRIO)

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
    'd' -> show <$> randomRIO (0 :: Int, 9999)
    _ -> pure ['%', ch]
  next <- generate rest
  pure (rep ++ next)
generate (ch : rest) = do
  rest' <- generate rest
  pure (ch : rest')

regenerate :: String -> Set String -> IO String
regenerate pat set = do
  next <- generate pat
  if next `Set.member` set
    then regenerate pat set
    else do
      modifyIORef' strings (Set.insert next)
      pure next

randomString :: String -> IO String
randomString pat = do
  set <- readIORef strings
  regenerate pat set
