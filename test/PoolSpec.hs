-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module PoolSpec where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), throwIO, try)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Pool (pooled)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = describe "Pool" $ do
  it "folds what the actions gave in the order they were listed" $
    pooled 3 [threadDelay (pause * 1000) >> pure pause | pause <- [9, 1, 7, 2, 5]] (\acc pause -> pure (acc ++ [pause])) []
      `shouldReturn` [9, 1, 7, 2, 5 :: Int]
  it "runs no more actions at once than it was given" $ do
    running <- newIORef (0 :: Int)
    most <- newIORef (0 :: Int)
    let action :: IO ()
        action = do
          now <- atomicModifyIORef' running (\count -> (count + 1, count + 1))
          atomicModifyIORef' most (\peak -> (max peak now, ()))
          threadDelay 2000
          atomicModifyIORef' running (\count -> (count - 1, ()))
    pooled 2 (replicate 7 action) (\_ _ -> pure ()) ()
    readIORef most `shouldReturn` 2
  it "throws what an action threw once the actions before it are folded" $ do
    folded <- newIORef []
    outcome <- try (pooled 2 [pure 'x', throwIO (ErrorCall "broken"), pure 'z'] (\_ ch -> atomicModifyIORef' folded (\seen -> (seen ++ [ch], ()))) ())
    (,) outcome <$> readIORef folded `shouldReturn` (Left (ErrorCall "broken"), "x")
  it "folds nothing where it is given no action" $
    pooled 4 ([] :: [IO Int]) (\acc val -> pure (acc + val)) 42 `shouldReturn` 42
  it "folds every action where it may run one at a time" $
    pooled 1 (map pure [3, 8, 1]) (\acc val -> pure (acc * 10 + val)) 0 `shouldReturn` (381 :: Int)
