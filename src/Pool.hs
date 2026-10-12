{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Pool (pooled) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeAsyncException, SomeException, finally, fromException, mask, throwIO, try)
import Control.Monad (foldM, replicateM)
import Data.IORef (IORef, atomicModifyIORef', newIORef)

pooled :: forall a b. Int -> [IO a] -> (b -> a -> IO b) -> b -> IO b
pooled width actions fold start = do
  boxes <- mapM (const newEmptyMVar) actions
  pending <- newIORef (zip actions boxes)
  threads <- replicateM (min (max 1 width) (length actions)) (mask $ \restore -> forkIO (restore (work pending)))
  foldM collected start boxes `finally` mapM_ killThread threads
  where
    work :: IORef [(IO a, MVar (Either SomeException a))] -> IO ()
    work pending = do
      job <- atomicModifyIORef' pending next
      case job of
        Nothing -> pure ()
        Just (action, box) -> do
          outcome <- try action
          putMVar box outcome
          case outcome of
            Left (e :: SomeException) | Just (_ :: SomeAsyncException) <- fromException e -> pure ()
            _ -> work pending
    next :: [(IO a, MVar (Either SomeException a))] -> ([(IO a, MVar (Either SomeException a))], Maybe (IO a, MVar (Either SomeException a)))
    next [] = ([], Nothing)
    next (job : rest) = (rest, Just job)
    collected :: b -> MVar (Either SomeException a) -> IO b
    collected acc box = takeMVar box >>= either throwIO (fold acc)
