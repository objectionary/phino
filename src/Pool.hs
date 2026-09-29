{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- A handful of workers taking independent actions off one list, which is
-- how the '--deep' walk under '--jobs' morphs the bindings of the formation
-- it starts at side by side (#1534). What the actions gave is folded in the
-- order they were listed and not in the order they finished, so whatever
-- the fold writes, the protocol above all, comes out the same however the
-- workers were scheduled, and it comes out as soon as an action and every
-- one before it are done rather than once the slowest of them is.
module Pool (pooled) where

import Control.Concurrent (QSem, ThreadId, forkIO, killThread, newQSem, signalQSem, waitQSem)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket_, finally, mask, throwIO, try)
import Control.Monad (foldM)

-- Run the actions, at most as many at once as the first argument says, and
-- fold what they gave in the order they were listed. An action that threw
-- has its exception thrown once everything listed before it is folded, and
-- the actions still running are stopped then, since nobody waits for them.
pooled :: forall a b. Int -> [IO a] -> (b -> a -> IO b) -> b -> IO b
pooled width actions fold start = do
  gate <- newQSem (max 1 width)
  launched <- mapM (launch gate) actions
  foldM collected start (map snd launched) `finally` mapM_ (killThread . fst) launched
  where
    launch :: QSem -> IO a -> IO (ThreadId, MVar (Either SomeException a))
    launch gate action = do
      box <- newEmptyMVar
      thread <- mask $ \restore -> forkIO (try (restore (bracket_ (waitQSem gate) (signalQSem gate) action)) >>= putMVar box)
      pure (thread, box)
    collected :: b -> MVar (Either SomeException a) -> IO b
    collected acc box = takeMVar box >>= either throwIO (fold acc)
