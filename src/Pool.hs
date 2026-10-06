{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Pool (pooled) where

import Control.Concurrent (QSem, ThreadId, forkIO, killThread, newQSem, signalQSem, waitQSem)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket_, finally, mask, throwIO, try)
import Control.Monad (foldM)

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
