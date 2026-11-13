{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The names a run gives the sides of the formations its joins fabricate
-- (#2377). A fabricated formation binds its sides to these names, not to
-- the sides themselves, so a walk over it stops at a name and never copies
-- a side out again. A name is an 'ExMeta' spelled '𝑛·k', and the store
-- keeps what every name stands for, for the whole run.
module Sides (Sides, opened, named, resolver, sides) where

import AST
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data Store = Store
  { _terms :: !(Map.Map Text Expression)
  , _names :: !(IntMap.IntMap [(Expression, Text)])
  , _count :: !Int
  }

newtype Sides = Sides (IORef Store)

sides :: IO Sides
sides = Sides <$> newIORef (Store Map.empty IntMap.empty 0)

-- The name of the term, the one an equal term already has, if any, since
-- the memo and the cut find a repeat by comparing terms, and two names for
-- one term would hide it.
named :: Sides -> Expression -> IO Expression
named _ term@(ExMeta meta) | isName meta = pure term
named (Sides store) term = atomicModifyIORef' store given
  where
    digest :: Int
    digest = hashExpression term
    given :: Store -> (Store, Expression)
    given known@(Store terms names count) = case lookup term (IntMap.findWithDefault [] digest names) of
      Just name -> (known, ExMeta name)
      Nothing ->
        let name = T.pack ("n·" ++ show (succ count))
         in (Store (Map.insert name term terms) (IntMap.insertWith (++) digest [(term, name)] names) (succ count), ExMeta name)

-- The term the name stands for, or the term itself, if it is no name.
opened :: Sides -> Expression -> IO Expression
opened store term = do
  look <- resolver store
  pure (fromMaybe term (look =<< meta term))
  where
    meta :: Expression -> Maybe Text
    meta (ExMeta name) = Just name
    meta _ = Nothing

-- What every name the run gave so far stands for, for a comparison that
-- looks through names.
resolver :: Sides -> IO (Text -> Maybe Expression)
resolver (Sides store) = do
  Store terms _ _ <- readIORef store
  pure (`Map.lookup` terms)
