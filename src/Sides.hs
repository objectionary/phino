{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Sides (Sides, minted, opened, named, resolver, sides, spelled) where

import AST
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

data Store = Store
  { _terms :: !(Map.Map Text Expression)
  , _names :: !(IntMap.IntMap [(Expression, Text)])
  , _count :: !Int
  , _spelt :: !(Set.Set Text)
  }

newtype Sides = Sides (IORef Store)

sides :: IO Sides
sides = Sides <$> newIORef (Store Map.empty IntMap.empty 0 Set.empty)

named :: Sides -> Expression -> IO Expression
named store term = fst <$> given False store term

minted :: Sides -> Expression -> IO (Expression, Bool)
minted = given True

given :: Bool -> Sides -> Expression -> IO (Expression, Bool)
given _ _ term@(ExMeta meta) | isName meta = pure (term, False)
given spelt (Sides store) term = atomicModifyIORef' store found
  where
    digest :: Int
    digest = hashExpression term
    found :: Store -> (Store, (Expression, Bool))
    found known@(Store terms names count kept) = case lookup term (IntMap.findWithDefault [] digest names) of
      Just name -> (known, (ExMeta name, False))
      Nothing ->
        let name = T.pack ("n·" ++ show (succ count))
         in (Store (Map.insert name term terms) (IntMap.insertWith (++) digest [(term, name)] names) (succ count) (if spelt then Set.insert name kept else kept), (ExMeta name, True))

spelled :: Sides -> Expression -> IO Expression
spelled (Sides store) whole = do
  Store terms _ _ kept <- readIORef store
  cache <- newIORef Map.empty
  let goExpr :: Expression -> IO Expression
      goExpr term@(ExMeta name)
        | Set.member name kept = do
            done <- readIORef cache
            case Map.lookup name done of
              Just again -> pure again
              Nothing -> do
                made <- maybe (pure term) goExpr (Map.lookup name terms)
                made <$ modifyIORef' cache (Map.insert name made)
      goExpr term@(ExFormation bds) = intact bds term ExFormation <$> mapM goBinding bds
      goExpr term@(ExApplication inner arg) = intact (inner, arg) term (uncurry ExApplication) <$> ((,) <$> goExpr inner <*> goArgument arg)
      goExpr term@(ExDispatch inner attr) = intact inner term (`ExDispatch` attr) <$> goExpr inner
      goExpr term = pure term
      goBinding :: Binding -> IO Binding
      goBinding bd@(BiTau attr term) = intact term bd (BiTau attr) <$> goExpr term
      goBinding bd = pure bd
      goArgument :: Argument -> IO Argument
      goArgument arg@(ArTau attr term) = intact term arg (ArTau attr) <$> goExpr term
      goArgument arg@(ArAlpha alpha term) = intact term arg (ArAlpha alpha) <$> goExpr term
  goExpr whole

opened :: Sides -> Expression -> IO Expression
opened store term = do
  look <- resolver store
  pure (fromMaybe term (look =<< meta term))
  where
    meta :: Expression -> Maybe Text
    meta (ExMeta name) = Just name
    meta _ = Nothing

resolver :: Sides -> IO (Text -> Maybe Expression)
resolver (Sides store) = do
  Store terms _ _ _ <- readIORef store
  pure (`Map.lookup` terms)
