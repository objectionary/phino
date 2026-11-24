{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Sides (Sides, gauge, minted, opened, named, resolver, sides, spelled) where

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
  , _depths :: !(Map.Map Text Int)
  }

newtype Sides = Sides (IORef Store)

sides :: IO Sides
sides = Sides <$> newIORef (Store Map.empty IntMap.empty 0 Set.empty Map.empty)

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
    found known@(Store terms names count kept depths) = case lookup term (IntMap.findWithDefault [] digest names) of
      Just name -> (known, (ExMeta name, False))
      Nothing ->
        let name = T.pack ("n·" ++ show (succ count))
         in (Store (Map.insert name term terms) (IntMap.insertWith (++) digest [(term, name)] names) (succ count) (if spelt then Set.insert name kept else kept) (Map.insert name (deepness (`Map.lookup` terms) (`Map.lookup` depths) term) depths), (ExMeta name, True))

spelled :: Sides -> Expression -> IO Expression
spelled (Sides store) whole = do
  Store terms _ _ kept _ <- readIORef store
  if Set.null kept
    then pure whole
    else do
      cache <- newIORef IntMap.empty
      goExpr terms kept cache whole
  where
    goExpr :: Map.Map Text Expression -> Set.Set Text -> IORef (IntMap.IntMap [(Expression, Maybe Expression)]) -> Expression -> IO Expression
    goExpr terms kept cache term = do
      done <- readIORef cache
      case lookup term (IntMap.findWithDefault [] digest done) of
        Just again -> pure (fromMaybe term again)
        Nothing -> do
          made <- nested term
          let change = if made == term then Nothing else Just made
          made <$ modifyIORef' cache (IntMap.insertWith (++) digest [(term, change)])
      where
        digest :: Int
        digest = hashExpression term
        walk :: Expression -> IO Expression
        walk = goExpr terms kept cache
        nested :: Expression -> IO Expression
        nested meta@(ExMeta name)
          | Set.member name kept = maybe (pure meta) walk (Map.lookup name terms)
        nested form@(ExFormation bds) = intact bds form ExFormation <$> mapM goBinding bds
        nested app@(ExApplication inner arg) = intact (inner, arg) app (uncurry ExApplication) <$> ((,) <$> walk inner <*> goArgument arg)
        nested disp@(ExDispatch inner attr) = intact inner disp (`ExDispatch` attr) <$> walk inner
        nested other = pure other
        goBinding :: Binding -> IO Binding
        goBinding bd@(BiTau attr expr) = intact expr bd (BiTau attr) <$> walk expr
        goBinding bd = pure bd
        goArgument :: Argument -> IO Argument
        goArgument arg@(ArTau attr expr) = intact expr arg (ArTau attr) <$> walk expr
        goArgument arg@(ArAlpha alpha expr) = intact expr arg (ArAlpha alpha) <$> walk expr

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
  Store terms _ _ _ _ <- readIORef store
  pure (`Map.lookup` terms)

gauge :: Sides -> IO (Text -> Maybe Int)
gauge (Sides store) = do
  Store _ _ _ _ depths <- readIORef store
  pure (`Map.lookup` depths)
