-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module provides commonly used helper functions for other modules
module Misc
  ( toDouble
  , fqnToAttrs
  , attributesFromBindings
  , attributesFromBindings'
  , attributeFromBinding
  , uniqueBindings
  , uniqueBindings'
  , orThrow
  )
where

import AST
import Control.Exception
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Text.Printf (printf)

-- Unwrap a pure 'Either String' in IO, throwing the built exception on 'Left'
orThrow :: (Exception e) => (String -> e) -> Either String a -> IO a
orThrow _ (Right value) = pure value
orThrow asException (Left err) = throwIO (asException err)

-- Extract attribute from binding
attributeFromBinding :: Binding -> Maybe Attribute
attributeFromBinding (BiTau attr _) = Just attr
attributeFromBinding (BiVoid attr) = Just attr
attributeFromBinding (BiDelta _) = Just AtDelta
attributeFromBinding (BiLambda _) = Just AtLambda
attributeFromBinding (BiMeta _) = Nothing
attributeFromBinding (BiAny _) = Nothing

-- Extract attributes from bindings
attributesFromBindings :: [Binding] -> [Attribute]
attributesFromBindings [] = []
attributesFromBindings bds = catMaybes (attributesFromBindings' bds)

attributesFromBindings' :: [Binding] -> [Maybe Attribute]
attributesFromBindings' = map attributeFromBinding

uniqueBindings' :: [Binding] -> IO [Binding]
uniqueBindings' bds = case uniqueBindings bds of
  Left msg -> throwIO (userError msg)
  Right _ -> pure bds

-- Check if given binding list consists of unique attributes
uniqueBindings :: [Binding] -> Either String [Binding]
uniqueBindings bds = case duplicated bds Set.empty of
  Just attr ->
    Left
      ( printf
          "Duplicated attribute '%s' found in %s"
          (show attr)
          (intercalate ", " (map show (attributesFromBindings bds)))
      )
  _ -> Right bds
  where
    duplicated :: [Binding] -> Set.Set Attribute -> Maybe Attribute
    duplicated [] _ = Nothing
    duplicated (bd : rest) seen = case attributeFromBinding bd of
      Just attr
        | attr `Set.member` seen -> Just attr
        | otherwise -> duplicated rest (Set.insert attr seen)
      Nothing -> duplicated rest seen

-- Transform dispatch to list of attributes
-- >>> fqnToAttrs (ExDispatch (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number"))
-- Just [org,eolang,number]
-- >>> fqnToAttrs (ExFormation [])
-- Nothing
-- >>> fqnToAttrs ExRoot
-- Just []
fqnToAttrs :: Expression -> Maybe [Attribute]
fqnToAttrs expr = go expr <&> reverse
  where
    go :: Expression -> Maybe [Attribute]
    go ExRoot = Just []
    go (ExDispatch ex at) = go ex <&> (:) at
    go _ = Nothing

-- >>> toDouble 5
-- 5.0
toDouble :: Int -> Double
toDouble = fromIntegral
