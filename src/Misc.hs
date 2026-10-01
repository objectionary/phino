-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

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
import Text.Printf (printf)

orThrow :: (Exception e) => (String -> e) -> Either String a -> IO a
orThrow _ (Right value) = pure value
orThrow asException (Left err) = throwIO (asException err)

attributesFromBindings :: [Binding] -> [Attribute]
attributesFromBindings [] = []
attributesFromBindings bds = catMaybes (attributesFromBindings' bds)

attributesFromBindings' :: [Binding] -> [Maybe Attribute]
attributesFromBindings' = map attributeFromBinding

uniqueBindings' :: [Binding] -> IO [Binding]
uniqueBindings' bds = case uniqueBindings bds of
  Left msg -> throwIO (userError msg)
  Right _ -> pure bds

uniqueBindings :: [Binding] -> Either String [Binding]
uniqueBindings bds = case repeated bds of
  Just attr ->
    Left
      ( printf
          "Duplicated attribute '%s' found in %s"
          (show attr)
          (intercalate ", " (map show (attributesFromBindings bds)))
      )
  _ -> Right bds

fqnToAttrs :: Expression -> Maybe [Attribute]
fqnToAttrs expr = go expr <&> reverse
  where
    go :: Expression -> Maybe [Attribute]
    go ExRoot = Just []
    go (ExDispatch ex at) = go ex <&> (:) at
    go _ = Nothing

toDouble :: Int -> Double
toDouble = fromIntegral
