{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module provides commonly used helper functions for other modules
module Misc
  ( withVoidRho
  , recoverFormations
  , toDouble
  , fqnToAttrs
  , attributesFromBindings
  , attributesFromBindings'
  , attributeFromBinding
  , uniqueBindings
  , uniqueBindings'
  , orThrow
  , matchDataObject
  , pattern DataObject
  , pattern DataString
  , pattern DataNumber
  , pattern BaseObject
  )
where

import AST
import Control.Exception
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as T

-- import Printer (printExpression)

import Data.Functor ((<&>))
import Text.Printf (printf)

-- Unwrap a pure 'Either String' in IO, throwing the built exception on 'Left'
orThrow :: (Exception e) => (String -> e) -> Either String a -> IO a
orThrow _ (Right value) = pure value
orThrow asException (Left err) = throwIO (asException err)

matchBaseObject :: Expression -> Maybe T.Text
matchBaseObject (ExDispatch ExRoot (AtLabel label)) = Just label
matchBaseObject _ = Nothing

pattern BaseObject :: T.Text -> Expression
pattern BaseObject label <- (matchBaseObject -> Just label)
  where
    BaseObject label = ExDispatch ExRoot (AtLabel label)

-- Minimal matcher function (required for view pattern)
matchDataObject :: Expression -> Maybe (T.Text, Bytes)
matchDataObject (ExApplication outer (ArAlpha (Alpha 0) inner)) = case (matchOuter outer, matchInner inner) of
  (Just label, Just bts) -> Just (label, bts)
  _ -> Nothing
  where
    matchOuter :: Expression -> Maybe T.Text
    matchOuter (BaseObject label) = Just label
    matchOuter (ExPhiAgain _ _ (BaseObject label)) = Just label
    matchOuter _ = Nothing
    matchInner :: Expression -> Maybe Bytes
    matchInner (ExPhiAgain _ _ inner') = matchInner inner'
    matchInner inner' = matchInner' inner'
    matchInner' :: Expression -> Maybe Bytes
    matchInner' (ExApplication bytes (ArAlpha (Alpha 0) formation)) = case (matchesBytes bytes, matchFormation formation) of
      (True, Just bts) -> Just bts
      _ -> Nothing
    matchInner' _ = Nothing
    matchesBytes :: Expression -> Bool
    matchesBytes (BaseObject "bytes") = True
    matchesBytes (ExPhiAgain _ _ (BaseObject "bytes")) = True
    matchesBytes _ = False
    matchFormation :: Expression -> Maybe Bytes
    matchFormation (ExFormation [BiDelta bts, BiVoid AtRho]) = Just bts
    matchFormation (ExPhiAgain _ _ (ExFormation [BiDelta bts, BiVoid AtRho])) = Just bts
    matchFormation _ = Nothing
matchDataObject _ = Nothing

pattern DataString :: Bytes -> Expression
pattern DataString bts = DataObject "string" bts

pattern DataNumber :: Bytes -> Expression
pattern DataNumber bts = DataObject "number" bts

pattern DataObject :: T.Text -> Bytes -> Expression
pattern DataObject label bts <- (matchDataObject -> Just (label, bts))
  where
    DataObject label bts =
      ExApplication
        (BaseObject label)
        ( ArAlpha
            (Alpha 0)
            ( ExApplication
                (BaseObject "bytes")
                ( ArAlpha
                    (Alpha 0)
                    (ExFormation [BiDelta bts, BiVoid AtRho])
                )
            )
        )

-- Extract attribute from binding
attributeFromBinding :: Binding -> Maybe Attribute
attributeFromBinding (BiTau attr _) = Just attr
attributeFromBinding (BiVoid attr) = Just attr
attributeFromBinding (BiDelta _) = Just AtDelta
attributeFromBinding (BiLambda _) = Just AtLambda
attributeFromBinding (BiMeta _) = Nothing

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

-- Add void rho binding to the end of the list of any rho binding is not present
withVoidRho :: [Binding] -> [Binding]
withVoidRho bds = withVoidRho' bds False
  where
    withVoidRho' :: [Binding] -> Bool -> [Binding]
    withVoidRho' [] hasRho = [BiVoid AtRho | not hasRho]
    withVoidRho' (bd : rest) hasRho =
      case bd of
        BiMeta _ -> bd : rest
        BiVoid (AtMeta _) -> bd : rest
        BiTau (AtMeta _) _ -> bd : rest
        BiVoid AtRho -> bd : withVoidRho' rest True
        BiTau AtRho _ -> bd : withVoidRho' rest True
        _ -> bd : withVoidRho' rest hasRho

-- Recursively ensure all formations have a BiVoid AtRho binding (ρ ↦ ∅).
-- Fixes in-memory ExFormation [] to ExFormation [BiVoid AtRho] after rewriting,
-- keeping the invariant that the parser enforces via withVoidRho.
recoverFormations :: Expression -> Expression
recoverFormations (ExFormation bindings) = ExFormation (withVoidRho (map recoverFormations' bindings))
recoverFormations (ExDispatch expr attr) = ExDispatch (recoverFormations expr) attr
recoverFormations (ExApplication expr arg) = ExApplication (recoverFormations expr) (recoverArgument arg)
recoverFormations expr = expr

recoverFormations' :: Binding -> Binding
recoverFormations' (BiTau attr expr) = BiTau attr (recoverFormations expr)
recoverFormations' binding = binding

recoverArgument :: Argument -> Argument
recoverArgument (ArTau attr expr) = ArTau attr (recoverFormations expr)
recoverArgument (ArAlpha alpha expr) = ArAlpha alpha (recoverFormations expr)

-- Transform dispatch to list of attributes
-- >>> fqnToAttrs (ExDispatch (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number"))
-- Just [org,eolang,number]
-- >>> fqnToAttrs (ExFormation [])
-- Nothing
-- >>> fqnToAttrs ExRoot
-- Just []
fqnToAttrs :: Expression -> Maybe [Attribute]
fqnToAttrs expr = fqnToAttrs' expr <&> reverse
  where
    fqnToAttrs' :: Expression -> Maybe [Attribute]
    fqnToAttrs' ExRoot = Just []
    fqnToAttrs' (ExDispatch ex at) = fqnToAttrs' ex <&> (:) at
    fqnToAttrs' _ = Nothing

-- >>> toDouble 5
-- 5.0
toDouble :: Int -> Double
toDouble = fromIntegral
