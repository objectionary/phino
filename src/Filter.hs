{-# LANGUAGE TupleSections #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Filter (include, include', exclude, exclude') where

import AST
import Control.Exception (throwIO)
import Locator (LocatorException (CanNotFindObjectByLocator, CanNotHideWholeProgram, InvalidLocatorProvided))
import Misc
import Rewriter

exclude' :: Expression -> [Expression] -> IO Expression
exclude' expr [] = pure expr
exclude' _ (ExRoot : _) = throwIO (CanNotHideWholeProgram ExRoot)
exclude' expr (fqn : remaining) = case fqnToAttrs fqn of
  Just attrs@(_ : _) -> maybe (throwIO (CanNotFindObjectByLocator fqn)) (`exclude'` remaining) (excludedFormation expr attrs)
  _ -> throwIO (InvalidLocatorProvided fqn)
  where
    excludedFormation :: Expression -> [Attribute] -> Maybe Expression
    excludedFormation (ExFormation bindings) [at]
      | any ((== Just at) . attributeFromBinding) bindings = Just (ExFormation [bd | bd <- bindings, attributeFromBinding bd /= Just at])
    excludedFormation (ExFormation bindings) (at : rest) = case break (nested at) bindings of
      (before, BiTau at' form : after) -> (\form' -> ExFormation (before ++ BiTau at' form' : after)) <$> excludedFormation form rest
      _ -> Nothing
    excludedFormation _ _ = Nothing
    nested :: Attribute -> Binding -> Bool
    nested at (BiTau at' (ExFormation _)) = at == at'
    nested _ _ = False

exclude :: [Rewritten] -> [Expression] -> IO [Rewritten]
exclude rs exprs = traverse (\(expr, maybeRule) -> (,maybeRule) <$> exclude' expr exprs) rs

include' :: Expression -> [Expression] -> IO Expression
include' expr [] = pure expr
include' expr fqns
  | ExRoot `elem` fqns = pure expr
  | otherwise = mergeForms <$> traverse pick fqns
  where
    pick :: Expression -> IO Expression
    pick fqn = case fqnToAttrs fqn of
      Just attrs -> maybe (throwIO (CanNotFindObjectByLocator fqn)) pure (includedFormation expr attrs)
      _ -> throwIO (InvalidLocatorProvided fqn)
    mergeForms :: [Expression] -> Expression
    mergeForms forms =
      let bds = concat [bs | ExFormation bs <- forms]
          bds' = filter (\bd -> attributeFromBinding bd /= Just AtRho) bds
       in ExFormation bds'
    includedFormation :: Expression -> [Attribute] -> Maybe Expression
    includedFormation (ExFormation bindings) [at] =
      let bs = [bd | bd <- bindings, attributeFromBinding bd == Just at]
       in if null bs then Nothing else Just (ExFormation bs)
    includedFormation (ExFormation bindings) atts = includedBindings bindings atts >>= (Just . ExFormation . pure)
      where
        includedBindings :: [Binding] -> [Attribute] -> Maybe Binding
        includedBindings ((BiTau at' form@(ExFormation _)) : bs) as@(at'' : rs)
          | at' == at'' = includedFormation form rs >>= Just . BiTau at'
          | otherwise = includedBindings bs as
        includedBindings _ _ = Nothing
    includedFormation _ _ = Nothing

include :: [Rewritten] -> [Expression] -> IO [Rewritten]
include rs exprs = traverse (\(expr, maybeRule) -> (,maybeRule) <$> include' expr exprs) rs
