{-# LANGUAGE TupleSections #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Filter (include, include', exclude, exclude') where

import AST
import Control.Exception (throwIO)
import Locator (LocatorException (CanNotFindObjectByLocator, InvalidLocatorProvided))
import Misc
import Rewriter

exclude' :: Expression -> [Expression] -> Expression
exclude' expr [] = expr
exclude' expr@(ExFormation _) (fqn : remaining) = case fqnToAttrs fqn of
  Just fqn' -> exclude' (excludedFormation expr fqn') remaining
  _ -> expr
  where
    excludedFormation :: Expression -> [Attribute] -> Expression
    excludedFormation (ExFormation bindings) [at] = ExFormation [bd | bd <- bindings, attributeFromBinding bd /= Just at]
    excludedFormation (ExFormation bindings) atts = ExFormation (excludedBindings bindings atts)
      where
        excludedBindings :: [Binding] -> [Attribute] -> [Binding]
        excludedBindings [] _ = []
        excludedBindings (bd@(BiTau at' form@(ExFormation _)) : bs) as@(at'' : rs)
          | at' == at'' = BiTau at' (excludedFormation form rs) : bs
          | otherwise = bd : excludedBindings bs as
        excludedBindings (bd : bs) as = bd : excludedBindings bs as
    excludedFormation e _ = e
exclude' expr _ = expr

exclude :: [Rewritten] -> [Expression] -> [Rewritten]
exclude [] _ = []
exclude rs [] = rs
exclude ((expr, maybeRule) : rest) exprs = (exclude' expr exprs, maybeRule) : exclude rest exprs

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
