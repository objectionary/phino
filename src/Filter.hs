-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Filter (include, exclude) where

import AST
import Data.Maybe (mapMaybe)
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

include' :: Expression -> [Expression] -> Expression
include' expr fqns = case mapMaybe pick fqns of
  [] -> def
  forms -> mergeForms forms
  where
    def :: Expression
    def = ExFormation [BiVoid AtRho]
    pick :: Expression -> Maybe Expression
    pick fqn = do
      attrs <- fqnToAttrs fqn
      includedFormation expr attrs
    mergeForms :: [Expression] -> Expression
    mergeForms forms =
      let bds = concat [bs | ExFormation bs <- forms]
          bds' = filter (\bd -> attributeFromBinding bd /= Just AtRho) bds
       in ExFormation (withVoidRho bds')
    includedFormation :: Expression -> [Attribute] -> Maybe Expression
    includedFormation (ExFormation bindings) [at] =
      let bs = [bd | bd <- bindings, attributeFromBinding bd == Just at]
       in if null bs then Nothing else Just (ExFormation (withVoidRho bs))
    includedFormation (ExFormation bindings) atts = includedBindings bindings atts >>= (Just . ExFormation . (: [BiVoid AtRho]))
      where
        includedBindings :: [Binding] -> [Attribute] -> Maybe Binding
        includedBindings ((BiTau at' form@(ExFormation _)) : bs) as@(at'' : rs)
          | at' == at'' = includedFormation form rs >>= Just . BiTau at'
          | otherwise = includedBindings bs as
        includedBindings _ _ = Nothing
    includedFormation _ _ = Nothing

include :: [Rewritten] -> [Expression] -> [Rewritten]
include [] _ = []
include rs [] = rs
include ((expr, maybeRule) : rest) exprs = (include' expr exprs, maybeRule) : include rest exprs
