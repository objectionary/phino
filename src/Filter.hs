-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Filter (include, exclude) where

import AST
import Misc
import Rewriter

fqnToAttrs :: Expression -> [Attribute]
fqnToAttrs = reverse . fqnToAttrs'
  where
    fqnToAttrs' :: Expression -> [Attribute]
    fqnToAttrs' (ExDispatch ExGlobal attr) = [attr]
    fqnToAttrs' (ExDispatch ex at) = at : fqnToAttrs' ex
    fqnToAttrs' _ = []

exclude' :: Program -> [Expression] -> Program
exclude' prog [] = prog
exclude' (Program ex@(ExFormation _)) (fqn : remaining) = exclude' (Program (excludedFormation ex (fqnToAttrs fqn))) remaining
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
exclude' prog _ = prog

exclude :: [Rewritten] -> [Expression] -> [Rewritten]
exclude [] _ = []
exclude rs [] = rs
exclude ((program, maybeRule) : rest) exprs = (exclude' program exprs, maybeRule) : exclude rest exprs

include' :: Program -> Expression -> Program
include' (Program ex@(ExFormation _)) fqn = case includedFormation ex (fqnToAttrs fqn) of
  Just e -> Program e
  _ -> Program (ExFormation [BiVoid AtRho])
  where
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
include' _ _ = Program (ExFormation [BiVoid AtRho])

include :: [Rewritten] -> [Expression] -> [Rewritten]
include [] _ = []
include rs [] = rs
include ((program, maybeRule) : rest) (expr : _) = (include' program expr, maybeRule) : include rest [expr]
