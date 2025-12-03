-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Filter (include, exclude) where

import AST
import Logger (logDebug)
import Misc
import Printer (logPrintConfig, printExpression, printExpression', printProgram)
import Rewriter
import Text.Printf (printf)

fqnToAttrs :: Expression -> [Attribute]
fqnToAttrs = reverse . fqnToAttrs'
  where
    fqnToAttrs' :: Expression -> [Attribute]
    fqnToAttrs' (ExDispatch ExGlobal attr) = [attr]
    fqnToAttrs' (ExDispatch expr attr) = attr : fqnToAttrs' expr
    fqnToAttrs' expr = []

exclude' :: Program -> [Expression] -> Program
exclude' prog [] = prog
exclude' (Program expr@(ExFormation _)) (fqn : rest) = exclude' (Program (excludedFormation expr (fqnToAttrs fqn))) rest
  where
    excludedFormation :: Expression -> [Attribute] -> Expression
    excludedFormation (ExFormation bds) [attr] = ExFormation [bd | bd <- bds, attributeFromBinding bd /= Just attr]
    excludedFormation (ExFormation bds) attrs = ExFormation (excludedBindings bds attrs)
      where
        excludedBindings :: [Binding] -> [Attribute] -> [Binding]
        excludedBindings [] _ = []
        excludedBindings (bd@(BiTau attr form@(ExFormation _)) : bds) attrs@(attr' : rest)
          | attr == attr' = BiTau attr (excludedFormation form rest) : bds
          | otherwise = bd : excludedBindings bds attrs
        excludedBindings (bd : bds) attrs = bd : excludedBindings bds attrs
    excludedFormation expr _ = expr
exclude' prog _ = prog

exclude :: [Rewritten] -> [Expression] -> [Rewritten]
exclude [] _ = []
exclude rs [] = rs
exclude ((program, maybeRule) : rest) exprs = (exclude' program exprs, maybeRule) : exclude rest exprs

include' :: Program -> Expression -> Program
include' prog@(Program expr@(ExFormation _)) fqn = case includedFormation expr (fqnToAttrs fqn) of
  Just expr -> Program expr
  _ -> Program (ExFormation [BiVoid AtRho])
  where
    includedFormation :: Expression -> [Attribute] -> Maybe Expression
    includedFormation (ExFormation bds) [attr] =
      let bds' = [bd | bd <- bds, attributeFromBinding bd == Just attr]
       in if null bds' then Nothing else Just (ExFormation (withVoidRho bds'))
    includedFormation (ExFormation bds) attrs = includedBindings bds attrs >>= (Just . ExFormation . (: [BiVoid AtRho]))
      where
        includedBindings :: [Binding] -> [Attribute] -> Maybe Binding
        includedBindings (bd@(BiTau attr form@(ExFormation _)) : bds) attrs@(attr' : rest)
          | attr == attr' = includedFormation form rest >>= Just . BiTau attr
          | otherwise = includedBindings bds attrs
        includedBindings _ _ = Nothing
    includedFormation _ _ = Nothing
include' _ _ = Program (ExFormation [BiVoid AtRho])

include :: [Rewritten] -> [Expression] -> [Rewritten]
include [] _ = []
include rs [] = rs
include ((program, maybeRule) : rest) (expr : _) = (include' program expr, maybeRule) : include rest [expr]