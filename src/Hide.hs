{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Hide (hide, hide') where

import AST
import Logger (logDebug)
import Misc
import Printer (logPrintConfig, printExpression')
import Rewriter
import Text.Printf (printf)

fqnToAttrs :: Expression -> [Attribute]
fqnToAttrs = reverse . fqnToAttrs'
  where
    fqnToAttrs' :: Expression -> [Attribute]
    fqnToAttrs' (ExDispatch ExGlobal attr) = [attr]
    fqnToAttrs' (ExDispatch expr attr) = attr : fqnToAttrs' expr
    fqnToAttrs' expr = []

hide' :: Program -> [Expression] -> Program
hide' prog [] = prog
hide' (Program expr@(ExFormation _)) (fqn : rest) = hide' (Program (hiddenFormation expr (fqnToAttrs fqn))) rest
  where
    hiddenFormation :: Expression -> [Attribute] -> Expression
    hiddenFormation (ExFormation bds) [attr] =
      let bds' = [bd | bd <- bds, attributeFromBinding bd /= Just attr]
       in ExFormation bds'
    hiddenFormation (ExFormation bds) attrs = ExFormation (hiddenBindings bds attrs)
      where
        hiddenBindings :: [Binding] -> [Attribute] -> [Binding]
        hiddenBindings [] _ = []
        hiddenBindings (bd@(BiTau attr form@(ExFormation _)) : bds) attrs@(attr' : rest)
          | attr == attr' = BiTau attr (hiddenFormation form rest) : bds
          | otherwise = bd : hiddenBindings bds attrs
        hiddenBindings bds _ = bds
    hiddenFormation expr attr = expr
hide' prog (fqn : rest) = prog

hide :: [Rewritten] -> [Expression] -> [Rewritten]
hide [] _ = []
hide (Rewritten {..} : rest) exprs = Rewritten (hide' program exprs) maybeRule : hide rest exprs
