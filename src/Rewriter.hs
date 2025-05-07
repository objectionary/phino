-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter where

import Ast
import Matcher (Tail (TaApplication, TaDispatch))

rewriteTauBinding :: TauBinding -> Expression -> [Expression] -> (TauBinding, [Expression])
rewriteTauBinding (TauBinding attr expr) ptn repls = do
  let (expr', rest) = rewriteExpression expr ptn repls
  (TauBinding attr expr', rest)

rewriteBindings :: [Binding] -> Expression -> [Expression] -> ([Binding], [Expression])
rewriteBindings bds _ [] = (bds, [])
rewriteBindings [] _ repls = ([], repls)
rewriteBindings (BiTau tau:bds) ptn repls = do
  let (tau', rest) = rewriteTauBinding tau ptn repls
  let (bds', rest') = rewriteBindings bds ptn rest
  (BiTau tau':bds', rest')
rewriteBindings bds _ repls = (bds, repls)
  
rewriteTauBindings :: [TauBinding] -> Expression -> [Expression] -> ([TauBinding], [Expression])
rewriteTauBindings taus _ [] = (taus, [])
rewriteTauBindings [] _ repls = ([], repls)
rewriteTauBindings (tau:taus) ptn repls = do
  let (tau', rest) = rewriteTauBinding tau ptn repls
  let (taus', rest') = rewriteTauBindings taus ptn rest
  (tau':taus', rest')

rewriteExpression :: Expression -> Expression -> [Expression] -> (Expression, [Expression])
rewriteExpression expr ptn [] = (expr, [])
rewriteExpression expr ptn repls = do
  let (repl:repls') = repls
  if expr == ptn
    then (repl, repls')
    else case expr of
      ExDispatch inner attr -> do
        let (expr', rest) = rewriteExpression inner ptn repls
        (ExDispatch expr' attr, rest)
      ExFormation bds -> do
        let (bds', rest) = rewriteBindings bds ptn repls
        (ExFormation bds', rest)
      ExApplication inner taus -> do
        let (expr', rest) = rewriteExpression inner ptn repls
        let (taus', rest') = rewriteTauBindings taus ptn rest
        (ExApplication expr' taus', rest')
      _ -> (expr, repls)

rewriteProgram :: Program -> Expression -> [Expression] -> Program
rewriteProgram (Program expr) ptn repls = do
  let (expr', _) = rewriteExpression expr ptn repls
  Program expr'
