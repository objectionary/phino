-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to rewrite Program with replacing
-- pattern sub expression with target expressions
module Rewriter (rewriteProgram) where

import Ast
import Matcher (Tail (TaApplication, TaDispatch))

rewriteBindings :: [Binding] -> [Expression] -> [Expression] -> ([Binding], [Expression], [Expression])
rewriteBindings bds [] [] = (bds, [], [])
rewriteBindings [] ptns repls = ([], ptns, repls)
rewriteBindings (BiTau attr expr : bds) ptns repls = do
  let (expr', ptns', repls') = rewriteExpression expr ptns repls
  let (bds', ptns'', repls'') = rewriteBindings bds ptns' repls'
  (BiTau attr expr' : bds', ptns'', repls'')
rewriteBindings (bd : bds) ptns repls = do
  let (bds', ptns', repls') = rewriteBindings bds ptns repls
  (bd : bds', ptns', repls')

rewriteExpression :: Expression -> [Expression] -> [Expression] -> (Expression, [Expression], [Expression])
rewriteExpression expr [] [] = (expr, [], [])
rewriteExpression expr ptns repls = do
  let (ptn : ptnsRest) = ptns
  let (repl : replsRest) = repls
  if expr == ptn
    then (repl, ptnsRest, replsRest)
    else case expr of
      ExDispatch inner attr -> do
        let (expr', ptns', repls') = rewriteExpression inner ptns repls
        (ExDispatch expr' attr, ptns', repls')
      ExApplication inner taus -> do
        let (expr', ptns', repls') = rewriteExpression inner ptns repls
        let (taus', ptns'', repls'') = rewriteBindings taus ptns' repls'
        (ExApplication expr' taus', ptns'', repls'')
      ExFormation bds -> do
        let (bds', ptns', repls') = rewriteBindings bds ptns repls
        (ExFormation bds', ptns', repls')
      _ -> (expr, ptns, repls)

-- >>> rewriteProgram (Program (ExDispatch ExThis (AtLabel "x"))) [ExThis] [ExGlobal]
-- Just (Program (ExDispatch ExGlobal (AtLabel "x")))
rewriteProgram :: Program -> [Expression] -> [Expression] -> Maybe Program
rewriteProgram (Program expr) ptns repls
  | length ptns == length repls = do
      let (expr', _, _) = rewriteExpression expr ptns repls
      Just (Program expr')
  | otherwise = Nothing
