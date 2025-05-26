-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse though the Program with replacing
-- pattern sub expression with target expressions
module Replacer (replaceProgram) where

import Ast
import Matcher (Tail (TaApplication, TaDispatch))

replaceBindings :: [Binding] -> [Expression] -> [Expression] -> ([Binding], [Expression], [Expression])
replaceBindings bds [] [] = (bds, [], [])
replaceBindings [] ptns repls = ([], ptns, repls)
replaceBindings (BiTau attr expr : bds) ptns repls = do
  let (expr', ptns', repls') = replaceExpression expr ptns repls
  let (bds', ptns'', repls'') = replaceBindings bds ptns' repls'
  (BiTau attr expr' : bds', ptns'', repls'')
replaceBindings (bd : bds) ptns repls = do
  let (bds', ptns', repls') = replaceBindings bds ptns repls
  (bd : bds', ptns', repls')

replaceExpression :: Expression -> [Expression] -> [Expression] -> (Expression, [Expression], [Expression])
replaceExpression expr [] [] = (expr, [], [])
replaceExpression expr ptns repls = do
  let (ptn : ptnsRest) = ptns
  let (repl : replsRest) = repls
  if expr == ptn
    then (repl, ptnsRest, replsRest)
    else case expr of
      ExDispatch inner attr -> do
        let (expr', ptns', repls') = replaceExpression inner ptns repls
        (ExDispatch expr' attr, ptns', repls')
      ExApplication inner taus -> do
        let (expr', ptns', repls') = replaceExpression inner ptns repls
        let (taus', ptns'', repls'') = replaceBindings taus ptns' repls'
        (ExApplication expr' taus', ptns'', repls'')
      ExFormation bds -> do
        let (bds', ptns', repls') = replaceBindings bds ptns repls
        (ExFormation bds', ptns', repls')
      _ -> (expr, ptns, repls)

replaceProgram :: Program -> [Expression] -> [Expression] -> Maybe Program
replaceProgram (Program expr) ptns repls
  | length ptns == length repls = do
      let (expr', _, _) = replaceExpression expr ptns repls
      Just (Program expr')
  | otherwise = Nothing
