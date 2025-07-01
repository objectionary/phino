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
replaceBindings (BiTau attr expr : bds) ptns repls =
  let (expr', ptns', repls') = replaceExpression expr ptns repls
      (bds', ptns'', repls'') = replaceBindings bds ptns' repls'
   in (BiTau attr expr' : bds', ptns'', repls'')
replaceBindings (bd : bds) ptns repls =
  let (bds', ptns', repls') = replaceBindings bds ptns repls
   in (bd : bds', ptns', repls')

replaceExpression :: Expression -> [Expression] -> [Expression] -> (Expression, [Expression], [Expression])
replaceExpression expr [] [] = (expr, [], [])
replaceExpression expr ptns repls =
  let (ptn : ptnsRest) = ptns
      (repl : replsRest) = repls
   in if expr == ptn
        then replaceExpression repl ptnsRest replsRest
        else case expr of
          ExDispatch inner attr ->
            let (expr', ptns', repls') = replaceExpression inner ptns repls
             in (ExDispatch expr' attr, ptns', repls')
          ExApplication inner tau ->
            let (expr', ptns', repls') = replaceExpression inner ptns repls
                ([tau'], ptns'', repls'') = replaceBindings [tau] ptns' repls'
             in (ExApplication expr' tau', ptns'', repls'')
          ExFormation bds ->
            let (bds', ptns', repls') = replaceBindings bds ptns repls
             in (ExFormation bds', ptns', repls')
          _ -> (expr, ptns, repls)

replaceProgram :: Program -> [Expression] -> [Expression] -> Maybe Program
replaceProgram (Program expr) ptns repls
  | length ptns == length repls =
      let (expr', _, _) = replaceExpression expr ptns repls
       in Just (Program expr')
  | otherwise = Nothing
