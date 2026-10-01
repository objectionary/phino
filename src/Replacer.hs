-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Replacer
  ( replaceExpression
  , replaceExpressionFast
  , ReplaceExpressionFunc
  )
where

import AST
import Data.List (isInfixOf, isPrefixOf)

type ReplaceState a = (a, [Expression], [Expression -> Expression])

type ReplaceExpressionFunc' = ReplaceState Expression -> ReplaceState Expression

type ReplaceExpressionFunc = ReplaceState Expression -> Expression

replaceBindings :: ReplaceState [Binding] -> ReplaceExpressionFunc' -> ReplaceState [Binding]
replaceBindings state@(_, [], _) _ = state
replaceBindings state@(_, _, []) _ = state
replaceBindings state@([], _, _) _ = state
replaceBindings (BiTau attr expr : bds, ptns, repls) func =
  let (expr', ptns', repls') = func (expr, ptns, repls)
      (bds', ptns'', repls'') = replaceBindings (bds, ptns', repls') func
   in (BiTau attr expr' : bds', ptns'', repls'')
replaceBindings (bd : bds, ptns, repls) func =
  let (bds', ptns', repls') = replaceBindings (bds, ptns, repls) func
   in (bd : bds', ptns', repls')

replaceArgument :: ReplaceState Argument -> ReplaceExpressionFunc' -> ReplaceState Argument
replaceArgument (ArTau attr expr, ptns, repls) func =
  let (expr', ptns', repls') = func (expr, ptns, repls)
   in (ArTau attr expr', ptns', repls')
replaceArgument (ArAlpha alpha expr, ptns, repls) func =
  let (expr', ptns', repls') = func (expr, ptns, repls)
   in (ArAlpha alpha expr', ptns', repls')

replaceExpression' :: ReplaceExpressionFunc'
replaceExpression' state@(expr, ptns@(ptn : _ptns), repls@(repl : _repls))
  | inert expr && not (inert ptn) = state
  | expr == ptn = replaceExpression' (repl expr, _ptns, _repls)
  | otherwise = case expr of
      ExDispatch inner attr ->
        let (expr', ptns', repls') = replaceExpression' (inner, ptns, repls)
         in (ExDispatch expr' attr, ptns', repls')
      ExApplication inner arg ->
        let (expr', ptns', repls') = replaceExpression' (inner, ptns, repls)
            (arg', ptns'', repls'') = replaceArgument (arg, ptns', repls') replaceExpression'
         in (ExApplication expr' arg', ptns'', repls'')
      ExFormation bds ->
        let (bds', ptns', repls') = replaceBindings (bds, ptns, repls) replaceExpression'
         in (ExFormation bds', ptns', repls')
      _ -> state
replaceExpression' state = state

replaceBindingsFast :: Expression -> ReplaceState [Binding] -> ReplaceState [Binding]
replaceBindingsFast _ state@(_, [], _) = state
replaceBindingsFast _ state@(_, _, []) = state
replaceBindingsFast expr (bds, ptn : ptns, repl : repls) = case (ptn, repl expr) of
  (ExFormation [], ExFormation rbds) -> replaceBindingsFast expr (rbds, ptns, repls)
  (ExFormation pbds, ExFormation rbds)
    | pbds `isInfixOf` bds -> replaceBindingsFast expr (findAndReplace bds pbds rbds, ptns, repls)
  _ ->
    let (bds', ptns', repls') = replaceBindingsFast expr (bds, ptns, repls)
     in (bds', ptn : ptns', repl : repls')
  where
    findAndReplace :: [Binding] -> [Binding] -> [Binding] -> [Binding]
    findAndReplace [] _ _ = []
    findAndReplace _ [] rbds = rbds
    findAndReplace xs@(x : xs') pbds rbds
      | pbds `isPrefixOf` xs = rbds ++ findAndReplace (drop (length pbds) xs) pbds rbds
      | otherwise = x : findAndReplace xs' pbds rbds

replaceExpressionFast' :: ReplaceExpressionFunc'
replaceExpressionFast' state@(_, [], _) = state
replaceExpressionFast' state@(_, _, []) = state
replaceExpressionFast' state@(expr, ptns, repls) = case expr of
  ExFormation bds ->
    let (bds', ptns', repls') = replaceBindings (replaceBindingsFast expr (bds, ptns, repls)) replaceExpressionFast'
     in (ExFormation bds', ptns', repls')
  ExDispatch inner attr ->
    let (expr', ptns', repls') = replaceExpressionFast' (inner, ptns, repls)
     in (ExDispatch expr' attr, ptns', repls')
  ExApplication inner arg ->
    let (expr', ptns', repls') = replaceExpressionFast' (inner, ptns, repls)
        (arg', ptns'', repls'') = replaceArgument (arg, ptns', repls') replaceExpressionFast'
     in (ExApplication expr' arg', ptns'', repls'')
  _ -> state

replaceExpression :: ReplaceExpressionFunc
replaceExpression state =
  let (expr, _, _) = replaceExpression' state
   in expr

replaceExpressionFast :: ReplaceExpressionFunc
replaceExpressionFast state =
  let (expr, _, _) = replaceExpressionFast' state
   in expr
