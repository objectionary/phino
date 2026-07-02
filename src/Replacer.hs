{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse through the expression with replacing
-- pattern sub expression with target expressions
module Replacer
  ( replaceExpression
  , replaceExpressionFast
  , ReplaceContext (..)
  , ReplaceExpressionFunc
  )
where

import AST
import Data.List (isPrefixOf)

type ReplaceState a = (a, [Expression], [Expression -> Expression])

type ReplaceExpressionFunc' = ReplaceState Expression -> ReplaceContext -> ReplaceState Expression

type ReplaceExpressionFunc = ReplaceState Expression -> Expression

newtype ReplaceContext = ReplaceCtx {_maxDepth :: Int}

replaceBindings :: ReplaceState [Binding] -> ReplaceContext -> ReplaceExpressionFunc' -> ReplaceState [Binding]
replaceBindings state@(_, [], _) _ _ = state
replaceBindings state@(_, _, []) _ _ = state
replaceBindings state@([], _, _) _ _ = state
replaceBindings (BiTau attr expr : bds, ptns, repls) ctx func =
  let (expr', ptns', repls') = func (expr, ptns, repls) ctx
      (bds', ptns'', repls'') = replaceBindings (bds, ptns', repls') ctx func
   in (BiTau attr expr' : bds', ptns'', repls'')
replaceBindings (bd : bds, ptns, repls) ctx func =
  let (bds', ptns', repls') = replaceBindings (bds, ptns, repls) ctx func
   in (bd : bds', ptns', repls')

replaceArgument :: ReplaceState Argument -> ReplaceContext -> ReplaceExpressionFunc' -> ReplaceState Argument
replaceArgument (ArTau attr expr, ptns, repls) ctx func =
  let (expr', ptns', repls') = func (expr, ptns, repls) ctx
   in (ArTau attr expr', ptns', repls')
replaceArgument (ArAlpha alpha expr, ptns, repls) ctx func =
  let (expr', ptns', repls') = func (expr, ptns, repls) ctx
   in (ArAlpha alpha expr', ptns', repls')

replaceExpression' :: ReplaceExpressionFunc'
replaceExpression' state@(expr, ptns@(ptn : _ptns), repls@(repl : _repls)) ctx =
  if expr == ptn
    then replaceExpression' (repl expr, _ptns, _repls) ctx
    else case expr of
      ExDispatch inner attr ->
        let (expr', ptns', repls') = replaceExpression' (inner, ptns, repls) ctx
         in (ExDispatch expr' attr, ptns', repls')
      ExApplication inner arg ->
        let (expr', ptns', repls') = replaceExpression' (inner, ptns, repls) ctx
            (arg', ptns'', repls'') = replaceArgument (arg, ptns', repls') ctx replaceExpression'
         in (ExApplication expr' arg', ptns'', repls'')
      ExFormation bds ->
        let (bds', ptns', repls') = replaceBindings (bds, ptns, repls) ctx replaceExpression'
         in (ExFormation bds', ptns', repls')
      _ -> state
replaceExpression' state _ = state

replaceBindingsFast :: [Binding] -> [Expression] -> [Expression] -> [Binding]
replaceBindingsFast bds ((ExFormation pbds) : _ptns) ((ExFormation rbds) : _repls) =
  let replaced = findAndReplace bds pbds rbds
   in replaceBindingsFast replaced _ptns _repls
  where
    findAndReplace :: [Binding] -> [Binding] -> [Binding] -> [Binding]
    findAndReplace [] _ _ = []
    findAndReplace _ [] repl = repl
    findAndReplace xs@(x : xs') ptn repl
      | ptn `isPrefixOf` xs = repl ++ findAndReplace (drop (length ptn) xs) ptn repl
      | otherwise = x : findAndReplace xs' ptn repl
replaceBindingsFast bds _ _ = bds

replaceExpressionFast' :: ReplaceExpressionFunc'
replaceExpressionFast' = _replaceExpressionFast 0
  where
    _replaceExpressionFast :: Int -> ReplaceExpressionFunc'
    _replaceExpressionFast _ state@(_, [], _) _ = state
    _replaceExpressionFast _ state@(_, _, []) _ = state
    _replaceExpressionFast depth state@(expr, ptns, repls) ctx@ReplaceCtx{..} =
      if depth == _maxDepth
        then (expr, [], [])
        else case expr of
          ExFormation bds ->
            let replaced = replaceBindingsFast bds ptns (map (\rep -> rep expr) repls)
                (bds', ptns', repls') = replaceBindings (replaced, ptns, repls) ctx (_replaceExpressionFast (depth + 1))
             in (ExFormation bds', ptns', repls')
          ExDispatch inner attr ->
            let (expr', ptns', repls') = replaceExpressionFast' (inner, ptns, repls) ctx
             in (ExDispatch expr' attr, ptns', repls')
          ExApplication inner arg ->
            let (expr', ptns', repls') = replaceExpressionFast' (inner, ptns, repls) ctx
                (arg', ptns'', repls'') = replaceArgument (arg, ptns', repls') ctx replaceExpressionFast'
             in (ExApplication expr' arg', ptns'', repls'')
          _ -> state

replaceExpression :: ReplaceExpressionFunc
replaceExpression state =
  let (expr, _, _) = replaceExpression' state (ReplaceCtx 0)
   in expr

replaceExpressionFast :: ReplaceContext -> ReplaceExpressionFunc
replaceExpressionFast ctx state =
  let (expr, _, _) = replaceExpressionFast' state ctx
   in expr
