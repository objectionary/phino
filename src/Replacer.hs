{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse though the Program with replacing
-- pattern sub expression with target expressions
module Replacer
  ( replaceProgram
  , replaceProgramFast
  , ReplaceContext (..)
  , ReplaceProgramFunc
  )
where

import AST
import Control.Exception (Exception, throwIO)
import Data.List (isPrefixOf)
import Matcher (Tail (TaApplication, TaDispatch))
import Printer (printProgram)
import Text.Printf (printf)

type ReplaceState a = (a, [Expression], [Expression -> Expression])

type ReplaceExpressionFunc = ReplaceState Expression -> ReplaceContext -> ReplaceState Expression

type ReplaceProgramFunc = ReplaceState Program -> Program

newtype ReplaceContext = ReplaceCtx {_maxDepth :: Int}

replaceBindings :: ReplaceState [Binding] -> ReplaceContext -> ReplaceExpressionFunc -> ReplaceState [Binding]
replaceBindings state@(bds, [], _) _ _ = state
replaceBindings state@(bds, _, []) _ _ = state
replaceBindings state@([], ptns, repls) _ _ = state
replaceBindings (BiTau attr expr : bds, ptns, repls) ctx func =
  let (expr', ptns', repls') = func (expr, ptns, repls) ctx
      (bds', ptns'', repls'') = replaceBindings (bds, ptns', repls') ctx func
   in (BiTau attr expr' : bds', ptns'', repls'')
replaceBindings (bd : bds, ptns, repls) ctx func =
  let (bds', ptns', repls') = replaceBindings (bds, ptns, repls) ctx func
   in (bd : bds', ptns', repls')

replaceExpression :: ReplaceExpressionFunc
replaceExpression state@(expr, ptns@(ptn : _ptns), repls@(repl : _repls)) ctx =
  if expr == ptn
    then replaceExpression (repl expr, _ptns, _repls) ctx
    else case expr of
      ExDispatch inner attr ->
        let (expr', ptns', repls') = replaceExpression (inner, ptns, repls) ctx
         in (ExDispatch expr' attr, ptns', repls')
      ExApplication inner tau ->
        let (expr', ptns', repls') = replaceExpression (inner, ptns, repls) ctx
            ([tau'], ptns'', repls'') = replaceBindings ([tau], ptns', repls') ctx replaceExpression
         in (ExApplication expr' tau', ptns'', repls'')
      ExFormation bds ->
        let (bds', ptns', repls') = replaceBindings (bds, ptns, repls) ctx replaceExpression
         in (ExFormation bds', ptns', repls')
      _ -> state
replaceExpression state _ = state

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

replaceExpressionFast :: ReplaceExpressionFunc
replaceExpressionFast = replaceExpressionFast' 0
  where
    replaceExpressionFast' :: Int -> ReplaceExpressionFunc
    replaceExpressionFast' _ state@(expr, [], _) _ = state
    replaceExpressionFast' _ state@(expr, _, []) _ = state
    replaceExpressionFast' depth state@(expr, ptns, repls) ctx@ReplaceCtx{..} =
      if depth == _maxDepth
        then (expr, [], [])
        else case expr of
          ExFormation bds ->
            let replaced = replaceBindingsFast bds ptns (map (\rep -> rep expr) repls)
                (bds', ptns', repls') = replaceBindings (replaced, ptns, repls) ctx (replaceExpressionFast' (depth + 1))
             in (ExFormation bds', ptns', repls')
          ExDispatch inner attr ->
            let (expr', ptns', repls') = replaceExpressionFast (inner, ptns, repls) ctx
             in (ExDispatch expr' attr, ptns', repls')
          ExApplication inner (BiTau attr arg) ->
            let (expr', ptns', repls') = replaceExpressionFast (inner, ptns, repls) ctx
                (expr'', ptns'', repls'') = replaceExpressionFast (arg, ptns', repls') ctx
             in (ExApplication expr' (BiTau attr expr''), ptns'', repls'')
          _ -> state

replaceProgram :: ReplaceProgramFunc
replaceProgram (Program expr, ptns, repls) =
  let (expr', _, _) = replaceExpression (expr, ptns, repls) (ReplaceCtx 0)
   in Program expr'

replaceProgramFast :: ReplaceContext -> ReplaceProgramFunc
replaceProgramFast ctx (Program expr, ptns, repls) =
  let (expr', _, _) = replaceExpressionFast (expr, ptns, repls) ctx
   in Program expr'
