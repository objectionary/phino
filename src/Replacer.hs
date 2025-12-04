{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse though the Program with replacing
-- pattern sub expression with target expressions
module Replacer
  ( replaceProgramThrows
  , replaceProgram
  , replaceProgramFast
  , replaceProgramFastThrows
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

type ReplaceProgramFunc a = ReplaceState Program -> ReplaceContext -> a Program

newtype ReplaceContext = ReplaceCtx {_maxDepth :: Integer}

newtype ReplaceException = CouldNotReplace {prog :: Program}
  deriving (Exception)

instance Show ReplaceException where
  show CouldNotReplace{..} =
    printf
      "Couldn't replace expression in program, lists of patterns and targets has different lengths\nProgram: %s"
      (printProgram prog)

replaceBindings :: ReplaceState [Binding] -> ReplaceContext -> ReplaceExpressionFunc -> ReplaceState [Binding]
replaceBindings state@(bds, [], []) _ _ = state
replaceBindings state@([], ptns, repls) _ _ = state
replaceBindings (BiTau attr expr : bds, ptns, repls) ctx func =
  let (expr', ptns', repls') = func (expr, ptns, repls) ctx
      (bds', ptns'', repls'') = replaceBindings (bds, ptns', repls') ctx func
   in (BiTau attr expr' : bds', ptns'', repls'')
replaceBindings (bd : bds, ptns, repls) ctx func =
  let (bds', ptns', repls') = replaceBindings (bds, ptns, repls) ctx func
   in (bd : bds', ptns', repls')

replaceExpression :: ReplaceExpressionFunc
replaceExpression state@(expr, [], []) _ = state
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

replaceBindingsFast :: [Binding] -> [Expression] -> [Expression] -> [Binding]
replaceBindingsFast bds [] [] = bds
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
    replaceExpressionFast' :: Integer -> ReplaceExpressionFunc
    replaceExpressionFast' _ state@(expr, [], []) _ = state
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

replaceProgram' :: ReplaceExpressionFunc -> ReplaceProgramFunc Maybe
replaceProgram' func (Program expr, ptns, repls) ctx
  | length ptns == length repls =
      let (expr', _, _) = func (expr, ptns, repls) ctx
       in Just (Program expr')
  | otherwise = Nothing

replaceProgram :: ReplaceProgramFunc Maybe
replaceProgram = replaceProgram' replaceExpression

replaceProgramThrows' :: ReplaceExpressionFunc -> ReplaceProgramFunc IO
replaceProgramThrows' func state@(prog, ptns, repls) ctx = case replaceProgram' func state ctx of
  Just prog' -> pure prog'
  _ -> throwIO (CouldNotReplace prog)

replaceProgramThrows :: ReplaceProgramFunc IO
replaceProgramThrows = replaceProgramThrows' replaceExpression

replaceProgramFast :: ReplaceProgramFunc Maybe
replaceProgramFast = replaceProgram' replaceExpressionFast

replaceProgramFastThrows :: ReplaceProgramFunc IO
replaceProgramFastThrows = replaceProgramThrows' replaceExpressionFast
