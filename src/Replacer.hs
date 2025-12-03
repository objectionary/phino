{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse though the Program with replacing
-- pattern sub expression with target expressions
module Replacer
  ( replaceProgram
  , replaceProgramThrows
  , replaceProgramFast
  , replaceProgramFastThrows
  , ReplaceProgramThrowsFunc
  , ReplaceProgramFunc
  , ReplaceProgramContext (..)
  )
where

import AST
import Control.Exception (Exception, throwIO)
import Data.List (isPrefixOf)
import Matcher (Tail (TaApplication, TaDispatch))
import Printer (printProgram)
import Text.Printf (printf)

data ReplaceProgramContext = ReplaceProgramContext
  { _program :: Program
  , _maxDepth :: Integer
  }

data ReplaceExpressionContext = ReplaceExpressionContext
  { _expression :: Expression
  , _maxDepth :: Integer
  }

updateExpressionContext :: ReplaceExpressionContext -> Expression -> ReplaceExpressionContext
updateExpressionContext ReplaceExpressionContext{..} expr = ReplaceExpressionContext expr _maxDepth

type ReplaceProgramThrowsFunc = [Expression] -> [Expression] -> ReplaceProgramContext -> IO Program

type ReplaceProgramFunc = [Expression] -> [Expression] -> ReplaceProgramContext -> Maybe Program

type ReplaceExpressionFunc = [Expression] -> [Expression] -> ReplaceExpressionContext -> (Expression, [Expression], [Expression])

newtype ReplaceException = CouldNotReplace {prog :: Program}
  deriving (Exception)

instance Show ReplaceException where
  show CouldNotReplace{..} =
    printf
      "Couldn't replace expression in program, lists of patterns and targets has different lengths\nProgram: %s"
      (printProgram prog)

replaceBindings :: [Binding] -> [Expression] -> [Expression] -> ReplaceExpressionContext -> ReplaceExpressionFunc -> ([Binding], [Expression], [Expression])
replaceBindings bds [] [] _ _ = (bds, [], [])
replaceBindings [] ptns repls _ _ = ([], ptns, repls)
replaceBindings (BiTau attr expr : bds) ptns repls ctx func =
  let (expr', ptns', repls') = func ptns repls (updateExpressionContext ctx expr)
      (bds', ptns'', repls'') = replaceBindings bds ptns' repls' ctx func
   in (BiTau attr expr' : bds', ptns'', repls'')
replaceBindings (bd : bds) ptns repls ctx func =
  let (bds', ptns', repls') = replaceBindings bds ptns repls ctx func
   in (bd : bds', ptns', repls')

replaceExpression :: ReplaceExpressionFunc
replaceExpression [] [] ReplaceExpressionContext{..} = (_expression, [], [])
replaceExpression ptns@(ptn : ptnsRest) repls@(repl : replsRest) ctx@ReplaceExpressionContext{..} =
  if _expression == ptn
    then replaceExpression ptnsRest replsRest (updateExpressionContext ctx repl)
    else case _expression of
      ExDispatch inner attr ->
        let (expr', ptns', repls') = replaceExpression ptns repls (updateExpressionContext ctx inner)
         in (ExDispatch expr' attr, ptns', repls')
      ExApplication inner tau ->
        let (expr', ptns', repls') = replaceExpression ptns repls (updateExpressionContext ctx inner)
            ([tau'], ptns'', repls'') = replaceBindings [tau] ptns' repls' ctx replaceExpression
         in (ExApplication expr' tau', ptns'', repls'')
      ExFormation bds ->
        let (bds', ptns', repls') = replaceBindings bds ptns repls ctx replaceExpression
         in (ExFormation bds', ptns', repls')
      _ -> (_expression, ptns, repls)

replaceBindingsFast :: [Binding] -> [Expression] -> [Expression] -> [Binding]
replaceBindingsFast bds [] [] = bds
replaceBindingsFast bds ((ExFormation pbds) : rptns) ((ExFormation rbds) : rrepls) =
  let replaced = replaceBindingsFast' bds pbds rbds
   in replaceBindingsFast replaced rptns rrepls
  where
    replaceBindingsFast' :: [Binding] -> [Binding] -> [Binding] -> [Binding]
    replaceBindingsFast' bds pattern replacement
      | null pattern = replacement
      | otherwise = findAndReplace bds pattern replacement
    findAndReplace :: [Binding] -> [Binding] -> [Binding] -> [Binding]
    findAndReplace [] _ _ = []
    findAndReplace xs@(x : xs') pattern replacement
      | pattern `isPrefixOf` xs = replacement ++ findAndReplace (drop (length pattern) xs) pattern replacement
      | otherwise = x : findAndReplace xs' pattern replacement

replaceExpressionFast :: ReplaceExpressionFunc
replaceExpressionFast = replaceExpressionFast' 0
  where
    replaceExpressionFast' :: Integer -> ReplaceExpressionFunc
    replaceExpressionFast' _ [] [] ReplaceExpressionContext{..} = (_expression, [], [])
    replaceExpressionFast' depth ptns@((ExFormation pbds) : rptns) repls@((ExFormation rbds) : rrepls) ctx@ReplaceExpressionContext{..} =
      if depth == _maxDepth
        then (_expression, [], [])
        else case _expression of
          ExFormation bds ->
            let replaced = replaceBindingsFast bds ptns repls
                (bds', ptns', repls') = replaceBindings replaced ptns repls ctx (replaceExpressionFast' (depth + 1))
             in (ExFormation bds', ptns', repls')
          ExDispatch inner attr ->
            let (expr', ptns', repls') = replaceExpressionFast ptns repls (updateExpressionContext ctx inner)
             in (ExDispatch expr' attr, ptns', repls')
          ExApplication inner (BiTau attr texpr) ->
            let (expr', ptns', repls') = replaceExpressionFast ptns repls (updateExpressionContext ctx inner)
                (expr'', ptns'', repls'') = replaceExpressionFast ptns' repls' (updateExpressionContext ctx texpr)
             in (ExApplication expr' (BiTau attr expr''), ptns'', repls'')
          _ -> (_expression, ptns, repls)
    replaceExpressionFast' _ ptns repls ctx@ReplaceExpressionContext{..} = (_expression, ptns, repls)

replaceProgram' :: ReplaceExpressionFunc -> ReplaceProgramFunc
replaceProgram' func ptns repls ReplaceProgramContext{_program = Program expr, ..}
  | length ptns == length repls =
      let (expr', _, _) = func ptns repls (ReplaceExpressionContext expr _maxDepth)
       in Just (Program expr')
  | otherwise = Nothing

replaceProgram :: ReplaceProgramFunc
replaceProgram = replaceProgram' replaceExpression

replaceProgramThrows' :: ReplaceExpressionFunc -> ReplaceProgramThrowsFunc
replaceProgramThrows' func ptns repls ctx = case replaceProgram' func ptns repls ctx of
  Just prog' -> pure prog'
  _ -> throwIO (CouldNotReplace (_program ctx))

replaceProgramThrows :: ReplaceProgramThrowsFunc
replaceProgramThrows = replaceProgramThrows' replaceExpression

replaceProgramFast :: ReplaceProgramFunc
replaceProgramFast = replaceProgram' replaceExpressionFast

replaceProgramFastThrows :: ReplaceProgramThrowsFunc
replaceProgramFastThrows = replaceProgramThrows' replaceExpressionFast
