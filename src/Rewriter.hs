{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite, RewriteContext (..), Rewritten) where

import AST
import Builder
import Control.Exception (Exception, throwIO)
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Deps
import Locator (locatedExpression, withLocatedExpression)
import Logger (logDebug)
import Matcher (Subst)
import Must (Must (..), exceedsUpperBound, inRange)
import Printer (printExpression)
import Replacer (ReplaceContext (ReplaceCtx), ReplaceExpressionFunc, replaceExpression, replaceExpressionFast)
import Rule (RuleContext (RuleContext))
import qualified Rule as R
import Text.Printf (printf)
import qualified Yaml as Y

type RewriteState = ([Rewritten], Set.Set Expression)

type Rewritten = (Program, Maybe String)

type ToReplace = (Expression, Expression, Expression, [Subst])

data RewriteContext = RewriteContext
  { _locator :: Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _depthSensitive :: Bool
  , _buildTerm :: BuildTermFunc
  , _must :: Must
  , _saveStep :: SaveStepFunc
  }

data RewriteException
  = MustBeGoing Must Int
  | MustStopBefore Must Int
  | StoppedOnLimit String Int
  | LoopingRewriting String String Int
  deriving (Exception)

instance Show RewriteException where
  show (MustBeGoing mst cnt) =
    printf
      "With option --must=%s it's expected rewriting cycles to be in range [%s], but rewriting stopped after %d cycles"
      (show mst)
      (show mst)
      cnt
  show (MustStopBefore mst cnt) =
    printf
      "With option --must=%s it's expected rewriting cycles to be in range [%s], but rewriting has already reached %d cycles and is still going"
      (show mst)
      (show mst)
      cnt
  show (StoppedOnLimit flg lim) =
    printf
      "With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --%s=%d"
      flg
      lim
  show (LoopingRewriting expr rul stp) =
    printf
      "On rewriting step '%d' of rule '%s' we got the same expression as we got at one of the previous step, it seems rewriting is looping\nExpression: %s"
      stp
      rul
      expr

-- Build pattern and result expression and replace patterns to results in given expression
buildAndReplace' :: ToReplace -> ReplaceExpressionFunc -> IO Expression
buildAndReplace' (expr, ptn, res, substs) func = do
  ptns <- buildExpressionsThrows ptn substs
  repls <- buildExpressionsThrows res substs
  let ptns' = map fst ptns
      repls' = map (\ex _ -> fst ex) repls
  pure (func (expr, ptns', repls'))

-- If pattern and replacement are appropriate for fast replacing - does it.
-- Pattern and replacement expressions can be used in fast replacing only if
-- 1. they are both formations
-- 2. they start and end with the same meta bindings, e.g. [!B1, ..., !B2]
-- 3. the does not have meta bindings between first and last meta bindings
-- In such case we can just replace bindings one by one without building whole expression.
-- You can find more details in this ticket: https://github.com/objectionary/phino/issues/321
-- If we don't meet the conditions above - just do a regular replacing
tryBuildAndReplaceFast :: ToReplace -> ReplaceContext -> IO Expression
tryBuildAndReplaceFast state@(expr, ExFormation pbds, ExFormation rbds, substs) ctx =
  let pbds' = init (tail pbds)
      rbds' = init (tail rbds)
   in if startsAndEndsWithMeta pbds
        && startsAndEndsWithMeta rbds
        && head pbds == head rbds
        && last pbds == last rbds
        && not (hasMetaBindings pbds')
        && not (hasMetaBindings rbds')
        then do
          logDebug "Applying fast replacing since 'pattern' and 'result' are suitable for this..."
          buildAndReplace' (expr, ExFormation pbds', ExFormation rbds', substs) (replaceExpressionFast ctx)
        else do
          logDebug "Applying regular replacing..."
          buildAndReplace' state replaceExpression
  where
    startsAndEndsWithMeta :: [Binding] -> Bool
    startsAndEndsWithMeta bds =
      length bds > 1
        && isMetaBinding (head bds)
        && isMetaBinding (last bds)
    hasMetaBindings :: [Binding] -> Bool
    isMetaBinding :: Binding -> Bool
    isMetaBinding = \case
      BiMeta _ -> True
      _ -> False
    hasMetaBindings = foldl (\acc bd -> acc || isMetaBinding bd) False
tryBuildAndReplaceFast state _ = buildAndReplace' state replaceExpression

-- The function returns tuple (X, Y) where
-- - X is sequence of programs;
-- - Y is Set of unique programs after each rule application. It allows to stop the rewriting if we're getting
--   into loop and get back to program which we've already got before
rewrite' :: RewriteState -> [Y.Rule] -> Int -> RewriteContext -> IO RewriteState
rewrite' state [] _ _ = pure state
rewrite' state (rule : rest) iteration ctx@RewriteContext{..} = do
  state' <- _rewrite state 1
  rewrite' state' rest iteration ctx
  where
    _rewrite :: RewriteState -> Int -> IO RewriteState
    _rewrite (_rewrittens, _unique) _count =
      let ruleName = fromMaybe "unknown" (Y.name rule)
          ptn = Y.pattern rule
          res = Y.result rule
          (program, _) = head _rewrittens
       in if _count - 1 == _maxDepth
            then do
              logDebug (printf "Max amount of rewriting cycles (%d) for rule '%s' has been reached, rewriting is stopped" _maxDepth ruleName)
              if _depthSensitive
                then throwIO (StoppedOnLimit "max-depth" _maxDepth)
                else pure (_rewrittens, _unique)
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName _count _maxDepth)
              expression <- locatedExpression _locator program
              matched <- R.matchExpressionWithRule expression rule (RuleContext _buildTerm)
              if null matched
                then do
                  logDebug (printf "Rule '%s' does not match, rewriting is stopped" ruleName)
                  pure (_rewrittens, _unique)
                else do
                  logDebug (printf "Rule '%s' has been matched, applying..." ruleName)
                  expr <- tryBuildAndReplaceFast (expression, ptn, res, matched) (ReplaceCtx _maxDepth)
                  if expression == expr
                    then do
                      logDebug (printf "Applied '%s', no changes made" ruleName)
                      pure (_rewrittens, _unique)
                    else
                      if Set.member expr _unique
                        then throwIO (LoopingRewriting (printExpression expr) ruleName _count)
                        else do
                          logDebug
                            ( printf
                                "Applied '%s' (%d nodes -> %d nodes)\n%s"
                                ruleName
                                (countNodes expression)
                                (countNodes expr)
                                (printExpression expr)
                            )
                          prog <- withLocatedExpression _locator expr program
                          _saveStep prog (((iteration - 1) * _maxDepth) + _count)
                          _rewrite (leadsTo prog, Set.insert expr _unique) (_count + 1)
      where
        leadsTo :: Program -> [Rewritten]
        leadsTo _prog = case _rewrittens of
          (program, _) : rest -> (_prog, Nothing) : (program, Just (map toLower (fromMaybe "unknown" (Y.name rule)))) : rest
          [] -> [(_prog, Nothing)]

-- Rewrite program from RewriteContext by provided locator
rewrite :: Program -> [Y.Rule] -> RewriteContext -> IO [Rewritten]
rewrite prog rules ctx@RewriteContext{..} = _rewrite ([(prog, Nothing)], Set.empty) 0 <&> reverse
  where
    _rewrite :: RewriteState -> Int -> IO [Rewritten]
    _rewrite state@(rewrittens, _) count
      | not (inRange _must count) && count > 0 && exceedsUpperBound _must count = throwIO (MustStopBefore _must count)
      | count == _maxCycles = do
          logDebug (printf "Max amount of rewriting cycles for all rules (%d) has been reached, rewriting is stopped" _maxCycles)
          if _depthSensitive
            then throwIO (StoppedOnLimit "max-cycles" _maxCycles)
            else pure rewrittens
      | otherwise = do
          logDebug (printf "Starting rewriting cycle for all rules: %d out of %d" count _maxCycles)
          state'@(rewrittens', _) <- rewrite' state rules count ctx
          let (program', _) = head rewrittens'
              (program, _) = head rewrittens
          if program' == program
            then do
              logDebug "Rewriting is stopped since it has no effect"
              if not (inRange _must count)
                then throwIO (MustBeGoing _must count)
                else pure rewrittens'
            else _rewrite state' (count + 1)
