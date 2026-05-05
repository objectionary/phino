{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite, RewriteContext (..), Rewritten, Rewrittens, Rewrittens') where

import AST
import Builder
import Control.Exception (Exception, throwIO)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Deps
import Locator (locatedExpression, withLocatedExpression)
import Logger (logDebug)
import Matcher (Match)
import Must (Must (..), exceedsUpperBound, inRange)
import Printer (printExpression)
import Replacer (ReplaceContext (ReplaceCtx), ReplaceExpressionFunc, replaceExpression, replaceExpressionFast)
import Rule (RuleContext (RuleContext))
import qualified Rule as R
import Text.Printf (printf)
import qualified Yaml as Y

type RewriteState = (NonEmpty Rewritten, Set.Set Expression, Bool)

type Rewritten = (Program, Maybe String)

type Rewrittens = (NonEmpty Rewritten, Bool)

type Rewrittens' = ([Rewritten], Bool)

type ToReplace = (Expression, Expression, Expression, [Match])

data RewriteContext = RewriteContext
  { _locator :: Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _depthSensitive :: Bool
  , _buildTerm :: BuildTermFunc
  , _must :: Must
  , _breakpoint :: Maybe String
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

-- Replace each matched fragment in the given expression with the corresponding
-- result, built from the matched substitution. Patterns aren't rebuilt — the
-- matcher already returned the exact subexpressions where the rule fired.
buildAndReplace' :: ToReplace -> ReplaceExpressionFunc -> IO Expression
buildAndReplace' (expr, _ptn, res, fragSubsts) func = do
  let frags = map fst fragSubsts
      substs = map snd fragSubsts
  repls <- buildExpressionsThrows res substs
  let repls' = map (\ex _ -> fst ex) repls
  pure (func (expr, frags, repls'))

-- If pattern and replacement are appropriate for fast replacing - does it.
-- Pattern and replacement expressions can be used in fast replacing only if
-- 1. they are both formations
-- 2. they start and end with the same NAMED meta bindings, e.g. [!B1, ..., !B2]
-- 3. they do not have meta bindings between first and last meta bindings
-- 4. neither side contains any anonymous meta variables (which don't bind in
--    the substitution and so can't be filled in during result-build)
-- In such case we can just replace bindings one by one without building whole expression.
-- You can find more details in this ticket: https://github.com/objectionary/phino/issues/321
-- If we don't meet the conditions above - just do a regular replacing
tryBuildAndReplaceFast :: ToReplace -> ReplaceContext -> IO Expression
tryBuildAndReplaceFast state@(expr, ptn@(ExFormation _pbds@(pbd : pbds)), res@(ExFormation _rbds@(rbd : rbds)), substs) ctx =
  let pbds' = init pbds
      rbds' = init rbds
   in if startsAndEndsWithNamedMeta _pbds
        && startsAndEndsWithNamedMeta _rbds
        && pbd == rbd
        && last pbds == last rbds
        && not (hasMetaBindings pbds')
        && not (hasMetaBindings rbds')
        && not (hasAnonExpr ptn)
        && not (hasAnonExpr res)
        then do
          logDebug "Applying fast replacing since 'pattern' and 'result' are suitable for this..."
          buildAndReplaceFast (expr, ExFormation pbds', ExFormation rbds', substs) ctx
        else do
          logDebug "Applying regular replacing..."
          buildAndReplace' state replaceExpression
  where
    startsAndEndsWithNamedMeta :: [Binding] -> Bool
    startsAndEndsWithNamedMeta [] = False
    startsAndEndsWithNamedMeta bds@(bd : _) =
      length bds > 1
        && isNamedMetaBinding bd
        && isNamedMetaBinding (last bds)
    hasMetaBindings :: [Binding] -> Bool
    isNamedMetaBinding :: Binding -> Bool
    isNamedMetaBinding = \case
      BiMeta (Just _) -> True
      _ -> False
    isMetaBinding :: Binding -> Bool
    isMetaBinding = \case
      BiMeta _ -> True
      _ -> False
    hasMetaBindings = foldl (\acc bd -> acc || isMetaBinding bd) False
tryBuildAndReplaceFast state _ = buildAndReplace' state replaceExpression

-- Variant of buildAndReplace' for the fast path: builds middle pattern/result
-- from the named substitution and uses replaceExpressionFast for bulk binding
-- replacement.
buildAndReplaceFast :: ToReplace -> ReplaceContext -> IO Expression
buildAndReplaceFast (expr, ptn, res, fragSubsts) ctx = do
  let substs = map snd fragSubsts
  ptns <- buildExpressionsThrows ptn substs
  repls <- buildExpressionsThrows res substs
  let ptns' = map fst ptns
      repls' = map (\ex _ -> fst ex) repls
  pure (replaceExpressionFast ctx (expr, ptns', repls'))

-- True if an expression contains any anonymous meta variable anywhere.
hasAnonExpr :: Expression -> Bool
hasAnonExpr (ExMeta Nothing) = True
hasAnonExpr (ExMeta (Just _)) = False
hasAnonExpr (ExMetaTail _ Nothing) = True
hasAnonExpr (ExMetaTail e (Just _)) = hasAnonExpr e
hasAnonExpr (ExFormation bds) = any hasAnonBinding bds
hasAnonExpr (ExDispatch e a) = hasAnonAttr a || hasAnonExpr e
hasAnonExpr (ExApplication e bd) = hasAnonExpr e || hasAnonBinding bd
hasAnonExpr (ExPhiMeet _ _ e) = hasAnonExpr e
hasAnonExpr (ExPhiAgain _ _ e) = hasAnonExpr e
hasAnonExpr _ = False

hasAnonBinding :: Binding -> Bool
hasAnonBinding (BiMeta Nothing) = True
hasAnonBinding (BiMetaLambda Nothing) = True
hasAnonBinding (BiTau a e) = hasAnonAttr a || hasAnonExpr e
hasAnonBinding (BiVoid a) = hasAnonAttr a
hasAnonBinding (BiDelta (BtMeta Nothing)) = True
hasAnonBinding _ = False

hasAnonAttr :: Attribute -> Bool
hasAnonAttr (AtMeta Nothing) = True
hasAnonAttr _ = False

-- The function returns tuple (X, Y, Z) where
-- - X is sequence of programs;
-- - Y is Set of unique programs after each rule application. It allows to stop the rewriting if we're getting
--   into loop and get back to program which we've already got before
-- - Z is boolean flag which tells us if we reach breakpoint. If unmatched rule is equal to breakpoint rule - entire
--   rewriting must be stopped and original program must be returned
rewrite' :: RewriteState -> [Y.Rule] -> Int -> RewriteContext -> IO RewriteState
rewrite' state [] _ _ = pure state
rewrite' state (rule : rest) iteration ctx@RewriteContext{..} = do
  state' <- _rewrite state 1
  case state' of
    (_, _, True) -> pure state'
    _ -> rewrite' state' rest iteration ctx
  where
    _rewrite :: RewriteState -> Int -> IO RewriteState
    _rewrite (_rewrittens@((program, _) :| _), _unique, _) _count =
      let ruleName = Y.name rule
          ptn = Y.pattern rule
          res = Y.result rule
       in if _count - 1 == _maxDepth
            then do
              logDebug (printf "Max amount of rewriting cycles (%d) for rule '%s' has been reached, rewriting is stopped" _maxDepth ruleName)
              if _depthSensitive
                then throwIO (StoppedOnLimit "max-depth" _maxDepth)
                else pure (_rewrittens, _unique, False)
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName _count _maxDepth)
              expression <- locatedExpression _locator program
              R.matchExpressionWithRule expression rule (RuleContext _buildTerm) >>= \case
                [] -> do
                  logDebug (printf "Rule '%s' does not match, rewriting is stopped" ruleName)
                  if _breakpoint == Just ruleName
                    then do
                      logDebug (printf "Rule '%s' is a breakpoint, dropping down all the previous rewritings..." ruleName)
                      pure (_rewrittens, _unique, True)
                    else pure (_rewrittens, _unique, False)
                matched -> do
                  logDebug (printf "Rule '%s' has been matched, applying..." ruleName)
                  expr <- tryBuildAndReplaceFast (expression, ptn, res, matched) (ReplaceCtx _maxDepth)
                  if expression == expr
                    then do
                      logDebug (printf "Applied '%s', no changes made" ruleName)
                      pure (_rewrittens, _unique, False)
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
                          _rewrite (leadsTo prog, Set.insert expr _unique, False) (_count + 1)
      where
        leadsTo :: Program -> NonEmpty Rewritten
        leadsTo _prog =
          let (program, _) :| rest = _rewrittens
           in (_prog, Nothing) :| (program, Just (Y.name rule)) : rest

-- Rewrite program by provided locator from RewriteContext
rewrite :: Program -> [Y.Rule] -> RewriteContext -> IO Rewrittens
rewrite prog rules ctx@RewriteContext{..} = do
  (rewrittens, exceeded) <- _rewrite ((prog, Nothing) :| [], Set.empty, False) 0
  pure (NE.reverse rewrittens, exceeded)
  where
    _rewrite :: RewriteState -> Int -> IO Rewrittens
    _rewrite state@(rewrittens@((program, _) :| _), _, _) count
      | not (inRange _must count) && count > 0 && exceedsUpperBound _must count = throwIO (MustStopBefore _must count)
      | count == _maxCycles = do
          logDebug (printf "Max amount of rewriting cycles for all rules (%d) has been reached, rewriting is stopped" _maxCycles)
          if _depthSensitive
            then throwIO (StoppedOnLimit "max-cycles" _maxCycles)
            else pure (rewrittens, True)
      | otherwise = do
          logDebug (printf "Starting rewriting cycle for all rules: %d out of %d" count _maxCycles)
          rewrite' state rules count ctx >>= \case
            (_, _, True) -> pure ((prog, Nothing) :| [], False) -- breakpoint, return original program
            state'@(rewrittens'@((program', _) :| _), _, False) ->
              if program' == program
                then do
                  logDebug "Rewriting is stopped since it has no effect"
                  if not (inRange _must count)
                    then throwIO (MustBeGoing _must count)
                    else pure (rewrittens', False)
                else _rewrite state' (count + 1)
