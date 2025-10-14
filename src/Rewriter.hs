{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite, rewrite', RewriteContext (..)) where

import Ast
import Builder
import Control.Exception (Exception, throwIO)
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import Deps
import Logger (logDebug)
import Matcher (MetaValue (MvAttribute, MvBindings, MvBytes, MvExpression), Subst (Subst), combine, combineMany, defaultScope, matchProgram, substEmpty, substSingle)
import Misc (ensuredFile)
import Must (Must (..), exceedsUpperBound, inRange)
import Parser (parseProgram, parseProgramThrows)
import Pretty (Encoding (UNICODE), PrintMode (SWEET), prettyAttribute, prettyBytes, prettyExpression, prettyExpression', prettyProgram, prettyProgram', prettySubsts)
import Replacer (ReplaceProgramContext (ReplaceProgramContext), ReplaceProgramThrowsFunc, replaceProgramFastThrows, replaceProgramThrows)
import Rule (RuleContext (RuleContext), matchProgramWithRule)
import qualified Rule as R
import Text.Printf
import Yaml (ExtraArgument (..))
import qualified Yaml as Y

data RewriteContext = RewriteContext
  { _program :: Program,
    _maxDepth :: Integer,
    _maxCycles :: Integer,
    _depthSensitive :: Bool,
    _buildTerm :: BuildTermFunc,
    _must :: Must,
    _saveStep :: SaveStepFunc
  }

data RewriteException
  = MustBeGoing {must :: Must, count :: Integer}
  | MustStopBefore {must :: Must, count :: Integer}
  | StoppedOnLimit {flag :: String, limit :: Integer}
  | LoopingRewriting {prog :: String, rule :: String, step :: Integer}
  deriving (Exception)

instance Show RewriteException where
  show MustBeGoing {..} =
    printf
      "With option --must=%s it's expected rewriting cycles to be in range [%s], but rewriting stopped after %d cycles"
      (show must)
      (show must)
      count
  show MustStopBefore {..} =
    printf
      "With option --must=%s it's expected rewriting cycles to be in range [%s], but rewriting has already reached %d cycles and is still going"
      (show must)
      (show must)
      count
  show StoppedOnLimit {..} =
    printf
      "With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --%s=%d"
      flag
      limit
  show LoopingRewriting {..} =
    printf
      "On rewriting step '%d' of rule '%s' we got the same program as we got at one of the previous step, it seems rewriting is looping\nProgram: %s"
      step
      rule
      prog

-- Build pattern and result expression and replace patterns to results in given program
buildAndReplace' :: Expression -> Expression -> [Subst] -> ReplaceProgramThrowsFunc -> ReplaceProgramContext -> IO Program
buildAndReplace' ptn res substs func ctx = do
  ptns <- buildExpressions ptn substs
  repls <- buildExpressions res substs
  let repls' = map fst repls
      ptns' = map fst ptns
  func ptns' repls' ctx

-- If pattern and replacement are appropriate for fast replacing - does it.
-- Pattern and replacement expressions can be used in fast replacing only if
-- 1. they are both formations
-- 2. they start and end with the same meta bindings, e.g. [!B1, ..., !B2]
-- 3. the does not have meta bindings between first and last meta bindings
-- In such case we can just replace bindings one by one without building whole expression.
-- You can find more details in this ticket: https://github.com/objectionary/phino/issues/321
-- If we don't meet the conditions above - just do a regular replacing
tryBuildAndReplaceFast :: Expression -> Expression -> [Subst] -> ReplaceProgramContext -> IO Program
tryBuildAndReplaceFast (ExFormation pbds) (ExFormation rbds) substs ctx =
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
          buildAndReplace' (ExFormation pbds') (ExFormation rbds') substs replaceProgramFastThrows ctx
        else do
          logDebug "Applying regular replacing..."
          buildAndReplace' (ExFormation pbds) (ExFormation rbds) substs replaceProgramThrows ctx
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
tryBuildAndReplaceFast ptn res substs ctx = buildAndReplace' ptn res substs replaceProgramThrows ctx

rewrite :: Program -> [Y.Rule] -> Set.Set Program -> Integer -> RewriteContext -> IO (Program, Set.Set Program)
rewrite program [] progs _ _ = pure (program, progs)
rewrite program (rule : rest) progs iteration ctx@RewriteContext {..} = do
  (prog, _progs) <- _rewrite program 1 progs
  rewrite prog rest _progs iteration ctx
  where
    _rewrite :: Program -> Integer -> Set.Set Program -> IO (Program, Set.Set Program)
    _rewrite prog count progs' =
      let ruleName = fromMaybe "unknown" (Y.name rule)
          ptn = Y.pattern rule
          res = Y.result rule
       in if count - 1 == _maxDepth
            then do
              logDebug (printf "Max amount of rewriting cycles (%d) for rule '%s' has been reached, rewriting is stopped" _maxDepth ruleName)
              if _depthSensitive
                then throwIO (StoppedOnLimit "max-depth" _maxDepth)
                else pure (prog, progs')
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName count _maxDepth)
              matched <- R.matchProgramWithRule prog rule (RuleContext _program _buildTerm)
              if null matched
                then do
                  logDebug (printf "Rule '%s' does not match, rewriting is stopped" ruleName)
                  pure (prog, progs')
                else do
                  logDebug (printf "Rule '%s' has been matched, applying..." ruleName)
                  prog' <- tryBuildAndReplaceFast ptn res matched (ReplaceProgramContext prog _maxDepth)
                  if prog == prog'
                    then do
                      logDebug (printf "Applied '%s', no changes made" ruleName)
                      pure (prog, progs')
                    else
                      if Set.member prog' progs
                        then throwIO (LoopingRewriting (prettyProgram' prog' SWEET UNICODE) ruleName count)
                        else do
                          logDebug
                            ( printf
                                "Applied '%s' (%d nodes -> %d nodes)"
                                ruleName
                                (countNodes prog)
                                (countNodes prog')
                            )
                          _saveStep prog' (((iteration - 1) * _maxDepth) + count)
                          _rewrite prog' (count + 1) (Set.insert prog' progs)

rewrite' :: Program -> [Y.Rule] -> RewriteContext -> IO Program
rewrite' prog rules ctx = _rewrite prog 1 Set.empty
  where
    _rewrite :: Program -> Integer -> Set.Set Program -> IO Program
    _rewrite prog count progs = do
      let cycles = _maxCycles ctx
          must = _must ctx
          current = count - 1
      if not (inRange must current) && current > 0 && exceedsUpperBound must current
        then throwIO (MustStopBefore must current)
        else
          if current == cycles
            then do
              logDebug (printf "Max amount of rewriting cycles for all rules (%d) has been reached, rewriting is stopped" cycles)
              if _depthSensitive ctx
                then throwIO (StoppedOnLimit "max-cycles" cycles)
                else pure prog
            else do
              logDebug (printf "Starting rewriting cycle for all rules: %d out of %d" count cycles)
              (rewritten, progs') <- rewrite prog rules progs count ctx
              if rewritten == prog
                then do
                  logDebug "Rewriting is stopped since it has no effect"
                  if not (inRange must current)
                    then throwIO (MustBeGoing must current)
                    else pure rewritten
                else _rewrite rewritten (count + 1) progs'
