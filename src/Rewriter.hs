{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite, rewrite', RewriteContext (..), Rewritten (..)) where

import AST
import Builder
import Control.Exception (Exception, throwIO)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import Deps
import Logger (logDebug)
import Matcher (MetaValue (MvAttribute, MvBindings, MvBytes, MvExpression), Subst (Subst), combine, combineMany, defaultScope, matchProgram, substEmpty, substSingle)
import Misc (ensuredFile)
import Must (Must (..), exceedsUpperBound, inRange)
import Parser (parseProgram, parseProgramThrows)
import Printer (printProgram)
import Replacer (ReplaceProgramContext (ReplaceProgramContext), ReplaceProgramThrowsFunc, replaceProgramFastThrows, replaceProgramThrows)
import Rule (RuleContext (RuleContext), matchProgramWithRule)
import qualified Rule as R
import Text.Printf
import Yaml (ExtraArgument (..))
import qualified Yaml as Y

type RewriteState = ([Rewritten], Set.Set Program)

data Rewritten = Rewritten
  { program :: Program,
    maybeRule :: Maybe Y.Rule
  }

instance Eq Rewritten where
  left == right = program left == program right

data RewriteContext = RewriteContext
  { _maxDepth :: Integer,
    _maxCycles :: Integer,
    _depthSensitive :: Bool,
    _sequence :: Bool,
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

-- The function returns tuple (X, Y) where
-- - X is sequence of programs; for more details about this sequence see description of rewrite' function
-- - Y is Set of unique programs after each rule application. It allows to stop the rewriting if we're getting
--   into loop and get back to program which we've already got before
rewrite :: RewriteState -> [Y.Rule] -> Integer -> RewriteContext -> IO RewriteState
rewrite state [] _ _ = pure state
rewrite state (rule : rest) iteration ctx@RewriteContext {..} = do
  state' <- _rewrite state 1
  rewrite state' rest iteration ctx
  where
    _rewrite :: RewriteState -> Integer -> IO RewriteState
    _rewrite (_rewrittens, _unique) _count =
      let ruleName = fromMaybe "unknown" (Y.name rule)
          ptn = Y.pattern rule
          res = Y.result rule
          Rewritten {..} = head _rewrittens
       in if _count - 1 == _maxDepth
            then do
              logDebug (printf "Max amount of rewriting cycles (%d) for rule '%s' has been reached, rewriting is stopped" _maxDepth ruleName)
              if _depthSensitive
                then throwIO (StoppedOnLimit "max-depth" _maxDepth)
                else pure (_rewrittens, _unique)
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName _count _maxDepth)
              matched <- R.matchProgramWithRule program rule (RuleContext _buildTerm)
              if null matched
                then do
                  logDebug (printf "Rule '%s' does not match, rewriting is stopped" ruleName)
                  pure (_rewrittens, _unique)
                else do
                  logDebug (printf "Rule '%s' has been matched, applying..." ruleName)
                  prog <- tryBuildAndReplaceFast ptn res matched (ReplaceProgramContext program _maxDepth)
                  if program == prog
                    then do
                      logDebug (printf "Applied '%s', no changes made" ruleName)
                      pure (_rewrittens, _unique)
                    else
                      if Set.member prog _unique
                        then throwIO (LoopingRewriting (printProgram prog) ruleName _count)
                        else do
                          logDebug
                            ( printf
                                "Applied '%s' (%d nodes -> %d nodes)\n%s"
                                ruleName
                                (countNodes program)
                                (countNodes prog)
                                (printProgram prog)
                            )
                          _saveStep prog (((iteration - 1) * _maxDepth) + _count)
                          _rewrite (rewriteSequence prog, Set.insert prog _unique) (_count + 1)
      where
        rewriteSequence :: Program -> [Rewritten]
        rewriteSequence _prog
          | _sequence =
              let Rewritten {..} : rest = _rewrittens
               in Rewritten _prog Nothing : Rewritten program (Just rule) : rest
          | otherwise = [Rewritten _prog Nothing]

-- The function accepts single program but returns sequence of programs
-- If RewriteContext has _sequence == True - all the intermediate
-- programs after each rule application are included into sequence
-- Otherwise sequence contains only one program
rewrite' :: Program -> [Y.Rule] -> RewriteContext -> IO [Rewritten]
rewrite' prog rules ctx = _rewrite ([Rewritten prog Nothing], Set.empty) 1 ctx <&> reverse
  where
    _rewrite :: RewriteState -> Integer -> RewriteContext -> IO [Rewritten]
    _rewrite state@(rewrittens, unique) count ctx@RewriteContext{..} = do
      let cycles = _maxCycles
          must = _must
          current = count - 1
      if not (inRange must current) && current > 0 && exceedsUpperBound must current
        then throwIO (MustStopBefore must current)
        else
          if current == cycles
            then do
              logDebug (printf "Max amount of rewriting cycles for all rules (%d) has been reached, rewriting is stopped" cycles)
              if _depthSensitive
                then throwIO (StoppedOnLimit "max-cycles" cycles)
                else pure rewrittens
            else do
              logDebug (printf "Starting rewriting cycle for all rules: %d out of %d" count cycles)
              state'@(rewrittens', unique') <- rewrite state rules count ctx
              if head rewrittens' == head rewrittens
                then do
                  logDebug "Rewriting is stopped since it has no effect"
                  if not (inRange must current)
                    then throwIO (MustBeGoing must current)
                    else pure rewrittens'
                else _rewrite state' (count + 1) ctx
