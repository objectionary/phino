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
import IOFormat (IOFormat(..))
import Logger (logDebug)
import Matcher (MetaValue (MvAttribute, MvBindings, MvBytes, MvExpression), Subst (Subst), combine, combineMany, defaultScope, matchProgram, substEmpty, substSingle)
import Misc (ensuredFile)
import Parser (parseProgram, parseProgramThrows)
import Pretty (PrintMode (SWEET), prettyAttribute, prettyBytes, prettyExpression, prettyExpression', prettyProgram, prettyProgram', prettySubsts)
import Replacer (ReplaceProgramContext (ReplaceProgramContext), ReplaceProgramThrowsFunc, replaceProgramFastThrows, replaceProgramThrows)
import Rule (RuleContext (RuleContext), matchProgramWithRule)
import qualified Rule as R
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Term
import Text.Printf
import XMIR (XmirContext(..), programToXMIR, printXMIR)
import Yaml (ExtraArgument (..))
import qualified Yaml as Y

-- | Context for controlling the rewriting process.
-- Contains all configuration and state needed to perform program rewriting.
data RewriteContext = RewriteContext
  { -- | The initial program before any rewriting
    _program :: Program,
    -- | Maximum number of rewriting iterations per rule
    _maxDepth :: Integer,
    -- | Maximum number of rewriting cycles across all rules
    _maxCycles :: Integer,
    -- | If True, fail when reaching max depth/cycles limits; if False, just stop
    _depthSensitive :: Bool,
    -- | Function to build terms during rewriting
    _buildTerm :: BuildTermFunc,
    -- | Expected exact number of rewrites (0 means no requirement)
    _must :: Integer,
    -- | Optional directory to save intermediate rewriting steps
    _stepsDir :: Maybe FilePath,
    -- | Output format for saved steps (PHI or XMIR)
    _outputFormat :: IOFormat,
    -- | Print mode for PHI output (SWEET or SALTY)
    _printMode :: PrintMode,
    -- | Context for XMIR output generation
    _xmirContext :: XmirContext
  }

data RewriteException
  = MustBeGoing {must :: Integer, count :: Integer}
  | MustStopBefore {must :: Integer}
  | StoppedOnLimit {flag :: String, limit :: Integer}
  deriving (Exception)

instance Show RewriteException where
  show MustBeGoing {..} =
    printf
      "With option --must=%d it's expected exactly %d rewriting cycles happened, but rewriting stopped after %d"
      must
      must
      count
  show MustStopBefore {..} =
    printf
      "With option --must=%d it's expected exactly %d rewriting cycles happened, but rewriting is still going"
      must
      must
  show StoppedOnLimit {..} =
    printf
      "With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --%s=%d"
      flag
      limit

-- Save a step to file in the steps directory
saveStep :: RewriteContext -> Program -> Integer -> IO ()
saveStep ctx prog stepNum = case _stepsDir ctx of
  Nothing -> pure ()
  Just dir -> do
    createDirectoryIfMissing True dir
    let fileName = printf "%05d.%s" stepNum ext
        filePath = dir </> fileName
        ext = case _outputFormat ctx of
          PHI -> "phi"
          XMIR -> "xmir"
    content <- case _outputFormat ctx of
      PHI -> pure $ prettyProgram' prog (_printMode ctx)
      XMIR -> do
        xmir <- programToXMIR prog (_xmirContext ctx)
        pure $ printXMIR xmir
    writeFile filePath content
    logDebug (printf "Saved step %d to %s" stepNum filePath)

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

-- Version of rewrite that tracks steps for saving
rewriteWithSteps :: Program -> [Y.Rule] -> RewriteContext -> Integer -> IO Program
rewriteWithSteps program [] _ _ = pure program
rewriteWithSteps program (rule : rest) ctx stepNum = do
  (prog, nextStep) <- _rewriteRule program 1 stepNum
  rewriteWithSteps prog rest ctx nextStep
  where
    _rewriteRule :: Program -> Integer -> Integer -> IO (Program, Integer)
    _rewriteRule prog count currentStep =
      let ruleName = fromMaybe "unknown" (Y.name rule)
          ptn = Y.pattern rule
          res = Y.result rule
          depth = _maxDepth ctx
       in if count - 1 == depth
            then do
              logDebug (printf "Max amount of rewriting cycles (%d) for rule '%s' has been reached, rewriting is stopped" depth ruleName)
              if _depthSensitive ctx
                then throwIO (StoppedOnLimit "max-depth" depth)
                else pure (prog, currentStep)
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName count depth)
              matched <- R.matchProgramWithRule prog rule (RuleContext (_program ctx) (_buildTerm ctx))
              if null matched
                then do
                  logDebug (printf "Rule '%s' does not match, rewriting is stopped" ruleName)
                  pure (prog, currentStep)
                else do
                  logDebug (printf "Rule '%s' has been matched, applying..." ruleName)
                  prog' <- tryBuildAndReplaceFast ptn res matched (ReplaceProgramContext prog depth)
                  if prog == prog'
                    then do
                      logDebug (printf "Applied '%s', no changes made" ruleName)
                      pure (prog, currentStep)
                    else do
                      logDebug
                        ( printf
                            "Applied '%s' (%d nodes -> %d nodes)"
                            ruleName
                            (countNodes prog)
                            (countNodes prog')
                        )
                      -- Save the step after each successful rewrite
                      saveStep ctx prog' currentStep
                      _rewriteRule prog' (count + 1) (currentStep + 1)

-- Original rewrite function (unchanged for backward compatibility)
rewrite :: Program -> [Y.Rule] -> RewriteContext -> IO Program
rewrite program [] _ = pure program
rewrite program (rule : rest) ctx = do
  prog <- _rewrite program 1
  rewrite prog rest ctx
  where
    _rewrite :: Program -> Integer -> IO Program
    _rewrite prog count =
      let ruleName = fromMaybe "unknown" (Y.name rule)
          ptn = Y.pattern rule
          res = Y.result rule
          depth = _maxDepth ctx
       in if count - 1 == depth
            then do
              logDebug (printf "Max amount of rewriting cycles (%d) for rule '%s' has been reached, rewriting is stopped" depth ruleName)
              if _depthSensitive ctx
                then throwIO (StoppedOnLimit "max-depth" depth)
                else pure prog
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName count depth)
              matched <- R.matchProgramWithRule prog rule (RuleContext (_program ctx) (_buildTerm ctx))
              if null matched
                then do
                  logDebug (printf "Rule '%s' does not match, rewriting is stopped" ruleName)
                  pure prog
                else do
                  logDebug (printf "Rule '%s' has been matched, applying..." ruleName)
                  prog' <- tryBuildAndReplaceFast ptn res matched (ReplaceProgramContext prog depth)
                  if prog == prog'
                    then do
                      logDebug (printf "Applied '%s', no changes made" ruleName)
                      pure prog
                    else do
                      logDebug
                        ( printf
                            "Applied '%s' (%d nodes -> %d nodes)"
                            ruleName
                            (countNodes prog)
                            (countNodes prog')
                        )
                      _rewrite prog' (count + 1)

-- @todo #169:30min Memorize previous rewritten programs. Right now in order not to
--  get an infinite recursion during rewriting we just count have many times we apply
--  rewriting rules. If we reach given amount - we just stop. It's not idiomatic and may
--  not work on big programs. We need to introduce some mechanism which would memorize
--  all rewritten program on each step and if on some step we get the program that have already
--  been memorized - we fail because we got into infinite recursion. Ofc we should keep counting
--  rewriting cycles if program just only grows on each rewriting.
rewrite' :: Program -> [Y.Rule] -> RewriteContext -> IO Program
rewrite' prog rules ctx = do
  -- Save initial program as step 1
  saveStep ctx prog 1
  _rewriteWithSteps prog 1 2
  where
    _rewriteWithSteps :: Program -> Integer -> Integer -> IO Program
    _rewriteWithSteps prog count stepNum = do
      let cycles = _maxCycles ctx
          must = _must ctx
      if must /= 0 && count - 1 > must
        then throwIO (MustStopBefore must)
        else
          if count - 1 == cycles
            then do
              logDebug (printf "Max amount of rewriting cycles for all rules (%d) has been reached, rewriting is stopped" cycles)
              if _depthSensitive ctx
                then throwIO (StoppedOnLimit "max-cycles" cycles)
                else pure prog
            else do
              logDebug (printf "Starting rewriting cycle for all rules: %d out of %d" count cycles)
              -- Track the step counter while rewriting
              (rewritten, nextStepNum) <- if isJust (_stepsDir ctx)
                then do
                  -- Use step-tracking version
                  prog' <- rewriteWithSteps prog rules ctx stepNum
                  -- Calculate how many steps were taken (simplified - assuming one per successful rewrite)
                  let stepsAdded = if prog' /= prog then 1 else 0
                  pure (prog', stepNum + stepsAdded)
                else do
                  -- Use regular version when no steps directory
                  prog' <- rewrite prog rules ctx
                  pure (prog', stepNum)
              if rewritten == prog
                then do
                  logDebug "No rule matched, rewriting is stopped"
                  if must /= 0 && count - 1 /= must
                    then throwIO (MustBeGoing must (count - 1))
                    else pure rewritten
                else _rewriteWithSteps rewritten (count + 1) nextStepNum
