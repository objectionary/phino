{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Logger (logDebug)
import Matcher (MetaValue (MvAttribute, MvBindings, MvBytes, MvExpression), Subst (Subst), combine, combineMany, defaultScope, matchProgram, substEmpty, substSingle)
import Misc (ensuredFile)
import Parser (parseProgram, parseProgramThrows)
import Pretty (PrintMode (SWEET), prettyAttribute, prettyBytes, prettyExpression, prettyExpression', prettyProgram, prettyProgram', prettySubsts)
import Replacer (replaceProgram, replaceProgramThrows)
import Rule (RuleContext (RuleContext), matchProgramWithRule)
import qualified Rule as R
import Term
import Text.Printf
import Yaml (ExtraArgument (..))
import qualified Yaml as Y

data RewriteContext = RewriteContext
  { _program :: Program,
    _maxDepth :: Integer,
    _buildTerm :: BuildTermFunc,
    _must :: Integer
  }

data MustException
  = StoppedBefore {must :: Integer, count :: Integer}
  | ContinueAfter {must :: Integer}
  deriving (Exception)

instance Show MustException where
  show StoppedBefore {..} =
    printf
      "With option --must=%d it's expected exactly %d rewriting cycles happened, but rewriting stopped after %d"
      must
      must
      count
  show ContinueAfter {..} =
    printf
      "With option --must=%d it's expected exactly %d rewriting cycles happened, but rewriting is still going"
      must
      must

-- Build pattern and result expression and replace patterns to results in given program
buildAndReplace :: Program -> Expression -> Expression -> [Subst] -> IO Program
buildAndReplace program ptn res substs = do
  ptns <- buildExpressions ptn substs
  repls <- buildExpressions res substs
  let repls' = map fst repls
      ptns' = map fst ptns
  replaceProgramThrows program ptns' repls'

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
              pure prog
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName count depth)
              matched <- R.matchProgramWithRule prog rule (RuleContext (_program ctx) (_buildTerm ctx))
              if null matched
                then do
                  logDebug (printf "Rule '%s' does not match, rewriting is stoped" ruleName)
                  pure prog
                else do
                  logDebug (printf "Rule '%s' has been matched, applying..." ruleName)
                  prog' <- buildAndReplace prog ptn res matched
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
rewrite' prog rules ctx = _rewrite prog 1
  where
    _rewrite :: Program -> Integer -> IO Program
    _rewrite prog count = do
      let depth = _maxDepth ctx
          must = _must ctx
      if must /= 0 && count - 1 > must
        then throwIO (ContinueAfter must)
        else
          if count - 1 == depth
            then do
              logDebug (printf "Max amount of rewriting cycles for all rules (%d) has been reached, rewriting is stopped" depth)
              pure prog
            else do
              logDebug (printf "Starting rewriting cycle for all rules: %d out of %d" count depth)
              rewritten <- rewrite prog rules ctx
              if rewritten == prog
                then do
                  logDebug "No rule matched, rewriting is stopped"
                  if must /= 0 && count - 1 /= must
                    then throwIO (StoppedBefore must (count - 1))
                    else pure rewritten
                else _rewrite rewritten (count + 1)
