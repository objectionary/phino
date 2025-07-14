{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite, rewrite', RewriteContext (..)) where

import Ast
import Builder
import qualified Condition as C
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Debug.Trace (trace)
import Logger (logDebug)
import Matcher (MetaValue (MvAttribute, MvBindings, MvExpression, MvBytes), Subst (Subst), combine, combineMany, defaultScope, matchProgram, substEmpty, substSingle)
import Misc (ensuredFile)
import Parser (parseProgram, parseProgramThrows)
import Pretty (PrintMode (SWEET), prettyAttribute, prettyExpression, prettyExpression', prettyProgram, prettyProgram', prettySubsts, prettyBytes)
import Replacer (replaceProgram, replaceProgramThrows)
import Term
import Text.Printf
import Yaml (ExtraArgument (..))
import qualified Yaml as Y

data RewriteContext = RewriteContext
  { _program :: Program,
    _maxDepth :: Integer,
    _buildTerm :: BuildTermFunc
  }

-- Build pattern and result expression and replace patterns to results in given program
buildAndReplace :: Program -> Expression -> Expression -> [Subst] -> IO Program
buildAndReplace program ptn res substs = do
  ptns <- buildExpressions ptn substs
  repls <- buildExpressions res substs
  let repls' = map fst repls
      ptns' = map fst ptns
  replaceProgramThrows program ptns' repls'

-- Extend list of given substitutions with extra substitutions from 'where' yaml rule section
extraSubstitutions :: Maybe [Y.Extra] -> [Subst] -> RewriteContext -> IO [Subst]
extraSubstitutions extras substs RewriteContext {..} = case extras of
  Nothing -> pure substs
  Just extras' -> do
    res <-
      sequence
        [ foldlM
            ( \(Just subst') extra -> do
                let maybeName = case Y.meta extra of
                      ArgExpression (ExMeta name) -> Just name
                      ArgAttribute (AtMeta name) -> Just name
                      ArgBinding (BiMeta name) -> Just name
                      ArgBytes (BtMeta name) -> Just name
                      _ -> Nothing
                    func = Y.function extra
                    args = Y.args extra
                term <- _buildTerm func args subst' _program
                meta <- case term of
                  TeExpression expr -> do
                    logDebug (printf "Function %s() returned expression:\n%s" func (prettyExpression' expr))
                    pure (MvExpression expr defaultScope)
                  TeAttribute attr -> do
                    logDebug (printf "Function %s() returned attribute:\n%s" func (prettyAttribute attr))
                    pure (MvAttribute attr)
                  TeBytes bytes -> do
                    logDebug (printf "Function %s() returned bytes: %s" func (prettyBytes bytes))
                    pure (MvBytes bytes)
                case maybeName of
                  Just name -> pure (combine (substSingle name meta) subst')
                  _ -> pure Nothing
            )
            (Just subst)
            extras'
          | subst <- substs
        ]
    pure (catMaybes res)

rewrite :: Program -> [Y.Rule] -> RewriteContext -> IO Program
rewrite program [] _ = pure program
rewrite program (rule : rest) ctx = do
  let ptn = Y.pattern rule
      res = Y.result rule
      condition = Y.when rule
  prog <- case C.matchProgramWithCondition ptn condition program of
    Nothing -> pure program
    Just matched -> do
      let ruleName = fromMaybe "unknown" (Y.name rule)
      logDebug (printf "Rule %s has been matched, applying..." ruleName)
      substs <- extraSubstitutions (Y.where_ rule) matched ctx
      prog' <- buildAndReplace program ptn res substs
      if program == prog'
        then logDebug (printf "Applied %s, no changes made" ruleName)
        else
          logDebug
            ( printf
                "Applied %s (%d nodes -> %d nodes):\n%s"
                ruleName
                (countNodes program)
                (countNodes prog')
                (prettyProgram' prog' SWEET)
            )
      pure prog'
  rewrite prog rest ctx

-- @todo #169:30min Memorize previous rewritten programs. Right now in order not to
--  get an infinite recursion during rewriting we just count have many times we apply
--  rewriting rules. If we reach given amount - we just stop. It's not idiomatic and may
--  not work on big programs. We need to introduce some mechanism which would memorize
--  all rewritten program on each step and if on some step we get the program that have already
--  been memorized - we fail because we got into infinite recursion. Ofc we should keep counting
--  rewriting cycles if program just only grows on each rewriting.
rewrite' :: Program -> [Y.Rule] -> RewriteContext -> IO Program
rewrite' prog rules ctx = _rewrite prog 0
  where
    _rewrite :: Program -> Integer -> IO Program
    _rewrite prog count = do
      let depth = _maxDepth ctx
      logDebug (printf "Starting rewriting cycle %d out of %d" count depth)
      if count == depth
        then do
          logDebug (printf "Max amount of rewriting cycles has been reached, rewriting is stopped")
          pure prog
        else do
          rewritten <- rewrite prog rules ctx
          if rewritten == prog
            then do
              logDebug "No rule matched, rewriting is stopped"
              pure rewritten
            else _rewrite rewritten (count + 1)
