{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite, rewrite', RewriteContext(..), defaultRewriteContext) where

import Ast
import Builder
import qualified Condition as C
import Control.Exception
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Logger (logDebug)
import Matcher (MetaValue (MvAttribute, MvBindings, MvExpression), Subst (Subst), combine, combineMany, defaultScope, matchProgram, substEmpty, substSingle)
import Misc (ensuredFile)
import Parser (parseProgram, parseProgramThrows)
import Pretty (PrintMode (SWEET), prettyExpression, prettyProgram, prettyProgram', prettySubsts)
import Replacer (replaceProgram)
import Text.Printf
import qualified Yaml as Y

data RewriteContext = RewriteContext
  { program :: Program,
    maxDepth :: Integer
  }

defaultRewriteContext :: Program -> RewriteContext
defaultRewriteContext prog = RewriteContext prog 25

data RewriteException
  = CouldNotBuild {expr :: Expression, substs :: [Subst]}
  | CouldNotReplace {prog :: Program, ptn :: Expression, res :: Expression}
  deriving (Exception)

instance Show RewriteException where
  show CouldNotBuild {..} =
    printf
      "Couldn't build given expression with provided substitutions\n--Expression: %s\n--Substitutions: %s"
      (prettyExpression expr)
      (prettySubsts substs)
  show CouldNotReplace {..} =
    printf
      "Couldn't replace expression in program by pattern\nProgram: %s\n--Pattern: %s\n--Result: %s"
      (prettyProgram prog)
      (prettyExpression ptn)
      (prettyExpression res)

-- Build pattern and result expression and replace patterns to results in given program
buildAndReplace :: Program -> Expression -> Expression -> [Subst] -> IO Program
buildAndReplace program ptn res substs =
  case (buildExpressions ptn substs, buildExpressions res substs) of
    (Just ptns, Just repls) ->
      let repls' = map fst repls
          ptns' = map fst ptns
       in case replaceProgram program ptns' repls' of
            Just prog -> pure prog
            _ -> throwIO (CouldNotReplace program ptn res)
    (Nothing, _) -> throwIO (CouldNotBuild ptn substs)
    (_, Nothing) -> throwIO (CouldNotBuild res substs)

-- Extend list of given substitutions with extra substitutions from 'where' yaml rule section
extraSubstitutions :: Program -> Maybe [Y.Extra] -> [Subst] -> [Subst]
extraSubstitutions prog extras substs = case extras of
  Nothing -> substs
  Just extras' ->
    catMaybes
      [ foldl
          ( \(Just subst') extra -> case Y.meta extra of
              ExMeta name -> do
                let func = Y.function extra
                    args = Y.args extra
                expr <- buildExpressionFromFunction func args subst' prog
                combine (substSingle name (MvExpression expr defaultScope)) subst'
              _ -> Just subst'
          )
          (Just subst)
          extras'
        | subst <- substs
      ]

rewrite :: Program -> Program -> [Y.Rule] -> IO Program
rewrite program _ [] = pure program
rewrite program program' (rule : rest) = do
  let ptn = Y.pattern rule
      res = Y.result rule
      condition = Y.when rule
  prog <- case C.matchProgramWithCondition ptn condition program of
    Nothing -> pure program
    Just matched -> do
      let substs = extraSubstitutions program' (Y.where_ rule) matched
      prog' <- buildAndReplace program ptn res substs
      logDebug (printf "%s\n%s" (fromMaybe "unknown" (Y.name rule)) (prettyProgram' prog' SWEET))
      pure prog'
  rewrite prog program' rest

-- @todo #169:30min Memorize previous rewritten programs. Right now in order not to
--  get an infinite recursion during rewriting we just count have many times we apply
--  rewriting rules. If we reach given amount - we just stop. It's not idiomatic and may
--  not work on big programs. We need to introduce some mechanism which would memorize
--  all rewritten program on each step and if on some step we get the program that have already
--  been memorized - we fail because we got into infinite recursion. Ofc we should keep counting
--  rewriting cycles if program just only grows on each rewriting.
rewrite' :: Program -> [Y.Rule] -> RewriteContext -> IO Program
rewrite' prog rules RewriteContext{..} = _rewrite prog 0
  where
    _rewrite :: Program -> Integer -> IO Program
    _rewrite prog count = do
      logDebug (printf "Starting rewriting cycle %d out of %d" count maxDepth)
      if count == maxDepth
        then do
          logDebug (printf "Max amount of rewriting cycles is reached, rewriting is stopped")
          pure prog
        else do
          rewritten <- rewrite prog program rules
          if rewritten == prog
            then do
              logDebug "Rewriting is stopped since it does not affect program anymore"
              pure rewritten
            else _rewrite rewritten (count + 1)
