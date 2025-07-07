{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite, rewrite') where

import Ast
import Builder
import qualified Condition as C
import Control.Exception
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Logger (logDebug)
import Matcher (MetaValue (MvAttribute, MvBindings, MvExpression), Subst (Subst), combine, combineMany, matchProgram, substEmpty, substSingle)
import Misc (ensuredFile)
import Parser (parseProgram, parseProgramThrows)
import Pretty (prettyExpression, prettyProgram, prettySubsts)
import Replacer (replaceProgram)
import Text.Printf
import qualified Yaml as Y

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
                combine (substSingle name (MvExpression expr (ExFormation []))) subst'
              _ -> Just subst'
          )
          (Just subst)
          extras'
        | subst <- substs
      ]

rewrite :: Program -> [Y.Rule] -> IO Program
rewrite program [] = pure program
rewrite program (rule : rest) = do
  logDebug (printf "Trying to apply rule: %s" (fromMaybe "unknown" (Y.name rule)))
  let ptn = Y.pattern rule
      res = Y.result rule
      condition = Y.when rule
  prog <- case C.matchProgramWithCondition ptn condition program of
    Nothing -> do
      logDebug "Rule didn't match"
      pure program
    Just matched -> do
      let substs = extraSubstitutions program (Y.where_ rule) matched
      logDebug (printf "Rule has been matched, substitutions are:\n%s" (prettySubsts substs))
      buildAndReplace program ptn res substs
  rewrite prog rest

rewrite' :: Program -> [Y.Rule] -> Integer -> IO Program
rewrite' prog rules maxDepth = _rewrite 0
  where
    _rewrite :: Integer -> IO Program
    _rewrite count = do
      logDebug (printf "Starting rewriting cycle %d out of %d" count maxDepth)
      if count == maxDepth
        then do
          logDebug (printf "Max amount of rewriting cycles is reached, rewriting is stopped")
          pure prog
        else do
          rewritten <- rewrite prog rules
          if rewritten == prog
            then do
              logDebug "Rewriting is stopped since it does not affect program anymore"
              pure rewritten
            else rewrite' rewritten rules (count + 1)
