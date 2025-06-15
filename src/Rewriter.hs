{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite) where

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
import Printer (printExpression, printProgram, printSubstitutions)
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
      (printExpression expr)
      (printSubstitutions substs)
  show CouldNotReplace {..} =
    printf
      "Couldn't replace expression in program by pattern\nProgram: %s\n--Pattern: %s\n--Result: %s"
      (printProgram prog)
      (printExpression ptn)
      (printExpression res)

-- Build pattern and result expression and replace patterns to results in given program
buildAndReplace :: Program -> Expression -> Expression -> [Subst] -> IO Program
buildAndReplace program ptn res substs =
  case (buildExpressions ptn substs, buildExpressions res substs) of
    (Just ptns, Just repls) -> case replaceProgram program ptns repls of
      Just prog -> pure prog
      _ -> throwIO (CouldNotReplace program ptn res)
    (Nothing, _) -> throwIO (CouldNotBuild ptn substs)
    (_, Nothing) -> throwIO (CouldNotBuild res substs)

-- Extend list of given substitutions with extra substitutions from 'where' yaml rule section
extraSubstitutions :: Program -> Maybe [Y.Extra] -> [Subst] -> [Subst]
extraSubstitutions prog extras substs = case extras of
  Nothing -> substs
  Just extras' -> do
    catMaybes
      [ case Y.meta extra of
          ExMeta name -> do
            let func = Y.function extra
                args = Y.args extra
            expr <- buildExpressionFromFunction func args subst prog
            combine (substSingle name (MvExpression expr)) subst
          _ -> Just subst
        | subst <- substs,
          extra <- extras'
      ]

rewrite :: Program -> [Y.Rule] -> IO Program
rewrite program [] = pure program
rewrite program (rule : rest) = do
  logDebug (printf "Trying to apply rule: %s" (fromMaybe "unknown" (Y.name rule)))
  let ptn = Y.pattern rule
      res = Y.result rule
      condition = Y.when rule
      replaced = buildAndReplace program ptn res
      extended = extraSubstitutions program (Y.where_ rule)
  prog <- case C.matchProgramWithCondition ptn condition program of
    Nothing -> do
      logDebug "Rule didn't match"
      pure program
    Just matched -> do
      let substs = extended matched
      logDebug (printf "Rule has been matched, substitutions are:\n%s" (printSubstitutions substs))
      replaced substs
  rewrite prog rest
