{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 ObjectionarR.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite) where

import Ast
import Builder
import Control.Exception
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import Data.Text (intercalate)
import Matcher (MetaValue (MvAttribute, MvBindings, MvExpression), Subst (Subst), combine, combineMany, matchProgram, substEmpty, substSingle)
import Misc (ensuredFile)
import Parser (parseProgram, parseProgramThrows)
import Printer (printExpression, printProgram, printSubstitutions)
import Replacer (replaceProgram)
import qualified Rule as R
import Text.Printf

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
extraSubstitutions :: Program -> Maybe [R.Extra] -> [Subst] -> [Subst]
extraSubstitutions prog extras substs = case extras of
  Nothing -> substs
  Just extras' -> do
    catMaybes
      [ case R.meta extra of
          ExMeta name -> do
            let func = R.function extra
                args = R.args extra
            expr <- buildExpressionFromFunction func args subst prog
            combine (substSingle name (MvExpression expr)) subst
          _ -> Just subst
        | subst <- substs,
          extra <- extras'
      ]

applyRules :: Program -> [R.Rule] -> IO Program
applyRules program [] = pure program
applyRules program (rule : rest) = do
  let ptn = R.pattern rule
      res = R.result rule
      condition = R.when rule
      replaced = buildAndReplace program ptn res
      extended = extraSubstitutions program (R.where_ rule)
  prog <- case R.matchProgramWithCondition ptn condition program of
    Nothing -> pure program
    Just matched -> replaced (extended matched)
  applyRules prog rest

rewrite :: String -> [R.Rule] -> IO Program
rewrite prog rules = do
  program <- parseProgramThrows prog
  applyRules program rules
