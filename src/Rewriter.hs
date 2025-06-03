{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite) where

import Ast
import Builder (buildExpressions)
import Control.Exception
import Matcher (Subst, matchProgram)
import Misc (ensuredFile)
import Parser (parseProgram, parseProgramThrows)
import Printer (printExpression, printProgram, printSubstitutions)
import Replacer (replaceProgram)
import System.Directory
import Text.Printf
import qualified Yaml as Y
import Yaml (Rule)

data RewriteException
  = CouldNotMatch {pattern :: Expression, program :: Program}
  | CouldNotBuild {expr :: Expression, substs :: [Subst]}
  | CouldNotReplace {prog :: Program, ptn :: Expression, res :: Expression}
  deriving (Exception)

instance Show RewriteException where
  show CouldNotMatch {..} =
    printf
      "Couldn't find given pattern in provided program\n--Pattern: %s\n--Program: %s"
      (printExpression pattern)
      (printProgram program)
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

applyRules :: Program -> [Y.Rule] -> IO Program
applyRules program [] = pure program
applyRules program [rule] = do
  let ptn = Y.pattern rule
  let res = Y.result rule
  case matchProgram ptn program of
    [] -> throwIO (CouldNotMatch ptn program)
    substs -> case (buildExpressions ptn substs, buildExpressions res substs) of
      (Just ptns, Just repls) -> case replaceProgram program ptns repls of
        Just prog -> pure prog
        _ -> throwIO (CouldNotReplace program ptn res)
      (Nothing, _) -> throwIO (CouldNotBuild ptn substs)
      (_, Nothing) -> throwIO (CouldNotBuild res substs)
applyRules program (rule : rest) = do
  prog <- applyRules program [rule]
  applyRules prog rest

rewrite :: String -> [Y.Rule] -> IO String
rewrite prog rules = do
  program <- parseProgramThrows prog
  rewritten <- applyRules program rules
  pure (printProgram rewritten)
