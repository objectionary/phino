{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite) where

import Ast
import Builder
import Control.Exception
import Data.Text (intercalate)
import Matcher (Subst (Subst), matchProgram, MetaValue (MvAttribute))
import Misc (ensuredFile)
import Parser (parseProgram, parseProgramThrows)
import Printer (printExpression, printProgram, printSubstitutions)
import Replacer (replaceProgram)
import System.Directory
import Text.Printf
import qualified Yaml as Y
import qualified Data.Map.Strict as M

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

-- Check if given attribute is present in given binding
attrInBinding :: Attribute -> Binding -> Bool
attrInBinding attr (BiTau battr _) = attr == battr
attrInBinding attr (BiVoid battr) = attr == battr
attrInBinding AtLambda (BiLambda _) = True
attrInBinding AtDelta (BiDelta _) = True
attrInBinding _ _ = False

-- Check if all given attributes are present in given bindings
attrsInBindings :: [Attribute] -> [Binding] -> Bool
attrsInBindings [] _ = True
attrsInBindings attrs [] = False
attrsInBindings [attr] (bd : rest) = attrInBinding attr bd || attrsInBindings [attr] rest
attrsInBindings (attr : rest) bds = attrsInBindings [attr] bds && attrsInBindings rest bds

-- For each substitution check if it meets to given condition
-- If substitution does not meet the condition - it's thrown out
-- and is not used in replacement
meets :: Y.Condition -> [Subst] -> [Subst]
meets _ [] = []
-- OR
meets (Y.Or []) substs = substs
meets (Y.Or (cond : rest)) [subst] = case meets cond [subst] of
  [] -> meets (Y.Or rest) [subst]
  substs -> substs
-- AND
meets (Y.And []) substs = substs
meets (Y.And (cond : rest)) [subst] = case meets cond [subst] of
  [] -> []
  _ -> meets (Y.And rest) [subst]
-- NOT
meets (Y.Not cond) [subst] = case meets cond [subst] of
  [] -> [subst]
  _ -> []
-- IN
meets (Y.In attrs bindings) [subst] =
  case (traverse (`buildAttribute` subst) attrs, buildBindings bindings subst) of
    (Just attrs', Just bds) -> [subst | attrsInBindings attrs' bds] -- if attrsInBindings attrs' bds then [subst] else []
    (_, _) -> []
meets (Y.In attrs bindings) (subst : rest) = do
  let cond = Y.In attrs bindings
      substs = meets cond [subst]
  head substs : meets cond rest
-- ALPHA
meets (Y.Alpha (AtAlpha _)) substs = substs
meets (Y.Alpha (AtMeta name)) [Subst mp] = case M.lookup name mp of
  Just (MvAttribute (AtAlpha _)) -> [Subst mp]
  _ -> []
meets (Y.Alpha _) _ = []
-- EQ
meets (Y.Eq (AtMeta left) (AtMeta right)) [subst] = [subst | left == right]
meets (Y.Eq attr (AtMeta meta)) [Subst mp] = case M.lookup meta mp of
  Just (MvAttribute found) -> [Subst mp | attr == found]
  _ -> []
meets (Y.Eq (AtMeta meta) attr) [Subst mp] = case M.lookup meta mp of
  Just (MvAttribute found) -> [Subst mp | attr == found]
  _ -> []
meets (Y.Eq left right) [subst] = [subst | right == left]
-- Any condition with many substitutions
meets cond (subst : rest) = do
  let first = meets cond [subst]
      next = meets cond rest
  if null first
    then next
    else head first : next

-- Build pattern and result expression and replace patterns to results in given program
buildAndReplace :: Program -> Expression -> Expression -> [Subst] -> IO Program
buildAndReplace program ptn res substs =
  case (buildExpressions ptn substs, buildExpressions res substs) of
    (Just ptns, Just repls) -> case replaceProgram program ptns repls of
      Just prog -> pure prog
      _ -> throwIO (CouldNotReplace program ptn res)
    (Nothing, _) -> throwIO (CouldNotBuild ptn substs)
    (_, Nothing) -> throwIO (CouldNotBuild res substs)

applyRules :: Program -> [Y.Rule] -> IO Program
applyRules program [] = pure program
applyRules program [rule] = do
  let ptn = Y.pattern rule
      res = Y.result rule
  case matchProgram ptn program of
    [] -> pure program
    substs -> do
      let replaced = buildAndReplace program ptn res
      case Y.when rule of
        Just cond -> case meets cond substs of
          [] -> pure program
          substs' -> replaced substs'
        Nothing -> replaced substs
applyRules program (rule : rest) = do
  prog <- applyRules program [rule]
  applyRules prog rest

rewrite :: String -> [Y.Rule] -> IO String
rewrite prog rules = do
  program <- parseProgramThrows prog
  rewritten <- applyRules program rules
  pure (printProgram rewritten)
