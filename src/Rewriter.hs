{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (rewrite, meets) where

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
import System.Directory
import Text.Printf
import Yaml (Comparable (CmpAttr))
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

-- Check if given attribute is present in given binding
attrInBindings :: Attribute -> [Binding] -> Bool
attrInBindings attr (bd : bds) = attrInBinding attr bd || attrInBindings attr bds
  where
    attrInBinding :: Attribute -> Binding -> Bool
    attrInBinding attr (BiTau battr _) = attr == battr
    attrInBinding attr (BiVoid battr) = attr == battr
    attrInBinding AtLambda (BiLambda _) = True
    attrInBinding AtDelta (BiDelta _) = True
    attrInBinding _ _ = False
attrInBindings _ _ = False

-- Apply 'eq' yaml condition to attributes
compareAttrs :: Attribute -> Attribute -> Subst -> Bool
compareAttrs (AtMeta left) (AtMeta right) _ = left == right
compareAttrs attr (AtMeta meta) (Subst mp) = case M.lookup meta mp of
  Just (MvAttribute found) -> attr == found
  _ -> False
compareAttrs (AtMeta meta) attr (Subst mp) = case M.lookup meta mp of
  Just (MvAttribute found) -> attr == found
  _ -> False
compareAttrs left right subst = right == left

-- Convert Y.Number to Integer
numToInt :: Y.Number -> Subst -> Maybe Integer
numToInt (Y.Ordinal (AtMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvAttribute (AtAlpha idx)) -> Just idx
  _ -> Nothing
numToInt (Y.Ordinal (AtAlpha idx)) subst = Just idx
numToInt (Y.Length (BiMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvBindings bds) -> Just (toInteger (length bds))
  _ -> Nothing
numToInt (Y.Add left right) subst = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> Just (left_ + right_)
  _ -> Nothing
numToInt (Y.Literal num) subst = Just num
numToInt _ _ = Nothing

-- For each substitution check if it meets to given condition
-- If substitution does not meet the condition - it's thrown out
-- and is not used in replacement
meets :: Y.Condition -> [Subst] -> [Subst]
meets _ [] = []
meets (Y.Or []) substs = substs
meets (Y.Or (cond : rest)) [subst] = do
  let met = meets cond [subst]
  if null met
    then meets (Y.Or rest) [subst]
    else met
meets (Y.And []) substs = substs
meets (Y.And (cond : rest)) [subst] = do
  let met = meets cond [subst]
  if null met
    then []
    else meets (Y.And rest) [subst]
meets (Y.Not cond) [subst] = do
  let met = meets cond [subst]
  [subst | null met]
meets (Y.In attr binding) [subst] =
  case (buildAttribute attr subst, buildBinding binding subst) of
    (Just attr, Just bds) -> [subst | attrInBindings attr bds] -- if attrInBindings attr bd then [subst] else []
    (_, _) -> []
meets (Y.Alpha (AtAlpha _)) substs = substs
meets (Y.Alpha (AtMeta name)) [Subst mp] = case M.lookup name mp of
  Just (MvAttribute (AtAlpha _)) -> [Subst mp]
  _ -> []
meets (Y.Alpha _) _ = []
meets (Y.Eq (Y.CmpNum left) (Y.CmpNum right)) [subst] = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> [subst | left_ == right_]
  (_, _) -> []
meets (Y.Eq (Y.CmpAttr left) (Y.CmpAttr right)) [subst] = [subst | compareAttrs left right subst]
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

applyRules :: Program -> [Y.Rule] -> IO Program
applyRules program [] = pure program
applyRules program (rule : rest) = do
  let ptn = Y.pattern rule
      res = Y.result rule
      matched = matchProgram ptn program
      condition = Y.when rule
      replaced = buildAndReplace program ptn res
      extended = extraSubstitutions program (Y.where_ rule)
  prog <-
    if null matched
      then pure program
      else case condition of
        Nothing -> replaced (extended matched)
        Just cond -> do
          let met = meets cond matched
          if null met
            then pure program
            else replaced (extended met)
  applyRules prog rest

rewrite :: String -> [Y.Rule] -> IO Program
rewrite prog rules = do
  program <- parseProgramThrows prog
  applyRules program rules
