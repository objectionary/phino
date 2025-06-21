-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Condition where

import Ast
import Builder (buildAttribute, buildBinding)
import Data.Aeson (FromJSON)
import qualified Data.Map.Strict as M
import GHC.IO (unsafePerformIO)
import Matcher
import Misc (allPathsIn)
import Pretty (prettyExpression, prettySubsts)
import Yaml (normalizationRules)
import qualified Yaml as Y

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

-- Convert Number to Integer
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

meetCondition' :: Y.Condition -> Subst -> [Subst]
meetCondition' (Y.Or []) subst = [subst]
meetCondition' (Y.Or (cond : rest)) subst = do
  let met = meetCondition' cond subst
  if null met
    then meetCondition' (Y.Or rest) subst
    else met
meetCondition' (Y.And []) subst = [subst]
meetCondition' (Y.And (cond : rest)) subst = do
  let met = meetCondition' cond subst
  if null met
    then []
    else meetCondition' (Y.And rest) subst
meetCondition' (Y.Not cond) subst = do
  let met = meetCondition' cond subst
  [subst | null met]
meetCondition' (Y.In attr binding) subst =
  case (buildAttribute attr subst, buildBinding binding subst) of
    (Just attr, Just bds) -> [subst | attrInBindings attr bds] -- if attrInBindings attr bd then [subst] else []
    (_, _) -> []
meetCondition' (Y.Alpha (AtAlpha _)) subst = [subst]
meetCondition' (Y.Alpha (AtMeta name)) (Subst mp) = case M.lookup name mp of
  Just (MvAttribute (AtAlpha _)) -> [Subst mp]
  _ -> []
meetCondition' (Y.Alpha _) _ = []
meetCondition' (Y.Eq (Y.CmpNum left) (Y.CmpNum right)) subst = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> [subst | left_ == right_]
  (_, _) -> []
meetCondition' (Y.Eq (Y.CmpAttr left) (Y.CmpAttr right)) subst = [subst | compareAttrs left right subst]
meetCondition' (Y.Eq _ _) _ = []
-- @todo #89:30min Extend list of expressions. There are expressions where we can definitely say
--  if this expression in normal form or not. In common case the expression in normal form if
--  it does not match with any of normalization rules. But it's quite expensive operation comparing to
--  simple list filtering and pattern matching. For example if expression is formation where all bindings are
--  void, lambda, or delta - this expression in normal form and there's no need to try to match it with normalization
--  rules. So we need find more such cases and introduce them.
meetCondition' (Y.NF (ExMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvExpression expr) -> case expr of
    ExThis -> [Subst mp]
    ExGlobal -> [Subst mp]
    ExTermination -> [Subst mp]
    ExDispatch ExThis _ -> [Subst mp]
    ExDispatch ExGlobal _ -> [Subst mp]
    ExDispatch ExTermination _ -> [] -- dd rule
    ExApplication ExTermination _ -> [] -- dc rule
    ExFormation [] -> [Subst mp]
    _ -> [Subst mp | not (matchesAnyNormalizationRule expr normalizationRules)]
  _ -> []
  where
    -- Returns True if given expression matches with any of given normalization rules
    matchesAnyNormalizationRule :: Expression -> [Y.Rule] -> Bool
    matchesAnyNormalizationRule _ [] = False
    matchesAnyNormalizationRule expr (rule : rules) =
      case matchProgramWithCondition (Y.pattern rule) (Y.when rule) (Program expr) of
        Just matched -> not (null matched) || matchesAnyNormalizationRule expr rules
        Nothing -> matchesAnyNormalizationRule expr rules
meetCondition' (Y.NF _) _ = []
meetCondition' (Y.FN (ExMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvExpression expr) -> meetCondition' (Y.FN expr) (Subst mp)
  _ -> []
meetCondition' (Y.FN (ExFormation _)) subst = [subst]
meetCondition' (Y.FN ExThis) subst = []
meetCondition' (Y.FN ExGlobal) subst = [subst]
meetCondition' (Y.FN (ExApplication expr (BiTau attr texpr))) subst = do
  let onExpr = meetCondition' (Y.FN expr) subst
      onTau = meetCondition' (Y.FN texpr) subst
  [subst | not (null onExpr) && not (null onTau)]
meetCondition' (Y.FN (ExDispatch expr _)) subst = meetCondition' (Y.FN expr) subst

-- For each substitution check if it meetCondition to given condition
-- If substitution does not meet the condition - it's thrown out
-- and is not used in replacement
meetCondition :: Y.Condition -> [Subst] -> [Subst]
meetCondition _ [] = []
meetCondition cond (subst : rest) = do
  let first = meetCondition' cond subst
  let next = meetCondition cond rest
  if null first
    then next
    else head first : next

-- Returns Just [...] if
-- 1. program matches pattern and
-- 2.1. condition is not present, or
-- 2.2. condition is present and met
-- Otherwise returns Nothing
matchProgramWithCondition :: Expression -> Maybe Y.Condition -> Program -> Maybe [Subst]
matchProgramWithCondition ptn condition program = do
  let matched = matchProgram ptn program
  if null matched
    then Nothing
    else case condition of
      Nothing -> Just matched
      Just cond -> do
        let met = meetCondition cond matched
        if null met
          then Nothing
          else Just met
