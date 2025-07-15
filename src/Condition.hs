{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Condition where

import Ast
import Builder (buildAttribute, buildBinding)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Functions (buildTermFromFunction)
import GHC.IO (unsafePerformIO)
import Matcher
import Misc (allPathsIn, btsToUnescapedStr)
import Pretty (prettyExpression, prettySubsts)
import Regexp (match)
import Term (Term (TeBytes))
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
numToInt (Y.Literal num) subst = Just num
numToInt _ _ = Nothing

-- Returns True if given expression matches with any of given normalization rules
-- Here we use unsafePerformIO because we're sure that conditions which are used
-- in normalization rules doesn't throw an exception.
matchesAnyNormalizationRule :: Expression -> Bool
matchesAnyNormalizationRule expr = matchesAnyNormalizationRule' expr normalizationRules
  where
    matchesAnyNormalizationRule' :: Expression -> [Y.Rule] -> Bool
    matchesAnyNormalizationRule' _ [] = False
    matchesAnyNormalizationRule' expr (rule : rules) =
      case unsafePerformIO (matchProgramWithCondition (Y.pattern rule) (Y.when rule) (Program expr)) of
        Just matched -> not (null matched) || matchesAnyNormalizationRule' expr rules
        Nothing -> matchesAnyNormalizationRule' expr rules

-- Returns True if given expression is in the normal form
isNF :: Expression -> Bool
isNF ExThis = True
isNF ExGlobal = True
isNF ExTermination = True
isNF (ExDispatch ExThis _) = True
isNF (ExDispatch ExGlobal _) = True
isNF (ExDispatch ExTermination _) = False -- dd rule
isNF (ExApplication ExTermination _) = False -- dc rule
isNF (ExFormation []) = True
isNF (ExFormation bds) = normalBindings bds || not (matchesAnyNormalizationRule (ExFormation bds))
  where
    -- Returns True if all given bindings are 100% in normal form
    normalBindings :: [Binding] -> Bool
    normalBindings [] = True
    normalBindings (bd : bds) =
      let next = normalBindings bds
       in case bd of
            BiDelta _ -> next
            BiVoid _ -> next
            BiLambda _ -> next
            _ -> False
isNF expr = not (matchesAnyNormalizationRule expr)

meetCondition' :: Y.Condition -> Subst -> IO [Subst]
meetCondition' (Y.Or []) subst = pure [subst]
meetCondition' (Y.Or (cond : rest)) subst = do
  met <- meetCondition' cond subst
  if null met
    then meetCondition' (Y.Or rest) subst
    else pure met
meetCondition' (Y.And []) subst = pure [subst]
meetCondition' (Y.And (cond : rest)) subst = do
  met <- meetCondition' cond subst
  if null met
    then pure []
    else meetCondition' (Y.And rest) subst
meetCondition' (Y.Not cond) subst = do
  met <- meetCondition' cond subst
  pure [subst | null met]
meetCondition' (Y.In attr binding) subst =
  case (buildAttribute attr subst, buildBinding binding subst) of
    (Just attr, Just bds) -> pure [subst | attrInBindings attr bds] -- if attrInBindings attr bd then [subst] else []
    (_, _) -> pure []
meetCondition' (Y.Alpha (AtAlpha _)) subst = pure [subst]
meetCondition' (Y.Alpha (AtMeta name)) (Subst mp) = case M.lookup name mp of
  Just (MvAttribute (AtAlpha _)) -> pure [Subst mp]
  _ -> pure []
meetCondition' (Y.Alpha _) _ = pure []
meetCondition' (Y.Eq (Y.CmpNum left) (Y.CmpNum right)) subst = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> pure [subst | left_ == right_]
  (_, _) -> pure []
meetCondition' (Y.Eq (Y.CmpAttr left) (Y.CmpAttr right)) subst = pure [subst | compareAttrs left right subst]
meetCondition' (Y.Eq _ _) _ = pure []
meetCondition' (Y.NF (ExMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvExpression expr _) -> pure [Subst mp | isNF expr]
  _ -> pure []
meetCondition' (Y.NF expr) (Subst mp) = pure [Subst mp | isNF expr]
meetCondition' (Y.XI (ExMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvExpression expr _) -> meetCondition' (Y.XI expr) (Subst mp)
  _ -> pure []
meetCondition' (Y.XI (ExFormation _)) subst = pure [subst]
meetCondition' (Y.XI ExThis) subst = pure []
meetCondition' (Y.XI ExGlobal) subst = pure [subst]
meetCondition' (Y.XI (ExApplication expr (BiTau attr texpr))) subst = do
  onExpr <- meetCondition' (Y.XI expr) subst
  onTau <- meetCondition' (Y.XI texpr) subst
  pure [subst | not (null onExpr) && not (null onTau)]
meetCondition' (Y.XI (ExDispatch expr _)) subst = meetCondition' (Y.XI expr) subst
meetCondition' (Y.Match pat (ExMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvExpression expr _) -> meetCondition' (Y.Match pat expr) (Subst mp)
  _ -> pure []
meetCondition' (Y.Match pat expr) subst = do
  (TeBytes tgt) <- buildTermFromFunction "dataize" [Y.ArgExpression expr] subst (Program expr)
  matched <- match (B.pack pat) (B.pack (btsToUnescapedStr tgt))
  pure [subst | matched]

-- For each substitution check if it meetCondition to given condition
-- If substitution does not meet the condition - it's thrown out
-- and is not used in replacement
meetCondition :: Y.Condition -> [Subst] -> IO [Subst]
meetCondition _ [] = pure []
meetCondition cond (subst : rest) = do
  first <- meetCondition' cond subst
  next <- meetCondition cond rest
  if null first
    then pure next
    else pure (head first : next)

-- Returns Just [...] if
-- 1. program matches pattern and
-- 2.1. condition is not present, or
-- 2.2. condition is present and met
-- Otherwise returns Nothing
matchProgramWithCondition :: Expression -> Maybe Y.Condition -> Program -> IO (Maybe [Subst])
matchProgramWithCondition ptn condition program =
  let matched = matchProgram ptn program
   in if null matched
        then pure Nothing
        else case condition of
          Nothing -> pure (Just matched)
          Just cond -> do
            met <- meetCondition cond matched
            if null met
              then pure Nothing
              else pure (Just met)
