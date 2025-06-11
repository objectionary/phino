-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionarcom
-- SPDX-License-Identifier: MIT

module Condition where

import Ast
import GHC.Generics
import Data.Aeson (FromJSON)
import Matcher
import qualified Data.Map.Strict as M
import Builder (buildAttribute, buildBinding)
import Misc (allPathsIn)
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

-- For each substitution check if it meetCondition to given condition
-- If substitution does not meet the condition - it's thrown out
-- and is not used in replacement
meetCondition :: Y.Condition -> [Subst] -> IO [Subst]
meetCondition _ [] = pure []
meetCondition (Y.Or []) substs = pure substs
meetCondition (Y.Or (cond : rest)) [subst] = do
  met <- meetCondition cond [subst]
  if null met
    then meetCondition (Y.Or rest) [subst]
    else pure met
meetCondition (Y.And []) substs = pure substs
meetCondition (Y.And (cond : rest)) [subst] = do
  met <- meetCondition cond [subst]
  if null met
    then pure []
    else meetCondition (Y.And rest) [subst]
meetCondition (Y.Not cond) [subst] = do
  met <- meetCondition cond [subst]
  pure [subst | null met]
meetCondition (Y.In attr binding) [subst] =
  case (buildAttribute attr subst, buildBinding binding subst) of
    (Just attr, Just bds) -> pure [subst | attrInBindings attr bds] -- if attrInBindings attr bd then [subst] else []
    (_, _) -> pure []
meetCondition (Y.Alpha (AtAlpha _)) substs = pure substs
meetCondition (Y.Alpha (AtMeta name)) [Subst mp] = case M.lookup name mp of
  Just (MvAttribute (AtAlpha _)) -> pure [Subst mp]
  _ -> pure []
meetCondition (Y.Alpha _) _ = pure []
meetCondition (Y.Eq (Y.CmpNum left) (Y.CmpNum right)) [subst] = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> pure [subst | left_ == right_]
  (_, _) -> pure []
meetCondition (Y.Eq (Y.CmpAttr left) (Y.CmpAttr right)) [subst] = pure [subst | compareAttrs left right subst]
meetCondition (Y.NF (ExMeta meta)) [Subst mp] = case M.lookup meta mp of
  Just expr -> do
    rules <- allPathsIn "resources"
    pure []
  _ -> pure []
meetCondition (Y.NF _) _ = pure[]
meetCondition cond (subst : rest) = do
  first <- meetCondition cond [subst]
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
matchProgramWithCondition ptn condition program = do
  let matched = matchProgram ptn program
  if null matched
    then pure Nothing
    else case condition of
      Nothing -> pure (Just matched)
      Just cond -> do
        met <- meetCondition cond matched
        if null met
          then pure Nothing
          else pure (Just met)
