{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionarcom
-- SPDX-License-Identifier: MIT

module Rule where

import Ast
import GHC.Generics
import Data.Aeson (FromJSON)
import Matcher
import qualified Data.Map.Strict as M
import Builder (buildAttribute, buildBinding)

data Number
  = Ordinal Attribute
  | Length Binding
  | Add Number Number
  | Literal Integer
  deriving (Generic, Show)

data Comparable
  = CmpAttr Attribute
  | CmpNum Number
  deriving (Generic, Show)

data Condition
  = And [Condition]
  | Or [Condition]
  | In Attribute Binding
  | Not Condition
  | Alpha Attribute
  | Eq Comparable Comparable
  | NF Expression
  deriving (Generic, Show)

data Extra = Extra
  {
    meta :: Expression,
    function :: String,
    args :: [Expression]
  }
  deriving (Generic, Show)

data Rule = Rule
  { name :: Maybe String,
    pattern :: Expression,
    result :: Expression,
    when :: Maybe Condition,
    where_ :: Maybe [Extra]
  }
  deriving (Generic, Show)

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
numToInt :: Number -> Subst -> Maybe Integer
numToInt (Ordinal (AtMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvAttribute (AtAlpha idx)) -> Just idx
  _ -> Nothing
numToInt (Ordinal (AtAlpha idx)) subst = Just idx
numToInt (Length (BiMeta meta)) (Subst mp) = case M.lookup meta mp of
  Just (MvBindings bds) -> Just (toInteger (length bds))
  _ -> Nothing
numToInt (Add left right) subst = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> Just (left_ + right_)
  _ -> Nothing
numToInt (Literal num) subst = Just num
numToInt _ _ = Nothing

-- For each substitution check if it meetCondition to given condition
-- If substitution does not meet the condition - it's thrown out
-- and is not used in replacement
meetCondition :: Condition -> [Subst] -> [Subst]
meetCondition _ [] = []
meetCondition (Or []) substs = substs
meetCondition (Or (cond : rest)) [subst] = do
  let met = meetCondition cond [subst]
  if null met
    then meetCondition (Or rest) [subst]
    else met
meetCondition (And []) substs = substs
meetCondition (And (cond : rest)) [subst] = do
  let met = meetCondition cond [subst]
  if null met
    then []
    else meetCondition (And rest) [subst]
meetCondition (Not cond) [subst] = do
  let met = meetCondition cond [subst]
  [subst | null met]
meetCondition (In attr binding) [subst] =
  case (buildAttribute attr subst, buildBinding binding subst) of
    (Just attr, Just bds) -> [subst | attrInBindings attr bds] -- if attrInBindings attr bd then [subst] else []
    (_, _) -> []
meetCondition (Alpha (AtAlpha _)) substs = substs
meetCondition (Alpha (AtMeta name)) [Subst mp] = case M.lookup name mp of
  Just (MvAttribute (AtAlpha _)) -> [Subst mp]
  _ -> []
meetCondition (Alpha _) _ = []
meetCondition (Eq (CmpNum left) (CmpNum right)) [subst] = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> [subst | left_ == right_]
  (_, _) -> []
meetCondition (Eq (CmpAttr left) (CmpAttr right)) [subst] = [subst | compareAttrs left right subst]
meetCondition cond (subst : rest) = do
  let first = meetCondition cond [subst]
      next = meetCondition cond rest
  if null first
    then next
    else head first : next

matchProgramWithCondition :: Expression -> Maybe Condition -> Program -> Maybe [Subst]
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
