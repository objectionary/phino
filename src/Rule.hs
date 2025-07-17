{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rule where

import Ast
import Builder (buildAttribute, buildBinding, buildBindingThrows, buildExpression, buildExpressionThrows)
import Control.Exception (SomeException (SomeException), evaluate)
import Control.Exception.Base (try)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import GHC.IO (unsafePerformIO)
import Logger (logDebug)
import Matcher
import Misc (allPathsIn, btsToUnescapedStr)
import Pretty (prettyAttribute, prettyBytes, prettyExpression, prettyExpression', prettySubsts)
import Regexp (match)
import Term (BuildTermFunc, Term (..))
import Text.Printf (printf)
import Yaml (normalizationRules)
import qualified Yaml as Y

data RuleContext = RuleContext
  { _program :: Program,
    _buildTerm :: BuildTermFunc
  }

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
matchesAnyNormalizationRule :: Expression -> RuleContext -> Bool
matchesAnyNormalizationRule expr ctx = matchesAnyNormalizationRule' expr normalizationRules ctx
  where
    matchesAnyNormalizationRule' :: Expression -> [Y.Rule] -> RuleContext -> Bool
    matchesAnyNormalizationRule' _ [] _ = False
    matchesAnyNormalizationRule' expr (rule : rules) ctx =
      let matched = unsafePerformIO (matchProgramWithRule (Program expr) rule ctx)
       in not (null matched) || matchesAnyNormalizationRule' expr rules ctx

-- Returns True if given expression is in the normal form
isNF :: Expression -> RuleContext -> Bool
isNF ExThis _ = True
isNF ExGlobal _ = True
isNF ExTermination _ = True
isNF (ExDispatch ExThis _) _ = True
isNF (ExDispatch ExGlobal _) _ = True
isNF (ExDispatch ExTermination _) _ = False -- dd rule
isNF (ExApplication ExTermination _) _ = False -- dc rule
isNF (ExFormation []) _ = True
isNF (ExFormation bds) ctx = normalBindings bds || not (matchesAnyNormalizationRule (ExFormation bds) ctx)
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
isNF expr ctx = not (matchesAnyNormalizationRule expr ctx)

meetCondition' :: Y.Condition -> Subst -> RuleContext -> IO [Subst]
meetCondition' (Y.Or []) subst _ = pure [subst]
meetCondition' (Y.Or (cond : rest)) subst ctx = do
  met <- meetCondition' cond subst ctx
  if null met
    then meetCondition' (Y.Or rest) subst ctx
    else pure met
meetCondition' (Y.And []) subst _ = pure [subst]
meetCondition' (Y.And (cond : rest)) subst ctx = do
  met <- meetCondition' cond subst ctx
  if null met
    then pure []
    else meetCondition' (Y.And rest) subst ctx
meetCondition' (Y.Not cond) subst ctx = do
  met <- meetCondition' cond subst ctx
  pure [subst | null met]
meetCondition' (Y.In attr binding) subst _ =
  case (buildAttribute attr subst, buildBinding binding subst) of
    (Just attr, Just bds) -> pure [subst | attrInBindings attr bds] -- if attrInBindings attr bd then [subst] else []
    (_, _) -> pure []
meetCondition' (Y.Alpha (AtAlpha _)) subst _ = pure [subst]
meetCondition' (Y.Alpha (AtMeta name)) (Subst mp) _ = case M.lookup name mp of
  Just (MvAttribute (AtAlpha _)) -> pure [Subst mp]
  _ -> pure []
meetCondition' (Y.Alpha _) _ _ = pure []
meetCondition' (Y.Eq (Y.CmpNum left) (Y.CmpNum right)) subst _ = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> pure [subst | left_ == right_]
  (_, _) -> pure []
meetCondition' (Y.Eq (Y.CmpAttr left) (Y.CmpAttr right)) subst _ = pure [subst | compareAttrs left right subst]
meetCondition' (Y.Eq _ _) _ _ = pure []
meetCondition' (Y.NF (ExMeta meta)) (Subst mp) ctx = case M.lookup meta mp of
  Just (MvExpression expr _) -> pure [Subst mp | isNF expr ctx]
  _ -> pure []
meetCondition' (Y.NF expr) (Subst mp) ctx = pure [Subst mp | isNF expr ctx]
meetCondition' (Y.XI (ExMeta meta)) (Subst mp) ctx = case M.lookup meta mp of
  Just (MvExpression expr _) -> meetCondition' (Y.XI expr) (Subst mp) ctx
  _ -> pure []
meetCondition' (Y.XI (ExFormation _)) subst _ = pure [subst]
meetCondition' (Y.XI ExThis) subst _ = pure []
meetCondition' (Y.XI ExGlobal) subst _ = pure [subst]
meetCondition' (Y.XI (ExApplication expr (BiTau attr texpr))) subst ctx = do
  onExpr <- meetCondition' (Y.XI expr) subst ctx
  onTau <- meetCondition' (Y.XI texpr) subst ctx
  pure [subst | not (null onExpr) && not (null onTau)]
meetCondition' (Y.XI (ExDispatch expr _)) subst ctx = meetCondition' (Y.XI expr) subst ctx
meetCondition' (Y.Matches pat (ExMeta meta)) (Subst mp) ctx = case M.lookup meta mp of
  Just (MvExpression expr _) -> meetCondition' (Y.Matches pat expr) (Subst mp) ctx
  _ -> pure []
meetCondition' (Y.Matches pat expr) subst ctx = do
  (TeBytes tgt) <- _buildTerm ctx "dataize" [Y.ArgExpression expr] subst (Program expr)
  matched <- match (B.pack pat) (B.pack (btsToUnescapedStr tgt))
  pure [subst | matched]
meetCondition' (Y.PartOf exp bd) subst _ = do
  (exp', _) <- buildExpressionThrows exp subst
  bds <- buildBindingThrows bd subst
  pure [subst | partOf exp' bds]
  where
    partOf :: Expression -> [Binding] -> Bool
    partOf expr [] = False
    partOf expr (BiTau _ expr' : rest) = expr == expr' || partOf expr rest
    partOf expr (bd : rest) = partOf expr rest

-- For each substitution check if it meetCondition to given condition
-- If substitution does not meet the condition - it's thrown out
-- and is not used in replacement
meetCondition :: Y.Condition -> [Subst] -> RuleContext -> IO [Subst]
meetCondition _ [] _ = pure []
meetCondition cond (subst : rest) ctx = do
  met <- try (meetCondition' cond subst ctx) :: IO (Either SomeException [Subst])
  case met of
    Right first -> do
      next <- meetCondition cond rest ctx
      if null first
        then pure next
        else pure (head first : next)
    Left _ -> meetCondition cond rest ctx

meetMaybeCondition :: Maybe Y.Condition -> [Subst] -> RuleContext -> IO [Subst]
meetMaybeCondition Nothing substs _ = pure substs
meetMaybeCondition (Just cond) substs ctx = meetCondition cond substs ctx

-- Extend list of given substitutions with extra substitutions from 'where' yaml rule section
extraSubstitutions :: [Subst] -> Maybe [Y.Extra] -> RuleContext -> IO [Subst]
extraSubstitutions substs extras RuleContext {..} = case extras of
  Nothing -> pure substs
  Just extras' -> do
    res <-
      sequence
        [ foldlM
            ( \(Just subst') extra -> do
                let maybeName = case Y.meta extra of
                      Y.ArgExpression (ExMeta name) -> Just name
                      Y.ArgAttribute (AtMeta name) -> Just name
                      Y.ArgBinding (BiMeta name) -> Just name
                      Y.ArgBytes (BtMeta name) -> Just name
                      _ -> Nothing
                    func = Y.function extra
                    args = Y.args extra
                term <- _buildTerm func args subst' _program
                meta <- case term of
                  TeExpression expr -> do
                    logDebug (printf "Function %s() returned expression:\n%s" func (prettyExpression' expr))
                    pure (MvExpression expr defaultScope)
                  TeAttribute attr -> do
                    logDebug (printf "Function %s() returned attribute:\n%s" func (prettyAttribute attr))
                    pure (MvAttribute attr)
                  TeBytes bytes -> do
                    logDebug (printf "Function %s() returned bytes: %s" func (prettyBytes bytes))
                    pure (MvBytes bytes)
                case maybeName of
                  Just name -> pure (combine (substSingle name meta) subst')
                  _ -> pure Nothing
            )
            (Just subst)
            extras'
          | subst <- substs
        ]
    pure (catMaybes res)

matchProgramWithRule :: Program -> Y.Rule -> RuleContext -> IO [Subst]
matchProgramWithRule program rule ctx =
  let ptn = Y.pattern rule
      matched = matchProgram ptn program
   in if null matched
        then pure []
        else do
          when <- meetMaybeCondition (Y.when rule) matched ctx
          if null when
            then pure []
            else do
              extended <- extraSubstitutions when (Y.where_ rule) ctx
              meetMaybeCondition (Y.having rule) extended ctx
