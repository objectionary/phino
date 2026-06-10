{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rule (RuleContext (..), isNF, matchProgramWithRule, matchExpressionWithRule, matchExpressionWithRule', meetCondition) where

import AST
import Builder
  ( buildAttribute
  , buildBinding
  , buildBindingThrows
  , buildExpressionThrows
  )
import Control.Exception.Base (SomeException, try)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldlM)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Deps (BuildTermFunc, Term (..))
import GHC.IO (unsafePerformIO)
import Logger (logDebug)
import Matcher
import Misc (btsToUnescapedStr)
import Printer
import Regexp (match)
import Text.Printf (printf)
import Yaml (normalizationRules)
import qualified Yaml as Y

newtype RuleContext = RuleContext {_buildTerm :: BuildTermFunc}

-- Returns True if given expression matches with any of given normalization rules
-- Here we use unsafePerformIO because we're sure that conditions which are used
-- in normalization rules doesn't throw an exception.
matchesAnyNormalizationRule :: Expression -> RuleContext -> Bool
matchesAnyNormalizationRule expr ctx = matchesAnyNormalizationRule' expr normalizationRules ctx
  where
    matchesAnyNormalizationRule' :: Expression -> [Y.Rule] -> RuleContext -> Bool
    matchesAnyNormalizationRule' _ [] _ = False
    matchesAnyNormalizationRule' expr (rule : rules) ctx =
      let matched = unsafePerformIO (matchExpressionWithRule expr rule ctx)
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

_or :: [Y.Condition] -> Subst -> RuleContext -> IO [Subst]
_or [] _ _ = pure []
_or (cond : rest) subst ctx = do
  met <- meetCondition' cond subst ctx
  if null met
    then _or rest subst ctx
    else pure met

_and :: [Y.Condition] -> Subst -> RuleContext -> IO [Subst]
_and [] subst _ = pure [subst]
_and (cond : rest) subst ctx = do
  met <- meetCondition' cond subst ctx
  if null met
    then pure []
    else _and rest subst ctx

_not :: Y.Condition -> Subst -> RuleContext -> IO [Subst]
_not cond subst ctx = do
  met <- meetCondition' cond subst ctx
  pure [subst | null met]

_in :: Attribute -> Binding -> Subst -> RuleContext -> IO [Subst]
_in attr binding subst _ =
  case (buildAttribute attr subst, buildBinding binding subst) of
    (Right attr, Right bds) -> pure [subst | attrInBindings attr bds] -- if attrInBindings attr bd then [subst] else []
    (_, _) -> pure []
  where
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

_alpha :: Attribute -> Subst -> RuleContext -> IO [Subst]
_alpha (AtAlpha _) subst _ = pure [subst]
_alpha (AtMeta name) (Subst mp) _ = case M.lookup name mp of
  Just (MvAttribute (AtAlpha _)) -> pure [Subst mp]
  _ -> pure []
_alpha _ _ _ = pure []

_eq :: Y.Comparable -> Y.Comparable -> Subst -> RuleContext -> IO [Subst]
_eq (Y.CmpNum left) (Y.CmpNum right) subst _ = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> pure [subst | left_ == right_]
  (_, _) -> pure []
  where
    -- Convert Number to Int
    numToInt :: Y.Number -> Subst -> Maybe Int
    numToInt (Y.Index (AtMeta meta)) (Subst mp) = case M.lookup meta mp of
      Just (MvAttribute (AtAlpha idx)) -> Just idx
      _ -> Nothing
    numToInt (Y.Index (AtAlpha idx)) _ = Just idx
    numToInt (Y.Length (BiMeta meta)) (Subst mp) = case M.lookup meta mp of
      Just (MvBindings bds) -> Just (length bds)
      _ -> Nothing
    numToInt (Y.Domain (BiMeta meta)) (Subst mp) = case M.lookup meta mp of
      Just (MvBindings bds) -> Just (length (filter notAsset bds))
      _ -> Nothing
    numToInt (Y.Literal num) _ = Just num
    numToInt _ _ = Nothing
    notAsset (BiDelta _) = False
    notAsset (BiLambda _) = False
    notAsset (BiMetaLambda _) = False
    notAsset _ = True
_eq (Y.CmpAttr left) (Y.CmpAttr right) subst _ = pure [subst | compareAttrs left right subst]
  where
    compareAttrs :: Attribute -> Attribute -> Subst -> Bool
    compareAttrs (AtMeta left) (AtMeta right) (Subst mp) = case (M.lookup left mp, M.lookup right mp) of
      (Just (MvAttribute left'), Just (MvAttribute right')) -> compareAttrs left' right' (Subst mp)
      _ -> False
    compareAttrs attr (AtMeta meta) (Subst mp) = case M.lookup meta mp of
      Just (MvAttribute found) -> attr == found
      _ -> False
    compareAttrs (AtMeta meta) attr (Subst mp) = case M.lookup meta mp of
      Just (MvAttribute found) -> attr == found
      _ -> False
    compareAttrs left right _ = right == left
_eq (Y.CmpExpr left) (Y.CmpExpr right) subst _ = pure [subst | compareExprs left right subst]
  where
    compareExprs :: Expression -> Expression -> Subst -> Bool
    compareExprs (ExMeta left) (ExMeta right) (Subst mp) = case (M.lookup left mp, M.lookup right mp) of
      (Just (MvExpression left'), Just (MvExpression right')) -> compareExprs left' right' (Subst mp)
      _ -> False
    compareExprs expr (ExMeta meta) (Subst mp) = case M.lookup meta mp of
      Just (MvExpression found) -> expr == found
      _ -> False
    compareExprs (ExMeta meta) expr (Subst mp) = case M.lookup meta mp of
      Just (MvExpression found) -> expr == found
      _ -> False
    compareExprs left right _ = left == right
_eq _ _ _ _ = pure []

_nf :: Expression -> Subst -> RuleContext -> IO [Subst]
_nf (ExMeta meta) (Subst mp) ctx = case M.lookup meta mp of
  Just (MvExpression expr) -> _nf expr (Subst mp) ctx
  _ -> pure []
_nf expr subst ctx = pure [subst | isNF expr ctx]

-- Checks the structural part of absoluteness: an expression is absolute in
-- structure when it is Φ, a formation, a dispatch with an absolute subject, or
-- an application with an absolute subject and argument (equivalently, no ξ
-- leaks outside of a formation). It does not enforce normal form on its own;
-- the '𝑘' meta-variable combines this structural check (applied first, since it
-- is cheap) with the normal-form check, so that 𝒦 ⊆ 𝒩.
_absolute :: Expression -> Subst -> RuleContext -> IO [Subst]
_absolute (ExMeta meta) (Subst mp) ctx = case M.lookup meta mp of
  Just (MvExpression expr) -> _absolute expr (Subst mp) ctx
  _ -> pure []
_absolute expr subst _ = pure [subst | absolute expr]
  where
    absolute :: Expression -> Bool
    absolute (ExFormation _) = True
    absolute ExGlobal = True
    absolute (ExApplication e (BiTau _ te)) = absolute e && absolute te
    absolute (ExDispatch e _) = absolute e
    absolute _ = False

_matches :: String -> Expression -> Subst -> RuleContext -> IO [Subst]
_matches pat (ExMeta meta) (Subst mp) ctx = case M.lookup meta mp of
  Just (MvExpression expr) -> _matches pat expr (Subst mp) ctx
  _ -> pure []
_matches pat expr subst ctx = do
  (TeBytes tgt) <- _buildTerm ctx "dataize" [Y.ArgExpression expr] subst
  matched <- match (B.pack pat) (B.pack (btsToUnescapedStr tgt))
  pure [subst | matched]

_partOf :: Expression -> Binding -> Subst -> RuleContext -> IO [Subst]
_partOf exp bd subst _ = do
  exp' <- buildExpressionThrows exp subst
  bds <- buildBindingThrows bd subst
  pure [subst | partOf exp' bds]
  where
    partOf :: Expression -> [Binding] -> Bool
    partOf _ [] = False
    partOf expr (BiTau _ (ExFormation bds) : rest) = expr == ExFormation bds || partOf expr bds || partOf expr rest
    partOf expr (BiTau _ expr' : rest) = expr == expr' || partOf expr rest
    partOf expr (_ : rest) = partOf expr rest

-- A primitive is the termination ⊥ or a formation without a λ binding;
-- expression metas are resolved through the substitution first.
_primitive :: Expression -> Subst -> RuleContext -> IO [Subst]
_primitive (ExMeta meta) (Subst mp) ctx = case M.lookup meta mp of
  Just (MvExpression expr) -> _primitive expr (Subst mp) ctx
  _ -> pure []
_primitive expr subst _ = pure [subst | primitive expr]
  where
    primitive :: Expression -> Bool
    primitive ExTermination = True
    primitive (ExFormation bds) = not (any lambda bds)
    primitive _ = False
    lambda :: Binding -> Bool
    lambda (BiLambda _) = True
    lambda (BiMetaLambda _) = True
    lambda _ = False

-- Hold if none of the given attributes is present in the union of the
-- bindings captured by the given binding metas.
_disjoint :: [Attribute] -> [Binding] -> Subst -> RuleContext -> IO [Subst]
_disjoint attrs bindings subst _ =
  case (traverse (`buildAttribute` subst) attrs, traverse (`buildBinding` subst) bindings) of
    (Right attrs', Right bdss) ->
      let bds = concat bdss
       in pure [subst | not (any (`presentIn` bds) attrs')]
    (_, _) -> pure []
  where
    presentIn :: Attribute -> [Binding] -> Bool
    presentIn attr = any (presentInBinding attr)
    presentInBinding :: Attribute -> Binding -> Bool
    presentInBinding attr (BiTau battr _) = attr == battr
    presentInBinding attr (BiVoid battr) = attr == battr
    presentInBinding AtLambda (BiLambda _) = True
    presentInBinding AtLambda (BiMetaLambda _) = True
    presentInBinding AtDelta (BiDelta _) = True
    presentInBinding _ _ = False

meetCondition' :: Y.Condition -> Subst -> RuleContext -> IO [Subst]
meetCondition' (Y.Or conds) = _or conds
meetCondition' (Y.And conds) = _and conds
meetCondition' (Y.Not cond) = _not cond
meetCondition' (Y.In attr binding) = _in attr binding
meetCondition' (Y.Alpha attr) = _alpha attr
meetCondition' (Y.Eq left right) = _eq left right
meetCondition' (Y.NF expr) = _nf expr
meetCondition' (Y.Absolute expr) = _absolute expr
meetCondition' (Y.Matches pat expr) = _matches pat expr
meetCondition' (Y.PartOf expr bd) = _partOf expr bd
meetCondition' (Y.Primitive expr) = _primitive expr
meetCondition' (Y.Disjoint attrs bds) = _disjoint attrs bds

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
      case first of
        [] -> pure next
        sbt : _ -> pure (sbt : next)
    Left _ -> meetCondition cond rest ctx

meetMaybeCondition :: Maybe Y.Condition -> [Subst] -> RuleContext -> IO [Subst]
meetMaybeCondition Nothing substs _ = pure substs
meetMaybeCondition (Just cond) substs ctx = meetCondition cond substs ctx

-- Extend list of given substitutions with extra substitutions from 'where' yaml rule section
extraSubstitutions :: [Subst] -> Maybe [Y.Extra] -> RuleContext -> IO [Subst]
extraSubstitutions substs extras RuleContext{..} = case extras of
  Nothing -> pure substs
  Just extras' -> do
    logDebug (printf "Building %d sets of extra substitutions.." (length substs))
    res <-
      sequence
        [ foldlM
            ( \maybeSubst extra -> case maybeSubst of
                Nothing -> pure Nothing
                Just subst' -> do
                  let maybeName = case Y.meta extra of
                        Y.ArgExpression (ExMeta name) -> Just name
                        Y.ArgAttribute (AtMeta name) -> Just name
                        Y.ArgBinding (BiMeta name) -> Just name
                        Y.ArgBytes (BtMeta name) -> Just name
                        _ -> Nothing
                      func = Y.function extra
                      args = Y.args extra
                  term <- _buildTerm func args subst'
                  meta <- case term of
                    TeExpression expr -> do
                      logDebug (printf "Function %s() returned expression:\n%s" func (printExpression expr))
                      pure (MvExpression expr)
                    TeAttribute attr -> do
                      logDebug (printf "Function %s() returned attribute: %s" func (printAttribute attr))
                      pure (MvAttribute attr)
                    TeBytes bytes -> do
                      logDebug (printf "Function %s() returned bytes: %s" func (printBytes bytes))
                      pure (MvBytes bytes)
                    TeBindings bds -> do
                      logDebug (printf "Function %s return bindings: %s" func (printExpression (ExFormation bds)))
                      pure (MvBindings bds)
                  case maybeName of
                    Just name -> pure (combine (substSingle name meta) subst')
                    _ -> pure Nothing
            )
            (Just subst)
            extras'
        | subst <- substs
        ]
    logDebug "Extra substitutions have been built"
    pure (catMaybes res)

-- Collect the names of constrained expression meta-variables with the given
-- one-character prefix used in a pattern. Each kind ('𝑛'/'!n' normal-form,
-- '𝑘'/'!k' absolute) lives in its own 'n'-/'k'-prefixed key-space, so a
-- pattern may freely mix them with plain '𝑒' captures.
metaNamesWithPrefix :: T.Text -> Expression -> [T.Text]
metaNamesWithPrefix prefix = nub . go
  where
    go :: Expression -> [T.Text]
    go (ExMeta mt)
      | T.isPrefixOf prefix mt = [mt]
      | otherwise = []
    go (ExFormation bds) = concatMap goBinding bds
    go (ExApplication e bd) = go e ++ goBinding bd
    go (ExDispatch e _) = go e
    go (ExMetaTail e _) = go e
    go (ExPhiMeet _ _ e) = go e
    go (ExPhiAgain _ _ e) = go e
    go _ = []
    goBinding :: Binding -> [T.Text]
    goBinding (BiTau _ e) = go e
    goBinding _ = []

nfMetaNames :: Expression -> [T.Text]
nfMetaNames = metaNamesWithPrefix "n"

kMetaNames :: Expression -> [T.Text]
kMetaNames = metaNamesWithPrefix "k"

matchExpressionWithRule :: Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionWithRule = matchExpressionBy matchExpression

-- Like 'matchExpressionWithRule' but matches the pattern against the whole
-- expression only (no deep, sub-expression matching). Used by the dataization
-- and morphing driver, where a rule applies to the entire configuration rather
-- than to nested redexes.
matchExpressionWithRule' :: Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionWithRule' = matchExpressionBy matchExpression'

matchExpressionBy :: MatchExpressionFunc -> Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionBy matcher expr rule ctx =
  let ptn = rule.pattern
      matched = matcher ptn expr
      name = rule.name
   in if null matched
        then do
          logDebug (printf "Pattern from rule '%s' was not matched:\n%s" name (printExpression' ptn logPrintConfig))
          pure []
        else do
          absolute <- foldlM (\substs nm -> meetCondition (Y.Absolute (ExMeta nm)) substs ctx) matched (kMetaNames ptn)
          normal <- foldlM (\substs nm -> meetCondition (Y.NF (ExMeta nm)) substs ctx) absolute (nfMetaNames ptn ++ kMetaNames ptn)
          if null normal
            then do
              logDebug "A '𝑛'/'𝑘' meta-variable is not in normal form, or a '𝑘' meta-variable is not absolute"
              pure []
            else do
              when' <- meetMaybeCondition rule.when normal ctx
              if null when'
                then do
                  logDebug "The 'when' condition wasn't met"
                  pure []
                else do
                  logDebug (printf "Rule %s" name)
                  extended <- extraSubstitutions when' rule.where_ ctx
                  if null extended
                    then do
                      logDebug "Substitution is empty after extending, maybe some metas are duplicated"
                      pure []
                    else do
                      met <- meetMaybeCondition rule.having extended ctx
                      when (null met) (logDebug "The 'having' condition wasn't met")
                      pure met

matchProgramWithRule :: Program -> Y.Rule -> RuleContext -> IO [Subst]
matchProgramWithRule (Program expr) = matchExpressionWithRule expr
