{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rule (RuleContext (..), Step (..), domainOf, isFormation, isNF, matchExpressionWithRule, matchExpressionWithRule', meetCondition, normal, normalHeld, normalWith, presentIn, redex, xiFree) where

import AST
import Builder
  ( buildAttribute
  , buildBindingThrows
  , buildBindingUnchecked
  , buildExpression
  , buildExpressionThrows
  )
import Bytes (btsToUnescapedStr)
import Control.Exception (Exception (displayException))
import Control.Exception.Base (SomeException, try)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldlM)
import Data.List (foldl', intersect, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Deps (BuildTermFunc, BuildTermMethod, Term (..))
import Functions (buildTerm, nameOf)
import GHC.IO (unsafePerformIO)
import Logger (logDebug)
import Matcher
import Printer
import Regexp (match)
import Text.Printf (printf)
import Yaml (normalizationRules)
import qualified Yaml as Y

data RuleContext = RuleContext
  { _buildTerm :: BuildTermFunc
  , _universe :: Maybe Expression
  , _normal :: Expression -> Bool
  }

data Step = Step
  { _name :: String
  , _applied :: RuleContext -> Expression -> IO (Maybe Expression)
  }

matchesAnyNormalizationRule :: Expression -> RuleContext -> Bool
matchesAnyNormalizationRule expr ctx = matchesAnyNormalizationRule' expr normalizationRules ctx
  where
    matchesAnyNormalizationRule' :: Expression -> [Y.Rule] -> RuleContext -> Bool
    matchesAnyNormalizationRule' _ [] _ = False
    matchesAnyNormalizationRule' expr (rule : rules) ctx =
      let matched = unsafePerformIO (admitted (deep rule) [substEmpty] expr rule ctx)
       in not (null matched) || matchesAnyNormalizationRule' expr rules ctx

isNF :: Expression -> RuleContext -> Bool
isNF expr ctx = normalWith (`matchesAnyNormalizationRule` ctx) expr

normal :: Expression -> Bool
normal expr = isNF expr (RuleContext buildTerm Nothing normal)

normalWith :: (Expression -> Bool) -> Expression -> Bool
normalWith _ ExXi = True
normalWith _ ExRoot = True
normalWith _ ExTermination = True
normalWith _ (ExDispatch ExXi _) = True
normalWith _ (ExDispatch ExRoot _) = True
normalWith _ (ExDispatch ExTermination _) = False
normalWith _ (ExApplication ExTermination _) = False
normalWith _ (ExFormation []) = True
normalWith matching (ExFormation bds) = normalBindings bds || not (matching (ExFormation bds))
  where
    normalBindings :: [Binding] -> Bool
    normalBindings bds = all inert bds && not (any delta bds && any lambda bds)
    inert :: Binding -> Bool
    inert (BiDelta _) = True
    inert (BiVoid _) = True
    inert (BiLambda _) = True
    inert _ = False
    delta :: Binding -> Bool
    delta (BiDelta _) = True
    delta _ = False
    lambda :: Binding -> Bool
    lambda (BiLambda _) = True
    lambda _ = False
normalWith matching expr = not (matching expr)

normalHeld :: (Expression -> Bool) -> Expression -> Bool
normalHeld _ (ExMeta _) = False
normalHeld _ (ExAny _) = False
normalHeld test expr = test expr

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

_in :: [Attribute] -> [Binding] -> Subst -> RuleContext -> IO [Subst]
_in attrs bindings subst _ =
  case (traverse (`buildAttribute` subst) attrs, traverse (`buildBindingUnchecked` subst) bindings) of
    (Right attrs', Right bdss) -> pure [subst | all (`presentIn` concat bdss) attrs']
    (_, _) -> pure []

numToInt :: Y.Number -> Subst -> Maybe Int
numToInt (Y.MetaIndex meta) (Subst mp) = case M.lookup (Named meta) mp of
  Just (MvIndex idx) -> Just idx
  _ -> Nothing
numToInt (Y.Length (BiMeta meta)) (Subst mp) = case M.lookup (Named meta) mp of
  Just (MvBindings bds) -> Just (length bds)
  _ -> Nothing
numToInt (Y.Domain (BiMeta meta)) (Subst mp) = case M.lookup (Named meta) mp of
  Just (MvBindings bds) -> Just (domainOf bds)
  _ -> Nothing
numToInt (Y.Literal num) _ = Just num
numToInt _ _ = Nothing

domainOf :: [Binding] -> Int
domainOf = length . filter notAsset
  where
    notAsset :: Binding -> Bool
    notAsset (BiDelta _) = False
    notAsset (BiLambda _) = False
    notAsset (BiVoid AtRho) = False
    notAsset (BiTau AtRho _) = False
    notAsset _ = True

_eq :: Y.Comparable -> Y.Comparable -> Subst -> RuleContext -> IO [Subst]
_eq (Y.CmpNum left) (Y.CmpNum right) subst _ = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> pure [subst | left_ == right_]
  (_, _) -> pure []
_eq (Y.CmpAttr left) (Y.CmpAttr right) subst _ = pure [subst | compareAttrs left right subst]
  where
    compareAttrs :: Attribute -> Attribute -> Subst -> Bool
    compareAttrs (AtMeta left) (AtMeta right) (Subst mp) = case (M.lookup (Named left) mp, M.lookup (Named right) mp) of
      (Just (MvAttribute left'), Just (MvAttribute right')) -> compareAttrs left' right' (Subst mp)
      _ -> False
    compareAttrs attr (AtMeta meta) (Subst mp) = case M.lookup (Named meta) mp of
      Just (MvAttribute found) -> attr == found
      _ -> False
    compareAttrs (AtMeta meta) attr (Subst mp) = case M.lookup (Named meta) mp of
      Just (MvAttribute found) -> attr == found
      _ -> False
    compareAttrs left right _ = right == left
_eq (Y.CmpExpr left) (Y.CmpExpr right) subst _ =
  case (buildExpression left subst, buildExpression right subst) of
    (Right left', Right right') -> pure [subst | left' == right']
    (_, _) -> pure []
_eq _ _ _ _ = pure []

_gt :: Y.Comparable -> Y.Comparable -> Subst -> RuleContext -> IO [Subst]
_gt (Y.CmpNum left) (Y.CmpNum right) subst _ = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> pure [subst | left_ > right_]
  (_, _) -> pure []
_gt _ _ _ _ = pure []

_nf :: Expression -> Subst -> RuleContext -> IO [Subst]
_nf (ExMeta meta) (Subst mp) ctx = case M.lookup (Named meta) mp of
  Just (MvExpression expr) | bound expr -> pure [Subst mp | _normal ctx expr]
  Just (MvExpression expr) -> _nf expr (Subst mp) ctx
  _ -> pure []
_nf (ExAny slot) (Subst mp) ctx = case M.lookup (Anon slot) mp of
  Just (MvExpression expr) | bound expr -> pure [Subst mp | _normal ctx expr]
  Just (MvExpression expr) -> _nf expr (Subst mp) ctx
  _ -> pure []
_nf expr subst ctx = do
  built <- buildExpressionThrows expr subst
  pure [subst | _normal ctx built]

_absolute :: Expression -> Subst -> RuleContext -> IO [Subst]
_absolute (ExMeta meta) (Subst mp) ctx = case M.lookup (Named meta) mp of
  Just (MvExpression expr) | bound expr -> pure [Subst mp | xiFree expr]
  Just (MvExpression expr) -> _absolute expr (Subst mp) ctx
  _ -> pure []
_absolute (ExAny slot) (Subst mp) ctx = case M.lookup (Anon slot) mp of
  Just (MvExpression expr) | bound expr -> pure [Subst mp | xiFree expr]
  Just (MvExpression expr) -> _absolute expr (Subst mp) ctx
  _ -> pure []
_absolute expr subst _ = do
  built <- buildExpressionThrows expr subst
  pure [subst | xiFree built]

bound :: Expression -> Bool
bound (ExMeta _) = False
bound (ExAny _) = False
bound _ = True

xiFree :: Expression -> Bool
xiFree (ExFormation _) = True
xiFree ExRoot = True
xiFree ExTermination = True
xiFree (ExApplication e (ArTau _ te)) = xiFree e && xiFree te
xiFree (ExApplication e (ArAlpha _ te)) = xiFree e && xiFree te
xiFree (ExDispatch e _) = xiFree e
xiFree _ = False

_isFormation :: Expression -> Subst -> RuleContext -> IO [Subst]
_isFormation (ExMeta meta) (Subst mp) ctx = case M.lookup (Named meta) mp of
  Just (MvExpression expr) -> _isFormation expr (Subst mp) ctx
  _ -> pure []
_isFormation expr subst _ = pure [subst | isFormation expr]

isFormation :: Expression -> Bool
isFormation (ExFormation _) = True
isFormation _ = False

_matches :: String -> Expression -> Subst -> RuleContext -> IO [Subst]
_matches pat (ExMeta meta) (Subst mp) ctx = case M.lookup (Named meta) mp of
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

_disjoint :: [Attribute] -> [Binding] -> Subst -> RuleContext -> IO [Subst]
_disjoint attrs bindings subst _ =
  case (traverse (`buildAttribute` subst) attrs, traverse (`buildBindingUnchecked` subst) bindings) of
    (Right attrs', Right bdss) -> pure [subst | not (any (`presentIn` concat bdss) attrs')]
    (_, _) -> pure []

presentIn :: Attribute -> [Binding] -> Bool
presentIn attr = any present
  where
    present :: Binding -> Bool
    present (BiTau battr _) = attr == battr
    present (BiVoid battr) = attr == battr
    present (BiLambda _) = attr == AtLambda
    present (BiDelta _) = attr == AtDelta
    present _ = False

meetCondition' :: Y.Condition -> Subst -> RuleContext -> IO [Subst]
meetCondition' (Y.Or conds) = _or conds
meetCondition' (Y.And conds) = _and conds
meetCondition' (Y.Not cond) = _not cond
meetCondition' (Y.In attrs bds) = _in attrs bds
meetCondition' (Y.Eq left right) = _eq left right
meetCondition' (Y.Gt left right) = _gt left right
meetCondition' (Y.NF expr) = _nf expr
meetCondition' (Y.Absolute expr) = _absolute expr
meetCondition' (Y.Matches pat expr) = _matches pat expr
meetCondition' (Y.PartOf expr bd) = _partOf expr bd
meetCondition' (Y.Disjoint attrs bds) = _disjoint attrs bds
meetCondition' (Y.IsFormation expr) = _isFormation expr

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
    Left err -> do
      logDebug (printf "Condition %s raised and was treated as not met: %s" (show cond) (displayException err))
      meetCondition cond rest ctx

meetMaybeCondition :: Maybe Y.Condition -> [Subst] -> RuleContext -> IO [Subst]
meetMaybeCondition Nothing substs _ = pure substs
meetMaybeCondition (Just cond) substs ctx = meetCondition cond substs ctx

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
                  term <- built func args subst'
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
  where
    built :: String -> BuildTermMethod
    built "named" = nameOf _universe
    built func = _buildTerm func

metasWithPrefix :: T.Text -> Expression -> [Expression]
metasWithPrefix prefix = nub . go
  where
    go :: Expression -> [Expression]
    go expr@(ExMeta mt)
      | T.isPrefixOf prefix mt = [expr]
      | otherwise = []
    go expr@(ExAny (Slot kind _))
      | T.isPrefixOf prefix kind = [expr]
      | otherwise = []
    go (ExFormation bds) = concatMap goBinding bds
    go (ExApplication e arg) = go e ++ goArgument arg
    go (ExDispatch e _) = go e
    go (ExPhiMeet _ _ e) = go e
    go (ExPhiAgain _ _ e) = go e
    go _ = []
    goBinding :: Binding -> [Expression]
    goBinding (BiTau _ e) = go e
    goBinding _ = []
    goArgument :: Argument -> [Expression]
    goArgument (ArTau _ expr) = go expr
    goArgument (ArAlpha _ expr) = go expr

matchExpressionWithRule :: Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionWithRule expr rule = matchExpressionBy (deep rule) [substEmpty] expr rule

deep :: Y.Rule -> MatchExpressionFunc
deep rule ptn tgt
  | reachable' (redex rule) ptn tgt = matchExpressionDeep' (redex rule) ptn tgt
  | otherwise = []

redex :: Y.Rule -> Bool
redex rule = case rule.pattern of
  ExDispatch head' _ -> stuck head'
  ExApplication head' _ -> stuck head'
  ExFormation bds -> all (`elem` (concatMap attribute bds ++ maybe [] (demanded bds) rule.when)) [AtLambda, AtDelta]
  _ -> False
  where
    stuck :: Expression -> Bool
    stuck (ExFormation _) = True
    stuck ExTermination = True
    stuck _ = False
    attribute :: Binding -> [Attribute]
    attribute (BiLambda _) = [AtLambda]
    attribute (BiDelta _) = [AtDelta]
    attribute _ = []
    demanded :: [Binding] -> Y.Condition -> [Attribute]
    demanded bds (Y.In attrs metas)
      | all (\meta -> isMeta meta && meta `elem` bds) metas = attrs
    demanded bds (Y.And conds) = concatMap (demanded bds) conds
    demanded bds (Y.Or (cond : conds)) = foldl' (\attrs cond' -> attrs `intersect` demanded bds cond') (demanded bds cond) conds
    demanded _ _ = []
    isMeta :: Binding -> Bool
    isMeta (BiMeta _) = True
    isMeta _ = False

matchExpressionWithRule' :: [Subst] -> Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionWithRule' = matchExpressionBy matchExpression'

matchExpressionBy :: MatchExpressionFunc -> [Subst] -> Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionBy matcher seed expr rule ctx = do
  when' <- admitted matcher seed expr rule ctx
  if null when'
    then pure []
    else do
      logDebug (printf "Rule %s" rule.name)
      extended <- extraSubstitutions when' rule.where_ ctx
      if null extended
        then do
          logDebug "Substitution is empty after extending, maybe some metas are duplicated"
          pure []
        else do
          met <- meetMaybeCondition rule.having extended ctx
          when (null met) (logDebug "The 'having' condition wasn't met")
          pure met

admitted :: MatchExpressionFunc -> [Subst] -> Expression -> Y.Rule -> RuleContext -> IO [Subst]
admitted matcher seed expr rule ctx =
  let ptn = rule.pattern
      matched = combineMany seed (matcher ptn expr)
   in if null matched
        then do
          logDebug (printf "Pattern from rule '%s' was not matched:\n%s" rule.name (printExpression' ptn logPrintConfig))
          pure []
        else do
          inXiFree <- foldlM (\substs mt -> meetCondition (Y.Absolute mt) substs ctx) matched (kMetas ptn)
          inNf <- foldlM (\substs mt -> meetCondition (Y.NF mt) substs ctx) inXiFree (nfMetas ptn ++ kMetas ptn)
          if null inNf
            then do
              logDebug "A '𝑛'/'𝑘' meta-variable is not in normal form, or a '𝑘' meta-variable is not xi-free"
              pure []
            else do
              when' <- meetMaybeCondition rule.when inNf ctx
              when (null when') (logDebug "The 'when' condition wasn't met")
              pure when'
  where
    nfMetas :: Expression -> [Expression]
    nfMetas = metasWithPrefix "n"

    kMetas :: Expression -> [Expression]
    kMetas = metasWithPrefix "k"
