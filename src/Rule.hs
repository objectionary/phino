{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rule (RuleContext (..), isNF, matchExpressionWithRule, matchExpressionWithRule', meetCondition) where

import AST
import Builder
  ( buildAttribute
  , buildBinding
  , buildBindingThrows
  , buildExpression
  , buildExpressionThrows
  )
import Bytes (btsToUnescapedStr)
import Control.Exception (Exception (displayException))
import Control.Exception.Base (SomeException, try)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldlM)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Deps (BuildTermFunc, BuildTermMethod, Term (..))
import Functions (nameOf)
import GHC.IO (unsafePerformIO)
import Logger (logDebug)
import Matcher
import Printer
import Regexp (match)
import Text.Printf (printf)
import Yaml (normalizationRules)
import qualified Yaml as Y

-- What a rule is matched and extended with: the builder of its 'where'
-- functions and the world the matched term stands in, where one is known.
-- A normalization rule is about a term alone, so the world stays out of its
-- YAML and reaches only the functions that need it, which is 'named' writing
-- 'Φ' or 'Φ.number' into a ρ instead of the object (#1318, #1460).
data RuleContext = RuleContext
  { _buildTerm :: BuildTermFunc
  , _universe :: Maybe Expression
  }

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
isNF ExXi _ = True
isNF ExRoot _ = True
isNF ExTermination _ = True
isNF (ExDispatch ExXi _) _ = True
isNF (ExDispatch ExRoot _) _ = True
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

-- Hold if every given attribute is present in the union of the bindings
-- captured by the given binding metas.
_in :: [Attribute] -> [Binding] -> Subst -> RuleContext -> IO [Subst]
_in attrs bindings subst _ =
  case (traverse (`buildAttribute` subst) attrs, traverse (`buildBinding` subst) bindings) of
    (Right attrs', Right bdss) -> pure [subst | all (`presentIn` concat bdss) attrs']
    (_, _) -> pure []

-- Convert a 'Number' to an 'Int' under the given substitution, resolving
-- index metas, binding lengths and formation domains.
numToInt :: Y.Number -> Subst -> Maybe Int
numToInt (Y.MetaIndex meta) (Subst mp) = case M.lookup (Named meta) mp of
  Just (MvIndex idx) -> Just idx
  _ -> Nothing
numToInt (Y.Length (BiMeta meta)) (Subst mp) = case M.lookup (Named meta) mp of
  Just (MvBindings bds) -> Just (length bds)
  _ -> Nothing
numToInt (Y.Domain (BiMeta meta)) (Subst mp) = case M.lookup (Named meta) mp of
  Just (MvBindings bds) -> Just (length (filter notAsset bds))
  _ -> Nothing
  where
    notAsset (BiDelta _) = False
    notAsset (BiLambda _) = False
    notAsset (BiVoid AtRho) = False
    notAsset (BiTau AtRho _) = False
    notAsset _ = True
numToInt (Y.Literal num) _ = Just num
numToInt _ _ = Nothing

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
-- Both sides are built under the substitution before they are compared, so a
-- side written as a whole term — '⟦𝐵1, 𝜏1 ↦ 𝑛1, 𝐵2⟧' and not merely a meta
-- standing for one — is compared as the term it stands for rather than as the
-- pattern it was written as. A side holding a meta nothing bound cannot be
-- built, and an equality nobody can work out does not hold.
_eq (Y.CmpExpr left) (Y.CmpExpr right) subst _ =
  case (buildExpression left subst, buildExpression right subst) of
    (Right left', Right right') -> pure [subst | left' == right']
    (_, _) -> pure []
_eq _ _ _ _ = pure []

-- Hold if the left number is strictly greater than the right one. Only
-- numeric comparables are ordered; anything else fails to hold.
_gt :: Y.Comparable -> Y.Comparable -> Subst -> RuleContext -> IO [Subst]
_gt (Y.CmpNum left) (Y.CmpNum right) subst _ = case (numToInt left subst, numToInt right subst) of
  (Just left_, Just right_) -> pure [subst | left_ > right_]
  (_, _) -> pure []
_gt _ _ _ _ = pure []

_nf :: Expression -> Subst -> RuleContext -> IO [Subst]
_nf (ExMeta meta) (Subst mp) ctx = case M.lookup (Named meta) mp of
  Just (MvExpression expr) -> _nf expr (Subst mp) ctx
  _ -> pure []
_nf (ExAny slot) (Subst mp) ctx = case M.lookup (Anon slot) mp of
  Just (MvExpression expr) -> _nf expr (Subst mp) ctx
  _ -> pure []
_nf expr subst ctx = pure [subst | isNF expr ctx]

-- An expression is xi-free when it contains no ξ outside of a formation: it is
-- Φ, ⊥, a formation, a dispatch with a xi-free subject, or an application with
-- a xi-free subject and argument. ⊥ holds no ξ to capture, so it is xi-free
-- (and 'isNF ⊥ = True' already), which lets the copy rule accept a ⊥ argument.
-- Together with a normal-form check this is what makes an expression absolute
-- (𝒦 ⊆ 𝒩); the '𝑘' meta-variable applies this xi-free check first (cheap,
-- structural, rules out the ξ-recursion the normal-form check could loop on)
-- and the normal-form check second.
_absolute :: Expression -> Subst -> RuleContext -> IO [Subst]
_absolute (ExMeta meta) (Subst mp) ctx = case M.lookup (Named meta) mp of
  Just (MvExpression expr) -> _absolute expr (Subst mp) ctx
  _ -> pure []
_absolute (ExAny slot) (Subst mp) ctx = case M.lookup (Anon slot) mp of
  Just (MvExpression expr) -> _absolute expr (Subst mp) ctx
  _ -> pure []
_absolute expr subst _ = pure [subst | xiFree expr]
  where
    xiFree :: Expression -> Bool
    xiFree (ExFormation _) = True
    xiFree ExRoot = True
    xiFree ExTermination = True
    xiFree (ExApplication e (ArTau _ te)) = xiFree e && xiFree te
    xiFree (ExApplication e (ArAlpha _ te)) = xiFree e && xiFree te
    xiFree (ExDispatch e _) = xiFree e
    xiFree _ = False

-- Hold when the given expression is a formation (an abstraction ⟦…⟧). A meta
-- is resolved first, so 'binding 𝑛' inspects whatever 𝑛 is bound to.
_isFormation :: Expression -> Subst -> RuleContext -> IO [Subst]
_isFormation (ExMeta meta) (Subst mp) ctx = case M.lookup (Named meta) mp of
  Just (MvExpression expr) -> _isFormation expr (Subst mp) ctx
  _ -> pure []
_isFormation expr subst _ = pure [subst | isFormation expr]
  where
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

-- Hold if none of the given attributes is present in the union of the
-- bindings captured by the given binding metas.
_disjoint :: [Attribute] -> [Binding] -> Subst -> RuleContext -> IO [Subst]
_disjoint attrs bindings subst _ =
  case (traverse (`buildAttribute` subst) attrs, traverse (`buildBinding` subst) bindings) of
    (Right attrs', Right bdss) -> pure [subst | not (any (`presentIn` concat bdss) attrs')]
    (_, _) -> pure []

-- Tell whether the attribute is present among the bindings.
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
    -- A condition that raises is treated as not met: that is the policy
    -- #1079 questions, and it stays until the maintainers answer. The
    -- silence on top of it is nobody's friend — say what raised, at debug
    -- level, so a broken 'when'/'having' can be found with --log-level=debug
    Left err -> do
      logDebug (printf "Condition %s raised and was treated as not met: %s" (show cond) (displayException err))
      meetCondition cond rest ctx

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

-- Collect the constrained expression meta-variables with the given
-- one-character prefix used in a pattern. Each kind ('𝑛'/'!n' normal-form,
-- '𝑘'/'!k' absolute) lives in its own 'n'-/'k'-prefixed key-space, so a
-- pattern may freely mix them with plain '𝑒' captures. An anonymous meta
-- carries the same prefix as the sigil it was written with, so a bare '𝑛' is
-- held to the normal form just as '𝑛1' is.
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

-- Match a rewriting rule against an expression and every place inside it.
-- The deep matcher is asked only where the pattern fits somewhere in the term
-- at all (see 'reachable'), since trying it at every place of a term holding
-- copies of big objects is what a rule that fits nowhere used to cost (#1453).
matchExpressionWithRule :: Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionWithRule = matchExpressionBy deep [substEmpty]
  where
    deep :: MatchExpressionFunc
    deep ptn tgt
      | reachable ptn tgt = matchExpression ptn tgt
      | otherwise = []

-- Like 'matchExpressionWithRule' but matches the pattern against the whole
-- expression only (no deep, sub-expression matching). Used by the dataization
-- and morphing driver, where a rule applies to the entire configuration rather
-- than to nested redexes. The leading '[Subst]' seeds matching with pre-bound
-- meta-variables: the morphing driver passes the global universe bound to 'e',
-- the second argument of 𝕄(n, e), so the 'universe' rule reads it directly instead
-- of through a 'global()' build-term function. Pass '[substEmpty]' for no seed.
matchExpressionWithRule' :: [Subst] -> Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionWithRule' = matchExpressionBy matchExpression'

-- The seed substitutions are combined into every match, so a pre-bound meta in
-- the seed is dropped only when the pattern binds the same name to a different
-- value; rules that do not mention the name simply carry it along unused.
matchExpressionBy :: MatchExpressionFunc -> [Subst] -> Expression -> Y.Rule -> RuleContext -> IO [Subst]
matchExpressionBy matcher seed expr rule ctx =
  let ptn = rule.pattern
      matched = combineMany seed (matcher ptn expr)
      name = rule.name
   in if null matched
        then do
          logDebug (printf "Pattern from rule '%s' was not matched:\n%s" name (printExpression' ptn logPrintConfig))
          pure []
        else do
          -- A '𝑘' meta-variable is absolute (𝒦 ⊆ 𝒩): check it is xi-free first
          -- (cheap, structural), then fold its name into the same normal-form
          -- check used for '𝑛' metas, so 'isNF' is applied in a single place.
          inXiFree <- foldlM (\substs mt -> meetCondition (Y.Absolute mt) substs ctx) matched (kMetas ptn)
          inNf <- foldlM (\substs mt -> meetCondition (Y.NF mt) substs ctx) inXiFree (nfMetas ptn ++ kMetas ptn)
          if null inNf
            then do
              logDebug "A '𝑛'/'𝑘' meta-variable is not in normal form, or a '𝑘' meta-variable is not xi-free"
              pure []
            else do
              when' <- meetMaybeCondition rule.when inNf ctx
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
  where
    nfMetas :: Expression -> [Expression]
    nfMetas = metasWithPrefix "n"

    kMetas :: Expression -> [Expression]
    kMetas = metasWithPrefix "k"
