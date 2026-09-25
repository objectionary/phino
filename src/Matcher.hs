-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse given AST and build substitutions
-- from meta variables to appropriate meta values
module Matcher where

import AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)

-- Meta value
-- The right part of substitution
data MetaValue
  = MvAttribute Attribute -- !t
  | MvIndex Int -- α𝑖
  | MvBytes Bytes -- !b
  | MvBindings [Binding] -- !B
  | MvFunction Function -- !F
  | MvExpression Expression -- !e
  deriving (Eq, Show)

-- The left-hand side of a substitution: a meta-variable the rule author named
-- and may reference from a result, or an anonymous slot that only the pattern
-- it was written in can address
data Meta
  = Named Text
  | Anon Slot
  deriving (Eq, Ord, Show)

-- Substitution
-- Shows the match of meta variable to meta value
newtype Subst = Subst (Map Meta MetaValue)
  deriving (Eq, Show)

-- A way to match a pattern expression against a target expression, yielding
-- the substitutions under which they agree.
type MatchExpressionFunc = Expression -> Expression -> [Subst]

-- Empty substitution
substEmpty :: Subst
substEmpty = Subst Map.empty

-- Singleton substitution with one (key -> value) pair
substSingle :: Text -> MetaValue -> Subst
substSingle key value = Subst (Map.singleton (Named key) value)

-- Singleton substitution binding one anonymous slot
substSlot :: Slot -> MetaValue -> Subst
substSlot slot value = Subst (Map.singleton (Anon slot) value)

-- Combine two substitutions into a single one
-- Fails if values by the same keys are not equal
combine :: Subst -> Subst -> Maybe Subst
combine (Subst a) (Subst b) = go (Map.toList b) a
  where
    go :: [(Meta, MetaValue)] -> Map Meta MetaValue -> Maybe Subst
    go [] acc = Just (Subst acc)
    go ((key, value) : rest) acc = case Map.lookup key acc of
      Just found
        | found == value -> go rest acc
        | otherwise -> Nothing
      Nothing -> go rest (Map.insert key value acc)

combineMany :: [Subst] -> [Subst] -> [Subst]
combineMany xs xy = catMaybes [combine x y | x <- xs, y <- xy]

matchAttribute :: Attribute -> Attribute -> [Subst]
matchAttribute (AtMeta meta) tgt = [substSingle meta (MvAttribute tgt)]
matchAttribute (AtAny slot) tgt = [substSlot slot (MvAttribute tgt)]
matchAttribute ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

matchAlpha :: Alpha -> Alpha -> [Subst]
matchAlpha (AlMeta meta) (Alpha idx) = [substSingle meta (MvIndex idx)]
matchAlpha (AlAny slot) (Alpha idx) = [substSlot slot (MvIndex idx)]
matchAlpha ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

-- A λ meta stands for any λ name at all — an ordinary one and a symbol alike,
-- since a symbol is a name nothing answers and not a variable of the rule
-- language (see 'FnSymbol'). Every other pair matches only itself.
matchFunction :: Function -> Function -> [Subst]
matchFunction (FnMeta meta) tgt
  | named tgt = [substSingle meta (MvFunction tgt)]
matchFunction (FnAny slot) tgt
  | named tgt = [substSlot slot (MvFunction tgt)]
matchFunction ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

-- Whether a λ function is a name a program wrote rather than a meta-variable
-- a rule wrote.
named :: Function -> Bool
named (Function _) = True
named (FnSymbol _) = True
named _ = False

matchBinding :: Binding -> Binding -> [Subst]
matchBinding (BiVoid pattr) (BiVoid tattr) = matchAttribute pattr tattr
matchBinding (BiDelta (BtMeta meta)) (BiDelta tdata) = [substSingle meta (MvBytes tdata)]
matchBinding (BiDelta (BtAny slot)) (BiDelta tdata) = [substSlot slot (MvBytes tdata)]
matchBinding (BiDelta pdata) (BiDelta tdata)
  | pdata == tdata = [substEmpty]
  | otherwise = []
matchBinding (BiLambda pFunc) (BiLambda tFunc) = matchFunction pFunc tFunc
matchBinding (BiTau pattr pexp) (BiTau tattr texp) = combineMany (matchAttribute pattr tattr) (matchExpression' pexp texp)
matchBinding _ _ = []

matchArgument :: Argument -> Argument -> [Subst]
matchArgument (ArTau pattr pexp) (ArTau tattr texp) = combineMany (matchAttribute pattr tattr) (matchExpression' pexp texp)
matchArgument (ArAlpha palpha pexp) (ArAlpha talpha texp) = combineMany (matchAlpha palpha talpha) (matchExpression' pexp texp)
matchArgument _ _ = []

-- Match bindings with ordering
matchBindings :: [Binding] -> [Binding] -> [Subst]
matchBindings [] [] = [substEmpty]
matchBindings [] _ = []
matchBindings ((BiMeta name) : pbs) tbs = matchBindingsMeta (substSingle name) pbs tbs
matchBindings ((BiAny slot) : pbs) tbs = matchBindingsMeta (substSlot slot) pbs tbs
matchBindings (pb : pbs) (tb : tbs) = combineMany (matchBinding pb tb) (matchBindings pbs tbs)
matchBindings _ _ = []

-- A meta binding stands for any leading run of the target bindings, so every
-- way of splitting the target into that run and the rest is tried. The rest is
-- carried down one binding at a time instead of being cut out of the target
-- anew at every index, which is what made a formation of N bindings cost N
-- walks of itself rather than one, and the run itself is put together only
-- where the pattern after it matched, so a split the pattern throws away costs
-- nothing to name. A meta binding with nothing after it takes the whole rest in
-- one step: the pattern is out of bindings, so the only split that matches is
-- the one leaving nothing behind (#1316).
matchBindingsMeta :: (MetaValue -> Subst) -> [Binding] -> [Binding] -> [Subst]
matchBindingsMeta bind [] tbs = [bind (MvBindings tbs)]
matchBindingsMeta bind pbs tbs = go [] tbs
  where
    go :: [Binding] -> [Binding] -> [Subst]
    go before after =
      catMaybes [combine (bind (MvBindings (reverse before))) subst | subst <- matchBindings pbs after]
        ++ case after of
          [] -> []
          (tb : rest) -> go (tb : before) rest

matchExpression' :: MatchExpressionFunc
matchExpression' (ExMeta meta) tgt = [substSingle meta (MvExpression tgt)]
matchExpression' (ExAny slot) tgt = [substSlot slot (MvExpression tgt)]
matchExpression' ExXi ExXi = [substEmpty]
matchExpression' ExRoot ExRoot = [substEmpty]
matchExpression' ExTermination ExTermination = [substEmpty]
matchExpression' (ExFormation pbs) (ExFormation tbs) = matchBindings pbs tbs
matchExpression' (ExDispatch pexp pattr) (ExDispatch texp tattr) = combineMany (matchAttribute pattr tattr) (matchExpression' pexp texp)
matchExpression' (ExApplication pexp parg) (ExApplication texp targ) = combineMany (matchExpression' pexp texp) (matchArgument parg targ)
matchExpression' (ExPhiAgain prefix idx expr) (ExPhiAgain prefix' idx' expr')
  | prefix == prefix' && idx == idx' = matchExpression' expr expr'
  | otherwise = []
matchExpression' (ExPhiMeet prefix idx expr) (ExPhiMeet prefix' idx' expr')
  | prefix == prefix' && idx == idx' = matchExpression' expr expr'
  | otherwise = []
matchExpression' _ _ = []

-- Deep match pattern to expression inside binding
matchBindingExpression :: Binding -> Expression -> [Subst]
matchBindingExpression (BiTau _ expr) ptn = matchExpressionDeep ptn expr
matchBindingExpression _ _ = []

matchArgumentExpression :: Argument -> Expression -> [Subst]
matchArgumentExpression (ArTau _ expr) ptn = matchExpressionDeep ptn expr
matchArgumentExpression (ArAlpha _ expr) ptn = matchExpressionDeep ptn expr

-- Match expression with deep nested expression(s) matching
matchExpressionDeep :: MatchExpressionFunc
matchExpressionDeep ptn tgt =
  let matched = matchExpression' ptn tgt
      deep = case tgt of
        ExFormation bds -> concatMap (`matchBindingExpression` ptn) bds
        ExDispatch expr _ -> matchExpressionDeep ptn expr
        ExApplication expr arg -> matchExpressionDeep ptn expr ++ matchArgumentExpression arg ptn
        _ -> []
   in matched ++ deep

matchExpression :: MatchExpressionFunc
matchExpression = matchExpressionDeep

-- Whether the pattern could match at some place of the target where the deep
-- matcher looks, judged by the shape of each place alone: the constructors
-- down the head of the pattern, the attribute a dispatch or an application
-- names, and the kinds of bindings a formation of the pattern asks for. It
-- never says no where 'matchExpressionDeep' would find a match, and it walks
-- the target once without building a single substitution, so a rule whose
-- pattern fits nowhere in a term is told so without the deep matcher trying
-- it at every place of that term (#1453).
reachable :: Expression -> Expression -> Bool
reachable ptn = go
  where
    go :: Expression -> Bool
    go tgt =
      fits ptn tgt || case tgt of
        ExFormation bds -> any inside bds
        ExDispatch expr _ -> go expr
        ExApplication expr (ArTau _ arg) -> go expr || go arg
        ExApplication expr (ArAlpha _ arg) -> go expr || go arg
        _ -> False
    inside :: Binding -> Bool
    inside (BiTau _ expr) = go expr
    inside _ = False
    fits :: Expression -> Expression -> Bool
    fits (ExMeta _) _ = True
    fits (ExAny _) _ = True
    fits ExXi ExXi = True
    fits ExRoot ExRoot = True
    fits ExTermination ExTermination = True
    fits (ExFormation pbs) (ExFormation tbs) = all (\pbd -> loose pbd || any (kin pbd) tbs) pbs
    fits (ExDispatch pexp pattr) (ExDispatch texp tattr) = same pattr tattr && fits pexp texp
    fits (ExApplication pexp (ArTau pattr _)) (ExApplication texp (ArTau tattr _)) = same pattr tattr && fits pexp texp
    fits (ExApplication pexp (ArAlpha _ _)) (ExApplication texp (ArAlpha _ _)) = fits pexp texp
    fits (ExPhiAgain{}) (ExPhiAgain{}) = True
    fits (ExPhiMeet{}) (ExPhiMeet{}) = True
    fits _ _ = False
    loose :: Binding -> Bool
    loose (BiMeta _) = True
    loose (BiAny _) = True
    loose _ = False
    kin :: Binding -> Binding -> Bool
    kin (BiTau pattr _) (BiTau tattr _) = same pattr tattr
    kin (BiVoid pattr) (BiVoid tattr) = same pattr tattr
    kin (BiLambda _) (BiLambda _) = True
    kin (BiDelta _) (BiDelta _) = True
    kin _ _ = False
    same :: Attribute -> Attribute -> Bool
    same (AtMeta _) _ = True
    same (AtAny _) _ = True
    same pattr tattr = pattr == tattr
