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
  | MvFunction Text -- !F
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

matchFunction :: Function -> Function -> [Subst]
matchFunction (FnMeta meta) (Function name) = [substSingle meta (MvFunction name)]
matchFunction (FnAny slot) (Function name) = [substSlot slot (MvFunction name)]
matchFunction ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

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
-- way of splitting the target into that run and the rest is tried.
matchBindingsMeta :: (MetaValue -> Subst) -> [Binding] -> [Binding] -> [Subst]
matchBindingsMeta bind pbs tbs =
  catMaybes
    [ combine (bind (MvBindings before)) subst
    | (before, after) <- [splitAt idx tbs | idx <- [0 .. length tbs]]
    , subst <- matchBindings pbs after
    ]

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
