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
  = MvAttribute Attribute -- !a
  | MvAlpha Alpha -- !h
  | MvBytes Bytes -- !b
  | MvBindings [Binding] -- !B
  | MvFunction Text -- !F
  | MvExpression Expression -- !e
  deriving (Eq, Show)

-- Substitution
-- Shows the match of meta name to meta value
newtype Subst = Subst (Map Text MetaValue)
  deriving (Eq, Show)

-- A way to match a pattern expression against a target expression, yielding
-- the substitutions under which they agree.
type MatchExpressionFunc = Expression -> Expression -> [Subst]

-- Empty substitution
substEmpty :: Subst
substEmpty = Subst Map.empty

-- Singleton substitution with one (key -> value) pair
substSingle :: Text -> MetaValue -> Subst
substSingle key value = Subst (Map.singleton key value)

-- Combine two substitutions into a single one
-- Fails if values by the same keys are not equal
combine :: Subst -> Subst -> Maybe Subst
combine (Subst a) (Subst b) = combine' (Map.toList b) a
  where
    combine' :: [(Text, MetaValue)] -> Map Text MetaValue -> Maybe Subst
    combine' [] acc = Just (Subst acc)
    combine' ((key, value) : rest) acc = case Map.lookup key acc of
      Just found
        | found == value -> combine' rest acc
        | otherwise -> Nothing
      Nothing -> combine' rest (Map.insert key value acc)

combineMany :: [Subst] -> [Subst] -> [Subst]
combineMany xs xy = catMaybes [combine x y | x <- xs, y <- xy]

matchAttribute :: Attribute -> Attribute -> [Subst]
matchAttribute (AtMeta meta) tgt = [substSingle meta (MvAttribute tgt)]
matchAttribute ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

matchAlpha :: Alpha -> Alpha -> [Subst]
matchAlpha (AlMeta meta) tgt = [substSingle meta (MvAlpha tgt)]
matchAlpha ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

matchFunction :: Function -> Function -> [Subst]
matchFunction (FnMeta meta) (Function name) = [substSingle meta (MvFunction name)]
matchFunction ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

matchBinding :: Binding -> Binding -> [Subst]
matchBinding (BiVoid pattr) (BiVoid tattr) = matchAttribute pattr tattr
matchBinding (BiDelta (BtMeta meta)) (BiDelta tdata) = [substSingle meta (MvBytes tdata)]
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
matchBindings ((BiMeta name) : pbs) tbs =
  let splits = [splitAt idx tbs | idx <- [0 .. length tbs]]
   in catMaybes
        [ combine (substSingle name (MvBindings before)) subst
        | (before, after) <- splits
        , subst <- matchBindings pbs after
        ]
matchBindings (pb : pbs) (tb : tbs) = combineMany (matchBinding pb tb) (matchBindings pbs tbs)
matchBindings _ _ = []

matchExpression' :: MatchExpressionFunc
matchExpression' (ExMeta meta) tgt = [substSingle meta (MvExpression tgt)]
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

matchProgram :: Expression -> Program -> [Subst]
matchProgram ptn (Program expr) = matchExpression ptn expr
