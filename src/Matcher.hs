-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse given Ast and build substitutions
-- from meta variables to appropriate meta values
module Matcher where

import Ast
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

-- Meta value
-- The right part of substitution
data MetaValue
  = MvAttribute Attribute -- !a
  | MvBytes String -- !b
  | MvBindings [Binding] -- !B
  | MvFunction String -- !F
  | MvExpression Expression Expression -- !e, the second expression is scope, which is closest formation
  | MvTail [Tail] -- !t
  deriving (Eq, Show)

-- Tail operation after expression
-- Dispatch or application
data Tail
  = TaApplication Binding -- BiTau only
  | TaDispatch Attribute
  deriving (Eq, Show)

-- Substitution
-- Shows the match of meta name to meta value
newtype Subst = Subst (Map String MetaValue)
  deriving (Eq, Show)

-- Empty substitution
substEmpty :: Subst
substEmpty = Subst Map.empty

-- Singleton substitution with one (key -> value) pair
substSingle :: String -> MetaValue -> Subst
substSingle key value = Subst (Map.singleton key value)

defaultScope :: Expression
defaultScope = ExFormation [BiVoid AtRho]

-- Combine two substitutions into a single one
-- Fails if values by the same keys are not equal
combine :: Subst -> Subst -> Maybe Subst
combine (Subst a) (Subst b) = combine' (Map.toList b) a
  where
    combine' :: [(String, MetaValue)] -> Map String MetaValue -> Maybe Subst
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

matchBinding :: Binding -> Binding -> Expression -> [Subst]
matchBinding (BiVoid pattr) (BiVoid tattr) _ = matchAttribute pattr tattr
matchBinding (BiDelta pbts) (BiDelta tbts) _
  | pbts == tbts = [substEmpty]
  | otherwise = []
matchBinding (BiMetaDelta meta) (BiDelta tBts) _ = [substSingle meta (MvBytes tBts)]
matchBinding (BiLambda pFunc) (BiLambda tFunc) _
  | pFunc == tFunc = [substEmpty]
  | otherwise = []
matchBinding (BiMetaLambda meta) (BiLambda tFunc) _ = [substSingle meta (MvFunction tFunc)]
matchBinding (BiTau pattr pexp) (BiTau tattr texp) scope = combineMany (matchAttribute pattr tattr) (matchExpression pexp texp scope)
matchBinding _ _ _ = []

-- Match bindings with ordering
matchBindings :: [Binding] -> [Binding] -> Expression -> [Subst]
matchBindings [] [] _ = [substEmpty]
matchBindings [] _ _ = []
matchBindings ((BiMeta name) : pbs) tbs scope =
  let splits = [splitAt idx tbs | idx <- [0 .. length tbs]]
   in catMaybes
        [ combine (substSingle name (MvBindings before)) subst
          | (before, after) <- splits,
            subst <- matchBindings pbs after scope
        ]
matchBindings (pb : pbs) (tb : tbs) scope = combineMany (matchBinding pb tb scope) (matchBindings pbs tbs scope)
matchBindings _ _ _ = []

-- Recursively go through given target expression and try to find
-- the head expression which matches to given pattern.
-- If there's one - build the list of all the tail operations after head expression.
-- The tail operations may be only dispatches or applications
tailExpressions :: Expression -> Expression -> Expression -> ([Subst], [Tail])
tailExpressions ptn tgt scope =
  let (substs, tails) = tailExpressionsReversed ptn tgt
   in (substs, reverse tails)
  where
    tailExpressionsReversed :: Expression -> Expression -> ([Subst], [Tail])
    tailExpressionsReversed ptn' tgt' = case matchExpression ptn' tgt' scope of
      [] -> case tgt' of
        ExDispatch expr attr ->
          let (substs, tails) = tailExpressionsReversed ptn' expr
           in (substs, TaDispatch attr : tails)
        ExApplication expr tau ->
          let (substs, tails) = tailExpressionsReversed ptn' expr
           in (substs, TaApplication tau : tails)
        _ -> ([], [])
      substs -> (substs, [])

matchExpression :: Expression -> Expression -> Expression -> [Subst]
matchExpression (ExMeta meta) tgt scope = [substSingle meta (MvExpression tgt scope)]
matchExpression ExThis ExThis _ = [substEmpty]
matchExpression ExGlobal ExGlobal _ = [substEmpty]
matchExpression ExTermination ExTermination _ = [substEmpty]
matchExpression (ExFormation pbs) (ExFormation tbs) _ = matchBindings pbs tbs (ExFormation tbs)
matchExpression (ExDispatch pexp pattr) (ExDispatch texp tattr) scope = combineMany (matchAttribute pattr tattr) (matchExpression pexp texp scope)
matchExpression (ExApplication pexp pbd) (ExApplication texp tbd) scope = combineMany (matchExpression pexp texp scope) (matchBinding pbd tbd scope)
matchExpression (ExMetaTail exp meta) tgt scope = case tailExpressions exp tgt scope of
  ([], _) -> []
  (substs, tails) -> combineMany substs [substSingle meta (MvTail tails)]
matchExpression _ _ _ = []

-- Deep match pattern to expression inside binding
matchBindingExpression :: Binding -> Expression -> Expression -> [Subst]
matchBindingExpression (BiTau _ texp) ptn scope = matchExpressionDeep ptn texp scope
matchBindingExpression _ _ _ = []

-- Match expression with deep nested expression(s) matching
matchExpressionDeep :: Expression -> Expression -> Expression -> [Subst]
matchExpressionDeep ptn tgt scope =
  let matched = matchExpression ptn tgt scope
      deep = case tgt of
        ExFormation bds -> concatMap (\bd -> matchBindingExpression bd ptn (ExFormation bds)) bds
        ExDispatch exp _ -> matchExpressionDeep ptn exp scope
        ExApplication exp tau -> matchExpressionDeep ptn exp scope ++ matchBindingExpression tau ptn scope
        _ -> []
   in matched ++ deep

matchProgram :: Expression -> Program -> [Subst]
matchProgram ptn (Program exp) = matchExpressionDeep ptn exp defaultScope