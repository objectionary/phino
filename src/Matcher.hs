{-# LANGUAGE LambdaCase #-}

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
  | MvExpression Expression -- !e
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

matchBinding :: Binding -> Binding -> [Subst]
matchBinding (BiVoid pattr) (BiVoid tattr) = matchAttribute pattr tattr
matchBinding (BiDelta pbts) (BiDelta tbts)
  | pbts == tbts = [substEmpty]
  | otherwise = []
matchBinding (BiMetaDelta meta) (BiDelta tBts) = [substSingle meta (MvBytes tBts)]
matchBinding (BiLambda pFunc) (BiLambda tFunc)
  | pFunc == tFunc = [substEmpty]
  | otherwise = []
matchBinding (BiMetaLambda meta) (BiLambda tFunc) = [substSingle meta (MvFunction tFunc)]
matchBinding (BiTau pattr pexp) (BiTau tattr texp) = combineMany (matchAttribute pattr tattr) (matchExpression pexp texp)
matchBinding _ _ = []

-- Match bindings with ordering
matchBindings :: [Binding] -> [Binding] -> [Subst]
matchBindings [] [] = [substEmpty]
matchBindings [] _ = []
matchBindings ((BiMeta name) : pbs) tbs =
  let splits = [splitAt idx tbs | idx <- [0 .. length tbs]]
   in catMaybes
        [ combine (substSingle name (MvBindings before)) subst
          | (before, after) <- splits,
            subst <- matchBindings pbs after
        ]
matchBindings (pb : pbs) (tb : tbs) = combineMany (matchBinding pb tb) (matchBindings pbs tbs)
matchBindings _ _ = []

-- Recursively go through given target expression and try to find
-- the head expression which matches to given pattern.
-- If there's one - build the list of all the tail operations after head expression.
-- The tail operations may be only dispatches or applications
tailExpressions :: Expression -> Expression -> ([Subst], [Tail])
tailExpressions ptn tgt = case tailExpressionsReversed ptn tgt of
  Just (substs, tails) -> (substs, reverse tails)
  _ -> ([], [])
  where
    tailExpressionsReversed :: Expression -> Expression -> Maybe ([Subst], [Tail])
    tailExpressionsReversed ptn' tgt' = case matchExpression ptn' tgt' of
      [] -> case tgt' of
        ExDispatch expr attr -> do
          (substs, tails) <- tailExpressionsReversed ptn' expr
          Just (substs, TaDispatch attr : tails)
        ExApplication expr tau -> do
          (substs, tails) <- tailExpressionsReversed ptn' expr
          if not (null tails) && isDispatch (head tails)
            then Just (substs, TaApplication tau : tails)
            else Nothing
          where
            isDispatch :: Tail -> Bool
            isDispatch = \case
              TaDispatch _ -> True
              TaApplication _ -> False
        _ -> Just ([], [])
      substs -> Just (substs, [])

matchExpression :: Expression -> Expression -> [Subst]
matchExpression (ExMeta meta) tgt = [substSingle meta (MvExpression tgt)]
matchExpression ExThis ExThis = [substEmpty]
matchExpression ExGlobal ExGlobal = [substEmpty]
matchExpression ExTermination ExTermination = [substEmpty]
matchExpression (ExFormation pbs) (ExFormation tbs) = matchBindings pbs tbs
matchExpression (ExDispatch pexp pattr) (ExDispatch texp tattr) = combineMany (matchAttribute pattr tattr) (matchExpression pexp texp)
matchExpression (ExApplication pexp pbd) (ExApplication texp tbd) = combineMany (matchExpression pexp texp) (matchBinding pbd tbd)
matchExpression (ExMetaTail exp meta) tgt = case tailExpressions exp tgt of
  ([], _) -> []
  (substs, tails) -> combineMany substs [substSingle meta (MvTail tails)]
matchExpression _ _ = []

-- Deep match pattern to expression inside binding
matchBindingExpression :: Binding -> Expression -> [Subst]
matchBindingExpression (BiTau _ texp) ptn = matchExpressionDeep ptn texp
matchBindingExpression _ _ = []

-- Match expression with deep nested expression(s) matching
matchExpressionDeep :: Expression -> Expression -> [Subst]
matchExpressionDeep ptn tgt =
  let matched = matchExpression ptn tgt
      deep = case tgt of
        ExFormation bds -> concatMap (`matchBindingExpression` ptn) bds
        ExDispatch exp _ -> matchExpressionDeep ptn exp
        ExApplication exp tau -> matchExpressionDeep ptn exp ++ matchBindingExpression tau ptn
        _ -> []
   in matched ++ deep

matchProgram :: Expression -> Program -> [Subst]
matchProgram ptn (Program exp) = matchExpressionDeep ptn exp
