-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to build phi expression based on
-- pattern expression and set of substitutions by replacing
-- meta variables with appropriate meta values
module Builder
  ( buildExpressions,
    buildExpression,
    buildExpressionFromFunction,
    buildAttribute,
    buildBinding
  )
where

import Ast
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Matcher
import Misc

contextualize :: Expression -> Expression -> Program -> Maybe Expression
contextualize ExGlobal _ (Program expr) = Just expr
contextualize ExThis expr _ = Just expr
contextualize (ExFormation bds) _ _ = Just (ExFormation bds)
contextualize (ExDispatch expr attr) context prog = do
  inner <- contextualize expr context prog
  Just (ExDispatch inner attr)
contextualize (ExApplication expr (BiTau attr bexpr)) context prog = do
  expr' <- contextualize expr context prog
  bexpr' <- contextualize bexpr context prog
  Just (ExApplication expr' (BiTau attr bexpr'))

buildAttribute :: Attribute -> Subst -> Maybe Attribute
buildAttribute (AtMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvAttribute attr) -> Just attr
  _ -> Nothing
buildAttribute attr _ = Just attr

-- Build binding
-- The function returns [Binding] because the BiMeta is always attached
-- to the list of bindings
buildBinding :: Binding -> Subst -> Maybe [Binding]
buildBinding (BiTau attr expr) subst = do
  attribute <- buildAttribute attr subst
  expression <- buildExpression expr subst
  Just [BiTau attribute expression]
buildBinding (BiVoid attr) subst = do
  attribute <- buildAttribute attr subst
  Just [BiVoid attribute]
buildBinding (BiMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvBindings bds) -> Just bds
  _ -> Nothing
buildBinding (BiMetaDelta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvBytes bytes) -> Just [BiDelta bytes]
  _ -> Nothing
buildBinding (BiMetaLambda meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvFunction func) -> Just [BiLambda func]
  _ -> Nothing
buildBinding binding _ = Just [binding]

-- Build bindings that may contain meta binding (BiMeta)
buildBindings :: [Binding] -> Subst -> Maybe [Binding]
buildBindings [] _ = Just []
buildBindings (bd : rest) subst = do
  first <- buildBinding bd subst
  bds <- buildBindings rest subst
  Just (first ++ bds)

buildExpressionWithTails :: Expression -> [Tail] -> Subst -> Expression
buildExpressionWithTails expr [] _ = expr
buildExpressionWithTails expr (tail : rest) subst = case tail of
  TaApplication taus -> buildExpressionWithTails (ExApplication expr taus) rest subst
  TaDispatch attr -> buildExpressionWithTails (ExDispatch expr attr) rest subst

buildExpression :: Expression -> Subst -> Maybe Expression
buildExpression (ExDispatch expr attr) subst = do
  dispatched <- buildExpression expr subst
  attr <- buildAttribute attr subst
  return (ExDispatch dispatched attr)
buildExpression (ExApplication expr (BiTau battr bexpr)) subst = do
  applied <- buildExpression expr subst
  [binding] <- buildBinding (BiTau battr bexpr) subst
  Just (ExApplication applied binding)
buildExpression (ExApplication _ _) _ = Nothing
buildExpression (ExFormation bds) subst = buildBindings bds subst >>= (Just . ExFormation)
buildExpression (ExMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvExpression expr) -> Just expr
  _ -> Nothing
buildExpression (ExMetaTail expr meta) subst = do
  let (Subst mp) = subst
  expression <- buildExpression expr subst
  case Map.lookup meta mp of
    Just (MvTail tails) -> Just (buildExpressionWithTails expression tails subst)
    _ -> Nothing
buildExpression expr _ = Just expr

buildExpressionFromFunction :: String -> [Expression] -> Subst -> Program -> Maybe Expression
buildExpressionFromFunction "contextualize" [expr, context] subst prog = do
  expr' <- buildExpression expr subst
  context' <- buildExpression context subst
  contextualize expr' context' prog
buildExpressionFromFunction _ _ _ _ = Nothing

-- Build a several expression from one expression and several substitutions
buildExpressions :: Expression -> [Subst] -> Maybe [Expression]
buildExpressions expr = traverse (buildExpression expr)
