-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to build phi expression based on 
-- pattern expression and set of substitutions by replacing
-- meta variables with appropriate meta values
module Builder where

import Ast
import Misc
import Matcher
import qualified Data.Map.Strict as Map
import Data.List (findIndex)

buildAttribute :: Attribute -> Subst -> Maybe Attribute
buildAttribute (AtMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvAttribute attr) -> Just attr
  _ -> Nothing
buildAttribute attr _ = Just attr

buildTauBinding :: TauBinding -> Subst -> Maybe TauBinding
buildTauBinding (TauBinding attr expr) subst = do
  attribute <- buildAttribute attr subst
  expression <- buildExpression expr subst
  Just (TauBinding attribute expression)

-- Build binding
-- The function returns [Binding] because the BiMeta is always attached
-- to the list of bindings
buildBinding :: Binding -> Subst -> Maybe [Binding]
buildBinding (BiTau tau) subst = do
  binding <- buildTauBinding tau subst
  Just [BiTau binding]
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

-- Build bindings which don't contain meta binding (BiMeta)
buildExactBindings :: [Binding] -> Subst -> Maybe [Binding]
buildExactBindings [] _ = Just []
buildExactBindings (bd:rest) subst = do
  first <- buildBinding bd subst
  bds <- buildExactBindings rest subst
  Just (head first : bds)

-- Build bindings that may contain meta binding (BiMeta)
buildBindings :: [Binding] -> Subst -> Maybe [Binding]
buildBindings [] _ = Just []
buildBindings bindings subst = case findIndex isMetaBinding bindings of
  Just idx -> do
    exact <- buildExactBindings (withoutAt idx bindings) subst
    meta <- buildBinding (bindings !! idx) subst
    Just (exact ++ meta)
  _ -> buildExactBindings bindings subst

buildExpressionWithTails :: Expression -> [Tail] -> Subst -> Expression
buildExpressionWithTails expr [] _ = expr
buildExpressionWithTails expr (tail:rest) subst = case tail of
  TaApplication taus -> buildExpressionWithTails (ExApplication expr taus) rest subst
  TaDispatch attr -> buildExpressionWithTails (ExDispatch expr attr) rest subst

buildExpression :: Expression -> Subst -> Maybe Expression
buildExpression (ExDispatch expr attr) subst = do
  dispatched <- buildExpression expr subst
  attr <- buildAttribute attr subst
  return (ExDispatch dispatched attr)
buildExpression (ExApplication expr taus) subst = do
  applied <- buildExpression expr subst
  bindings <- mapM (`buildTauBinding` subst) taus -- mapM (\tau -> buildTauBinding tau subst) mapM
  Just (ExApplication applied bindings)
buildExpression (ExFormation bds) subst = do
  bindings <- buildBindings bds subst
  Just (ExFormation bindings)
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

-- Build a several expression from one expression and several substitutions
buildExpressions :: Expression -> [Subst] -> [Maybe Expression]
buildExpressions expr = map (buildExpression expr)
