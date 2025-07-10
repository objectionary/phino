-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to build phi expression based on
-- pattern expression and set of substitutions by replacing
-- meta variables with appropriate meta values
module Builder
  ( buildExpressions,
    buildExpression,
    buildTermFromFunction,
    buildAttribute,
    buildBinding,
    contextualize,
    Term (..),
  )
where

import Ast
import qualified Data.Map.Strict as Map
import Debug.Trace
import Matcher
import Pretty (prettyAttribute)
import Yaml (ExtraArgument (..))

data Term = TeExpression Expression | TeAttribute Attribute

contextualize :: Expression -> Expression -> Program -> Expression
contextualize ExGlobal _ (Program expr) = expr
contextualize ExThis expr _ = expr
contextualize ExTermination _ _ = ExTermination
contextualize (ExFormation bds) _ _ = ExFormation bds
contextualize (ExDispatch expr attr) context prog = ExDispatch (contextualize expr context prog) attr
contextualize (ExApplication expr (BiTau attr bexpr)) context prog =
  let expr' = contextualize expr context prog
      bexpr' = contextualize bexpr context prog
   in ExApplication expr' (BiTau attr bexpr')

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
  (expression, _) <- buildExpression expr subst
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

-- Build meta expression with given substitution
-- It returns tuple (X, Y)
-- where X is built expression and Y is context of X
-- If meta expression is built from MvExpression, is has
-- context from original Program. It have default context otherwise
buildExpression :: Expression -> Subst -> Maybe (Expression, Expression)
buildExpression (ExDispatch expr attr) subst = do
  (dispatched, scope) <- buildExpression expr subst
  attr <- buildAttribute attr subst
  return (ExDispatch dispatched attr, scope)
buildExpression (ExApplication expr (BiTau battr bexpr)) subst = do
  (applied, scope) <- buildExpression expr subst
  [binding] <- buildBinding (BiTau battr bexpr) subst
  Just (ExApplication applied binding, scope)
buildExpression (ExApplication _ _) _ = Nothing
buildExpression (ExFormation bds) subst = do
  bds' <- buildBindings bds subst
  Just (ExFormation bds', defaultScope)
buildExpression (ExMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvExpression expr scope) -> Just (expr, scope)
  _ -> Nothing
buildExpression (ExMetaTail expr meta) subst = do
  let (Subst mp) = subst
  (expression, scope) <- buildExpression expr subst
  case Map.lookup meta mp of
    Just (MvTail tails) -> Just (buildExpressionWithTails expression tails subst, scope)
    _ -> Nothing
buildExpression expr _ = Just (expr, defaultScope)

buildTermFromFunction :: String -> [ExtraArgument] -> Subst -> Program -> Maybe Term
buildTermFromFunction "contextualize" [ArgExpression expr, ArgExpression context] subst prog = do
  (expr', _) <- buildExpression expr subst
  (context', _) <- buildExpression context subst
  return (TeExpression (contextualize expr' context' prog))
buildTermFromFunction "scope" [ArgExpression expr] subst prog = do
  (expr', scope) <- buildExpression expr subst
  return (TeExpression scope)
buildTermFromFunction "random-tau" args subst _ = do
  attrs <- argsToAttrs args
  return (TeAttribute (AtLabel (randomTau 0 attrs)))
  where
    argsToAttrs :: [ExtraArgument] -> Maybe [String]
    argsToAttrs [] = Just []
    argsToAttrs (arg : rest) = case arg of
      ArgExpression _ -> argsToAttrs rest
      ArgAttribute attr -> do
        attr' <- buildAttribute attr subst
        rest' <- argsToAttrs rest
        Just (prettyAttribute attr' : rest')
      ArgBinding bd -> do
        bds <- buildBinding bd subst
        rest' <- argsToAttrs rest
        Just (attrsFromBindings bds ++ rest')
    attrsFromBindings :: [Binding] -> [String]
    attrsFromBindings [] = []
    attrsFromBindings (bd : bds) =
      let attr = case bd of
            BiTau attr _ -> attr
            BiDelta _ -> AtDelta
            BiLambda _ -> AtLambda
            BiVoid attr -> attr
       in prettyAttribute attr : attrsFromBindings bds
    randomTau :: Integer -> [String] -> String
    randomTau _ [] = cactoos
    randomTau idx attrs =
      let tau = cactoos ++ show idx
       in if tau `elem` attrs then randomTau (idx + 1) attrs else tau
    cactoos = "a🌵"
buildTermFromFunction _ _ _ _ = Nothing

-- Build a several expression from one expression and several substitutions
buildExpressions :: Expression -> [Subst] -> Maybe [(Expression, Expression)]
buildExpressions expr = traverse (buildExpression expr)
