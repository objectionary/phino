{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to build phi expression based on
-- pattern expression and set of substitutions by replacing
-- meta variables with appropriate meta values
module Builder
  ( buildExpressions,
    buildExpression,
    buildExpressionThrows,
    buildAttribute,
    buildAttributeThrows,
    buildBinding,
    buildBindingThrows,
    buildBytes,
    buildBytesThrows,
    contextualize,
    BuildException (..),
  )
where

import Ast
import Control.Exception (Exception, throwIO)
import qualified Data.Map.Strict as Map
import Matcher
import Pretty (prettyAttribute, prettyBinding, prettyExpression, prettySubst, prettyBytes)
import Text.Printf (printf)
import Yaml (ExtraArgument (..))

data BuildException
  = CouldNotBuildExpression {_expr :: Expression, _subst :: Subst}
  | CouldNotBuildAttribute {_attr :: Attribute, _subst :: Subst}
  | CouldNotBuildBinding {_bd :: Binding, _subst :: Subst}
  | CouldNotBuildBytes {_bts :: Bytes, _subst :: Subst}
  deriving (Exception)

instance Show BuildException where
  show CouldNotBuildExpression {..} =
    printf
      "Couldn't build given expression with provided substitutions\n--Expression: %s\n--Substitutions: %s"
      (prettyExpression _expr)
      (prettySubst _subst)
  show CouldNotBuildAttribute {..} =
    printf
      "Couldn't build given attribute with provided substitutions\n--Attribute: %s\n--Substitutions: %s"
      (prettyAttribute _attr)
      (prettySubst _subst)
  show CouldNotBuildBinding {..} =
    printf
      "Couldn't build given binding with provided substitutions\n--Binding: %s\n--Substitutions: %s"
      (prettyBinding _bd)
      (prettySubst _subst)
  show CouldNotBuildBytes {..} =
    printf
      "Couldn't build given bytes with provided substitutions\n--Bytes: %s\n--Substitutions: %s"
      (prettyBytes _bts)
      (prettySubst _subst)

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

buildBytes :: Bytes -> Subst -> Maybe Bytes
buildBytes (BtMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvBytes bytes) -> Just bytes
  _ -> Nothing
buildBytes bts _ = Just bts

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
buildBinding (BiDelta bytes) subst = do
  bts <- buildBytes bytes subst
  Just [BiDelta bts]
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

buildBytesThrows :: Bytes -> Subst -> IO Bytes
buildBytesThrows bytes subst = case buildBytes bytes subst of
  Just bts -> pure bts
  _ -> throwIO (CouldNotBuildBytes bytes subst)

buildBindingThrows :: Binding -> Subst -> IO [Binding]
buildBindingThrows bd subst = case buildBinding bd subst of
  Just bds -> pure bds
  _ -> throwIO (CouldNotBuildBinding bd subst)

buildAttributeThrows :: Attribute -> Subst -> IO Attribute
buildAttributeThrows attr subst = case buildAttribute attr subst of
  Just attr' -> pure attr'
  _ -> throwIO (CouldNotBuildAttribute attr subst)

buildExpressionThrows :: Expression -> Subst -> IO (Expression, Expression)
buildExpressionThrows expr subst = case buildExpression expr subst of
  Just built -> pure built
  _ -> throwIO (CouldNotBuildExpression expr subst)

-- Build a several expression from one expression and several substitutions
buildExpressions :: Expression -> [Subst] -> IO [(Expression, Expression)]
buildExpressions expr = traverse (buildExpressionThrows expr)
