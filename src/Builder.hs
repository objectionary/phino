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
  = CouldNotBuildExpression {_expr :: Expression, _meta :: String}
  | CouldNotBuildAttribute {_attr :: Attribute, _meta :: String}
  | CouldNotBuildBinding {_bd :: Binding, _meta :: String}
  | CouldNotBuildBytes {_bts :: Bytes, _meta :: String}
  deriving (Exception)

metaMsg :: String -> String
metaMsg = printf "meta '%s' is either does not exist or refers to an inapropriate term"

-- @todo #277:30min Error messages are too verbose. Now, if we can't build expression or binding, we
--  throw an exception and just print whole expression or binding to console.
--  If this elements are big, it's just a mess and error message became unreadable. It would be nice to
--  print expression or binding in some reduce way, removing some parts or printing only first N lines
instance Show BuildException where
  show CouldNotBuildExpression {..} =
    printf
      "Couldn't build expression, %s\n--Expression: %s"
      (metaMsg _meta)
      (prettyExpression _expr)
  show CouldNotBuildAttribute {..} =
    printf
      "Couldn't build attribute '%s', %s"
      (prettyAttribute _attr)
      (metaMsg _meta)
  show CouldNotBuildBinding {..} =
    printf
      "Couldn't build binding, %s\n--Binding: %s"
      (metaMsg _meta)
      (prettyBinding _bd)
  show CouldNotBuildBytes {..} =
    printf
      "Couldn't build bytes '%s', %s"
      (prettyBytes _bts)
      (metaMsg _meta)

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

buildAttribute :: Attribute -> Subst -> Either String Attribute
buildAttribute (AtMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvAttribute attr) -> Right attr
  _ -> Left meta
buildAttribute attr _ = Right attr

buildBytes :: Bytes -> Subst -> Either String Bytes
buildBytes (BtMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvBytes bytes) -> Right bytes
  _ -> Left meta
buildBytes bts _ = Right bts

-- Build binding
-- The function returns [Binding] because the BiMeta is always attached
-- to the list of bindings
buildBinding :: Binding -> Subst -> Either String [Binding]
buildBinding (BiTau attr expr) subst = do
  attribute <- buildAttribute attr subst
  (expression, _) <- buildExpression expr subst
  Right [BiTau attribute expression]
buildBinding (BiVoid attr) subst = do
  attribute <- buildAttribute attr subst
  Right [BiVoid attribute]
buildBinding (BiMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvBindings bds) -> Right bds
  _ -> Left meta
buildBinding (BiDelta bytes) subst = do
  bts <- buildBytes bytes subst
  Right [BiDelta bts]
buildBinding (BiMetaLambda meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvFunction func) -> Right [BiLambda func]
  _ -> Left meta
buildBinding binding _ = Right [binding]

-- Build bindings that may contain meta binding (BiMeta)
buildBindings :: [Binding] -> Subst -> Either String [Binding]
buildBindings [] _ = Right []
buildBindings (bd : rest) subst = do
  first <- buildBinding bd subst
  bds <- buildBindings rest subst
  Right (first ++ bds)

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
buildExpression :: Expression -> Subst -> Either String (Expression, Expression)
buildExpression (ExDispatch expr attr) subst = do
  (dispatched, scope) <- buildExpression expr subst
  attr <- buildAttribute attr subst
  Right (ExDispatch dispatched attr, scope)
buildExpression (ExApplication expr (BiTau battr bexpr)) subst = do
  (applied, scope) <- buildExpression expr subst
  bds <- buildBinding (BiTau battr bexpr) subst
  Right (ExApplication applied (head bds), scope)
buildExpression (ExFormation bds) subst = do
  bds' <- buildBindings bds subst
  Right (ExFormation bds', defaultScope)
buildExpression (ExMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvExpression expr scope) -> Right (expr, scope)
  _ -> Left meta
buildExpression (ExMetaTail expr meta) subst = do
  let (Subst mp) = subst
  (expression, scope) <- buildExpression expr subst
  case Map.lookup meta mp of
    Just (MvTail tails) -> Right (buildExpressionWithTails expression tails subst, scope)
    _ -> Left meta
buildExpression expr _ = Right (expr, defaultScope)

buildBytesThrows :: Bytes -> Subst -> IO Bytes
buildBytesThrows bytes subst = case buildBytes bytes subst of
  Right bts -> pure bts
  Left meta -> throwIO (CouldNotBuildBytes bytes meta)

buildBindingThrows :: Binding -> Subst -> IO [Binding]
buildBindingThrows bd subst = case buildBinding bd subst of
  Right bds -> pure bds
  Left meta -> throwIO (CouldNotBuildBinding bd meta)

buildAttributeThrows :: Attribute -> Subst -> IO Attribute
buildAttributeThrows attr subst = case buildAttribute attr subst of
  Right attr' -> pure attr'
  Left meta -> throwIO (CouldNotBuildAttribute attr meta)

buildExpressionThrows :: Expression -> Subst -> IO (Expression, Expression)
buildExpressionThrows expr subst = case buildExpression expr subst of
  Right built -> pure built
  Left meta -> throwIO (CouldNotBuildExpression expr meta)

-- Build a several expression from one expression and several substitutions
buildExpressions :: Expression -> [Subst] -> IO [(Expression, Expression)]
buildExpressions expr = traverse (buildExpressionThrows expr)
