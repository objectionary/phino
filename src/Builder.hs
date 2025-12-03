{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to build phi expression based on
-- pattern expression and set of substitutions by replacing
-- meta variables with appropriate meta values
module Builder
  ( buildExpressions
  , buildExpression
  , buildExpressionThrows
  , buildAttribute
  , buildAttributeThrows
  , buildBinding
  , buildBindingThrows
  , buildBytes
  , buildBytesThrows
  , contextualize
  , BuildException (..)
  )
where

import AST
import Control.Exception (Exception, throwIO)
import qualified Data.Map.Strict as Map
import Matcher
import Misc (uniqueBindings)
import Printer
import Text.Printf (printf)
import Yaml (ExtraArgument (..))

data BuildException
  = CouldNotBuildExpression {_expr :: Expression, _msg :: String}
  | CouldNotBuildAttribute {_attr :: Attribute, _msg :: String}
  | CouldNotBuildBinding {_bd :: Binding, _msg :: String}
  | CouldNotBuildBytes {_bts :: Bytes, _msg :: String}
  deriving (Exception)

metaMsg :: String -> String
metaMsg = printf "meta '%s' is either does not exist or refers to an inappropriate term"

type Built a = Either String a

instance Show BuildException where
  show CouldNotBuildExpression{..} =
    printf
      "Couldn't build expression, %s\n--Expression: %s"
      _msg
      (printExpression _expr)
  show CouldNotBuildAttribute{..} =
    printf
      "Couldn't build attribute '%s', %s"
      (printAttribute _attr)
      _msg
  show CouldNotBuildBinding{..} =
    printf
      "Couldn't build binding, %s\n--Binding: %s"
      _msg
      (printBinding _bd)
  show CouldNotBuildBytes{..} =
    printf
      "Couldn't build bytes '%s', %s"
      (printBytes _bts)
      _msg

contextualize :: Expression -> Expression -> Expression
contextualize ExGlobal _ = ExGlobal
contextualize ExThis expr = expr
contextualize ExTermination _ = ExTermination
contextualize (ExFormation bds) _ = ExFormation bds
contextualize (ExDispatch expr attr) context = ExDispatch (contextualize expr context) attr
contextualize (ExApplication expr (BiTau attr bexpr)) context =
  let expr' = contextualize expr context
      bexpr' = contextualize bexpr context
   in ExApplication expr' (BiTau attr bexpr')

buildAttribute :: Attribute -> Subst -> Built Attribute
buildAttribute (AtMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvAttribute attr) -> Right attr
  _ -> Left (metaMsg meta)
buildAttribute attr _ = Right attr

buildBytes :: Bytes -> Subst -> Built Bytes
buildBytes (BtMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvBytes bytes) -> Right bytes
  _ -> Left (metaMsg meta)
buildBytes bts _ = Right bts

-- Build binding
-- The function returns [Binding] because the BiMeta is always attached
-- to the list of bindings
buildBinding :: Binding -> Subst -> Built [Binding]
buildBinding (BiTau attr expr) subst = do
  attribute <- buildAttribute attr subst
  (expression, _) <- buildExpression expr subst
  Right [BiTau attribute expression]
buildBinding (BiVoid attr) subst = do
  attribute <- buildAttribute attr subst
  Right [BiVoid attribute]
buildBinding (BiMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvBindings bds) -> uniqueBindings bds
  _ -> Left (metaMsg meta)
buildBinding (BiDelta bytes) subst = do
  bts <- buildBytes bytes subst
  Right [BiDelta bts]
buildBinding (BiMetaLambda meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvFunction func) -> Right [BiLambda func]
  _ -> Left (metaMsg meta)
buildBinding binding _ = Right [binding]

-- Build bindings that may contain meta binding (BiMeta)
buildBindings :: [Binding] -> Subst -> Built [Binding]
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
buildExpression :: Expression -> Subst -> Built (Expression, Expression)
buildExpression (ExDispatch expr attr) subst = do
  (dispatched, scope) <- buildExpression expr subst
  attr <- buildAttribute attr subst
  Right (ExDispatch dispatched attr, scope)
buildExpression (ExApplication expr (BiTau battr bexpr)) subst = do
  (applied, scope) <- buildExpression expr subst
  bds <- buildBinding (BiTau battr bexpr) subst
  Right (ExApplication applied (head bds), scope)
buildExpression (ExFormation bds) subst = do
  bds' <- buildBindings bds subst >>= uniqueBindings
  Right (ExFormation bds', defaultScope)
buildExpression (ExMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvExpression expr scope) ->
    let res = Right (expr, scope)
     in case expr of
          ExFormation bds -> uniqueBindings bds >> res
          _ -> res
  _ -> Left (metaMsg meta)
buildExpression (ExMetaTail expr meta) subst = do
  let (Subst mp) = subst
  (expression, scope) <- buildExpression expr subst
  case Map.lookup meta mp of
    Just (MvTail tails) -> Right (buildExpressionWithTails expression tails subst, scope)
    _ -> Left (metaMsg meta)
buildExpression expr _ = Right (expr, defaultScope)

buildBytesThrows :: Bytes -> Subst -> IO Bytes
buildBytesThrows bytes subst = case buildBytes bytes subst of
  Right bts -> pure bts
  Left msg -> throwIO (CouldNotBuildBytes bytes msg)

buildBindingThrows :: Binding -> Subst -> IO [Binding]
buildBindingThrows bd subst = case buildBinding bd subst of
  Right bds -> pure bds
  Left msg -> throwIO (CouldNotBuildBinding bd msg)

buildAttributeThrows :: Attribute -> Subst -> IO Attribute
buildAttributeThrows attr subst = case buildAttribute attr subst of
  Right attr' -> pure attr'
  Left msg -> throwIO (CouldNotBuildAttribute attr msg)

buildExpressionThrows :: Expression -> Subst -> IO (Expression, Expression)
buildExpressionThrows expr subst = case buildExpression expr subst of
  Right built -> pure built
  Left msg -> throwIO (CouldNotBuildExpression expr msg)

-- Build a several expression from one expression and several substitutions
buildExpressions :: Expression -> [Subst] -> IO [(Expression, Expression)]
buildExpressions expr = traverse (buildExpressionThrows expr)
