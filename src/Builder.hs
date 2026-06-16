{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to build phi expression based on
-- pattern expression and set of substitutions by replacing
-- meta variables with appropriate meta values
module Builder
  ( buildExpressionsThrows
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
  , Built
  )
where

import AST
import Control.Exception (Exception, throwIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Matcher
import Misc (uniqueBindings)
import Printer
import Text.Printf (printf)

data BuildException
  = CouldNotBuildExpression {_expr :: Expression, _msg :: String}
  | CouldNotBuildAttribute {_attr :: Attribute, _msg :: String}
  | CouldNotBuildBinding {_bd :: Binding, _msg :: String}
  | CouldNotBuildBytes {_bts :: Bytes, _msg :: String}
  deriving (Exception)

metaMsg :: Text -> String
metaMsg = printf "meta '%s' is either does not exist or refers to an inappropriate term" . T.unpack

type Built a = Either String a

instance Show BuildException where
  show CouldNotBuildExpression{..} = printf "Couldn't build expression, %s\n--Expression: %s" _msg (printExpression _expr)
  show CouldNotBuildAttribute{..} = printf "Couldn't build attribute '%s', %s" (printAttribute _attr) _msg
  show CouldNotBuildBinding{..} = printf "Couldn't build binding, %s\n--Binding: %s" _msg (printBinding _bd)
  show CouldNotBuildBytes{..} = printf "Couldn't build bytes '%s', %s" (printBytes _bts) _msg

contextualize :: Expression -> Expression -> Expression
contextualize ExRoot _ = ExRoot
contextualize ExXi ex = ex
contextualize ExTermination _ = ExTermination
contextualize (ExFormation bds) _ = ExFormation bds
contextualize (ExDispatch ex at) context = ExDispatch (contextualize ex context) at
contextualize (ExApplication ex arg) context =
  ExApplication (contextualize ex context) (contextualizeArg arg)
  where
    contextualizeArg (ArTau at bexpr) = ArTau at (contextualize bexpr context)
    contextualizeArg (ArAlpha al bexpr) = ArAlpha al (contextualize bexpr context)
contextualize ex _ = ex

buildAttribute :: Attribute -> Subst -> Built Attribute
buildAttribute (AtMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvAttribute attr) -> Right attr
  _ -> Left (metaMsg meta)
buildAttribute attr _ = Right attr

buildAlpha :: Alpha -> Subst -> Built Alpha
buildAlpha (AlMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvAlpha a) -> Right a
  _ -> Left (metaMsg meta)
buildAlpha a _ = Right a

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
  expression <- buildExpression expr subst
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
buildBinding (BiLambda (FnMeta meta)) (Subst mp) = case Map.lookup meta mp of
  Just (MvFunction func) -> Right [BiLambda (Function func)]
  _ -> Left (metaMsg meta)
buildBinding binding _ = Right [binding]

buildArgument :: Argument -> Subst -> Built Argument
buildArgument (ArTau attr expr) subst = do
  attribute <- buildAttribute attr subst
  expression <- buildExpression expr subst
  Right (ArTau attribute expression)
buildArgument (ArAlpha alpha expr) subst = do
  alpha' <- buildAlpha alpha subst
  expression <- buildExpression expr subst
  Right (ArAlpha alpha' expression)

-- Build bindings that may contain meta binding (BiMeta)
buildBindings :: [Binding] -> Subst -> Built [Binding]
buildBindings [] _ = Right []
buildBindings (bd : rest) subst = do
  first <- buildBinding bd subst
  bds <- buildBindings rest subst
  Right (first ++ bds)

buildExpressionWithTails :: Expression -> [Tail] -> Subst -> Expression
buildExpressionWithTails expr [] _ = expr
buildExpressionWithTails ex (tl : rest) subst = case tl of
  TaApplication arg -> buildExpressionWithTails (ExApplication ex arg) rest subst
  TaDispatch at -> buildExpressionWithTails (ExDispatch ex at) rest subst

-- Build meta expression with given substitution
buildExpression :: Expression -> Subst -> Built Expression
buildExpression (ExDispatch ex at) subst = do
  dispatched <- buildExpression ex subst
  at' <- buildAttribute at subst
  Right (ExDispatch dispatched at')
buildExpression (ExApplication expr arg) subst = do
  applied <- buildExpression expr subst
  arg' <- buildArgument arg subst
  Right (ExApplication applied arg')
buildExpression (ExFormation bds) subst = do
  bds' <- buildBindings bds subst >>= uniqueBindings
  Right (ExFormation bds')
buildExpression (ExMeta meta) (Subst mp) = case Map.lookup meta mp of
  Just (MvExpression expr) ->
    let res = Right expr
     in case expr of
          ExFormation bds -> uniqueBindings bds >> res
          _ -> res
  _ -> Left (metaMsg meta)
buildExpression (ExMetaTail expr meta) subst = do
  let (Subst mp) = subst
  expression <- buildExpression expr subst
  case Map.lookup meta mp of
    Just (MvTail tails) -> Right (buildExpressionWithTails expression tails subst)
    _ -> Left (metaMsg meta)
buildExpression expr _ = Right expr

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

buildExpressionThrows :: Expression -> Subst -> IO Expression
buildExpressionThrows expr subst = case buildExpression expr subst of
  Right built -> pure built
  Left msg -> throwIO (CouldNotBuildExpression expr msg)

-- Build a several expression from one expression and several substitutions
buildExpressionsThrows :: Expression -> [Subst] -> IO [Expression]
buildExpressionsThrows expr = traverse (buildExpressionThrows expr)
