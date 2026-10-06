{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Builder
  ( buildExpressionsThrows
  , buildExpression
  , buildExpressionThrows
  , buildAttribute
  , buildAttributeThrows
  , buildBinding
  , buildBindingThrows
  , buildBindingUnchecked
  , buildBytes
  , buildBytesThrows
  , formed
  , nameIn
  , pathOf
  , BuildException (..)
  )
where

import AST
import Control.Exception (Exception, throw)
import Control.Monad (zipWithM)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Matcher
import Misc (orThrow, uniqueBindings)
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

slotMsg :: Slot -> String
slotMsg (Slot kind _) = printf "anonymous meta '!%s' cannot be referenced" (T.unpack kind)

type Built a = Either String a

instance Show BuildException where
  show CouldNotBuildExpression{..} = printf "Couldn't build expression, %s\n--Expression: %s" _msg (printExpression _expr)
  show CouldNotBuildAttribute{..} = printf "Couldn't build attribute '%s', %s" (printAttribute _attr) _msg
  show CouldNotBuildBinding{..} = printf "Couldn't build binding, %s\n--Binding: %s" _msg (printBinding _bd)
  show CouldNotBuildBytes{..} = printf "Couldn't build bytes '%s', %s" (printBytes _bts) _msg

buildAttribute :: Attribute -> Subst -> Built Attribute
buildAttribute (AtMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvAttribute attr) -> Right attr
  _ -> Left (metaMsg meta)
buildAttribute (AtAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvAttribute attr) -> Right attr
  _ -> Left (slotMsg slot)
buildAttribute attr _ = Right attr

buildAlpha :: Alpha -> Subst -> Built Alpha
buildAlpha (AlMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvIndex idx) -> Right (Alpha idx)
  _ -> Left (metaMsg meta)
buildAlpha (AlAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvIndex idx) -> Right (Alpha idx)
  _ -> Left (slotMsg slot)
buildAlpha a _ = Right a

buildBytes :: Bytes -> Subst -> Built Bytes
buildBytes (BtMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvBytes bytes) -> Right bytes
  _ -> Left (metaMsg meta)
buildBytes (BtAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvBytes bytes) -> Right bytes
  _ -> Left (slotMsg slot)
buildBytes bts _ = Right bts

buildBinding :: Binding -> Subst -> Built [Binding]
buildBinding bd subst = buildBindingUnchecked bd subst >>= uniqueBindings

buildBindingUnchecked :: Binding -> Subst -> Built [Binding]
buildBindingUnchecked (BiTau attr expr) subst = do
  attribute <- buildAttribute attr subst
  expression <- buildExpression expr subst
  Right [BiTau attribute expression]
buildBindingUnchecked (BiVoid attr) subst = do
  attribute <- buildAttribute attr subst
  Right [BiVoid attribute]
buildBindingUnchecked (BiMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvBindings bds) -> Right bds
  _ -> Left (metaMsg meta)
buildBindingUnchecked (BiAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvBindings bds) -> Right bds
  _ -> Left (slotMsg slot)
buildBindingUnchecked (BiDelta bytes) subst = do
  bts <- buildBytes bytes subst
  Right [BiDelta bts]
buildBindingUnchecked (BiLambda (FnMeta meta)) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvFunction func) -> Right [BiLambda func]
  _ -> Left (metaMsg meta)
buildBindingUnchecked (BiLambda (FnAny slot)) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvFunction func) -> Right [BiLambda func]
  _ -> Left (slotMsg slot)
buildBindingUnchecked (BiLambda (FnFresh slot)) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvFunction func) -> Right [BiLambda func]
  _ -> Left (slotMsg slot)
buildBindingUnchecked binding _ = Right [binding]

buildArgument :: Argument -> Subst -> Built Argument
buildArgument (ArTau attr expr) subst = do
  attribute <- buildAttribute attr subst
  expression <- buildExpression expr subst
  Right (ArTau attribute expression)
buildArgument (ArAlpha alpha expr) subst = do
  alpha' <- buildAlpha alpha subst
  expression <- buildExpression expr subst
  Right (ArAlpha alpha' expression)

buildBindings :: [Binding] -> Subst -> Built [Binding]
buildBindings [] _ = Right []
buildBindings (bd : rest) subst = do
  first <- buildBindingUnchecked bd subst
  bds <- buildBindings rest subst
  Right (first ++ bds)

pathOf :: Expression -> Expression -> Expression
pathOf universe@(ExFormation world) form@(ExFormation bds)
  | form == universe = ExRoot
  | otherwise = maybe form found (parent (find rho bds))
  where
    found :: (Expression, [Binding]) -> Expression
    found (path, siblings) = fromMaybe form (listToMaybe (mapMaybe (candidate path) siblings))
    parent :: Maybe Binding -> Maybe (Expression, [Binding])
    parent Nothing = Just (ExRoot, world)
    parent (Just (BiTau AtRho path)) = (,) path <$> declared path
    parent _ = Nothing
    declared :: Expression -> Maybe [Binding]
    declared ExRoot = Just world
    declared (ExApplication target _) = declared target
    declared (ExDispatch target attr) = do
      outer <- declared target
      BiTau _ (ExFormation inner) <- find (tau attr) outer
      Just inner
    declared _ = Nothing
    candidate :: Expression -> Binding -> Maybe Expression
    candidate path (BiTau attr (ExFormation origin))
      | attr /= AtRho && length origin == length bds = do
          args <- zipWithM (argument path) origin bds
          Just (foldl ExApplication (ExDispatch path attr) (concat args))
    candidate _ _ = Nothing
    argument :: Expression -> Binding -> Binding -> Maybe [Argument]
    argument path (BiVoid AtRho) (BiTau AtRho value)
      | value == path = Just []
    argument _ (BiVoid attr) (BiTau attr' value)
      | attr == attr' && attr /= AtRho && closed value = Just [ArTau attr value]
    argument _ origin binding
      | origin == binding = Just []
      | otherwise = Nothing
    rho :: Binding -> Bool
    rho (BiTau AtRho _) = True
    rho (BiVoid AtRho) = True
    rho _ = False
    tau :: Attribute -> Binding -> Bool
    tau attr (BiTau attr' _) = attr == attr'
    tau _ _ = False
    closed :: Expression -> Bool
    closed (ExFormation _) = True
    closed ExRoot = True
    closed ExTermination = True
    closed (ExApplication target (ArTau _ value)) = closed target && closed value
    closed (ExApplication target (ArAlpha _ value)) = closed target && closed value
    closed (ExDispatch target _) = closed target
    closed _ = False
pathOf _ form = form

nameIn :: Maybe Expression -> Expression -> Expression
nameIn universe form = maybe form (`pathOf` form) universe

formed :: [Binding] -> Expression
formed bds = either (throw . CouldNotBuildExpression (ExFormation bds)) id (unique (ExFormation bds))

unique :: Expression -> Built Expression
unique expr@(ExFormation bds)
  | distinct expr = Right expr
  | otherwise = uniqueBindings bds >> Right expr
unique expr = Right expr

buildExpression :: Expression -> Subst -> Built Expression
buildExpression (ExDispatch ex at) subst = do
  dispatched <- buildExpression ex subst
  at' <- buildAttribute at subst
  Right (ExDispatch dispatched at')
buildExpression (ExApplication ExRoot (ArTau AtRho expr)) subst = do
  _ <- buildExpression expr subst
  Right ExRoot
buildExpression (ExApplication expr arg) subst = do
  applied <- buildExpression expr subst
  arg' <- buildArgument arg subst
  Right (ExApplication applied arg')
buildExpression (ExFormation bds) subst = buildBindings bds subst >>= unique . ExFormation
buildExpression (ExMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvExpression expr) -> unique expr
  _ -> Left (metaMsg meta)
buildExpression (ExAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvExpression expr) -> unique expr
  _ -> Left (slotMsg slot)
buildExpression expr _ = Right expr

buildBytesThrows :: Bytes -> Subst -> IO Bytes
buildBytesThrows bytes subst = orThrow (CouldNotBuildBytes bytes) (buildBytes bytes subst)

buildBindingThrows :: Binding -> Subst -> IO [Binding]
buildBindingThrows bd subst = orThrow (CouldNotBuildBinding bd) (buildBinding bd subst)

buildAttributeThrows :: Attribute -> Subst -> IO Attribute
buildAttributeThrows attr subst = orThrow (CouldNotBuildAttribute attr) (buildAttribute attr subst)

buildExpressionThrows :: Expression -> Subst -> IO Expression
buildExpressionThrows expr subst = orThrow (CouldNotBuildExpression expr) (buildExpression expr subst)

buildExpressionsThrows :: Expression -> [Subst] -> IO [Expression]
buildExpressionsThrows expr = traverse (buildExpressionThrows expr)
