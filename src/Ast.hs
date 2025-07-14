{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents Ast tree for parsed phi-calculus program
module Ast where

import GHC.Generics (Generic)

newtype Program = Program Expression -- Q -> expr
  deriving (Eq, Show)

data Expression
  = ExFormation [Binding] -- [bindings]
  | ExThis
  | ExGlobal -- Q
  | ExTermination -- T
  | ExMeta String -- !e
  | ExApplication Expression Binding -- expr(attr -> expr)
  | ExDispatch Expression Attribute -- expr.attr
  | ExMetaTail Expression String -- expr * !t
  deriving (Eq, Show, Generic)

data Binding
  = BiTau Attribute Expression -- attr -> expr
  | BiMeta String -- !B
  | BiDelta Bytes -- Δ ⤍ 1F-2A
  | BiMetaDelta String -- Δ ⤍ !b
  | BiVoid Attribute -- attr ↦ ?
  | BiLambda String -- λ ⤍ Function
  | BiMetaLambda String -- λ ⤍ !F
  deriving (Eq, Show, Generic)

data Bytes
  = BtEmpty
  | BtOne String
  | BtMany [String]
  deriving (Eq, Show, Generic)

data Attribute
  = AtLabel String -- attr
  | AtAlpha Integer -- α1
  | AtPhi -- φ
  | AtRho -- ρ
  | AtLambda -- λ, used only in yaml conditions
  | AtDelta -- Δ, used only in yaml conditions
  | AtMeta String -- !a
  deriving (Eq, Show, Generic)

countNodes :: Program -> Integer
countNodes (Program expr) = countNodes' expr
  where
    countNodes' :: Expression -> Integer
    countNodes' ExGlobal = 1
    countNodes' ExTermination = 1
    countNodes' ExThis = 1
    countNodes' (ExApplication expr' (BiTau attr bexpr')) = 2 + countNodes' expr' + countNodes' bexpr'
    countNodes' (ExDispatch expr' attr) = 2 + countNodes' expr'
    countNodes' (ExFormation bds) = 1 + sum (map (\case BiTau attr expr' -> countNodes' expr'; _ -> 1) bds)
    countNodes' _ = 0
