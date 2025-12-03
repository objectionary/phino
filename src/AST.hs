{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents AST tree for parsed phi-calculus program
module AST where

import Data.List (intercalate)
import GHC.Generics (Generic)

newtype Program = Program Expression -- Q -> expr
  deriving (Eq, Ord, Show)

data Expression
  = ExFormation [Binding] -- [[ bindings ]]
  | ExThis
  | ExGlobal -- Q
  | ExTermination -- T
  | ExMeta String -- !e
  | ExApplication Expression Binding -- expr(attr -> expr)
  | ExDispatch Expression Attribute -- expr.attr
  | ExMetaTail Expression String -- expr * !t
  deriving (Eq, Ord, Show, Generic)

data Binding
  = BiTau Attribute Expression -- attr -> expr
  | BiMeta String -- !B
  | BiDelta Bytes -- Δ ⤍ 1F-2A
  | BiVoid Attribute -- attr ↦ ?
  | BiLambda String -- λ ⤍ Function
  | BiMetaLambda String -- λ ⤍ !F
  deriving (Eq, Ord, Show, Generic)

data Bytes
  = BtEmpty -- --
  | BtOne String -- 1F-
  | BtMany [String] -- 00-01-02-...04
  | BtMeta String -- !b
  deriving (Eq, Ord, Show, Generic)

data Attribute
  = AtLabel String -- attr
  | AtAlpha Integer -- α1
  | AtPhi -- φ
  | AtRho -- ρ
  | AtLambda -- λ, used only in yaml conditions
  | AtDelta -- Δ, used only in yaml conditions
  | AtMeta String -- !a
  deriving (Eq, Generic, Ord)

instance Show Attribute where
  show (AtLabel label) = label
  show (AtAlpha idx) = 'α' : show idx
  show AtRho = "ρ"
  show AtPhi = "φ"
  show AtDelta = "Δ"
  show AtLambda = "λ"
  show (AtMeta meta) = '!' : meta

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
