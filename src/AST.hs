{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents AST tree for parsed phi-calculus program
module AST where

import GHC.Generics (Generic)

newtype Program = Program Expression -- Q -> expr
  deriving (Eq, Ord, Show)


data Expression
  = ExFormation [Binding] -- [[ bindings ]]
  | ExThis -- $
  | ExGlobal -- Q
  | ExTermination -- T
  | ExApplication Expression Binding -- expr(attr -> expr)
  | ExDispatch Expression Attribute -- expr.attr
  | ExMeta String -- !e
  | ExMetaTail Expression String -- expr * !t
  | ExPhiMeet (Maybe String) Int Expression
  | ExPhiAgain (Maybe String) Int Expression
  deriving (Eq, Ord, Show, Generic)

data Binding
  = BiTau Attribute Expression -- attr -> expr
  | BiDelta Bytes -- Δ ⤍ 1F-2A
  | BiVoid Attribute -- attr ↦ ?
  | BiLambda String -- λ ⤍ Function
  | BiMeta String -- !B
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
  | AtAlpha Int -- α1
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

countNodes :: Expression -> Int
countNodes ExGlobal = 1
countNodes ExTermination = 1
countNodes ExThis = 1
countNodes (ExApplication expr' (BiTau _ bexpr')) = 2 + countNodes expr' + countNodes bexpr'
countNodes (ExDispatch expr' _) = 2 + countNodes expr'
countNodes (ExFormation bds) = 1 + sum (map (\case BiTau _ expr' -> countNodes expr'; _ -> 1) bds)
countNodes _ = 0
