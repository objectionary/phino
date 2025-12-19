{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents AST tree for parsed phi-calculus program
module AST where

import GHC.Generics (Generic)

newtype Program = Program Expression
  deriving (Eq, Ord, Show)

data Expression
  = ExFormation [Binding]
  | ExThis
  | ExGlobal
  | ExTermination
  | ExApplication Expression Binding
  | ExDispatch Expression Attribute
  | ExMeta String
  | ExMetaTail Expression String
  | ExPhiMeet (Maybe String) Int Expression
  | ExPhiAgain (Maybe String) Int Expression
  deriving (Eq, Ord, Show, Generic)

data Binding
  = BiTau Attribute Expression
  | BiDelta Bytes
  | BiVoid Attribute
  | BiLambda String
  | BiMeta String
  | BiMetaLambda String
  deriving (Eq, Ord, Show, Generic)

data Bytes
  = BtEmpty
  | BtOne String
  | BtMany [String]
  | BtMeta String
  deriving (Eq, Ord, Show, Generic)

data Attribute
  = AtLabel String
  | AtAlpha Int
  | AtPhi
  | AtRho
  | AtLambda
  | AtDelta
  | AtMeta String
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
