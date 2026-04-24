{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents AST tree for parsed phi-calculus program
module AST where

import Data.Text (Text)
import qualified Data.Text as T
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
  | ExMeta Text
  | ExMetaTail Expression Text
  | ExPhiMeet (Maybe String) Int Expression
  | ExPhiAgain (Maybe String) Int Expression
  deriving (Eq, Ord, Show, Generic)

data Binding
  = BiTau Attribute Expression
  | BiDelta Bytes
  | BiVoid Attribute
  | BiLambda Text
  | BiMeta Text
  | BiMetaLambda Text
  deriving (Eq, Ord, Show, Generic)

data Bytes
  = BtEmpty
  | BtOne String
  | BtMany [String]
  | BtMeta Text
  deriving (Eq, Ord, Show, Generic)

data Attribute
  = AtLabel Text
  | AtAlpha Int
  | AtPhi
  | AtRho
  | AtLambda
  | AtDelta
  | AtMeta Text
  deriving (Eq, Generic, Ord)

instance Show Attribute where
  show (AtLabel label) = T.unpack label
  show (AtAlpha idx) = 'α' : show idx
  show AtRho = "ρ"
  show AtPhi = "φ"
  show AtDelta = "Δ"
  show AtLambda = "λ"
  show (AtMeta meta) = '!' : T.unpack meta

countNodes :: Expression -> Int
countNodes (ExFormation bds) = 1 + sum (map nodesInBinding bds) + length bds
  where
    nodesInBinding :: Binding -> Int
    nodesInBinding (BiTau _ expr) = countNodes expr + 2
    nodesInBinding (BiMeta _) = 1
    nodesInBinding _ = 3
countNodes (ExApplication expr (BiTau _ expr')) = 4 + countNodes expr + countNodes expr'
countNodes (ExDispatch expr' _) = 2 + countNodes expr'
countNodes (ExMetaTail expr _) = 2 + countNodes expr
countNodes (ExPhiMeet _ _ expr) = countNodes expr
countNodes (ExPhiAgain _ _ expr) = countNodes expr
countNodes _ = 1
