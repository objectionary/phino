{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents AST tree for parsed phi-calculus program
module AST where

import Data.Bits (xor)
import Data.List (foldl')
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

-- A cheap, fixed-size digest of an expression, used for fast (dirty) equality
-- checks during loop detection. Equal expressions always produce the same
-- digest, but distinct expressions may collide, so a positive digest match
-- must always be confirmed with a full structural (==) comparison.
hashExpression :: Expression -> Int
hashExpression = goExpr fnvOffset
  where
    fnvPrime, fnvOffset :: Int
    fnvPrime = 1099511628211
    fnvOffset = 14695981039

    -- FNV-1a style mixing step (Int multiplication wraps silently).
    step :: Int -> Int -> Int
    step h x = (h `xor` x) * fnvPrime

    hashText :: Int -> Text -> Int
    hashText = T.foldl' (\h c -> step h (fromEnum c))

    hashString :: Int -> String -> Int
    hashString = foldl' (\h c -> step h (fromEnum c))

    hashMaybeString :: Int -> Maybe String -> Int
    hashMaybeString h Nothing = step h 0
    hashMaybeString h (Just s) = hashString (step h 1) s

    goExpr :: Int -> Expression -> Int
    goExpr h = \case
      ExFormation bds -> foldl' goBinding (step h 1) bds
      ExThis -> step h 2
      ExGlobal -> step h 3
      ExTermination -> step h 4
      ExApplication ex bd -> goBinding (goExpr (step h 5) ex) bd
      ExDispatch ex at -> goAttribute (goExpr (step h 6) ex) at
      ExMeta t -> hashText (step h 7) t
      ExMetaTail ex t -> hashText (goExpr (step h 8) ex) t
      ExPhiMeet ms i ex -> goExpr (hashMaybeString (step (step h 9) i) ms) ex
      ExPhiAgain ms i ex -> goExpr (hashMaybeString (step (step h 10) i) ms) ex

    goBinding :: Int -> Binding -> Int
    goBinding h = \case
      BiTau at ex -> goExpr (goAttribute (step h 11) at) ex
      BiDelta bts -> goBytes (step h 12) bts
      BiVoid at -> goAttribute (step h 13) at
      BiLambda t -> hashText (step h 14) t
      BiMeta t -> hashText (step h 15) t
      BiMetaLambda t -> hashText (step h 16) t

    goBytes :: Int -> Bytes -> Int
    goBytes h = \case
      BtEmpty -> step h 17
      BtOne s -> hashString (step h 18) s
      BtMany ss -> foldl' hashString (step h 19) ss
      BtMeta t -> hashText (step h 20) t

    goAttribute :: Int -> Attribute -> Int
    goAttribute h = \case
      AtLabel t -> hashText (step h 21) t
      AtAlpha i -> step (step h 22) i
      AtPhi -> step h 23
      AtRho -> step h 24
      AtLambda -> step h 25
      AtDelta -> step h 26
      AtMeta t -> hashText (step h 27) t

-- A primitive is the termination ⊥ or a formation without a λ binding.
primitive :: Expression -> Bool
primitive ExTermination = True
primitive (ExFormation bds) = not (any lambda bds)
  where
    lambda :: Binding -> Bool
    lambda (BiLambda _) = True
    lambda (BiMetaLambda _) = True
    lambda _ = False
primitive _ = False

-- An alpha attribute is a positional index (α0, α1, …); a 𝜏-attribute never is.
alpha :: Attribute -> Bool
alpha (AtAlpha _) = True
alpha _ = False

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
