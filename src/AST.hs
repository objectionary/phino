{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents AST tree for parsed phi-calculus expression
module AST where

import Data.Bits (xor)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Expression
  = ExFormation [Binding]
  | ExXi
  | ExRoot
  | ExTermination
  | ExApplication Expression Argument
  | ExDispatch Expression Attribute
  | ExMeta Text
  | ExPhiMeet (Maybe String) Int Expression
  | ExPhiAgain (Maybe String) Int Expression
  | {- | Bare data 𝛿 — the raw bytes extracted by the 'delta' dataization rule.
    It is not a phi-calculus term but a rendering-only chain node, so a
    '--sequence' derivation can terminate at the data itself rather than at
    the data object it was pulled out of (see #980). It never flows into the
    matcher, builder or the dataization relation.
    -}
    ExBytes Bytes
  deriving (Eq, Ord, Show, Generic)

data Argument
  = ArTau Attribute Expression
  | ArAlpha Alpha Expression
  deriving (Eq, Ord, Show, Generic)

data Alpha
  = Alpha Int
  | AlMeta Text
  deriving (Eq, Ord, Generic)

data Binding
  = BiTau Attribute Expression
  | BiVoid Attribute
  | BiDelta Bytes
  | BiLambda Function
  | BiMeta Text
  deriving (Eq, Ord, Show, Generic)

data Bytes
  = BtEmpty
  | BtOne String
  | BtMany [String]
  | BtMeta Text
  deriving (Eq, Ord, Show, Generic)

data Attribute
  = AtLabel Text
  | AtPhi
  | AtRho
  | AtLambda
  | AtDelta
  | AtMeta Text
  deriving (Eq, Generic, Ord)

data Function
  = Function Text
  | FnMeta Text
  deriving (Eq, Generic, Show, Ord)

instance Show Attribute where
  show (AtLabel label) = T.unpack label
  show AtRho = "ρ"
  show AtPhi = "φ"
  show AtDelta = "Δ"
  show AtLambda = "λ"
  show (AtMeta meta) = '!' : T.unpack meta

instance Show Alpha where
  show (Alpha idx) = 'α' : show idx
  show (AlMeta meta) = 'α' : '!' : T.unpack meta

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
      ExXi -> step h 2
      ExRoot -> step h 3
      ExTermination -> step h 4
      ExApplication ex arg -> goArgument (goExpr (step h 5) ex) arg
      ExDispatch ex at -> goAttribute (goExpr (step h 6) ex) at
      ExMeta t -> hashText (step h 7) t
      ExPhiMeet ms i ex -> goExpr (hashMaybeString (step (step h 9) i) ms) ex
      ExPhiAgain ms i ex -> goExpr (hashMaybeString (step (step h 10) i) ms) ex
      ExBytes bts -> goBytes (step h 8) bts
    goBinding :: Int -> Binding -> Int
    goBinding h = \case
      BiTau at ex -> goExpr (goAttribute (step h 11) at) ex
      BiDelta bts -> goBytes (step h 12) bts
      BiVoid at -> goAttribute (step h 13) at
      BiLambda fn -> goFunction (step h 14) fn
      BiMeta t -> hashText (step h 15) t
    goBytes :: Int -> Bytes -> Int
    goBytes h = \case
      BtEmpty -> step h 17
      BtOne s -> hashString (step h 18) s
      BtMany ss -> foldl' hashString (step h 19) ss
      BtMeta t -> hashText (step h 20) t
    goAttribute :: Int -> Attribute -> Int
    goAttribute h = \case
      AtLabel t -> hashText (step h 21) t
      AtPhi -> step h 23
      AtRho -> step h 24
      AtLambda -> step h 25
      AtDelta -> step h 26
      AtMeta t -> hashText (step h 27) t
    goArgument :: Int -> Argument -> Int
    goArgument h = \case
      ArTau at ex -> goExpr (goAttribute (step h 22) at) ex
      ArAlpha al ex -> goExpr (goAlpha (step h 30) al) ex
    goAlpha :: Int -> Alpha -> Int
    goAlpha h = \case
      Alpha idx -> step (step h 31) idx
      AlMeta t -> hashText (step h 28) t
    goFunction :: Int -> Function -> Int
    goFunction h = \case
      Function t -> hashText (step h 16) t
      FnMeta t -> hashText (step h 29) t

countNodes :: Expression -> Int
countNodes (ExFormation bds) = 1 + sum (map nodesInBinding bds) + length bds
  where
    nodesInBinding :: Binding -> Int
    nodesInBinding (BiTau _ expr) = countNodes expr + 2
    nodesInBinding (BiMeta _) = 1
    nodesInBinding _ = 3
countNodes (ExApplication expr (ArTau _ expr')) = 4 + countNodes expr + countNodes expr'
countNodes (ExApplication expr (ArAlpha _ expr')) = 4 + countNodes expr + countNodes expr'
countNodes (ExDispatch expr' _) = 2 + countNodes expr'
countNodes (ExPhiMeet _ _ expr) = countNodes expr
countNodes (ExPhiAgain _ _ expr) = countNodes expr
countNodes _ = 1

matchBaseObject :: Expression -> Maybe T.Text
matchBaseObject (ExDispatch ExRoot (AtLabel label)) = Just label
matchBaseObject _ = Nothing

pattern BaseObject :: T.Text -> Expression
pattern BaseObject label <- (matchBaseObject -> Just label)
  where
    BaseObject label = ExDispatch ExRoot (AtLabel label)

-- Minimal matcher function (required for view pattern)
--
-- The primitive→bytes binding is named 'as-bytes' and the bytes→payload
-- binding is named 'data' (jeo-maven-plugin 0.15.3+, following phi-calculus
-- dropping positional attributes). The legacy positional α0 form is still
-- recognized so XMIR produced by older jeo versions keeps sugaring back.
matchDataObject :: Expression -> Maybe (T.Text, Bytes)
matchDataObject (ExApplication outer arg)
  | Just inner <- asBytesArg arg = case (matchOuter outer, matchInner inner) of
      (Just label, Just bts) -> Just (label, bts)
      _ -> Nothing
  where
    asBytesArg :: Argument -> Maybe Expression
    asBytesArg (ArTau (AtLabel "as-bytes") inner) = Just inner
    asBytesArg (ArAlpha (Alpha 0) inner) = Just inner
    asBytesArg _ = Nothing
    dataArg :: Argument -> Maybe Expression
    dataArg (ArTau (AtLabel "data") formation) = Just formation
    dataArg (ArAlpha (Alpha 0) formation) = Just formation
    dataArg _ = Nothing
    matchOuter :: Expression -> Maybe T.Text
    matchOuter (BaseObject label) = Just label
    matchOuter (ExPhiAgain _ _ (BaseObject label)) = Just label
    matchOuter _ = Nothing
    matchInner :: Expression -> Maybe Bytes
    matchInner (ExPhiAgain _ _ inner') = matchInner inner'
    matchInner inner' = matchInner' inner'
    matchInner' :: Expression -> Maybe Bytes
    matchInner' (ExApplication bytes arg')
      | Just formation <- dataArg arg' = case (matchesBytes bytes, matchFormation formation) of
          (True, Just bts) -> Just bts
          _ -> Nothing
    matchInner' _ = Nothing
    matchesBytes :: Expression -> Bool
    matchesBytes (BaseObject "bytes") = True
    matchesBytes (ExPhiAgain _ _ (BaseObject "bytes")) = True
    matchesBytes _ = False
    matchFormation :: Expression -> Maybe Bytes
    matchFormation (ExFormation [BiDelta bts, BiVoid AtRho]) = Just bts
    matchFormation (ExPhiAgain _ _ (ExFormation [BiDelta bts, BiVoid AtRho])) = Just bts
    matchFormation _ = Nothing
matchDataObject _ = Nothing

pattern DataString :: Bytes -> Expression
pattern DataString bts = DataObject "string" bts

pattern DataNumber :: Bytes -> Expression
pattern DataNumber bts = DataObject "number" bts

pattern DataObject :: T.Text -> Bytes -> Expression
pattern DataObject label bts <- (matchDataObject -> Just (label, bts))
  where
    DataObject label bts =
      ExApplication
        (BaseObject label)
        ( ArTau
            (AtLabel "as-bytes")
            ( ExApplication
                (BaseObject "bytes")
                ( ArTau
                    (AtLabel "data")
                    (ExFormation [BiDelta bts, BiVoid AtRho])
                )
            )
        )
