{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LambdasSpec (spec) where

import AST
import Control.Exception (SomeException)
import Control.Monad (forM_, (>=>))
import Data.List (isInfixOf)
import Data.Text qualified as T
import Fixtures (withFunctionsOf)
import Lambdas (Lambda (..), Lambdas, Meta (..), emptyLambdas, matched, minted, readLambdas)
import Test.Hspec

-- Read the given λ functions the way '--functions' reads them, out of a file
-- written for the occasion.
registered :: T.Text -> (Lambdas -> IO a) -> IO a
registered functions action = withFunctionsOf functions (readLambdas >=> action)

-- The same, for a file nothing can read back
unreadable :: T.Text -> String -> Expectation
unreadable functions message =
  withFunctionsOf functions $ \file ->
    readLambdas file `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))

spec :: Spec
spec = do
  -- The key of an entry is a regular expression over λ names and has to match
  -- the whole one, so a plain name means that λ function and not every name it
  -- is a part of.
  describe "matched" $ do
    it "finds the entry registered under the very name" $
      registered "- λ: L_plus\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" $ \known ->
        map (._key) (matched known "L_plus") `shouldBe` ["L_plus"]
    it "finds every entry of a family the key stands for" $
      registered "- λ: L_box_[0-9]+_number\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" $ \known ->
        length (matched known "L_box_42_number") `shouldBe` 1
    it "dont find a name the key matches only a part of" $
      registered "- λ: L_plus\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" $ \known ->
        map (._key) (matched known "L_plus_one") `shouldBe` []
    it "keeps both entries a name is answered by, in the order of the file" $
      registered "- λ: L_eq\n  𝑛: ⟦ Δ ⤍ FF- ⟧\n- λ: L_eq\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" $ \known ->
        map (._answer) (matched known "L_eq")
          `shouldBe` [ExFormation [BiDelta (BtOne "FF"), BiVoid AtRho], ExFormation [BiDelta (BtOne "00"), BiVoid AtRho]]
    it "finds nothing at all without the option" $
      map (._key) (matched emptyLambdas "L_plus") `shouldBe` []

  -- A file that cannot be read fails the run as it loads, before anything is
  -- parsed or dataized, so a mistake in it never surfaces half-way through a
  -- derivation.
  describe "readLambdas" $ do
    it "reads the metas an entry dataizes under the names 𝜑-calculus gives them" $
      registered "- λ: L_plus\n  dataize:\n    δ2: $.x\n    δ1: $.ρ\n  𝑛: ⟦ Δ ⤍ δ1 ⟧\n" $ \known ->
        concatMap (map (\(meta, _) -> (meta._spelling, meta._name)) . (._dataized)) (matched known "L_plus")
          `shouldBe` [("δ1", "d1"), ("δ2", "d2")]
    it "reads the metas an entry morphs under the names 𝜑-calculus gives them" $
      registered "- λ: L_plus\n  morph:\n    𝑛2: $.x\n    𝑛1: $.ρ\n  𝑛: 𝑛1\n" $ \known ->
        concatMap (map (\(meta, _) -> (meta._spelling, meta._name)) . (._morphed)) (matched known "L_plus")
          `shouldBe` [("𝑛1", "n1"), ("𝑛2", "n2")]
    it "reads an operand as the term of the calculus it is written as" $
      registered "- λ: L_plus\n  morph:\n    𝑛1: $.ρ.length\n  𝑛: 𝑛1\n" $ \known ->
        concatMap (map snd . (._morphed)) (matched known "L_plus")
          `shouldBe` [ExDispatch (ExDispatch ExXi AtRho) (AtLabel "length")]
    it "refuses an operand that is no term at all" $
      unreadable "- λ: L_plus\n  dataize:\n    δ1: '..'\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" "cannot be read"
    it "refuses an operand holding an anonymous meta" $
      unreadable "- λ: L_plus\n  morph:\n    𝑛1: '!e'\n  𝑛: 𝑛1\n" "cannot be referenced"
    it "refuses a key that is no regular expression" $
      unreadable "- λ: 'L_[('\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" "is not a regular expression"
    it "refuses a dataized operand named by something other than a bytes meta" $
      unreadable "- λ: L_plus\n  dataize:\n    𝑛1: $.ρ\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" "is not a bytes meta"
    it "refuses a morphed operand named by something other than an expression meta" $
      unreadable "- λ: L_plus\n  morph:\n    δ1: $.ρ\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" "is not an expression meta"
    it "refuses a symbol named by something other than a function meta" $
      unreadable "- λ: L_plus\n  symbols: [𝑛1]\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" "is not a function meta"
    it "refuses an entry with no answer under 𝑛" $
      unreadable "- λ: L_plus\n  dataize:\n    δ1: $.ρ\n" "cannot be read"
    it "refuses a file that is no list of entries at all" $
      unreadable "λ: L_plus\n" "cannot be read"

  -- Uniqueness is the state's job: every firing takes the names the run has
  -- not spent yet, so no two unknowns of one run are ever spelled alike.
  describe "minted" $ do
    let metas = [Meta "𝑓0" "F0", Meta "𝑓1" "F1"]
    it "names the symbols of the first firing from the first one" $
      map snd (fst (minted metas "")) `shouldBe` ["S_1", "S_2"]
    it "goes on where the state left off" $
      map snd (fst (minted metas "2")) `shouldBe` ["S_3", "S_4"]
    it "counts what it has spent into the new state" $
      snd (minted metas "2") `shouldBe` "4"
    it "mints no name for an entry with no symbols at all" $
      map snd (fst (minted [] "7")) `shouldBe` []
    it "spends nothing for an entry that mints nothing" $
      snd (minted [] "7") `shouldBe` "7"
    it "keeps the meta each name is bound to" $
      map (\(meta, _) -> meta._spelling) (fst (minted metas "")) `shouldBe` ["𝑓0", "𝑓1"]
    forM_ [("", "1"), ("0", "1"), ("41", "42")] $ \(spent, counted) ->
      it ("counts one symbol minted on the state '" ++ spent ++ "' as '" ++ counted ++ "'") $
        snd (minted [Meta "𝑓0" "F0"] spent) `shouldBe` counted
