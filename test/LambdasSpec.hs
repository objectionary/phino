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
import Lambdas (Lambda (..), Lambdas, Meta (..), attributeOf, emptyLambdas, matched, minted, readLambdas)
import Parser (parseExpressionThrows)
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
    it "reads the metas an entry binds under the names 𝜑-calculus gives them" $
      registered "- λ: L_plus\n  dataize:\n    𝑛2: x\n    𝑛1: ρ\n  𝑛: 𝑛1\n" $ \known ->
        map (\(meta, path) -> (meta._spelling, meta._name, path)) (head (matched known "L_plus"))._dataized
          `shouldBe` [("𝑛1", "n1", "ρ"), ("𝑛2", "n2", "x")]
    it "refuses a key that is no regular expression" $
      unreadable "- λ: 'L_[('\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" "is not a regular expression"
    it "refuses an operand named by something other than an expression meta" $
      unreadable "- λ: L_plus\n  dataize:\n    δ1: ρ\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" "is not an expression meta"
    it "refuses a symbol named by something other than a function meta" $
      unreadable "- λ: L_plus\n  symbols: [𝑛1]\n  𝑛: ⟦ Δ ⤍ 00- ⟧\n" "is not a function meta"
    it "refuses an entry with no answer under 𝑛" $
      unreadable "- λ: L_plus\n  dataize:\n    𝑛1: ρ\n" "cannot be read"
    it "refuses a file that is no list of entries at all" $
      unreadable "λ: L_plus\n" "cannot be read"

  -- A dotted path names a node of the formation being fired, in the scope it
  -- is bound in.
  describe "attributeOf" $ do
    let held path src = do
          term <- parseExpressionThrows src
          pure (attributeOf path term)
    it "takes the node one segment names" $ do
      found <- held "x" "[[ x -> [[ D> 2A- ]] ]]"
      found `shouldBe` Just (ExFormation [BiDelta (BtOne "2A"), BiVoid AtRho])
    it "goes as deep as the dots take it" $ do
      found <- held "x.y" "[[ x -> [[ y -> [[ D> 2A- ]] ]] ]]"
      found `shouldBe` Just (ExFormation [BiDelta (BtOne "2A"), BiVoid AtRho])
    it "takes the argument an application binds" $ do
      found <- held "x" "[[ x -> ? ]]( x -> [[ D> 2A- ]] )"
      found `shouldBe` Just (ExFormation [BiDelta (BtOne "2A"), BiVoid AtRho])
    it "answers nothing for an attribute that is void" $ do
      found <- held "x" "[[ x -> ? ]]"
      found `shouldBe` Nothing
    it "answers nothing for an attribute nothing carries" $ do
      found <- held "z" "[[ x -> [[ D> 2A- ]] ]]"
      found `shouldBe` Nothing
    it "answers nothing where a segment leads nowhere to go on into" $ do
      found <- held "x.y" "[[ x -> [[ D> 2A- ]] ]]"
      found `shouldBe` Nothing

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
