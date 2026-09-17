{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LambdasSpec (spec) where

import AST
import Control.Exception (SomeException)
import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.Text qualified as T
import Fixtures (withLambdasOf)
import Lambdas (Lambda (..), Lambdas, Meta (..), emptyLambdas, matched, minted, readLambdas, taken)
import Parser (parseExpressionThrows)
import Test.Hspec

-- The λ functions the given text spells, read out of a file of its own, which
-- is how '--symbolic' reads them and the only way they are ever read
lambdasOf :: T.Text -> IO Lambdas
lambdasOf text = withLambdasOf text readLambdas

-- One entry with the given key, one 'dataize' operand and an answer standing
-- for the unknown it came down to, which is the shape most cases start from
entry :: T.Text -> T.Text
entry key = "- λ: " <> key <> "\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"

-- The key the entry answering the given λ name is registered under, or nothing
-- where no entry answers it. Lookups go through this rather than through the
-- entry itself, since an entry is no value and nothing compares two of them.
answering :: Lambdas -> T.Text -> Maybe T.Text
answering known func = _key <$> matched known func

spec :: Spec
spec = do
  describe "readLambdas" $ do
    it "reads the λ function its key names" $ do
      known <- lambdasOf (entry "L_number_plus")
      answering known "L_number_plus" `shouldBe` Just "L_number_plus"

    -- A key is a regular expression over λ names, so one entry stands for the
    -- whole family of them a box numbers its functions with
    it "reads one λ function for the whole family its key spells" $ do
      known <- lambdasOf (entry "L_box_[0-9]+_number")
      answering known "L_box_42_number" `shouldBe` Just "L_box_[0-9]+_number"

    -- The expression matches the whole name and not a part of it, so a plain
    -- name keeps meaning that one λ function
    it "cannot read a λ function whose name merely starts with a key" $ do
      known <- lambdasOf (entry "L_number_plus")
      answering known "L_number_plus_twice" `shouldBe` Nothing

    it "cannot read a λ function no entry answers" $ do
      known <- lambdasOf (entry "L_number_plus")
      answering known "L_bytes_not" `shouldBe` Nothing

    -- A YAML mapping keeps no order of its own, so the metas are what orders
    -- the operands: 𝛿1 comes down before 𝛿2 however the file lists them
    it "reads the operands of 'dataize' in the order their metas number them" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿2: $.x\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_spelling . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` ["𝛿1", "𝛿2"]

    -- The protocol spells a meta the way the file does, while a substitution
    -- keeps it under the name 𝜑-calculus gives it, and the two differ
    it "reads a meta under both the name it is spelled with and the name it binds" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_name . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` ["d1"]

    it "reads the operands of 'morph' under expression metas" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  𝑛: 𝑛1\n"
      map (_spelling . fst) (maybe [] _morphed (matched known "L_fork")) `shouldBe` ["𝑛1", "𝑛2"]

    it "reads the term an operand is reduced from" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      operand <- parseExpressionThrows "$.x"
      map snd (maybe [] _dataized (matched known "L_pair")) `shouldBe` [operand]

    it "reads the term the firing answers with" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )\n"
      answer <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )"
      fmap _answer (matched known "L_pair") `shouldBe` Just answer

    it "reads an entry naming neither operand block" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_spelling . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` []

    -- Everything a file may be wrong about fails where it is read, before any
    -- reduction starts, so a run never gets half-way through a derivation to
    -- discover that one of its λ functions cannot be read at all
    forM_
      [ ("a file which is no list of entries" :: String, "λ: L_pair\n" :: T.Text, "cannot be read" :: String)
      , ("an entry with no answer at all", "- λ: L_pair\n", "cannot be read")
      , ("an entry whose answer is no term of the calculus", "- λ: L_pair\n  𝑛: ⟦ λ ⤍\n", "cannot be read")
      , ("two entries under one key", entry "L_pair" <> entry "L_pair", "is used by more than one entry")
      , ("a key which is no regular expression", entry "L_[pair", "is not a regular expression")
      , ("an operand of 'dataize' which is no bytes meta", "- λ: L_pair\n  dataize:\n    𝑛1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "is not a bytes meta")
      , ("an operand of 'morph' which is no expression meta", "- λ: L_pair\n  morph:\n    𝛿1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "is not an expression meta")
      , ("an operand referencing a meta the entry never matched", "- λ: L_pair\n  dataize:\n    𝛿1: '!n'\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "cannot be referenced")
      , ("an answer reading the data its operands came down to", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ Δ ⤍ 𝛿1 ⟧\n", "reads data")
      , ("an answer carrying an anonymous meta of another kind", "- λ: L_pair\n  𝑛: '⟦ φ ↦ !n ⟧'\n", "cannot be referenced")
      ]
      ( \(desc, text, message) ->
          it ("cannot read " ++ desc) $
            lambdasOf text `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))
      )

  describe "emptyLambdas" $
    -- A run without '--symbolic' fires against no λ function at all, which is
    -- what every name getting stuck means and what '--partial' parks on
    it "cannot read a λ function without the file naming one" $
      answering emptyLambdas "L_number_plus" `shouldBe` Nothing

  -- Which symbol a fresh 𝜎 becomes is the state's business and not the file's:
  -- the count of symbols the run has minted so far goes in and the names taken
  -- come back out, so no two unknowns of one run are ever spelled alike.
  describe "minted" $ do
    it "mints one fresh symbol per bare 𝜎 the answer carries" $ do
      answer <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎 ⟧, b ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧"
      map snd (fst (minted answer 4)) `shouldBe` [FnSymbol 5, FnSymbol 6]

    it "counts every symbol it minted into the state" $ do
      answer <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎 ⟧, b ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧"
      snd (minted answer 4) `shouldBe` 6

    -- A symbol the answer names is one the entry means, not one it asks for,
    -- so nothing is minted for it
    it "mints nothing for a symbol the answer already numbers" $ do
      answer <- parseExpressionThrows "⟦ λ ⤍ 𝜎1 ⟧"
      fst (minted answer 4) `shouldBe` []

    it "mints nothing for an answer carrying no symbol at all" $ do
      answer <- parseExpressionThrows "⟦ Δ ⤍ 00- ⟧"
      snd (minted answer 4) `shouldBe` 4

  -- A program written by an earlier run holds symbols of its own, and a fresh
  -- one must never be spelled like one of them
  describe "taken" $ do
    it "takes the last symbol the program already carries" $ do
      program <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎3 ⟧, b ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧"
      taken program `shouldBe` 7

    it "takes nothing from a program carrying no symbol" $ do
      program <- parseExpressionThrows "⟦ Δ ⤍ 00- ⟧"
      taken program `shouldBe` 0
