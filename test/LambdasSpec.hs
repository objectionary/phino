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
import Lambdas (Lambda (..), Lambdas, Meta (..), emptyLambdas, joined, matched, minted, readLambdas, symbolized, taken)
import Parser (parseExpressionThrows)
import Test.Hspec

lambdasOf :: T.Text -> IO Lambdas
lambdasOf text = withLambdasOf text readLambdas

entry :: T.Text -> T.Text
entry key = "- λ: " <> key <> "\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"

answering :: Lambdas -> T.Text -> Maybe T.Text
answering known func = _key <$> matched known func

joins :: Lambdas -> T.Text -> [(T.Text, (T.Text, T.Text))]
joins known func = map spelled (maybe [] _paired (matched known func))
  where
    spelled :: (Meta, (Meta, Meta)) -> (T.Text, (T.Text, T.Text))
    spelled (meta, (left, right)) = (_spelling meta, (_spelling left, _spelling right))

rewriting :: T.Text -> T.Text
rewriting lines' = "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  rewrite:\n" <> lines'

rules :: T.Text
rules = "      rules:\n        - name: no-x\n          pattern: ⟦ !B1, x ↦ ⟦⟧, !B2 ⟧\n          result: ⟦ !B1, !B2 ⟧\n"

rewrote :: (Meta, (Meta, [a])) -> (T.Text, T.Text, Int)
rewrote (meta, (source, written)) = (_spelling meta, _spelling source, length written)

spec :: Spec
spec = do
  describe "readLambdas" $ do
    it "reads the λ function its key names" $ do
      known <- lambdasOf (entry "L_number_plus")
      answering known "L_number_plus" `shouldBe` Just "L_number_plus"

    it "reads one λ function for the whole family its key spells" $ do
      known <- lambdasOf (entry "L_box_[0-9]+_number")
      answering known "L_box_42_number" `shouldBe` Just "L_box_[0-9]+_number"

    it "reads two keys whose families share no λ name" $ do
      known <- lambdasOf (entry "L_[a-z]+_plus" <> entry "L_number_[0-9]+")
      answering known "L_number_42" `shouldBe` Just "L_number_[0-9]+"

    it "cannot read a λ function whose name merely starts with a key" $ do
      known <- lambdasOf (entry "L_number_plus")
      answering known "L_number_plus_twice" `shouldBe` Nothing

    it "reads a single key that no other key is compared with, even an anchored one" $ do
      known <- lambdasOf (entry "^L_a$")
      answering known "L_a" `shouldBe` Just "^L_a$"

    it "cannot read a λ function no entry answers" $ do
      known <- lambdasOf (entry "L_number_plus")
      answering known "L_bytes_not" `shouldBe` Nothing

    it "reads the operands of 'dataize' in the order their metas number them" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿2: $.x\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_spelling . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` ["𝛿1", "𝛿2"]

    it "reads the operands of 'dataize' in numeric order past nine" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿10: $.b\n    𝛿9: $.a\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_spelling . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` ["𝛿1", "𝛿9", "𝛿10"]

    it "reads a 'symbolize' line that stands the term of line nine as line ten" $ do
      known <- lambdasOf "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛9: 𝑛1\n    𝑛10: 𝑛9\n  𝑛: 𝑛10\n"
      map (_spelling . fst) (maybe [] _symbolized (matched known "L_pair")) `shouldBe` ["𝑛9", "𝑛10"]

    it "reads a meta under both the name it is spelled with and the name it binds" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_name . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` ["d1"]

    it "reads the operands of 'morph' under expression metas" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  𝑛: 𝑛1\n"
      map (_spelling . fst) (maybe [] _morphed (matched known "L_fork")) `shouldBe` ["𝑛1", "𝑛2"]

    it "reads the operands of 'symbolize' under expression metas" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n"
      map (_spelling . fst) (maybe [] _symbolized (matched known "L_fork")) `shouldBe` ["𝑛2"]

    it "reads a 'symbolize' line standing the term the line above it made" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  symbolize:\n    𝑛2: 𝑛1\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n"
      map (_spelling . fst) (maybe [] _symbolized (matched known "L_fork")) `shouldBe` ["𝑛2", "𝑛3"]

    it "reads the term an operand is reduced from" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      operand <- parseExpressionThrows "$.x"
      map snd (maybe [] _dataized (matched known "L_pair")) `shouldBe` [operand]

    it "reads the term the firing answers with" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )\n"
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )"
      fmap _answer (matched known "L_pair") `shouldBe` Just term

    it "reads the two metas a 'join' line joins" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n"
      joins known "L_fork" `shouldBe` [("𝑛3", ("𝑛1", "𝑛2"))]

    it "reads the two metas of a 'join' line in the order it lists them" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛2, 𝑛1]\n  𝑛: 𝑛3\n"
      joins known "L_fork" `shouldBe` [("𝑛3", ("𝑛2", "𝑛1"))]

    it "reads a 'join' line joining the terms a 'symbolize' line stood" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  symbolize:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  join:\n    𝑛5: [𝑛3, 𝑛4]\n  𝑛: 𝑛5\n"
      joins known "L_fork" `shouldBe` [("𝑛5", ("𝑛3", "𝑛4"))]

    it "reads a 'join' line joining what a line above it joined" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n    𝑛3: $.c\n  join:\n    𝑛4: [𝑛1, 𝑛2]\n    𝑛5: [𝑛4, 𝑛3]\n  𝑛: 𝑛5\n"
      joins known "L_fork" `shouldBe` [("𝑛4", ("𝑛1", "𝑛2")), ("𝑛5", ("𝑛4", "𝑛3"))]

    it "reads the lines of 'rewrite' with the metas they read and their rules" $ do
      known <- lambdasOf (rewriting "    𝑛2:\n      of: 𝑛1\n" <> rules <> "    𝑛3:\n      of: 𝑛2\n" <> rules <> "  𝑛: 𝑛3\n")
      map rewrote (maybe [] _rewritten (matched known "L_fork")) `shouldBe` [("𝑛2", "𝑛1", 1), ("𝑛3", "𝑛2", 1)]

    it "reads a 'symbolize' line standing the term a 'rewrite' line made" $ do
      known <- lambdasOf (rewriting "    𝑛2:\n      of: 𝑛1\n" <> rules <> "  symbolize:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n")
      map (_spelling . fst) (maybe [] _symbolized (matched known "L_fork")) `shouldBe` ["𝑛3"]

    it "reads an entry naming no 'join' block at all" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      joins known "L_pair" `shouldBe` []

    it "reads an entry naming neither operand block" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_spelling . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` []

    forM_
      [ ("a file which is no list of entries" :: String, "λ: L_pair\n" :: T.Text, "cannot be read" :: String)
      , ("an entry with no λ key", "- 𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "no 'λ' key")
      , ("an entry with no answer at all", "- λ: L_pair\n", "no '𝑛' key")
      , ("an entry whose answer is no term of the calculus", "- λ: L_pair\n  𝑛: ⟦ λ ⤍\n", "cannot be read")
      , ("two entries under one key", entry "L_pair" <> entry "L_pair", "is used by more than one entry")
      ,
        ( "two entries with overlapping regular expressions"
        , entry "L_(foo|bar)" <> entry "L_foo"
        , "match some of the same lambda names"
        )
      ,
        ( "two keys overlapping on a name neither of them spells"
        , entry "L_[a-z]+_plus" <> entry "L_number_[a-z]+"
        , "such as 'L_number_plus'"
        )
      , ("a key with a back reference phino cannot compare", entry "L_(a)\\1" <> entry "L_b", "cannot be compared")
      , ("a key which is no regular expression", entry "L_[pair", "is not a regular expression")
      , ("an operand of 'dataize' which is no bytes meta", "- λ: L_pair\n  dataize:\n    𝑛1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "is not a bytes meta")
      , ("an operand of 'morph' which is no expression meta", "- λ: L_pair\n  morph:\n    𝛿1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "is not an expression meta")
      , ("an operand of 'symbolize' which is no expression meta", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝛿1: 𝑛1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("an operand of 'symbolize' the entry never bound", "- λ: L_pair\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n", "names no meta")
      , ("an operand of 'symbolize' which is no meta at all", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛2: $.x\n  𝑛: 𝑛2\n", "names no meta")
      , ("an operand of 'symbolize' standing the term of a line below it", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛2: 𝑛3\n    𝑛3: 𝑛1\n  𝑛: 𝑛2\n", "names no meta")
      , ("an operand referencing a meta the entry never matched", "- λ: L_pair\n  dataize:\n    𝛿1: '!n'\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "cannot be referenced")
      , ("an answer reading the data its operands came down to", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ Δ ⤍ 𝛿1 ⟧\n", "reads data")
      , ("a meta bound by 'morph' and again by 'symbolize'", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛1: 𝑛1\n  𝑛: 𝑛1\n", "The meta '𝑛1' of λ function 'L_pair' is bound by more than one line")
      , ("an answer writing a numbered symbol", "- λ: L_pair\n  𝑛: '⟦ a ↦ ⟦ λ ⤍ 𝜎1 ⟧, b ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧'\n", "writes the numbered symbol '𝜎1'")
      , ("an answer carrying an anonymous meta of another kind", "- λ: L_pair\n  𝑛: '⟦ φ ↦ !n ⟧'\n", "cannot be referenced")
      , ("a 'join' line joining one meta alone", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1]\n  𝑛: 𝑛2\n", "must join exactly two metas")
      , ("a 'join' line joining three metas", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n    𝑛3: $.c\n  join:\n    𝑛4: [𝑛1, 𝑛2, 𝑛3]\n  𝑛: 𝑛4\n", "must join exactly two metas")
      , ("a 'join' line joining what is no expression meta", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1, $.else]\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a 'join' line bound to what is no expression meta", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝛿1: [𝑛1, 𝑛2]\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a 'join' line joining a meta the entry never bound", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by a line above it")
      , ("a 'join' line joining a meta that came down to data", "- λ: L_fork\n  dataize:\n    𝛿1: $.ρ\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1, 𝛿1]\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a 'join' line joining what a line below it made", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝑛3: [𝑛1, 𝑛4]\n    𝑛4: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by a line above it")
      , ("a 'rewrite' line rewriting a meta the entry never bound", rewriting "    𝑛2:\n      of: 𝑛5\n" <> rules <> "  𝑛: 𝑛2\n", "names no meta bound by 'morph' or by a line above it")
      , ("a 'rewrite' line rewriting what a line below it made", rewriting "    𝑛2:\n      of: 𝑛3\n" <> rules <> "    𝑛3:\n      of: 𝑛1\n" <> rules <> "  𝑛: 𝑛2\n", "names no meta bound by 'morph' or by a line above it")
      , ("a 'rewrite' line rewriting what a 'symbolize' line made", rewriting "    𝑛3:\n      of: 𝑛2\n" <> rules <> "  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by a line above it")
      , ("a 'rewrite' line bound to what is no expression meta", rewriting "    𝛿1:\n      of: 𝑛1\n" <> rules <> "  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a 'rewrite' line with no rules at all", rewriting "    𝑛2:\n      of: 𝑛1\n  𝑛: 𝑛2\n", "cannot be read")
      , ("a rule of 'rewrite' minting a fresh symbol", rewriting "    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: minting\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n          result: ⟦ λ ⤍ 𝜎 ⟧\n  𝑛: 𝑛2\n", "writes a symbol 𝜎 into its result")
      , ("a rule of 'rewrite' writing a symbol nobody minted", rewriting "    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: naming\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n          result: ⟦ λ ⤍ 𝜎1 ⟧\n  𝑛: 𝑛2\n", "writes a symbol 𝜎 into its result")
      , ("a rule of 'rewrite' reading a meta it never binds", rewriting "    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: unbound\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n          result: ⟦ φ ↦ 𝑒2 ⟧\n  𝑛: 𝑛2\n", "reads the meta 'e2' it never binds")
      , ("a rule of 'rewrite' with no result", rewriting "    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: empty\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n  𝑛: 𝑛2\n", "cannot be read")
      , ("an answer naming a meta no block binds", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: 𝑛7\n", "reads the meta 'n7' that no block binds")
      , ("an operand of 'dataize' reading a meta", "- λ: L_pair\n  dataize:\n    𝛿1: 𝑛5\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "of 'dataize' of λ function 'L_pair' reads the meta 'n5'")
      , ("an operand of 'morph' reading a meta", "- λ: L_pair\n  morph:\n    𝑛1: $.x.𝜏1\n  𝑛: 𝑛1\n", "of 'morph' of λ function 'L_pair' reads the meta 't1'")
      , ("a 'symbolize' line standing what a 'join' line made", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  symbolize:\n    𝑛5: 𝑛3\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛5\n", "names no meta bound by 'morph' or by a line above it")
      ]
      ( \(desc, text, message) ->
          it ("cannot read " ++ desc) $
            lambdasOf text `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))
      )

  describe "emptyLambdas" $
    it "cannot read a λ function without the file naming one" $
      answering emptyLambdas "L_number_plus" `shouldBe` Nothing

  describe "minted" $ do
    it "mints one fresh symbol per bare 𝜎 the answer carries" $ do
      answer <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎 ⟧, b ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧"
      map snd (fst (minted answer 4)) `shouldBe` [FnSymbol 5, FnSymbol 6]

    it "counts every symbol it minted into the state" $ do
      answer <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎 ⟧, b ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧"
      snd (minted answer 4) `shouldBe` 6

    it "mints nothing for a symbol the answer already numbers" $ do
      answer <- parseExpressionThrows "⟦ λ ⤍ 𝜎1 ⟧"
      fst (minted answer 4) `shouldBe` []

    it "mints nothing for an answer carrying no symbol at all" $ do
      answer <- parseExpressionThrows "⟦ Δ ⤍ 00- ⟧"
      snd (minted answer 4) `shouldBe` 4

  describe "symbolized" $ do
    it "stands every datum of a term into an unknown" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ Δ ⤍ 00- ⟧ )( t ↦ ⟦ Δ ⤍ FF- ⟧ ) ⟧"
      unknown <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎6 ⟧ ) ⟧"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` unknown

    it "tells the data every symbol it minted stands for" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ Δ ⤍ 00- ⟧ )( t ↦ ⟦ Δ ⤍ FF- ⟧ ) ⟧"
      let (_, known, _) = symbolized term 4
      known `shouldBe` [(5, BtOne "00"), (6, BtOne "FF")]

    it "counts every symbol it minted into the state" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ Δ ⤍ 00- ⟧ )( t ↦ ⟦ Δ ⤍ FF- ⟧ ) ⟧"
      let (_, _, spent) = symbolized term 4
      spent `shouldBe` 6

    it "leaves a datum standing outside the φ chain alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧, neg ↦ ⟦ φ ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧"
      unknown <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, neg ↦ ⟦ φ ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧"
      let (masked, known, spent) = symbolized term 4
      masked `shouldBe` unknown
      known `shouldBe` [(5, BtOne "00")]
      spent `shouldBe` 5

    it "leaves a term carrying no datum as it was written" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` term

    it "stands a datum standing as the argument of an application" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 00- ⟧ ) )"
      unknown <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) )"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` unknown

    it "keeps what the formation of a datum carries besides the datum" $ do
      term <- parseExpressionThrows "⟦ Δ ⤍ 00-, ρ ↦ ⟦⟧ ⟧"
      unknown <- parseExpressionThrows "⟦ λ ⤍ 𝜎5, ρ ↦ ⟦⟧ ⟧"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` unknown

    it "leaves the data a ρ carries alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧"
      unknown <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` unknown

    it "mints nothing for a term carrying no datum at all" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      let (_, _, spent) = symbolized term 4
      spent `shouldBe` 4

  describe "joined" $ do
    let joining :: String -> String -> Int -> IO (Maybe (Expression, [(Int, (Int, Int))], Int))
        joining left right spent = do
          one <- parseExpressionThrows left
          two <- parseExpressionThrows right
          pure (joined one two spent)

    it "joins two branches differing in one symbol into a fresh one" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ⟧"
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "tells the two symbols every fresh one stands for" $ do
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧" 4
      fmap (\(_, facts, _) -> facts) made `shouldBe` Just [(5, (1, 2))]

    it "counts every symbol it minted into the state" $ do
      made <- joining "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎4 ⟧ ) ⟧" 4
      fmap (\(_, _, spent) -> spent) made `shouldBe` Just 6

    it "mints one symbol per pair of differing symbols" $ do
      made <- joining "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎4 ⟧ ) ⟧" 4
      fmap (\(_, facts, _) -> facts) made `shouldBe` Just [(5, (1, 3)), (6, (2, 4))]

    it "mints one symbol for the pair it meets twice" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) ⟧"
      made <- joining "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "carries a method from the first branch and mints nothing for it" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧ ⟧"
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎8 ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "refuses two branches whose bindings are named differently" $ do
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, m ↦ ⟦ x ↦ ∅ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, other ↦ ⟦ x ↦ ∅ ⟧ ⟧" 4
      made `shouldBe` Nothing

    it "joins two branches that are one term into that very term" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      made <- joining "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" 4
      made `shouldBe` Just (term, [], 4)

    it "joins two branches through the argument of an application" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )"
      made <- joining "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "leaves what a ρ carries alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧"
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, ρ ↦ ⟦ y ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "joins two branches differing in their ρ alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧"
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ y ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [], 4)

    forM_
      [ ("a datum with a symbol" :: String, "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧ ⟧" :: String, "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧" :: String)
      , ("two different data", "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧ ⟧", "⟦ φ ↦ ⟦ Δ ⤍ FF- ⟧ ⟧")
      , ("a symbol with a λ function nothing else names", "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧", "⟦ φ ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧")
      , ("two branches one of which carries a binding more", "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧", "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, x ↦ ⟦⟧ ⟧")
      , ("two branches binding their symbols under different attributes", "⟦ a ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧", "⟦ b ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧")
      , ("two branches of different forma", "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )", "Φ.bool( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )")
      ]
      ( \(desc, left, right) ->
          it ("cannot join " ++ desc) $ do
            made <- joining left right 4
            fmap (\(joint, _, _) -> joint) made `shouldBe` Nothing
      )

  describe "taken" $ do
    it "takes the last symbol the program already carries" $ do
      program <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎3 ⟧, b ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧"
      taken program `shouldBe` 7

    it "takes nothing from a program carrying no symbol" $ do
      program <- parseExpressionThrows "⟦ Δ ⤍ 00- ⟧"
      taken program `shouldBe` 0
