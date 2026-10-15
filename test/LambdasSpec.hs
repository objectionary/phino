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
import Lambdas (Lambda (..), Lambdas, Line (..), Meta (..), Option (..), emptyLambdas, joined, matched, message, minted, readLambdas, stayed, symbolized, taken, thrown)
import Parser (parseExpressionThrows)
import Test.Hspec

lambdasOf :: T.Text -> IO Lambdas
lambdasOf text = withLambdasOf text readLambdas

entry :: T.Text -> T.Text
entry key = "- λ: " <> key <> "\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"

answering :: Lambdas -> T.Text -> Maybe T.Text
answering known func = _key <$> matched known func

lined :: Lambdas -> T.Text -> [Line]
lined known func = maybe [] _lines (matched known func)

order :: Lambdas -> T.Text -> [T.Text]
order known func = map bound (lined known func)
  where
    bound :: Line -> T.Text
    bound (Evaluated meta _) = _spelling meta
    bound (Rewritten meta _ _) = _spelling meta
    bound (Stayed meta _) = _spelling meta
    bound (Symbolized meta _) = _spelling meta
    bound (Paired meta _ _) = _spelling meta

joins :: Lambdas -> T.Text -> [(T.Text, (T.Text, T.Text))]
joins known func = [(_spelling meta, (_spelling left, _spelling right)) | Paired meta left right <- lined known func]

fired :: Lambdas -> T.Text -> [(T.Text, T.Text)]
fired known func = [(_spelling meta, _spelling source) | Evaluated meta source <- lined known func]

stays :: Lambdas -> T.Text -> [(T.Text, T.Text)]
stays known func = [(_spelling meta, _spelling source) | Stayed meta source <- lined known func]

stood :: Lambdas -> T.Text -> [T.Text]
stood known func = [_spelling meta | Symbolized meta _ <- lined known func]

rewriting :: T.Text -> T.Text
rewriting lines' = "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  rewrite:\n" <> lines'

rules :: T.Text
rules = "      rules:\n        - name: no-x\n          pattern: ⟦ !B1, x ↦ ⟦⟧, !B2 ⟧\n          result: ⟦ !B1, !B2 ⟧\n"

rewrote :: Lambdas -> T.Text -> [(T.Text, T.Text, Int)]
rewrote known func = [(_spelling meta, _spelling source, length written) | Rewritten meta source written <- lined known func]

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
      stood known "L_pair" `shouldBe` ["𝑛9", "𝑛10"]

    it "reads a meta under both the name it is spelled with and the name it binds" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_name . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` ["d1"]

    it "reads the operands of 'morph' under expression metas" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  𝑛: 𝑛1\n"
      map (_spelling . fst) (maybe [] _morphed (matched known "L_fork")) `shouldBe` ["𝑛1", "𝑛2"]

    it "reads the operands of 'symbolize' under expression metas" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n"
      stood known "L_fork" `shouldBe` ["𝑛2"]

    it "reads a 'symbolize' line standing the term the line above it made" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  symbolize:\n    𝑛2: 𝑛1\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n"
      stood known "L_fork" `shouldBe` ["𝑛2", "𝑛3"]

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

    it "reads an entry whose 'join' lines are lenient with no option" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  lenient: []\n  𝑛: 𝑛3\n"
      fmap _lenient (matched known "L_fork") `shouldBe` Just (Just [])

    it "reads an entry saying nothing of leniency as strict" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n"
      fmap _lenient (matched known "L_fork") `shouldBe` Just Nothing

    it "reads a lenient entry that defers a copy reading a bare symbol" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  lenient: [defer]\n  𝑛: 𝑛3\n"
      fmap _lenient (matched known "L_fork") `shouldBe` Just (Just [Defer])

    it "reads the terminators an entry names beside its 'join' block" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  terminators: [L_throw, L_halt]\n  𝑛: 𝑛3\n"
      fmap _terminators (matched known "L_fork") `shouldBe` Just ["L_throw", "L_halt"]

    it "reads an entry saying nothing of terminators as one naming none" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n"
      fmap _terminators (matched known "L_fork") `shouldBe` Just []

    it "reads a terminator that no key of the file matches" $ do
      known <- lambdasOf ("- λ: L_fork\n  morph:\n    𝑛1: $.a\n  terminators: [L_throw]\n  𝑛: 𝑛1\n" <> entry "L_throw_[0-9]+")
      fmap _terminators (matched known "L_fork") `shouldBe` Just ["L_throw"]

    it "reads the lines of 'rewrite' with the metas they read and their rules" $ do
      known <- lambdasOf (rewriting "    𝑛2:\n      of: 𝑛1\n" <> rules <> "    𝑛3:\n      of: 𝑛2\n" <> rules <> "  𝑛: 𝑛3\n")
      rewrote known "L_fork" `shouldBe` [("𝑛2", "𝑛1", 1), ("𝑛3", "𝑛2", 1)]

    it "reads a 'symbolize' line standing the term a 'rewrite' line made" $ do
      known <- lambdasOf (rewriting "    𝑛2:\n      of: 𝑛1\n" <> rules <> "  symbolize:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n")
      stood known "L_fork" `shouldBe` ["𝑛3"]

    it "reads the metas the lines of 'evaluate' fire" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  evaluate:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  𝑛: 𝑛3\n"
      fired known "L_fork" `shouldBe` [("𝑛3", "𝑛1"), ("𝑛4", "𝑛2")]

    it "reads an 'evaluate' line firing what the line above it fired" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛1\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n"
      fired known "L_fork" `shouldBe` [("𝑛2", "𝑛1"), ("𝑛3", "𝑛2")]

    it "reads a 'rewrite' line rewriting what an 'evaluate' line fired" $ do
      known <- lambdasOf ("- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛1\n  rewrite:\n    𝑛3:\n      of: 𝑛2\n" <> rules <> "  𝑛: 𝑛3\n")
      rewrote known "L_fork" `shouldBe` [("𝑛3", "𝑛2", 1)]

    it "reads the metas the lines of 'stay' keep, a line keeping what the line above it kept" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛2: 𝑛1\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n"
      stays known "L_fork" `shouldBe` [("𝑛2", "𝑛1"), ("𝑛3", "𝑛2")]

    it "reads a 'stay' line keeping what a 'rewrite' line made" $ do
      known <- lambdasOf (rewriting "    𝑛2:\n      of: 𝑛1\n" <> rules <> "  stay:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n")
      stays known "L_fork" `shouldBe` [("𝑛3", "𝑛2")]

    it "reads a 'symbolize' line standing what a 'stay' line kept" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛2: 𝑛1\n  symbolize:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n"
      stood known "L_fork" `shouldBe` ["𝑛3"]

    it "runs a 'rewrite' line before the 'evaluate' line firing what it made" $ do
      known <- lambdasOf (rewriting "    𝑛2:\n      of: 𝑛1\n" <> rules <> "  evaluate:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n")
      order known "L_fork" `shouldBe` ["𝑛2", "𝑛3"]

    it "runs a line before the line of its own block reading what it made" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛3\n    𝑛3: 𝑛1\n  𝑛: 𝑛2\n"
      order known "L_fork" `shouldBe` ["𝑛3", "𝑛2"]

    it "runs a 'symbolize' line before the 'rewrite' line rewriting what it stood" $ do
      known <- lambdasOf (rewriting "    𝑛3:\n      of: 𝑛2\n" <> rules <> "  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n")
      order known "L_fork" `shouldBe` ["𝑛2", "𝑛3"]

    it "runs a 'symbolize' line before the 'stay' line keeping what it stood" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛3: 𝑛2\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n"
      order known "L_fork" `shouldBe` ["𝑛2", "𝑛3"]

    it "runs a 'join' line before the 'symbolize' line standing what it joined" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  symbolize:\n    𝑛4: 𝑛3\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛4\n"
      order known "L_fork" `shouldBe` ["𝑛3", "𝑛4"]

    it "runs the lines no meta orders in the order of their blocks" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  symbolize:\n    𝑛4: 𝑛1\n  stay:\n    𝑛5: 𝑛2\n  evaluate:\n    𝑛6: 𝑛1\n  𝑛: 𝑛3\n"
      order known "L_fork" `shouldBe` ["𝑛6", "𝑛5", "𝑛4", "𝑛3"]

    it "reads an entry naming no 'stay' block at all" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  𝑛: 𝑛1\n"
      stays known "L_fork" `shouldBe` []

    it "reads an entry naming no 'evaluate' block at all" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  𝑛: 𝑛1\n"
      fired known "L_fork" `shouldBe` []

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
      , ("an operand of 'symbolize' which is no meta at all", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛2: $.x\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("an operand referencing a meta the entry never matched", "- λ: L_pair\n  dataize:\n    𝛿1: '!n'\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "cannot be referenced")
      , ("an answer reading the data its operands came down to", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ Δ ⤍ 𝛿1 ⟧\n", "reads data")
      , ("a meta bound by 'morph' and again by 'symbolize'", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛1: 𝑛1\n  𝑛: 𝑛1\n", "The meta '𝑛1' of λ function 'L_pair' is bound by more than one line")
      , ("an answer writing a numbered symbol", "- λ: L_pair\n  𝑛: '⟦ a ↦ ⟦ λ ⤍ 𝜎1 ⟧, b ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧'\n", "writes the numbered symbol '𝜎1'")
      , ("an answer carrying an anonymous meta of another kind", "- λ: L_pair\n  𝑛: '⟦ φ ↦ !n ⟧'\n", "cannot be referenced")
      , ("a 'join' line joining one meta alone", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1]\n  𝑛: 𝑛2\n", "must join exactly two metas")
      , ("a 'join' line joining three metas", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n    𝑛3: $.c\n  join:\n    𝑛4: [𝑛1, 𝑛2, 𝑛3]\n  𝑛: 𝑛4\n", "must join exactly two metas")
      , ("a 'join' line joining what is no expression meta", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1, $.else]\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a 'join' line bound to what is no expression meta", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝛿1: [𝑛1, 𝑛2]\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a 'join' line joining a meta the entry never bound", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by any line")
      , ("a 'join' line joining a meta that came down to data", "- λ: L_fork\n  dataize:\n    𝛿1: $.ρ\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1, 𝛿1]\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a 'rewrite' line rewriting a meta the entry never bound", rewriting "    𝑛2:\n      of: 𝑛5\n" <> rules <> "  𝑛: 𝑛2\n", "names no meta bound by 'morph' or by any line")
      , ("a 'rewrite' line bound to what is no expression meta", rewriting "    𝛿1:\n      of: 𝑛1\n" <> rules <> "  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a 'rewrite' line with no rules at all", rewriting "    𝑛2:\n      of: 𝑛1\n  𝑛: 𝑛2\n", "cannot be read")
      , ("a rule of 'rewrite' minting a fresh symbol", rewriting "    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: minting\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n          result: ⟦ λ ⤍ 𝜎 ⟧\n  𝑛: 𝑛2\n", "writes a symbol 𝜎 into its result")
      , ("a rule of 'rewrite' writing a symbol nobody minted", rewriting "    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: naming\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n          result: ⟦ λ ⤍ 𝜎1 ⟧\n  𝑛: 𝑛2\n", "writes a symbol 𝜎 into its result")
      , ("a rule of 'rewrite' reading a meta it never binds", rewriting "    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: unbound\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n          result: ⟦ φ ↦ 𝑒2 ⟧\n  𝑛: 𝑛2\n", "reads the meta 'e2' it never binds")
      , ("a rule of 'rewrite' with no result", rewriting "    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: empty\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n  𝑛: 𝑛2\n", "cannot be read")
      , ("an 'evaluate' line firing a meta the entry never bound", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by any line")
      , ("an 'evaluate' line bound to what is no expression meta", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝛿1: 𝑛1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("an 'evaluate' line firing a meta that came down to data", "- λ: L_fork\n  dataize:\n    𝛿1: $.ρ\n  evaluate:\n    𝑛1: 𝛿1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("an 'evaluate' line firing a term instead of a meta", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: $.a\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a meta bound by 'morph' and again by 'evaluate'", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛1: 𝑛1\n  𝑛: 𝑛1\n", "The meta '𝑛1' of λ function 'L_fork' is bound by more than one line")
      , ("a 'stay' line keeping a meta the entry never bound", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by any line")
      , ("a 'stay' line bound to what is no expression meta", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  stay:\n    𝛿1: 𝑛1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a 'stay' line keeping a term instead of a meta", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛2: $.a\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a meta bound by 'morph' and again by 'stay'", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛1: 𝑛1\n  𝑛: 𝑛1\n", "The meta '𝑛1' of λ function 'L_fork' is bound by more than one line")
      , ("a line reading the meta it binds", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛2\n  𝑛: 𝑛2\n", "The line binding '𝑛2' of λ function 'L_fork' reads the meta it binds")
      , ("two lines reading what each other made", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛3\n  symbolize:\n    𝑛3: 𝑛2\n  𝑛: 𝑛2\n", "The line binding '𝑛2' of λ function 'L_fork' reads the meta it binds")
      , ("a line reading what two lines reading each other made, blaming one of the two", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛5: 𝑛3\n  symbolize:\n    𝑛3: 𝑛4\n    𝑛4: 𝑛3\n  𝑛: 𝑛5\n", "The line binding '𝑛3' of λ function 'L_fork' reads the meta it binds")
      , ("an answer naming a meta no block binds", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: 𝑛7\n", "reads the meta 'n7' that no block binds")
      , ("an operand of 'dataize' reading a meta", "- λ: L_pair\n  dataize:\n    𝛿1: 𝑛5\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "of 'dataize' of λ function 'L_pair' reads the meta 'n5'")
      , ("an operand of 'morph' reading a meta", "- λ: L_pair\n  morph:\n    𝑛1: $.x.𝜏1\n  𝑛: 𝑛1\n", "of 'morph' of λ function 'L_pair' reads the meta 't1'")
      , ("a lenient entry with no 'join' line to be lenient in", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  lenient: []\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "is lenient but has no 'join' line")
      , ("a 'lenient' key which is no list", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  lenient: true\n  𝑛: 𝑛3\n", "lenient: parsing [] failed, expected Array")
      , ("a 'lenient' key naming an option phino does not know", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  lenient: [defr]\n  𝑛: 𝑛3\n", "The option 'defr' of 'lenient' of λ function 'L_fork' is unknown")
      , ("a 'terminators' key which is no list", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  terminators: L_throw\n  𝑛: 𝑛1\n", "expected Array")
      ,
        ( "a terminator an entry of the file answers"
        , "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  terminators: [L_throw]\n  𝑛: 𝑛1\n" <> entry "L_th[a-z]+"
        , "the terminator 'L_throw' of the key 'L_fork' is answered by the key 'L_th[a-z]+'"
        )
      ]
      ( \(desc, text, reason) ->
          it ("cannot read " ++ desc) $
            lambdasOf text `shouldThrow` (\err -> reason `isInfixOf` show (err :: SomeException))
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

  describe "stayed" $ do
    let keeping :: String -> String -> IO (Expression, Expression)
        keeping written expected = do
          term <- parseExpressionThrows written
          kept <- parseExpressionThrows expected
          pure (stayed term, kept)

    it "drops the ρ a dispatch is applied to" $ do
      (term, kept) <- keeping "Φ.bytes( ρ ↦ ⟦ x ↦ ∅ ⟧ )" "Φ.bytes"
      term `shouldBe` kept

    it "drops the ρ a dispatch is applied to after an argument binding no ρ" $ do
      (term, kept) <- keeping "Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( ρ ↦ ⟦ x ↦ ∅ ⟧ )" "Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      term `shouldBe` kept

    it "drops every ρ a chain of applications gives a dispatch" $ do
      (term, kept) <- keeping "ξ.f( ρ ↦ ⟦ a ↦ ∅ ⟧ )( α0 ↦ ⟦⟧ )( ρ ↦ ⟦ b ↦ ∅ ⟧ )" "ξ.f( α0 ↦ ⟦⟧ )"
      term `shouldBe` kept

    it "keeps the ρ a formation is applied to" $ do
      (term, kept) <- keeping "⟦ ρ ↦ ∅, t ↦ ∅ ⟧( t ↦ ⟦⟧ )( ρ ↦ ⟦ x ↦ ∅ ⟧ )" "⟦ ρ ↦ ∅, t ↦ ∅ ⟧( t ↦ ⟦⟧ )( ρ ↦ ⟦ x ↦ ∅ ⟧ )"
      term `shouldBe` kept

    it "drops a ρ standing deep inside the argument of an application" $ do
      (term, kept) <- keeping "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( ρ ↦ ⟦ x ↦ ∅ ⟧ ) )" "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) )"
      term `shouldBe` kept

    it "drops a ρ standing in a method of a formation and not only in its φ" $ do
      (term, kept) <- keeping "⟦ φ ↦ ⟦⟧, m ↦ ⟦ φ ↦ Φ.f( ρ ↦ ⟦ x ↦ ∅ ⟧ ) ⟧ ⟧" "⟦ φ ↦ ⟦⟧, m ↦ ⟦ φ ↦ Φ.f ⟧ ⟧"
      term `shouldBe` kept

    it "drops a ρ standing inside a ρ the term keeps" $ do
      (term, kept) <- keeping "⟦ x ↦ ∅ ⟧( ρ ↦ Φ.f( ρ ↦ ⟦ y ↦ ∅ ⟧ ) )" "⟦ x ↦ ∅ ⟧( ρ ↦ Φ.f )"
      term `shouldBe` kept

  describe "joined" $ do
    let joining :: Bool -> String -> String -> Int -> IO (Maybe (Expression, [(Int, (Int, Int))], Int))
        joining lenient left right spent = do
          one <- parseExpressionThrows left
          two <- parseExpressionThrows right
          pure (joined lenient one two spent)

    it "joins two branches differing in one symbol into a fresh one" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ⟧"
      made <- joining False "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "tells the two symbols every fresh one stands for" $ do
      made <- joining False "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧" 4
      fmap (\(_, facts, _) -> facts) made `shouldBe` Just [(5, (1, 2))]

    it "counts every symbol it minted into the state" $ do
      made <- joining False "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎4 ⟧ ) ⟧" 4
      fmap (\(_, _, spent) -> spent) made `shouldBe` Just 6

    it "mints one symbol per pair of differing symbols" $ do
      made <- joining False "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎4 ⟧ ) ⟧" 4
      fmap (\(_, facts, _) -> facts) made `shouldBe` Just [(5, (1, 3)), (6, (2, 4))]

    it "mints one symbol for the pair it meets twice" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) ⟧"
      made <- joining False "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "carries a method from the first branch and mints nothing for it" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧ ⟧"
      made <- joining False "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎8 ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "refuses two branches whose bindings are named differently" $ do
      made <- joining False "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, m ↦ ⟦ x ↦ ∅ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, other ↦ ⟦ x ↦ ∅ ⟧ ⟧" 4
      made `shouldBe` Nothing

    it "joins two branches that are one term into that very term" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      made <- joining False "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" 4
      made `shouldBe` Just (term, [], 4)

    it "joins two branches through the argument of an application" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )"
      made <- joining False "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "leaves what a ρ carries alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧"
      made <- joining False "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, ρ ↦ ⟦ y ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "joins a bare symbol with the symbol the φ chain of a formation ends in" $ do
      term <- parseExpressionThrows "⟦ λ ⤍ 𝜎5 ⟧"
      made <- joining False "⟦ φ ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧, eq ↦ ⟦ b ↦ ∅ ⟧ ⟧" "⟦ λ ⤍ 𝜎2 ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "joins a formation with a bare symbol standing first" $ do
      term <- parseExpressionThrows "⟦ λ ⤍ 𝜎8 ⟧"
      made <- joining False "⟦ λ ⤍ 𝜎3 ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎6 ⟧, neg ↦ ⟦⟧ ⟧" 7
      made `shouldBe` Just (term, [(8, (3, 6))], 8)

    it "mints nothing for a bare symbol the φ chain of a formation ends in" $ do
      term <- parseExpressionThrows "⟦ λ ⤍ 𝜎2 ⟧"
      made <- joining False "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, m ↦ ⟦⟧ ⟧" "⟦ λ ⤍ 𝜎2 ⟧" 4
      made `shouldBe` Just (term, [], 4)

    it "joins two branches differing in their ρ alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧"
      made <- joining False "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ y ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [], 4)

    it "cannot join a term with a decorator of it unless the join is lenient" $ do
      made <- joining False "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" 4
      made `shouldBe` Nothing

    it "joins a term with a decorator of it into the decorator when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) ⟧"
      made <- joining True "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "joins a decorator with the term it decorates standing second when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎8 ⟧ ) ⟧"
      made <- joining True "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ ) ⟧" "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎6 ⟧ )" 7
      made `shouldBe` Just (term, [(8, (3, 6))], 8)

    it "drops every binding of the decorator but its φ when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) ⟧"
      made <- joining True "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ), ρ ↦ ⟦ x ↦ ⟦⟧ ⟧, neg ↦ ⟦⟧ ⟧" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "joins a copy with a decorator of another copy into the decorator when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, neg ↦ ⟦⟧ ⟧ ⟧"
      made <- joining True "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, neg ↦ ⟦⟧ ⟧" "⟦ φ ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, neg ↦ ⟦⟧ ⟧ ⟧" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "joins a decorator of a copy with another copy standing second into the decorator when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, neg ↦ ⟦⟧ ⟧ ⟧"
      made <- joining True "⟦ φ ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, neg ↦ ⟦⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, neg ↦ ⟦⟧ ⟧" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "joins a term with a decorator of a decorator of it when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) ⟧ ⟧"
      made <- joining True "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "⟦ φ ↦ ⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧ ⟧" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "joins a term with a decorator of it inside an argument when lenient" $ do
      term <- parseExpressionThrows "Φ.f( x ↦ ⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) ⟧ )"
      made <- joining True "Φ.f( x ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) )" "Φ.f( x ↦ ⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧ )" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "joins a bare symbol with a term holding one symbol into a copy of that term when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.bool( if ↦ ⟦ λ ⤍ L_fork, φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ⟧ ) ⟧"
      made <- joining True "⟦ λ ⤍ 𝜎1 ⟧" "⟦ φ ↦ Φ.bool( if ↦ ⟦ λ ⤍ L_fork, φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧ ) ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "joins a term holding one symbol with a bare symbol standing second when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.bool( if ↦ ⟦ λ ⤍ L_fork, φ ↦ ⟦ λ ⤍ 𝜎8 ⟧ ⟧ ) ⟧"
      made <- joining True "⟦ φ ↦ Φ.bool( if ↦ ⟦ λ ⤍ L_fork, φ ↦ ⟦ λ ⤍ 𝜎6 ⟧ ⟧ ) ⟧" "⟦ λ ⤍ 𝜎3 ⟧" 7
      made `shouldBe` Just (term, [(8, (6, 3))], 8)

    it "keeps every binding of the term holding one symbol when lenient" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ), ρ ↦ ⟦ x ↦ ⟦⟧ ⟧, neg ↦ ⟦⟧ ⟧"
      made <- joining True "⟦ λ ⤍ 𝜎1 ⟧" "⟦ φ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ), ρ ↦ ⟦ x ↦ ⟦⟧ ⟧, neg ↦ ⟦⟧ ⟧" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    it "replaces every place the one symbol of a term stands at when lenient" $ do
      term <- parseExpressionThrows "Φ.f( a ↦ ⟦ λ ⤍ 𝜎5 ⟧ )( b ↦ ⟦ λ ⤍ 𝜎5 ⟧ )"
      made <- joining True "⟦ λ ⤍ 𝜎1 ⟧" "Φ.f( a ↦ ⟦ λ ⤍ 𝜎2 ⟧ )( b ↦ ⟦ λ ⤍ 𝜎2 ⟧ )" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    it "still joins a bare symbol with the symbol a φ chain ends in into a bare one when lenient" $ do
      term <- parseExpressionThrows "⟦ λ ⤍ 𝜎8 ⟧"
      made <- joining True "⟦ λ ⤍ 𝜎3 ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎6 ⟧, neg ↦ ⟦⟧ ⟧" 7
      made `shouldBe` Just (term, [(8, (3, 6))], 8)

    forM_
      [ ("a term with a formation carrying no φ" :: String, "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" :: String, "⟦ neg ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" :: String)
      , ("a term with a decorator of a term of another forma", "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )", "⟦ φ ↦ Φ.bool( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧")
      , ("two decorators one of which carries a binding more", "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧", "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, x ↦ ⟦⟧ ⟧")
      , ("a bare symbol with a term holding two symbols", "⟦ λ ⤍ 𝜎1 ⟧", "Φ.f( a ↦ ⟦ λ ⤍ 𝜎2 ⟧ )( b ↦ ⟦ λ ⤍ 𝜎3 ⟧ )")
      , ("a bare symbol with a term holding a second symbol in its ρ", "⟦ λ ⤍ 𝜎1 ⟧", "⟦ φ ↦ Φ.bool( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ), ρ ↦ ⟦ x ↦ ⟦ λ ⤍ 𝜎3 ⟧ ⟧ ⟧")
      , ("a bare symbol with a term holding no symbol", "⟦ λ ⤍ 𝜎1 ⟧", "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧, x ↦ ⟦⟧ ⟧")
      ]
      ( \(desc, left, right) ->
          it ("cannot leniently join " ++ desc) $ do
            made <- joining True left right 4
            fmap (\(joint, _, _) -> joint) made `shouldBe` Nothing
      )

    forM_
      [ ("a datum with a symbol" :: String, "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧ ⟧" :: String, "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧" :: String)
      , ("two different data", "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧ ⟧", "⟦ φ ↦ ⟦ Δ ⤍ FF- ⟧ ⟧")
      , ("a symbol with a λ function nothing else names", "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧", "⟦ φ ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧")
      , ("two branches one of which carries a binding more", "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧", "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, x ↦ ⟦⟧ ⟧")
      , ("two branches binding their symbols under different attributes", "⟦ a ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧", "⟦ b ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧")
      , ("two branches of different forma", "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )", "Φ.bool( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )")
      , ("a bare symbol with a formation whose φ chain ends in a datum", "⟦ λ ⤍ 𝜎1 ⟧", "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧, x ↦ ⟦⟧ ⟧")
      , ("a bare symbol with a formation carrying no φ", "⟦ a ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧", "⟦ λ ⤍ 𝜎1 ⟧")
      , ("a bare symbol with an application", "⟦ λ ⤍ 𝜎1 ⟧", "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )")
      , ("a bare symbol with a decorator of an application", "⟦ λ ⤍ 𝜎1 ⟧", "⟦ φ ↦ Φ.bool( if ↦ ⟦ λ ⤍ L_fork, φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧ ) ⟧")
      ]
      ( \(desc, left, right) ->
          it ("cannot join " ++ desc) $ do
            made <- joining False left right 4
            fmap (\(joint, _, _) -> joint) made `shouldBe` Nothing
      )

  describe "thrown" $ do
    it "finds a terminator in a formation whose λ it names" $ do
      term <- parseExpressionThrows "⟦ message ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎4 ⟧ ), λ ⤍ L_throw ⟧"
      thrown ["L_halt", "L_throw"] term `shouldBe` True

    it "finds a terminator in an application of a formation whose λ it names" $ do
      term <- parseExpressionThrows "⟦ message ↦ ∅, code ↦ ∅, λ ⤍ L_throw ⟧( message ↦ ξ.x.printf( ξ.y ) )( code ↦ Φ.z )"
      thrown ["L_throw"] term `shouldBe` True

    forM_
      [ ("a formation whose λ it does not name" :: String, "⟦ message ↦ ⟦⟧, λ ⤍ L_halt ⟧" :: String)
      , ("a formation whose φ throws", "⟦ φ ↦ ⟦ message ↦ ⟦⟧, λ ⤍ L_throw ⟧ ⟧")
      , ("a dispatch from a formation whose λ it names", "⟦ message ↦ ⟦⟧, λ ⤍ L_throw ⟧.message")
      , ("an application whose argument throws", "Φ.f( x ↦ ⟦ message ↦ ⟦⟧, λ ⤍ L_throw ⟧ )")
      , ("the terminator ⊥", "⊥")
      ]
      ( \(desc, text) ->
          it ("finds no terminator in " ++ desc) $ do
            term <- parseExpressionThrows text
            thrown ["L_throw"] term `shouldBe` False
      )

  describe "message" $ do
    it "finds the message a formation that throws carries" $ do
      said <- parseExpressionThrows "Φ.pike( φ ↦ ⟦ λ ⤍ 𝜎9 ⟧ )"
      term <- parseExpressionThrows "⟦ message ↦ Φ.pike( φ ↦ ⟦ λ ⤍ 𝜎9 ⟧ ), λ ⤍ L_throw ⟧"
      message term `shouldBe` Just said

    it "finds the message an application gives a formation that throws" $ do
      said <- parseExpressionThrows "ξ.hog.printf( ξ.y )"
      term <- parseExpressionThrows "⟦ message ↦ ∅, code ↦ ∅, λ ⤍ L_throw ⟧( message ↦ ξ.hog.printf( ξ.y ) )( code ↦ Φ.z )"
      message term `shouldBe` Just said

    forM_
      [ ("the terminator ⊥" :: String, "⊥" :: String)
      , ("a formation whose message is void", "⟦ message ↦ ∅, λ ⤍ L_throw ⟧")
      , ("a formation that carries no message", "⟦ code ↦ Φ.cat, λ ⤍ L_throw ⟧")
      , ("an application that gives no message", "⟦ message ↦ ∅, code ↦ ∅, λ ⤍ L_throw ⟧( code ↦ Φ.cat )")
      ]
      ( \(desc, text) ->
          it ("finds no message in " ++ desc) $ do
            term <- parseExpressionThrows text
            message term `shouldBe` Nothing
      )

  describe "taken" $ do
    it "takes the last symbol the program already carries" $ do
      program <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎3 ⟧, b ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧"
      taken program `shouldBe` 7

    it "takes nothing from a program carrying no symbol" $ do
      program <- parseExpressionThrows "⟦ Δ ⤍ 00- ⟧"
      taken program `shouldBe` 0
