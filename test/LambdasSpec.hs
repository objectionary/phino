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

-- Every 'join' line of the entry answering the given λ name, spelled the way
-- the file spells it: the meta it binds and the two it joins.
joins :: Lambdas -> T.Text -> [(T.Text, (T.Text, T.Text))]
joins known func = map spelled (maybe [] _paired (matched known func))
  where
    spelled :: (Meta, (Meta, Meta)) -> (T.Text, (T.Text, T.Text))
    spelled (meta, (left, right)) = (_spelling meta, (_spelling left, _spelling right))

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

    it "reads the operands of 'symbolize' under expression metas" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n"
      map (_spelling . fst) (maybe [] _symbolized (matched known "L_fork")) `shouldBe` ["𝑛2"]

    -- A line of 'symbolize' stands data into unknowns, and the term it stands
    -- may already be one a line above it made, so the block reads top to
    -- bottom the way the metas number it
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

    -- A fork answers neither of its branches but the join of the two, which a
    -- 'join' line binds a meta of its own to, so the answer names that meta
    it "reads the two metas a 'join' line joins" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n"
      joins known "L_fork" `shouldBe` [("𝑛3", ("𝑛1", "𝑛2"))]

    -- The two are joined in the order the line lists them, which is the order
    -- the protocol writes the two symbols a fresh one stands for in
    it "reads the two metas of a 'join' line in the order it lists them" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  join:\n    𝑛3: [𝑛2, 𝑛1]\n  𝑛: 𝑛3\n"
      joins known "L_fork" `shouldBe` [("𝑛3", ("𝑛2", "𝑛1"))]

    -- 'join' runs after 'symbolize', so a line of it may join what that stage
    -- stood, and a line of it may join what a line above it made
    it "reads a 'join' line joining the terms a 'symbolize' line stood" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  symbolize:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  join:\n    𝑛5: [𝑛3, 𝑛4]\n  𝑛: 𝑛5\n"
      joins known "L_fork" `shouldBe` [("𝑛5", ("𝑛3", "𝑛4"))]

    it "reads a 'join' line joining what a line above it joined" $ do
      known <- lambdasOf "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n    𝑛3: $.c\n  join:\n    𝑛4: [𝑛1, 𝑛2]\n    𝑛5: [𝑛4, 𝑛3]\n  𝑛: 𝑛5\n"
      joins known "L_fork" `shouldBe` [("𝑛4", ("𝑛1", "𝑛2")), ("𝑛5", ("𝑛4", "𝑛3"))]

    it "reads an entry naming no 'join' block at all" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      joins known "L_pair" `shouldBe` []

    it "reads an entry naming neither operand block" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_spelling . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` []

    -- Everything a file may be wrong about fails where it is read, before any
    -- reduction starts, so a run never gets half-way through a derivation to
    -- discover that one of its λ functions cannot be read at all
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
      , ("a key which is no regular expression", entry "L_[pair", "is not a regular expression")
      , ("an operand of 'dataize' which is no bytes meta", "- λ: L_pair\n  dataize:\n    𝑛1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "is not a bytes meta")
      , ("an operand of 'morph' which is no expression meta", "- λ: L_pair\n  morph:\n    𝛿1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "is not an expression meta")
      , ("an operand of 'symbolize' which is no expression meta", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝛿1: 𝑛1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("an operand of 'symbolize' the entry never bound", "- λ: L_pair\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n", "names no meta")
      , ("an operand of 'symbolize' which is no meta at all", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛2: $.x\n  𝑛: 𝑛2\n", "names no meta")
      , ("an operand of 'symbolize' standing the term of a line below it", "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛2: 𝑛3\n    𝑛3: 𝑛1\n  𝑛: 𝑛2\n", "names no meta")
      , ("an operand referencing a meta the entry never matched", "- λ: L_pair\n  dataize:\n    𝛿1: '!n'\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "cannot be referenced")
      , ("an answer reading the data its operands came down to", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ Δ ⤍ 𝛿1 ⟧\n", "reads data")
      , ("an answer carrying an anonymous meta of another kind", "- λ: L_pair\n  𝑛: '⟦ φ ↦ !n ⟧'\n", "cannot be referenced")
      , ("a 'join' line joining one meta alone", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1]\n  𝑛: 𝑛2\n", "must join exactly two metas")
      , ("a 'join' line joining three metas", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n    𝑛3: $.c\n  join:\n    𝑛4: [𝑛1, 𝑛2, 𝑛3]\n  𝑛: 𝑛4\n", "must join exactly two metas")
      , ("a 'join' line joining what is no expression meta", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1, $.else]\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a 'join' line bound to what is no expression meta", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝛿1: [𝑛1, 𝑛2]\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a 'join' line joining a meta the entry never bound", "- λ: L_fork\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by a line above it")
      , ("a 'join' line joining a meta that came down to data", "- λ: L_fork\n  dataize:\n    𝛿1: $.ρ\n  morph:\n    𝑛1: $.then\n  join:\n    𝑛2: [𝑛1, 𝛿1]\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a 'join' line joining what a line below it made", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝑛3: [𝑛1, 𝑛4]\n    𝑛4: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by a line above it")
      , ("a 'symbolize' line standing what a 'join' line made", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  symbolize:\n    𝑛5: 𝑛3\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛5\n", "names no meta bound by 'morph' or by a line above it")
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

  -- A datum a term carries is a value somebody worked out, and a normal form
  -- reached from an unknown carries none, so the two compare as expressions
  -- only once the data of the one are unknowns too. Standing them is what a
  -- 'symbolize' line of an entry asks for.
  describe "symbolized" $ do
    it "stands every datum of a term into an unknown" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ Δ ⤍ 00- ⟧ )( t ↦ ⟦ Δ ⤍ FF- ⟧ ) ⟧"
      unknown <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎6 ⟧ ) ⟧"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` unknown

    -- A 𝜎 is the name of a λ function and no term, so the bytes are not bound
    -- to it: what is known is that dataizing the formation it names answers
    -- them
    it "tells the data every symbol it minted stands for" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ Δ ⤍ 00- ⟧ )( t ↦ ⟦ Δ ⤍ FF- ⟧ ) ⟧"
      let (_, known, _) = symbolized term 4
      known `shouldBe` [(5, BtOne "00"), (6, BtOne "FF")]

    it "counts every symbol it minted into the state" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ Δ ⤍ 00- ⟧ )( t ↦ ⟦ Δ ⤍ FF- ⟧ ) ⟧"
      let (_, _, spent) = symbolized term 4
      spent `shouldBe` 6

    -- A term carries the value it stands for where its φ chain ends, so a
    -- datum anywhere else is not that value: the literal of a method is the
    -- body of something nobody has called, and standing it would write an
    -- unknown nobody reads. The method comes back exactly as it was written
    -- (#1293).
    it "leaves a datum standing outside the φ chain alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧, neg ↦ ⟦ φ ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧"
      unknown <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, neg ↦ ⟦ φ ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧"
      let (masked, known, spent) = symbolized term 4
      masked `shouldBe` unknown
      known `shouldBe` [(5, BtOne "00")]
      spent `shouldBe` 5

    -- A term nobody worked a value out in is an unknown already, and standing
    -- it changes nothing
    it "leaves a term carrying no datum as it was written" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` term

    -- A literal is sugar for a datum sitting three levels down inside a
    -- formation, which is the very place a computed value keeps its unknown
    it "stands a datum standing as the argument of an application" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 00- ⟧ ) )"
      unknown <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) )"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` unknown

    -- It is the Δ binding that becomes an unknown and not the formation around
    -- it, since a datum carries a ρ of its own and so does the unknown it is
    -- put beside
    it "keeps what the formation of a datum carries besides the datum" $ do
      term <- parseExpressionThrows "⟦ Δ ⤍ 00-, ρ ↦ ⟦⟧ ⟧"
      unknown <- parseExpressionThrows "⟦ λ ⤍ 𝜎5, ρ ↦ ⟦⟧ ⟧"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` unknown

    -- A term carries the value it stands for where its φ chain ends, and a
    -- datum sitting under ρ belongs to the object around this one: a normal
    -- form drags the whole universe it was reduced inside along under ρ, so a
    -- walk reaching into it would stand the data of the whole program into
    -- unknowns to say one thing about one term
    it "leaves the data a ρ carries alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ Δ ⤍ 00- ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧"
      unknown <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧"
      let (masked, _, _) = symbolized term 4
      masked `shouldBe` unknown

    it "mints nothing for a term carrying no datum at all" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      let (_, _, spent) = symbolized term 4
      spent `shouldBe` 4

  -- Neither branch of a fork is the value the fork answers with, since nobody
  -- has picked between the two: what stands for either of them is the shape
  -- both of them have, with a fresh symbol wherever they differ
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

    -- A 𝜎 is the name of a λ function and nothing is assigned to it, so what
    -- comes back beside the term is which two symbols the fresh one stands for
    it "tells the two symbols every fresh one stands for" $ do
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧" 4
      fmap (\(_, facts, _) -> facts) made `shouldBe` Just [(5, (1, 2))]

    it "counts every symbol it minted into the state" $ do
      made <- joining "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎4 ⟧ ) ⟧" 4
      fmap (\(_, _, spent) -> spent) made `shouldBe` Just 6

    -- Two pairs are two choices and get two names of their own
    it "mints one symbol per pair of differing symbols" $ do
      made <- joining "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎4 ⟧ ) ⟧" 4
      fmap (\(_, facts, _) -> facts) made `shouldBe` Just [(5, (1, 3)), (6, (2, 4))]

    -- One pair met twice is one choice however often the two terms differ by
    -- it, so it keeps the symbol it was given the first time
    it "mints one symbol for the pair it meets twice" $ do
      term <- parseExpressionThrows "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎5 ⟧ ) ⟧"
      made <- joining "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) ⟧" "⟦ φ ↦ Φ.f( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )( t ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    -- Only the φ chain is compared, the value of a branch being where that
    -- chain ends. Two branches differing inside a method are not two values:
    -- the method is code nobody has called, the first branch's copy of it is
    -- what the answer keeps, and nothing is minted for the difference (#1293).
    it "carries a method from the first branch and mints nothing for it" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧ ⟧"
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, neg ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎8 ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    -- What the two branches are is still read off their shape: a binding one
    -- of them carries under a name the other does not is no fork at all, and
    -- carrying the first branch's bindings never papers over that
    it "refuses two branches whose bindings are named differently" $ do
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, m ↦ ⟦ x ↦ ∅ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, other ↦ ⟦ x ↦ ∅ ⟧ ⟧" 4
      made `shouldBe` Nothing

    -- Two branches nothing tells apart are the answer themselves: there is
    -- nothing to pick between and no unknown to stand for the pick
    it "joins two branches that are one term into that very term" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      made <- joining "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" 4
      made `shouldBe` Just (term, [], 4)

    it "joins two branches through the argument of an application" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )"
      made <- joining "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )" "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )" 4
      fmap (\(joint, _, _) -> joint) made `shouldBe` Just term

    -- A term carries the value it stands for where its φ chain ends, and what
    -- sits under ρ belongs to the object around this one: the two branches of
    -- a fork are reduced in scopes of their own, so their ρ differ wherever
    -- that reduction left a trace and comparing them would refuse the join
    -- over something saying nothing about either branch
    it "leaves what a ρ carries alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧"
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, ρ ↦ ⟦ y ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [(5, (1, 2))], 5)

    -- Two branches nothing but their ρ tells apart are one value, so nothing
    -- is minted for what stands under it
    it "joins two branches differing in their ρ alone" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧"
      made <- joining "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ x ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, ρ ↦ ⟦ y ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ ⟧" 4
      made `shouldBe` Just (term, [], 4)

    -- The join is strict and a datum is never joined with anything, which is
    -- why a branch carrying one goes through 'symbolized' first
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

  -- A program written by an earlier run holds symbols of its own, and a fresh
  -- one must never be spelled like one of them
  describe "taken" $ do
    it "takes the last symbol the program already carries" $ do
      program <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎3 ⟧, b ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧"
      taken program `shouldBe` 7

    it "takes nothing from a program carrying no symbol" $ do
      program <- parseExpressionThrows "⟦ Δ ⤍ 00- ⟧"
      taken program `shouldBe` 0
