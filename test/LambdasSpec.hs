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
import Functions (buildTerm)
import Lambdas (Ending (..), Lambda (..), Lambdas, Line (..), Meta (..), emptyLambdas, matched, minted, readLambdas, rewritten, stayed, symbolized, taken)
import Parser (parseExpressionThrows)
import Rule (RuleContext (..), normal)
import Test.Hspec
import Yaml qualified as Y

lambdasOf :: T.Text -> IO Lambdas
lambdasOf text = withLambdasOf text readLambdas

entry :: T.Text -> T.Text
entry key = "- λ: " <> key <> "\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"

joint :: T.Text
joint = "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿1\n    left: $.a\n    right: $.b\n"

answering :: Lambdas -> T.Text -> Maybe T.Text
answering known func = _key <$> matched known func

lined :: Lambdas -> T.Text -> [Line]
lined known func = maybe [] _lines (matched known func)

order :: Lambdas -> T.Text -> [T.Text]
order known func = map bound (lined known func)
  where
    bound :: Line -> T.Text
    bound (Morphed meta _) = _spelling meta
    bound (Evaluated meta _) = _spelling meta
    bound (Deferred meta _) = _spelling meta
    bound (Stayed meta _) = _spelling meta
    bound (Symbolized meta _) = _spelling meta
    bound (Rewritten meta _ _) = _spelling meta

morphs :: Lambdas -> T.Text -> [(T.Text, T.Text)]
morphs known func = [(_spelling meta, _spelling source) | Morphed meta source <- lined known func]

fired :: Lambdas -> T.Text -> [(T.Text, T.Text)]
fired known func = [(_spelling meta, _spelling source) | Evaluated meta source <- lined known func]

defers :: Lambdas -> T.Text -> [(T.Text, T.Text)]
defers known func = [(_spelling meta, _spelling source) | Deferred meta source <- lined known func]

stays :: Lambdas -> T.Text -> [(T.Text, T.Text)]
stays known func = [(_spelling meta, _spelling source) | Stayed meta source <- lined known func]

symbolizes :: Lambdas -> T.Text -> [(T.Text, T.Text)]
symbolizes known func = [(_spelling meta, _spelling source) | Symbolized meta source <- lined known func]

rewrites :: Lambdas -> T.Text -> [(T.Text, T.Text, Int)]
rewrites known func = [(_spelling meta, _spelling source, length rules) | Rewritten meta source rules <- lined known func]

truth :: T.Text
truth = "      rules:\n        - name: true-literal\n          pattern: Φ.true\n          result: Φ.bool( if ↦ ⟦ Δ ⤍ 01- ⟧ )\n"

rewrote :: [(String, String)] -> String -> IO Expression
rewrote rules term = do
  parsed <- mapM (\(source, target) -> (\from to -> Y.Rule "kq" Nothing Nothing from to Nothing Nothing Nothing) <$> parseExpressionThrows source <*> parseExpressionThrows target) rules
  parseExpressionThrows term >>= rewritten (RuleContext buildTerm Nothing normal) parsed

answers :: Lambdas -> T.Text -> Maybe Expression
answers known func = case _ending <$> matched known func of
  Just (Answering term) -> Just term
  _ -> Nothing

joining :: Lambdas -> T.Text -> Maybe (Either T.Text Expression)
joining known func = case _ending <$> matched known func of
  Just (Joining on _ _ _) -> Just (either (Left . _spelling) Right on)
  _ -> Nothing

sides :: Lambdas -> T.Text -> Maybe (Expression, Expression)
sides known func = case _ending <$> matched known func of
  Just (Joining _ left right _) -> Just (left, right)
  _ -> Nothing

fallback :: Lambdas -> T.Text -> Maybe Function
fallback known func = case _ending <$> matched known func of
  Just (Joining _ _ _ name) -> name
  _ -> Nothing

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

    it "reads a meta under both the name it is spelled with and the name it binds" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_name . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` ["d1"]

    it "reads the operands of 'morph' under expression metas" $ do
      known <- lambdasOf "- λ: L_pick\n  morph:\n    𝑛1: $.then\n    𝑛2: $.else\n  𝑛: 𝑛1\n"
      map (_spelling . fst) (maybe [] _morphed (matched known "L_pick")) `shouldBe` ["𝑛1", "𝑛2"]

    it "keeps under 'morph' only the operands reading '$'" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛3: 𝑛2\n  stay:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n"
      map (_spelling . fst) (maybe [] _morphed (matched known "L_hold")) `shouldBe` ["𝑛1"]

    it "reads an operand of 'morph' naming a meta as a line reading that meta" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛3: 𝑛2\n  stay:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n"
      morphs known "L_hold" `shouldBe` [("𝑛3", "𝑛2")]

    it "runs a 'morph' line after the 'rewrite' line binding the meta it reads" $ do
      known <- lambdasOf ("- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛3: 𝑛2\n  rewrite:\n    𝑛2:\n      of: 𝑛1\n" <> truth <> "  𝑛: 𝑛3\n")
      order known "L_hold" `shouldBe` ["𝑛2", "𝑛3"]

    it "runs a 'morph' line before an 'evaluate' line no meta orders" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛4: 𝑛2\n  stay:\n    𝑛2: 𝑛1\n  evaluate:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n"
      order known "L_hold" `shouldBe` ["𝑛2", "𝑛4", "𝑛3"]

    it "reads the term an operand is reduced from" $ do
      known <- lambdasOf "- λ: L_pair\n  dataize:\n    𝛿1: $.x\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      operand <- parseExpressionThrows "$.x"
      map snd (maybe [] _dataized (matched known "L_pair")) `shouldBe` [operand]

    it "reads the term the firing answers with" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )\n"
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )"
      answers known "L_pair" `shouldBe` Just term

    it "reads a 'join' block joining on a meta 'dataize' bound" $ do
      known <- lambdasOf joint
      joining known "L_fork" `shouldBe` Just (Left "𝛿1")

    it "reads a 'join' block joining on a path from '$' as one that dataizes nothing" $ do
      known <- lambdasOf "- λ: L_join\n  join:\n    on: $.on\n    left: $.left\n    right: $.right\n"
      path <- parseExpressionThrows "$.on"
      joining known "L_join" `shouldBe` Just (Right path)

    it "reads the two sides of a 'join' block as terms" $ do
      known <- lambdasOf joint
      left <- parseExpressionThrows "$.a"
      right <- parseExpressionThrows "$.b"
      sides known "L_fork" `shouldBe` Just (left, right)

    it "reads a 'join' block whose sides are metas 'morph' and 'stay' bound" $ do
      known <- lambdasOf "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  stay:\n    𝑛3: 𝑛1\n  join:\n    on: 𝛿1\n    left: 𝑛3\n    right: 𝑛2\n"
      left <- parseExpressionThrows "𝑛3"
      right <- parseExpressionThrows "𝑛2"
      sides known "L_fork" `shouldBe` Just (left, right)

    it "reads a 'join' block whose sides are metas 'symbolize' bound" $ do
      known <- lambdasOf "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  symbolize:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  join:\n    on: 𝛿1\n    left: 𝑛3\n    right: 𝑛4\n"
      left <- parseExpressionThrows "𝑛3"
      right <- parseExpressionThrows "𝑛4"
      sides known "L_fork" `shouldBe` Just (left, right)

    it "reads a 'join' block whose sides are metas 'defer' bound" $ do
      known <- lambdasOf "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  defer:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  join:\n    on: 𝛿1\n    left: 𝑛3\n    right: 𝑛4\n"
      left <- parseExpressionThrows "𝑛3"
      right <- parseExpressionThrows "𝑛4"
      sides known "L_fork" `shouldBe` Just (left, right)

    it "reads a 'join' block whose sides are metas 'evaluate' bound" $ do
      known <- lambdasOf "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  evaluate:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  join:\n    on: 𝛿1\n    left: 𝑛3\n    right: 𝑛4\n"
      left <- parseExpressionThrows "𝑛3"
      right <- parseExpressionThrows "𝑛4"
      sides known "L_fork" `shouldBe` Just (left, right)

    it "runs each 'evaluate' line of a join right after the 'stay' line it reads" $ do
      known <- lambdasOf "- λ: L_fork\n  dataize:\n    𝛿1: $.φ\n  morph:\n    𝑛1: $.left\n    𝑛2: $.right\n  stay:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  evaluate:\n    𝑛5: 𝑛3\n    𝑛6: 𝑛4\n  symbolize:\n    𝑛7: 𝑛5\n    𝑛8: 𝑛6\n  defer:\n    𝑛9: 𝑛7\n    𝑛10: 𝑛8\n  join:\n    on: 𝛿1\n    left: 𝑛9\n    right: 𝑛10\n"
      order known "L_fork" `shouldBe` ["𝑛3", "𝑛5", "𝑛4", "𝑛6", "𝑛7", "𝑛9", "𝑛8", "𝑛10"]

    it "runs each 'defer' line of a join right after the 'symbolize' line it reads" $ do
      known <- lambdasOf "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  defer:\n    𝑛7: 𝑛5\n    𝑛8: 𝑛6\n  symbolize:\n    𝑛5: 𝑛3\n    𝑛6: 𝑛4\n  stay:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  join:\n    on: 𝛿1\n    left: 𝑛7\n    right: 𝑛8\n"
      order known "L_fork" `shouldBe` ["𝑛3", "𝑛4", "𝑛5", "𝑛7", "𝑛6", "𝑛8"]

    it "reads a 'join' block with one side a path and the other a meta" $ do
      known <- lambdasOf "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.b\n  join:\n    on: 𝛿1\n    left: $.a\n    right: 𝑛1\n"
      left <- parseExpressionThrows "$.a"
      right <- parseExpressionThrows "𝑛1"
      sides known "L_fork" `shouldBe` Just (left, right)

    it "reads the λ function the 'otherwise' of a 'join' block names" $ do
      known <- lambdasOf (joint <> "    otherwise: L_join\n")
      fallback known "L_fork" `shouldBe` Just (Function "L_join")

    it "reads a 'join' block with no 'otherwise' as one fabricating nothing" $ do
      known <- lambdasOf joint
      fallback known "L_fork" `shouldBe` Nothing

    it "reads an entry with a 'join' block as one answering no term of its own" $ do
      known <- lambdasOf joint
      answers known "L_fork" `shouldBe` Nothing

    it "reads an entry with an '𝑛' key as one joining nowhere" $ do
      known <- lambdasOf (entry "L_pair")
      joining known "L_pair" `shouldBe` Nothing

    it "reads the lines of 'defer' with the metas they read" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  defer:\n    𝑛4: 𝑛2\n    𝑛3: 𝑛1\n  𝑛: 𝑛3\n"
      defers known "L_hold" `shouldBe` [("𝑛3", "𝑛1"), ("𝑛4", "𝑛2")]

    it "reads the metas the lines of 'evaluate' fire" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  evaluate:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  𝑛: 𝑛3\n"
      fired known "L_hold" `shouldBe` [("𝑛3", "𝑛1"), ("𝑛4", "𝑛2")]

    it "reads an 'evaluate' line firing what the line above it fired" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛1\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n"
      fired known "L_hold" `shouldBe` [("𝑛2", "𝑛1"), ("𝑛3", "𝑛2")]

    it "runs an 'evaluate' line before a 'defer' line no meta orders" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  defer:\n    𝑛4: 𝑛2\n  evaluate:\n    𝑛3: 𝑛1\n  𝑛: 𝑛3\n"
      order known "L_hold" `shouldBe` ["𝑛3", "𝑛4"]

    it "runs a line before the line of its own block reading what it made" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛3\n    𝑛3: 𝑛1\n  𝑛: 𝑛2\n"
      order known "L_hold" `shouldBe` ["𝑛3", "𝑛2"]

    it "runs the lines in the order of the metas they read, whatever the order of their blocks" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛3: 𝑛2\n  defer:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n"
      order known "L_hold" `shouldBe` ["𝑛2", "𝑛3"]

    it "reads an entry naming no 'evaluate' block at all" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  𝑛: 𝑛1\n"
      fired known "L_hold" `shouldBe` []

    it "reads the lines of 'stay' with the metas they read" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  stay:\n    𝑛4: 𝑛2\n    𝑛3: 𝑛1\n  𝑛: 𝑛3\n"
      stays known "L_hold" `shouldBe` [("𝑛3", "𝑛1"), ("𝑛4", "𝑛2")]

    it "runs a 'stay' line after the line binding the meta it reads" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛3: 𝑛2\n  evaluate:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n"
      order known "L_hold" `shouldBe` ["𝑛2", "𝑛3"]

    it "runs a 'defer' line before a 'stay' line no meta orders" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  stay:\n    𝑛3: 𝑛1\n  defer:\n    𝑛4: 𝑛2\n  𝑛: 𝑛3\n"
      order known "L_hold" `shouldBe` ["𝑛4", "𝑛3"]

    it "reads an entry naming no 'stay' block at all" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  𝑛: 𝑛1\n"
      stays known "L_hold" `shouldBe` []

    it "reads the lines of 'symbolize' with the metas they read" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  symbolize:\n    𝑛4: 𝑛2\n    𝑛3: 𝑛1\n  𝑛: 𝑛3\n"
      symbolizes known "L_hold" `shouldBe` [("𝑛3", "𝑛1"), ("𝑛4", "𝑛2")]

    it "runs a 'symbolize' line after the 'stay' line binding the meta it reads" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  symbolize:\n    𝑛3: 𝑛2\n  stay:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n"
      order known "L_hold" `shouldBe` ["𝑛2", "𝑛3"]

    it "runs a 'stay' line before a 'symbolize' line no meta orders" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  symbolize:\n    𝑛3: 𝑛1\n  stay:\n    𝑛4: 𝑛2\n  𝑛: 𝑛3\n"
      order known "L_hold" `shouldBe` ["𝑛4", "𝑛3"]

    it "reads an entry naming no 'symbolize' block at all" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  𝑛: 𝑛1\n"
      symbolizes known "L_hold" `shouldBe` []

    it "reads the lines of 'rewrite' with the metas they read and the count of their rules" $ do
      known <- lambdasOf ("- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  rewrite:\n    𝑛4:\n      of: 𝑛2\n" <> truth <> "        - name: false-literal\n          pattern: Φ.false\n          result: Φ.bool( if ↦ ⟦ Δ ⤍ 00- ⟧ )\n    𝑛3:\n      of: 𝑛1\n" <> truth <> "  𝑛: 𝑛3\n")
      rewrites known "L_hold" `shouldBe` [("𝑛3", "𝑛1", 1), ("𝑛4", "𝑛2", 2)]

    it "reads a line of 'rewrite' sharing its rules with another one through a YAML anchor" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  rewrite:\n    𝑛3:\n      of: 𝑛1\n      rules: &bool\n        - name: true-literal\n          pattern: Φ.true\n          result: Φ.bool( if ↦ ⟦ Δ ⤍ 01- ⟧ )\n    𝑛4:\n      of: 𝑛2\n      rules: *bool\n  𝑛: 𝑛3\n"
      rewrites known "L_hold" `shouldBe` [("𝑛3", "𝑛1", 1), ("𝑛4", "𝑛2", 1)]

    it "runs a 'rewrite' line after the 'evaluate' line binding the meta it reads" $ do
      known <- lambdasOf ("- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛3:\n      of: 𝑛2\n" <> truth <> "  evaluate:\n    𝑛2: 𝑛1\n  𝑛: 𝑛3\n")
      order known "L_hold" `shouldBe` ["𝑛2", "𝑛3"]

    it "runs a 'rewrite' line before a 'symbolize' line no meta orders" $ do
      known <- lambdasOf ("- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  symbolize:\n    𝑛3: 𝑛1\n  rewrite:\n    𝑛4:\n      of: 𝑛2\n" <> truth <> "  𝑛: 𝑛3\n")
      order known "L_hold" `shouldBe` ["𝑛4", "𝑛3"]

    it "reads a 'join' block whose sides are metas 'rewrite' bound" $ do
      known <- lambdasOf ("- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  rewrite:\n    𝑛3:\n      of: 𝑛1\n" <> truth <> "    𝑛4:\n      of: 𝑛2\n" <> truth <> "  join:\n    on: 𝛿1\n    left: 𝑛3\n    right: 𝑛4\n")
      left <- parseExpressionThrows "𝑛3"
      right <- parseExpressionThrows "𝑛4"
      sides known "L_fork" `shouldBe` Just (left, right)

    it "reads an entry naming no 'rewrite' block at all" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  𝑛: 𝑛1\n"
      rewrites known "L_hold" `shouldBe` []

    it "reads an entry naming no 'defer' block at all" $ do
      known <- lambdasOf "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  𝑛: 𝑛1\n"
      defers known "L_hold" `shouldBe` []

    it "reads an entry naming neither operand block" $ do
      known <- lambdasOf "- λ: L_pair\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n"
      map (_spelling . fst) (maybe [] _dataized (matched known "L_pair")) `shouldBe` []

    forM_
      [ ("a file which is no list of entries" :: String, "λ: L_pair\n" :: T.Text, "cannot be read" :: String)
      , ("an entry with no λ key", "- 𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "no 'λ' key")
      , ("an entry with no answer at all", "- λ: L_pair\n", "The entry of λ function 'L_pair' has neither an '𝑛' key nor a 'join' block")
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
      , ("an operand referencing a meta the entry never matched", "- λ: L_pair\n  dataize:\n    𝛿1: '!n'\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "cannot be referenced")
      , ("an answer reading the data its operands came down to", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: ⟦ Δ ⤍ 𝛿1 ⟧\n", "reads data")
      , ("an answer writing a numbered symbol", "- λ: L_pair\n  𝑛: '⟦ a ↦ ⟦ λ ⤍ 𝜎1 ⟧, b ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧'\n", "writes the numbered symbol '𝜎1'")
      , ("an answer carrying an anonymous meta of another kind", "- λ: L_pair\n  𝑛: '⟦ φ ↦ !n ⟧'\n", "cannot be referenced")
      , ("an 'evaluate' line firing a meta the entry never bound", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by any line")
      , ("an 'evaluate' line bound to what is no expression meta", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝛿1: 𝑛1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("an 'evaluate' line firing a meta that came down to data", "- λ: L_hold\n  dataize:\n    𝛿1: $.ρ\n  evaluate:\n    𝑛1: 𝛿1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("an 'evaluate' line firing a term instead of a meta", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: $.a\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a meta bound by 'morph' and again by 'evaluate'", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛1: 𝑛1\n  𝑛: 𝑛1\n", "The meta '𝑛1' of λ function 'L_hold' is bound by more than one line")
      , ("a line reading the meta it binds", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛2\n  𝑛: 𝑛2\n", "The line binding '𝑛2' of λ function 'L_hold' reads the meta it binds")
      , ("two lines reading what each other made", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛2: 𝑛3\n  defer:\n    𝑛3: 𝑛2\n  𝑛: 𝑛2\n", "The line binding '𝑛2' of λ function 'L_hold' reads the meta it binds")
      , ("a line reading what two lines reading each other made, blaming one of the two", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛5: 𝑛3\n  defer:\n    𝑛3: 𝑛4\n    𝑛4: 𝑛3\n  𝑛: 𝑛5\n", "The line binding '𝑛3' of λ function 'L_hold' reads the meta it binds")
      , ("an answer naming a meta no block binds", "- λ: L_pair\n  dataize:\n    𝛿1: $.ρ\n  𝑛: 𝑛7\n", "reads the meta 'n7' that no block binds")
      , ("an operand of 'dataize' reading a meta", "- λ: L_pair\n  dataize:\n    𝛿1: 𝑛5\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "of 'dataize' of λ function 'L_pair' reads the meta 'n5'")
      , ("an operand of 'morph' reading a meta", "- λ: L_pair\n  morph:\n    𝑛1: $.x.𝜏1\n  𝑛: 𝑛1\n", "of 'morph' of λ function 'L_pair' reads the meta 't1'")
      , ("an operand of 'morph' reading a meta inside a term", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: 𝑛1.x\n  𝑛: 𝑛2\n", "The operand '𝑛2' of 'morph' of λ function 'L_hold' reads the meta 'n1', while only a path from '$' or a meta alone can be reduced there")
      , ("an operand of 'morph' naming a meta nothing binds", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: 𝑛5\n  𝑛: 𝑛2\n", "The operand '𝑛5' of λ function 'L_hold' names no meta bound by 'morph' or by any line")
      , ("an operand of 'morph' which is no term of the calculus", "- λ: L_hold\n  morph:\n    𝑛1: ⟦ λ ⤍\n  𝑛: 𝑛1\n", "Error in $[0].morph.𝑛1: expression:1:6")
      , ("an operand of 'morph' naming the meta it binds", "- λ: L_hold\n  morph:\n    𝑛1: 𝑛1\n  𝑛: 𝑛1\n", "The line binding '𝑛1' of λ function 'L_hold' reads the meta it binds")
      , ("a 'defer' line deferring a meta the entry never bound", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  defer:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by any line")
      , ("a 'defer' line bound to what is no expression meta", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  defer:\n    𝛿1: 𝑛1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a meta bound by 'defer' and again by 'evaluate'", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  defer:\n    𝑛2: 𝑛1\n  evaluate:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n", "The meta '𝑛2' of λ function 'L_hold' is bound by more than one line")
      , ("a meta bound by 'evaluate' and again by 'defer' in its other spelling", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  evaluate:\n    𝑛3: 𝑛1\n  defer:\n    \"!n3\": 𝑛1\n  𝑛: 𝑛3\n", "The meta '!n3' of λ function 'L_hold' is bound by more than one line")
      , ("a meta bound twice by 'defer' in its two spellings", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  defer:\n    𝑛3: 𝑛1\n    \"!n3\": 𝑛2\n  𝑛: 𝑛3\n", "is bound by more than one line")
      , ("a 'stay' line keeping a meta the entry never bound", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by any line")
      , ("a 'stay' line bound to what is no expression meta", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  stay:\n    𝛿1: 𝑛1\n  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a 'stay' line keeping a term instead of a meta", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛2: $.a\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a 'symbolize' line reading a meta the entry never bound", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  symbolize:\n    𝑛3: 𝑛2\n  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by any line")
      , ("a 'symbolize' line reading a term instead of a meta", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  symbolize:\n    𝑛2: $.a\n  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a meta bound by 'stay' and again by 'symbolize'", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛2: 𝑛1\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n", "The meta '𝑛2' of λ function 'L_hold' is bound by more than one line")
      , ("a 'rewrite' line reading a meta the entry never bound", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛3:\n      of: 𝑛2\n" <> truth <> "  𝑛: 𝑛3\n", "names no meta bound by 'morph' or by any line")
      , ("a 'rewrite' line bound to what is no expression meta", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝛿1:\n      of: 𝑛1\n" <> truth <> "  𝑛: 𝑛1\n", "is not an expression meta")
      , ("a 'rewrite' line reading a term instead of a meta", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2:\n      of: $.a\n" <> truth <> "  𝑛: 𝑛2\n", "is not an expression meta")
      , ("a 'rewrite' line naming no meta it reads", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2:\n" <> truth <> "  𝑛: 𝑛2\n", "The line '𝑛2' of the 'rewrite' block of λ function 'L_hold' has no 'of' key")
      , ("a 'rewrite' line holding no rules", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2:\n      of: 𝑛1\n  𝑛: 𝑛2\n", "The line '𝑛2' of the 'rewrite' block of λ function 'L_hold' has no 'rules' key")
      , ("a 'rewrite' line with a key it does not read", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2:\n      of: 𝑛1\n      once: true\n" <> truth <> "  𝑛: 𝑛2\n", "The key 'once' of the line '𝑛2' of the 'rewrite' block of λ function 'L_hold' is none of its keys: of, rules")
      , ("a 'rewrite' line naming its meta alone, as a 'stay' line does", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n", "rewrite.𝑛2: parsing KeyMap failed, expected Object, but encountered String")
      , ("a rule of 'rewrite' minting a fresh symbol", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: minting\n          pattern: Φ.true\n          result: ⟦ λ ⤍ 𝜎 ⟧\n  𝑛: 𝑛2\n", "anonymous meta '!S' cannot be referenced in 'result' of rule 'minting'")
      , ("a rule of 'rewrite' writing a symbol nobody minted", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: naming\n          pattern: Φ.true\n          result: ⟦ λ ⤍ 𝜎4 ⟧\n  𝑛: 𝑛2\n", "The rule 'naming' of the 'rewrite' block of λ function 'L_hold' writes the symbol '𝜎4' into its result, while only a firing mints a symbol")
      , ("a rule of 'rewrite' reading a meta it never binds", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: unbound\n          pattern: ⟦ φ ↦ 𝑒1 ⟧\n          result: ⟦ φ ↦ 𝑒2 ⟧\n  𝑛: 𝑛2\n", "The rule 'unbound' reads the meta 'e2' it never binds")
      , ("a rule of 'rewrite' with no result", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  rewrite:\n    𝑛2:\n      of: 𝑛1\n      rules:\n        - name: empty\n          pattern: Φ.true\n  𝑛: 𝑛2\n", "key \"result\" not found")
      , ("a meta bound by 'morph' and again by 'stay'", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  stay:\n    𝑛1: 𝑛1\n  𝑛: 𝑛1\n", "The meta '𝑛1' of λ function 'L_hold' is bound by more than one line")
      , ("a meta bound by 'morph' and again by 'defer'", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  defer:\n    𝑛1: 𝑛1\n  𝑛: 𝑛1\n", "The meta '𝑛1' of λ function 'L_hold' is bound by more than one line")
      , ("an entry with both an '𝑛' key and a 'join' block", joint <> "  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "The entry of λ function 'L_fork' has both an '𝑛' key and a 'join' block, while a join answers the term it joins")
      , ("a 'join' block joining on an expression meta", "- λ: L_fork\n  join:\n    on: 𝑛1\n    left: $.a\n    right: $.b\n", "The 'on' of the 'join' block of λ function 'L_fork' is '𝑛1', while it must name a bytes meta of 'dataize', such as '𝛿1', or be a path from '$', such as '$.on'")
      , ("a 'join' block joining on a bytes meta 'dataize' never bound", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿2\n    left: $.a\n    right: $.b\n", "The 'on' of the 'join' block of λ function 'L_fork' is '𝛿2', while it must name a bytes meta of 'dataize'")
      , ("a 'join' block joining on a path off 'Φ'", "- λ: L_fork\n  join:\n    on: Φ.c\n    left: $.a\n    right: $.b\n", "The 'on' of the 'join' block of λ function 'L_fork' is 'Φ.c'")
      , ("a 'join' block joining on an application", "- λ: L_fork\n  join:\n    on: $.c(x ↦ $.d)\n    left: $.a\n    right: $.b\n", "The 'on' of the 'join' block of λ function 'L_fork' is '$.c(x ↦ $.d)'")
      , ("a 'join' block with no 'on' key", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    left: $.a\n    right: $.b\n", "The 'join' block of λ function 'L_fork' has no 'on' key")
      , ("a 'join' block with no 'left' key", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿1\n    right: $.b\n", "The 'join' block of λ function 'L_fork' has no 'left' key")
      , ("a 'join' block with no 'right' key", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿1\n    left: $.a\n", "The 'join' block of λ function 'L_fork' has no 'right' key")
      , ("a 'join' block with a key it does not read", joint <> "    middle: $.m\n", "The key 'middle' of the 'join' block of λ function 'L_fork' is none of its keys: on, left, right, otherwise")
      , ("a 'join' block whose 'otherwise' is no λ name", joint <> "    otherwise: l_join\n", "The 'otherwise' of the 'join' block of λ function 'L_fork' is 'l_join', while it must name a λ function, such as 'L_join'")
      , ("a 'join' block whose 'otherwise' names a symbol", joint <> "    otherwise: 𝜎3\n", "The 'otherwise' of the 'join' block of λ function 'L_fork' is '𝜎3', while it must name a λ function, such as 'L_join'")
      , ("a 'join' block which is no mapping", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join: [𝛿1, $.a, $.b]\n", "join: parsing")
      , ("a side of a 'join' block naming a meta nothing binds", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿1\n    left: 𝑛1\n    right: $.b\n", "The side 'left' of the 'join' block of λ function 'L_fork' reads the meta 'n1' that none of 'morph', 'evaluate', 'stay', 'rewrite', 'symbolize' and 'defer' binds")
      , ("a side of a 'join' block reading a meta inside a term", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n  join:\n    on: 𝛿1\n    left: 𝑛1.x\n    right: $.b\n", "The side 'left' of the 'join' block of λ function 'L_fork' reads the meta 'n1' inside a term, while a side is a path from '$' or a meta alone")
      , ("a side of a 'join' block which is no term of the calculus", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿1\n    left: ⟦ λ ⤍\n    right: $.b\n", "cannot be read")
      , ("a 'fork' block, which is gone", "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  fork:\n    on: 𝛿1\n    left: $.a\n    right: $.b\n", "The key 'fork' of λ function 'L_fork' is none of the keys an entry reads")
      , ("an 'unfold' block, which is gone", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  unfold:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n", "The key 'unfold' of λ function 'L_fork' is none of the keys an entry reads")
      , ("an 'unbox' block, which is gone", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  unbox:\n    𝑛2: 𝑛1\n  𝑛: 𝑛2\n", "The key 'unbox' of λ function 'L_fork' is none of the keys an entry reads")
      , ("an 'upcast' block, which is gone", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  upcast:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  𝑛: 𝑛3\n", "The key 'upcast' of λ function 'L_fork' is none of the keys an entry reads")
      , ("a 'terminators' key, which is gone", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  terminators: [L_throw]\n  𝑛: 𝑛1\n", "The key 'terminators' of λ function 'L_fork' is none of the keys an entry reads")
      , ("a 'lenient' key, which is gone", "- λ: L_fork\n  morph:\n    𝑛1: $.a\n  lenient: []\n  𝑛: 𝑛1\n", "The key 'lenient' of λ function 'L_fork' is none of the keys an entry reads")
      , ("a misspelled 'dataize' block", "- λ: L_pair\n  datize:\n    𝛿1: $.ρ\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n", "The key 'datize' of λ function 'L_pair' is none of the keys an entry reads")
      , ("a 'defer' key saying a flag instead of holding lines", "- λ: L_hold\n  morph:\n    𝑛1: $.a\n  defer: true\n  𝑛: 𝑛1\n", "defer: parsing")
      ]
      ( \(desc, text, reason) ->
          it ("cannot read " ++ desc) $
            lambdasOf text `shouldThrow` (\err -> reason `isInfixOf` show (err :: SomeException))
      )

  describe "stayed" $ do
    it "drops the ρ a dispatch is applied to" $ do
      term <- parseExpressionThrows "Φ.bytes( ρ ↦ ⟦ x ↦ ∅ ⟧ )"
      kept <- parseExpressionThrows "Φ.bytes"
      stayed term `shouldBe` kept

    it "drops the ρ a dispatch is applied to after an argument binding no ρ" $ do
      term <- parseExpressionThrows "Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( ρ ↦ ⟦ x ↦ ∅ ⟧ )"
      kept <- parseExpressionThrows "Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )"
      stayed term `shouldBe` kept

    it "drops every ρ a chain of applications gives a dispatch" $ do
      term <- parseExpressionThrows "ξ.f( ρ ↦ ⟦ a ↦ ∅ ⟧ )( α0 ↦ ⟦⟧ )( ρ ↦ ⟦ b ↦ ∅ ⟧ )"
      kept <- parseExpressionThrows "ξ.f( α0 ↦ ⟦⟧ )"
      stayed term `shouldBe` kept

    it "keeps the ρ a formation is applied to" $ do
      term <- parseExpressionThrows "⟦ ρ ↦ ∅, t ↦ ∅ ⟧( t ↦ ⟦⟧ )( ρ ↦ ⟦ x ↦ ∅ ⟧ )"
      kept <- parseExpressionThrows "⟦ ρ ↦ ∅, t ↦ ∅ ⟧( t ↦ ⟦⟧ )( ρ ↦ ⟦ x ↦ ∅ ⟧ )"
      stayed term `shouldBe` kept

    it "drops a ρ standing deep inside the argument of an application" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )( ρ ↦ ⟦ x ↦ ∅ ⟧ ) )"
      kept <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) )"
      stayed term `shouldBe` kept

    it "drops a ρ standing in a method of a formation and not only in its φ" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦⟧, m ↦ ⟦ φ ↦ Φ.f( ρ ↦ ⟦ x ↦ ∅ ⟧ ) ⟧ ⟧"
      kept <- parseExpressionThrows "⟦ φ ↦ ⟦⟧, m ↦ ⟦ φ ↦ Φ.f ⟧ ⟧"
      stayed term `shouldBe` kept

    it "drops a ρ standing inside a ρ the term keeps" $ do
      term <- parseExpressionThrows "⟦ x ↦ ∅ ⟧( ρ ↦ Φ.f( ρ ↦ ⟦ y ↦ ∅ ⟧ ) )"
      kept <- parseExpressionThrows "⟦ x ↦ ∅ ⟧( ρ ↦ Φ.f )"
      stayed term `shouldBe` kept

    it "keeps a term carrying no ρ at all" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) )"
      kept <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) )"
      stayed term `shouldBe` kept

  describe "rewritten" $ do
    it "rewrites the term a rule matches as a whole" $ do
      made <- rewrote [("Φ.true", "Φ.bool( if ↦ ⟦ Δ ⤍ 01- ⟧ )")] "Φ.true"
      expected <- parseExpressionThrows "Φ.bool( if ↦ ⟦ Δ ⤍ 01- ⟧ )"
      made `shouldBe` expected

    it "rewrites a term a rule matches inside the argument of an application" $ do
      made <- rewrote [("Φ.true", "Φ.bool( if ↦ ⟦ Δ ⤍ 01- ⟧ )")] "Φ.jq( α0 ↦ Φ.true )"
      expected <- parseExpressionThrows "Φ.jq( α0 ↦ Φ.bool( if ↦ ⟦ Δ ⤍ 01- ⟧ ) )"
      made `shouldBe` expected

    it "rewrites every place a rule matches inside a formation" $ do
      made <- rewrote [("Φ.true", "Φ.vw")] "⟦ kq ↦ Φ.true, φ ↦ ⟦ zt ↦ Φ.true ⟧ ⟧"
      expected <- parseExpressionThrows "⟦ kq ↦ Φ.vw, φ ↦ ⟦ zt ↦ Φ.vw ⟧ ⟧"
      made `shouldBe` expected

    it "binds the metas of the pattern in the result" $ do
      made <- rewrote [("Φ.flag( α0 ↦ 𝑒1 )", "Φ.bool( if ↦ 𝑒1 )")] "Φ.flag( α0 ↦ ⟦ λ ⤍ 𝜎6 ⟧ )"
      expected <- parseExpressionThrows "Φ.bool( if ↦ ⟦ λ ⤍ 𝜎6 ⟧ )"
      made `shouldBe` expected

    it "takes the first rule matching a term" $ do
      made <- rewrote [("Φ.true", "Φ.yq"), ("Φ.𝜏1", "Φ.nx")] "Φ.true"
      expected <- parseExpressionThrows "Φ.yq"
      made `shouldBe` expected

    it "tries a later rule where an earlier one does not match" $ do
      made <- rewrote [("Φ.false", "Φ.yq"), ("Φ.true", "Φ.nx")] "Φ.true"
      expected <- parseExpressionThrows "Φ.nx"
      made `shouldBe` expected

    it "rewrites the outermost term a rule matches and nothing inside what it made" $ do
      made <- rewrote [("⟦ φ ↦ 𝑒1 ⟧", "𝑒1")] "⟦ φ ↦ ⟦ φ ↦ ξ.ρ.kq ⟧ ⟧"
      expected <- parseExpressionThrows "⟦ φ ↦ ξ.ρ.kq ⟧"
      made `shouldBe` expected

    it "leaves what a rule made as it is, not normalized" $ do
      made <- rewrote [("Φ.true", "⟦ hv ↦ ⟦⟧ ⟧.hv")] "Φ.true"
      expected <- parseExpressionThrows "⟦ hv ↦ ⟦⟧ ⟧.hv"
      made `shouldBe` expected

    it "keeps a term no rule matches" $ do
      term <- parseExpressionThrows "Φ.false( α0 ↦ ⟦ Δ ⤍ 2E- ⟧ )"
      made <- rewrote [("Φ.true", "Φ.vw")] "Φ.false( α0 ↦ ⟦ Δ ⤍ 2E- ⟧ )"
      made `shouldBe` term

  describe "symbolized" $ do
    it "turns the datum of a formation into a fresh symbol" $ do
      term <- parseExpressionThrows "⟦ Δ ⤍ 4A-2C ⟧"
      masked <- parseExpressionThrows "⟦ λ ⤍ 𝜎8 ⟧"
      (\(made, _, _) -> made) (symbolized term 7) `shouldBe` masked

    it "tells the datum every fresh symbol stands for" $ do
      term <- parseExpressionThrows "⟦ Δ ⤍ 4A-2C ⟧"
      (\(_, known, _) -> known) (symbolized term 7) `shouldBe` [(8, BtMany ["4A", "2C"])]

    it "counts every symbol it minted" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ ⟦ Δ ⤍ 0E- ⟧ )"
      (\(_, _, spent) -> spent) (symbolized term 11) `shouldBe` 12

    it "walks the φ an application binds down to the datum" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 3F-F0-00-00-00-00-00-00 ⟧ ) )"
      masked <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ ) )"
      (\(made, _, _) -> made) (symbolized term 2) `shouldBe` masked

    it "walks the head of an application binding no φ" $ do
      term <- parseExpressionThrows "Φ.bytes( φ ↦ ⟦ Δ ⤍ 61- ⟧ )( ρ ↦ ⟦ w ↦ ∅ ⟧ )"
      masked <- parseExpressionThrows "Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎20 ⟧ )( ρ ↦ ⟦ w ↦ ∅ ⟧ )"
      (\(made, _, _) -> made) (symbolized term 19) `shouldBe` masked

    it "walks the φ of a formation down to the datum" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 07- ⟧ ⟧ ⟧"
      masked <- parseExpressionThrows "⟦ φ ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ ⟧ ⟧"
      (\(made, _, _) -> made) (symbolized term 4) `shouldBe` masked

    it "turns the datum a ρ of a formation holds into a fresh symbol" $ do
      term <- parseExpressionThrows "⟦ ρ ↦ ⟦ Δ ⤍ 02- ⟧, q ↦ ∅ ⟧"
      masked <- parseExpressionThrows "⟦ ρ ↦ ⟦ λ ⤍ 𝜎10 ⟧, q ↦ ∅ ⟧"
      (\(made, _, _) -> made) (symbolized term 9) `shouldBe` masked

    it "turns the datum a method of a formation holds into a fresh symbol" $ do
      term <- parseExpressionThrows "⟦ φ ↦ ⟦⟧, neg ↦ ⟦ Δ ⤍ BF-F0-00-00-00-00-00-00 ⟧ ⟧"
      masked <- parseExpressionThrows "⟦ φ ↦ ⟦⟧, neg ↦ ⟦ λ ⤍ 𝜎10 ⟧ ⟧"
      (\(made, _, _) -> made) (symbolized term 9) `shouldBe` masked

    it "turns the datum a named argument other than φ holds into a fresh symbol" $ do
      term <- parseExpressionThrows "Φ.f( q ↦ ⟦ Δ ⤍ 1D- ⟧ )"
      masked <- parseExpressionThrows "Φ.f( q ↦ ⟦ λ ⤍ 𝜎4 ⟧ )"
      (\(made, _, _) -> made) (symbolized term 3) `shouldBe` masked

    it "turns the datum a positional argument holds into a fresh symbol" $ do
      term <- parseExpressionThrows "Φ.vk( α0 ↦ ⟦ Δ ⤍ 5B- ⟧ )"
      masked <- parseExpressionThrows "Φ.vk( α0 ↦ ⟦ λ ⤍ 𝜎14 ⟧ )"
      (\(made, _, _) -> made) (symbolized term 13) `shouldBe` masked

    it "walks the target of a dispatch down to the datum" $ do
      term <- parseExpressionThrows "⟦ Δ ⤍ 6E- ⟧.kx"
      masked <- parseExpressionThrows "⟦ λ ⤍ 𝜎8 ⟧.kx"
      (\(made, _, _) -> made) (symbolized term 7) `shouldBe` masked

    it "walks a formation bound to an argument down to the datum it holds in φ" $ do
      term <- parseExpressionThrows "Φ.bool( if ↦ ⟦ λ ⤍ L_fork, φ ↦ ⟦ Δ ⤍ FF- ⟧ ⟧ )"
      masked <- parseExpressionThrows "Φ.bool( if ↦ ⟦ λ ⤍ L_fork, φ ↦ ⟦ λ ⤍ 𝜎147 ⟧ ⟧ )"
      (\(made, _, _) -> made) (symbolized term 146) `shouldBe` masked

    it "mints the symbols in the order the term spells its data" $ do
      term <- parseExpressionThrows "Φ.ws( φ ↦ ⟦ Δ ⤍ 3C- ⟧ )( tz ↦ ⟦ Δ ⤍ 9D- ⟧ )"
      (\(_, known, _) -> known) (symbolized term 5) `shouldBe` [(6, BtOne "3C"), (7, BtOne "9D")]

    it "mints nothing for a term holding no datum" $ do
      term <- parseExpressionThrows "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) )"
      (\(_, _, spent) -> spent) (symbolized term 6) `shouldBe` 6

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

  describe "taken" $ do
    it "takes the last symbol the program already carries" $ do
      program <- parseExpressionThrows "⟦ a ↦ ⟦ λ ⤍ 𝜎3 ⟧, b ↦ ⟦ λ ⤍ 𝜎7 ⟧ ⟧"
      taken program `shouldBe` 7

    it "takes nothing from a program carrying no symbol" $ do
      program <- parseExpressionThrows "⟦ Δ ⤍ 00- ⟧"
      taken program `shouldBe` 0
