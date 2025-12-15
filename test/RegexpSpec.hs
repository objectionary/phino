-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Regexp module that provides regular expression
matching and replacement using PCRE.
-}
module RegexpSpec where

import Data.ByteString.Char8 qualified as B
import Regexp qualified as R
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldReturn, shouldThrow)

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles a valid pattern" $ do
      _ <- R.compile (B.pack "foo")
      matched <- R.match (B.pack "foo") (B.pack "foobar")
      matched `shouldBe` True

    it "throws on invalid pattern" $
      R.compile (B.pack "[invalid") `shouldThrow` anyException

    it "compiles pattern with groups" $ do
      _ <- R.compile (B.pack "(a)(b)(c)")
      matched <- R.match (B.pack "(a)(b)(c)") (B.pack "abc")
      matched `shouldBe` True

    it "compiles pattern with unicode" $ do
      _ <- R.compile (B.pack "кирилиця")
      matched <- R.match (B.pack "кирилиця") (B.pack "текст кирилиця тут")
      matched `shouldBe` True

    it "compiles empty pattern" $ do
      _ <- R.compile B.empty
      matched <- R.match B.empty (B.pack "anything")
      matched `shouldBe` True

  describe "match" $ do
    it "returns true when pattern matches" $
      R.match (B.pack "hello") (B.pack "hello world") `shouldReturn` True

    it "returns false when pattern does not match" $
      R.match (B.pack "goodbye") (B.pack "hello world") `shouldReturn` False

    it "returns true for partial match" $
      R.match (B.pack "wor") (B.pack "hello world") `shouldReturn` True

    it "returns true for match at start" $
      R.match (B.pack "^hello") (B.pack "hello world") `shouldReturn` True

    it "returns false for anchored pattern not at start" $
      R.match (B.pack "^world") (B.pack "hello world") `shouldReturn` False

    it "returns true for match at end" $
      R.match (B.pack "world$") (B.pack "hello world") `shouldReturn` True

    it "returns true with empty input and empty pattern" $
      R.match B.empty B.empty `shouldReturn` True

    it "returns true with non-empty input and empty pattern" $
      R.match B.empty (B.pack "text") `shouldReturn` True

    it "returns false with empty input and non-empty pattern" $
      R.match (B.pack "text") B.empty `shouldReturn` False

    it "handles special regex characters" $
      R.match (B.pack "a\\.b") (B.pack "a.b") `shouldReturn` True

    it "handles character class" $
      R.match (B.pack "[0-9]+") (B.pack "abc123def") `shouldReturn` True

    it "handles alternation" $
      R.match (B.pack "cat|dog") (B.pack "I have a dog") `shouldReturn` True

    it "handles unicode input" $
      R.match (B.pack "日本語") (B.pack "これは日本語です") `shouldReturn` True

    it "handles case sensitive match" $
      R.match (B.pack "Hello") (B.pack "hello") `shouldReturn` False

  describe "extractGroups" $ do
    it "extracts groups from pattern with capturing groups" $ do
      regex <- R.compile (B.pack "(\\w+)@(\\w+)")
      groups <- R.extractGroups regex (B.pack "user@domain")
      groups `shouldBe` [B.pack "user@domain", B.pack "user", B.pack "domain"]

    it "returns empty list when no match" $ do
      regex <- R.compile (B.pack "(foo)")
      groups <- R.extractGroups regex (B.pack "bar")
      groups `shouldBe` []

    it "extracts nested groups" $ do
      regex <- R.compile (B.pack "((a)(b))")
      groups <- R.extractGroups regex (B.pack "ab")
      groups `shouldBe` [B.pack "ab", B.pack "ab", B.pack "a", B.pack "b"]

    it "handles optional group that did not match" $ do
      regex <- R.compile (B.pack "(a)(b)?")
      groups <- R.extractGroups regex (B.pack "a")
      length groups `shouldBe` 3

    it "extracts multiple groups" $ do
      regex <- R.compile (B.pack "(x)(y)(z)")
      groups <- R.extractGroups regex (B.pack "prefix xyz suffix")
      groups `shouldBe` [B.pack "xyz", B.pack "x", B.pack "y", B.pack "z"]

    it "handles pattern without groups" $ do
      regex <- R.compile (B.pack "test")
      groups <- R.extractGroups regex (B.pack "this is a test")
      groups `shouldBe` [B.pack "test"]

  describe "substituteGroups" $ do
    it "substitutes group zero" $
      R.substituteGroups (B.pack "[$0]") [B.pack "match"] `shouldBe` B.pack "[match]"

    it "substitutes multiple groups" $
      R.substituteGroups (B.pack "$1-$2") [B.pack "full", B.pack "a", B.pack "b"]
        `shouldBe` B.pack "a-b"

    it "keeps dollar sign when no digits follow" $
      R.substituteGroups (B.pack "$ test") [B.pack "x"] `shouldBe` B.pack "$ test"

    it "keeps original reference for out of bounds index" $
      R.substituteGroups (B.pack "$9") [B.pack "only"] `shouldBe` B.pack "$9"

    it "handles replacement without group references" $
      R.substituteGroups (B.pack "plain") [B.pack "x"] `shouldBe` B.pack "plain"

    it "handles empty replacement" $
      R.substituteGroups B.empty [B.pack "x"] `shouldBe` B.empty

    it "handles empty groups list with reference" $
      R.substituteGroups (B.pack "$0") [] `shouldBe` B.pack "$0"

    it "handles multi-digit group reference" $
      R.substituteGroups (B.pack "$12") (replicate 13 (B.pack "x"))
        `shouldBe` B.pack "x"

    it "handles consecutive group references" $
      R.substituteGroups (B.pack "$0$1$2") [B.pack "a", B.pack "b", B.pack "c"]
        `shouldBe` B.pack "abc"

    it "handles unicode in replacement" $
      R.substituteGroups (B.pack "結果: $1") [B.pack "all", B.pack "データ"]
        `shouldBe` B.pack "結果: データ"

    it "handles dollar at end of string" $
      R.substituteGroups (B.pack "test$") [B.pack "x"] `shouldBe` B.pack "test$"

    it "handles double dollar" $
      R.substituteGroups (B.pack "$$1") [B.pack "x", B.pack "y"]
        `shouldBe` B.pack "$y"

  describe "replaceFirst" $ do
    it "replaces first occurrence" $ do
      regex <- R.compile (B.pack "cat")
      result <- R.replaceFirst regex (B.pack "dog") (B.pack "cat and cat")
      result `shouldBe` B.pack "dog and cat"

    it "returns input when no match" $ do
      regex <- R.compile (B.pack "xyz")
      result <- R.replaceFirst regex (B.pack "abc") (B.pack "hello world")
      result `shouldBe` B.pack "hello world"

    it "replaces with empty string" $ do
      regex <- R.compile (B.pack "remove")
      result <- R.replaceFirst regex B.empty (B.pack "please remove this")
      result `shouldBe` B.pack "please  this"

    it "replaces at start of string" $ do
      regex <- R.compile (B.pack "^start")
      result <- R.replaceFirst regex (B.pack "begin") (B.pack "start here")
      result `shouldBe` B.pack "begin here"

    it "replaces at end of string" $ do
      regex <- R.compile (B.pack "end$")
      result <- R.replaceFirst regex (B.pack "finish") (B.pack "the end")
      result `shouldBe` B.pack "the finish"

    it "uses captured groups in replacement" $ do
      regex <- R.compile (B.pack "(\\w+)@(\\w+)")
      result <- R.replaceFirst regex (B.pack "[$1 AT $2]") (B.pack "email: test@example here")
      result `shouldBe` B.pack "email: [test AT example] here"

    it "handles unicode pattern and replacement" $ do
      regex <- R.compile (B.pack "古い")
      result <- R.replaceFirst regex (B.pack "新しい") (B.pack "これは古いです")
      result `shouldBe` B.pack "これは新しいです"

    it "handles empty input" $ do
      regex <- R.compile (B.pack "x")
      result <- R.replaceFirst regex (B.pack "y") B.empty
      result `shouldBe` B.empty

    it "replaces entire string when pattern matches all" $ do
      regex <- R.compile (B.pack "^.*$")
      result <- R.replaceFirst regex (B.pack "replaced") (B.pack "original")
      result `shouldBe` B.pack "replaced"

  describe "replaceAll" $ do
    it "replaces all occurrences" $ do
      regex <- R.compile (B.pack "a")
      result <- R.replaceAll regex (B.pack "X") (B.pack "banana")
      result `shouldBe` B.pack "bXnXnX"

    it "returns input when no match" $ do
      regex <- R.compile (B.pack "xyz")
      result <- R.replaceAll regex (B.pack "abc") (B.pack "hello world")
      result `shouldBe` B.pack "hello world"

    it "replaces consecutive matches" $ do
      regex <- R.compile (B.pack "o")
      result <- R.replaceAll regex (B.pack "0") (B.pack "oooo")
      result `shouldBe` B.pack "0000"

    it "replaces with captured groups" $ do
      regex <- R.compile (B.pack "(\\d+)")
      result <- R.replaceAll regex (B.pack "[$1]") (B.pack "a1b2c3")
      result `shouldBe` B.pack "a[1]b[2]c[3]"

    it "handles empty replacement" $ do
      regex <- R.compile (B.pack "x")
      result <- R.replaceAll regex B.empty (B.pack "axbxcx")
      result `shouldBe` B.pack "abc"

    it "handles empty input" $ do
      regex <- R.compile (B.pack "x")
      result <- R.replaceAll regex (B.pack "y") B.empty
      result `shouldBe` B.empty

    it "handles unicode input and pattern" $ do
      regex <- R.compile (B.pack "кіт")
      result <- R.replaceAll regex (B.pack "пес") (B.pack "кіт і кіт")
      result `shouldBe` B.pack "пес і пес"

    it "replaces overlapping potential matches correctly" $ do
      regex <- R.compile (B.pack "aa")
      result <- R.replaceAll regex (B.pack "X") (B.pack "aaaa")
      result `shouldBe` B.pack "XX"

    it "handles single character replacement" $ do
      regex <- R.compile (B.pack ".")
      result <- R.replaceAll regex (B.pack "*") (B.pack "abc")
      result `shouldBe` B.pack "***"

    it "handles word boundary" $ do
      regex <- R.compile (B.pack "\\bword\\b")
      result <- R.replaceAll regex (B.pack "WORD") (B.pack "word in a word")
      result `shouldBe` B.pack "WORD in a WORD"
