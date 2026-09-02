-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Regexp module that provides regular expression
matching and replacement using PCRE.
-}
module RegexpSpec where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, void)
import Data.ByteString.Char8 qualified as B
import Data.List (isInfixOf)
import Regexp qualified as R
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles a valid pattern" $ do
      _ <- R.compile (B.pack "foo")
      matched <- R.match (B.pack "foo") (B.pack "foobar")
      matched `shouldBe` True

    it "throws on invalid pattern" $
      R.compile (B.pack "[invalid") `shouldThrow` anyException

    it "throws with the underlying PCRE error embedded in the message" $ do
      result <- try (void (R.compile (B.pack "[invalid"))) :: IO (Either SomeException ())
      case result of
        Left exc -> displayException exc `shouldSatisfy` isInfixOf "Regex compilation failed:"
        Right () -> fail "expected R.compile to fail"

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

  describe "match" $
    forM_
      [ ("returns true when pattern matches", "hello", "hello world", True)
      , ("returns false when pattern does not match", "goodbye", "hello world", False)
      , ("returns true for partial match", "wor", "hello world", True)
      , ("returns true for match at start", "^hello", "hello world", True)
      , ("returns false for anchored pattern not at start", "^world", "hello world", False)
      , ("returns true for match at end", "world$", "hello world", True)
      , ("returns true with empty input and empty pattern", "", "", True)
      , ("returns true with non-empty input and empty pattern", "", "text", True)
      , ("returns false with empty input and non-empty pattern", "text", "", False)
      , ("handles special regex characters", "a\\.b", "a.b", True)
      , ("handles character class", "[0-9]+", "abc123def", True)
      , ("handles alternation", "cat|dog", "I have a dog", True)
      , ("handles unicode input", "日本語", "これは日本語です", True)
      , ("handles case sensitive match", "Hello", "hello", False)
      ]
      (\(desc, pattern, input, expected) -> it desc $ R.match (B.pack pattern) (B.pack input) `shouldReturn` expected)

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
      groups `shouldBe` [B.pack "a", B.pack "a", B.empty]

    it "extracts multiple groups" $ do
      regex <- R.compile (B.pack "(x)(y)(z)")
      groups <- R.extractGroups regex (B.pack "prefix xyz suffix")
      groups `shouldBe` [B.pack "xyz", B.pack "x", B.pack "y", B.pack "z"]

    it "handles pattern without groups" $ do
      regex <- R.compile (B.pack "test")
      groups <- R.extractGroups regex (B.pack "this is a test")
      groups `shouldBe` [B.pack "test"]

  describe "substituteGroups" $
    forM_
      [ ("substitutes group zero", "[$0]", ["match"], "[match]")
      , ("substitutes multiple groups", "$1-$2", ["full", "a", "b"], "a-b")
      , ("keeps dollar sign when no digits follow", "$ test", ["x"], "$ test")
      , ("keeps original reference for out of bounds index", "$9", ["only"], "$9")
      , ("handles replacement without group references", "plain", ["x"], "plain")
      , ("handles empty replacement", "", ["x"], "")
      , ("handles empty groups list with reference", "$0", [], "$0")
      , ("handles multi-digit group reference", "$12", replicate 13 "x", "x")
      , ("handles consecutive group references", "$0$1$2", ["a", "b", "c"], "abc")
      , ("handles unicode in replacement", "結果: $1", ["all", "データ"], "結果: データ")
      , ("handles dollar at end of string", "test$", ["x"], "test$")
      , ("handles double dollar", "$$1", ["x", "y"], "$y")
      ]
      ( \(desc, template, groups, expected) ->
          it desc $ R.substituteGroups (B.pack template) (map B.pack groups) `shouldBe` B.pack expected
      )

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

    it "terminates on an empty-match pattern (anchored ^)" $ do
      regex <- R.compile (B.pack "^")
      result <- R.replaceAll regex (B.pack "X") (B.pack "hello")
      result `shouldBe` B.pack "XhXeXlXlXoX"

    it "terminates on an empty regex pattern" $ do
      regex <- R.compile B.empty
      result <- R.replaceAll regex (B.pack "X") (B.pack "hello")
      result `shouldBe` B.pack "XhXeXlXlXoX"
