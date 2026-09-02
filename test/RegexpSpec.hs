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
    forM_
      [ ("compiles a valid pattern", "foo", "foobar")
      , ("compiles pattern with groups", "(a)(b)(c)", "abc")
      , ("compiles pattern with unicode", "кирилиця", "текст кирилиця тут")
      , ("compiles empty pattern", "", "anything")
      ]
      ( \(desc, pattern, input) -> it desc $ do
          _ <- R.compile (B.pack pattern)
          matched <- R.match (B.pack pattern) (B.pack input)
          matched `shouldBe` True
      )

    it "throws on invalid pattern" $
      R.compile (B.pack "[invalid") `shouldThrow` anyException

    it "throws with the underlying PCRE error embedded in the message" $ do
      result <- try (void (R.compile (B.pack "[invalid"))) :: IO (Either SomeException ())
      case result of
        Left exc -> displayException exc `shouldSatisfy` isInfixOf "Regex compilation failed:"
        Right () -> fail "expected R.compile to fail"

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

  describe "extractGroups" $
    forM_
      [ ("extracts groups from pattern with capturing groups", "(\\w+)@(\\w+)", "user@domain", ["user@domain", "user", "domain"])
      , ("returns empty list when no match", "(foo)", "bar", [])
      , ("extracts nested groups", "((a)(b))", "ab", ["ab", "ab", "a", "b"])
      , ("handles optional group that did not match", "(a)(b)?", "a", ["a", "a", ""])
      , ("extracts multiple groups", "(x)(y)(z)", "prefix xyz suffix", ["xyz", "x", "y", "z"])
      , ("handles pattern without groups", "test", "this is a test", ["test"])
      ]
      ( \(desc, pattern, input, expected) -> it desc $ do
          regex <- R.compile (B.pack pattern)
          groups <- R.extractGroups regex (B.pack input)
          groups `shouldBe` map B.pack expected
      )

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

  describe "replaceFirst" $
    forM_
      [ ("replaces first occurrence", "cat", "dog", "cat and cat", "dog and cat")
      , ("returns input when no match", "xyz", "abc", "hello world", "hello world")
      , ("replaces with empty string", "remove", "", "please remove this", "please  this")
      , ("replaces at start of string", "^start", "begin", "start here", "begin here")
      , ("replaces at end of string", "end$", "finish", "the end", "the finish")
      ,
        ( "uses captured groups in replacement"
        , "(\\w+)@(\\w+)"
        , "[$1 AT $2]"
        , "email: test@example here"
        , "email: [test AT example] here"
        )
      , ("handles unicode pattern and replacement", "古い", "新しい", "これは古いです", "これは新しいです")
      , ("handles empty input", "x", "y", "", "")
      , ("replaces entire string when pattern matches all", "^.*$", "replaced", "original", "replaced")
      ]
      ( \(desc, pattern, replacement, input, expected) -> it desc $ do
          regex <- R.compile (B.pack pattern)
          result <- R.replaceFirst regex (B.pack replacement) (B.pack input)
          result `shouldBe` B.pack expected
      )

  describe "replaceAll" $
    forM_
      [ ("replaces all occurrences", "a", "X", "banana", "bXnXnX")
      , ("returns input when no match", "xyz", "abc", "hello world", "hello world")
      , ("replaces consecutive matches", "o", "0", "oooo", "0000")
      , ("replaces with captured groups", "(\\d+)", "[$1]", "a1b2c3", "a[1]b[2]c[3]")
      , ("handles empty replacement", "x", "", "axbxcx", "abc")
      , ("handles empty input", "x", "y", "", "")
      , ("handles unicode input and pattern", "кіт", "пес", "кіт і кіт", "пес і пес")
      , ("replaces overlapping potential matches correctly", "aa", "X", "aaaa", "XX")
      , ("handles single character replacement", ".", "*", "abc", "***")
      , ("handles word boundary", "\\bword\\b", "WORD", "word in a word", "WORD in a WORD")
      , ("terminates on an empty-match pattern (anchored ^)", "^", "X", "hello", "XhXeXlXlXoX")
      , ("terminates on an empty regex pattern", "", "X", "hello", "XhXeXlXlXoX")
      ]
      ( \(desc, pattern, replacement, input, expected) -> it desc $ do
          regex <- R.compile (B.pack pattern)
          result <- R.replaceAll regex (B.pack replacement) (B.pack input)
          result `shouldBe` B.pack expected
      )
