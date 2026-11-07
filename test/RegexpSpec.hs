-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RegexpSpec where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Regexp qualified as R
import System.Timeout (timeout)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)

utf8 :: String -> ByteString
utf8 = TE.encodeUtf8 . T.pack

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
          _ <- R.compile (utf8 pattern)
          matched <- R.match (utf8 pattern) (utf8 input)
          matched `shouldBe` True
      )

    it "throws on invalid pattern" $
      R.compile (utf8 "[invalid") `shouldThrow` anyException

    it "throws with the underlying PCRE error embedded in the message" $ do
      result <- try (void (R.compile (utf8 "[invalid"))) :: IO (Either SomeException ())
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
      , ("matches non-ASCII letters with \\w", "^\\w+$", "héllo", True)
      , ("matches non-ASCII digits with \\d", "^\\d+$", "١٢٣", True)
      , ("matches non-ASCII letters with [[:alpha:]]", "^[[:alpha:]]+$", "héllo", True)
      ]
      (\(desc, pattern, input, expected) -> it desc $ R.match (utf8 pattern) (utf8 input) `shouldReturn` expected)

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
          regex <- R.compile (utf8 pattern)
          groups <- R.extractGroups regex (utf8 input)
          groups `shouldBe` map utf8 expected
      )

  describe "substituteGroups" $
    forM_
      [ ("substitutes group zero", "[$0]", ["match"], "[match]")
      , ("substitutes multiple groups", "$1-$2", ["full", "a", "b"], "a-b")
      , ("keeps dollar sign when no digits follow", "$ test", ["x"], "$ test")
      , ("keeps original reference for out of bounds index", "$9", ["only"], "$9")
      ,
        ( "keeps an oversized group reference literal"
        , "$999999999999999999999999999999999999999999"
        , ["only"]
        , "$999999999999999999999999999999999999999999"
        )
      ,
        ( "keeps a group reference that wraps to group one literal"
        , "$18446744073709551617"
        , ["only", "a"]
        , "$18446744073709551617"
        )
      ,
        ( "keeps a group reference that wraps to group zero literal"
        , "$18446744073709551616"
        , ["only", "a"]
        , "$18446744073709551616"
        )
      , ("substitutes the highest existing group", "$1", ["only", "a"], "a")
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
          it desc $ R.substituteGroups (utf8 template) (map utf8 groups) `shouldBe` utf8 expected
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
          regex <- R.compile (utf8 pattern)
          result <- R.replaceFirst regex (utf8 replacement) (utf8 input)
          result `shouldBe` utf8 expected
      )

  describe "replaceAll" $ do
    it "replaces many matches in linear time" $ do
      regex <- R.compile (utf8 "a")
      result <- timeout 10000000 (R.replaceAll regex (utf8 "bb") (utf8 (replicate 200000 'a')))
      result `shouldSatisfy` (== Just (utf8 (replicate 400000 'b')))
    it "replaces many empty matches in linear time" $ do
      regex <- R.compile (utf8 "")
      result <- timeout 10000000 (R.replaceAll regex (utf8 "-") (utf8 (replicate 200000 'a')))
      result `shouldSatisfy` (== Just (utf8 ('-' : concat (replicate 200000 "a-"))))
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
      , ("keeps lookbehind context across matches", "(?<=a)a", "X", "aaa", "aXX")
      , ("does not invent word boundaries after a match", "\\ba", "X", "aaa", "Xaa")
      , ("does not restart an absolute-start assertion", "\\Aa", "X", "aaa", "Xaa")
      , ("keeps captured lookbehind text after previous matches", "(?<=(a))a", "$1X", "aaa", "aaXaX")
      , ("keeps Unicode lookbehind context", "(?<=é)é", "X", "ééé", "éXX")
      , ("keeps context after an empty match", "\\b", "X", "éé", "XééX")
      , ("terminates on an empty-match pattern (anchored ^)", "^", "X", "hello", "Xhello")
      , ("terminates on an empty regex pattern", "", "X", "hello", "XhXeXlXlXoX")
      , ("keeps UTF-8 characters together after an empty match", "(?=.)", "X", "é", "Xé")
      ]
      ( \(desc, pattern, replacement, input, expected) -> it desc $ do
          regex <- R.compile (utf8 pattern)
          result <- R.replaceAll regex (utf8 replacement) (utf8 input)
          result `shouldBe` utf8 expected
      )
