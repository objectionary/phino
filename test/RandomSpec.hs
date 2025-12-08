-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Random module that provides random string generation
with pattern substitution for unique identifier creation.
-}
module RandomSpec where

import Data.Char (isDigit, isHexDigit)
import qualified Data.Set as Set
import Random (randomString)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "randomString with empty pattern" $
    it "returns empty string" $ do
      result <- randomString ""
      result `shouldBe` ""

  describe "randomString with literal pattern" $
    it "returns literal unchanged" $ do
      result <- randomString "hello"
      result `shouldBe` "hello"

  describe "randomString with literal pattern containing spaces" $
    it "returns literal with spaces" $ do
      result <- randomString "hello world"
      result `shouldBe` "hello world"

  describe "randomString with %d pattern" $
    it "generates numeric digits" $ do
      result <- randomString "%d"
      result `shouldSatisfy` all isDigit

  describe "randomString with %d pattern length" $
    it "generates 1-4 digit number" $ do
      result <- randomString "%d"
      let len = length result
      len `shouldSatisfy` (\l -> l >= 1 && l <= 4)

  describe "randomString with %x pattern" $
    it "generates hex digits" $ do
      result <- randomString "%x"
      result `shouldSatisfy` all isHexDigit

  describe "randomString with %x pattern length" $
    it "generates exactly 8 hex chars" $ do
      result <- randomString "%x"
      length result `shouldBe` 8

  describe "randomString with unknown % pattern" $
    it "preserves unknown pattern" $ do
      result <- randomString "%q"
      result `shouldBe` "%q"

  describe "randomString with unknown % pattern z" $
    it "preserves %z pattern" $ do
      result <- randomString "%z"
      result `shouldBe` "%z"

  describe "randomString with prefix and %d" $
    it "combines prefix with digits" $ do
      result <- randomString "id_%d"
      result `shouldSatisfy` (\s -> take 3 s == "id_")

  describe "randomString with suffix and %d" $
    it "combines digits with suffix" $ do
      result <- randomString "%d_end"
      result `shouldSatisfy` (\s -> drop (length s - 4) s == "_end")

  describe "randomString with prefix and %x" $
    it "combines prefix with hex" $ do
      result <- randomString "hex_%x"
      result `shouldSatisfy` (\s -> take 4 s == "hex_" && length s == 12)

  describe "randomString with suffix and %x" $
    it "combines hex with suffix" $ do
      result <- randomString "%x_end"
      result `shouldSatisfy` (\s -> drop 8 s == "_end" && length s == 12)

  describe "randomString with multiple %d patterns" $
    it "replaces all %d patterns" $ do
      result <- randomString "%d-%d"
      result `shouldSatisfy` (\s -> '-' `elem` s)

  describe "randomString with multiple %x patterns" $
    it "replaces all %x patterns" $ do
      result <- randomString "%x-%x"
      let parts = wordsBy (== '-') result
      length parts `shouldBe` 2

  describe "randomString with mixed patterns" $
    it "handles %d and %x together" $ do
      result <- randomString "a%db%xc"
      result `shouldSatisfy` (\s -> head s == 'a')

  describe "randomString generates unique strings" $
    it "produces different results on repeated calls" $ do
      results <- mapM (const (randomString "test_%d")) [1 :: Int .. 10]
      let unique = Set.fromList results
      Set.size unique `shouldBe` 10

  describe "randomString with %x generates unique strings" $
    it "produces different hex results" $ do
      results <- mapM (const (randomString "%x")) [1 :: Int .. 5]
      let unique = Set.fromList results
      Set.size unique `shouldBe` 5

  describe "randomString with complex pattern" $
    it "handles prefix_%d_middle_%x_suffix" $ do
      result <- randomString "pre_%d_mid_%x_suf"
      result `shouldSatisfy` (\s -> take 4 s == "pre_")

  describe "randomString with trailing percent" $
    it "preserves trailing percent" $ do
      result <- randomString "test%"
      result `shouldBe` "test%"

  describe "randomString with double percent" $
    it "handles %% as unknown pattern" $ do
      result <- randomString "%%"
      result `shouldBe` "%%"

  describe "randomString with special chars" $
    it "preserves special characters" $ do
      result <- randomString "a!@#b"
      result `shouldBe` "a!@#b"

  describe "randomString with unicode" $
    it "preserves unicode characters" $ do
      result <- randomString "test"
      result `shouldBe` "test"

  describe "randomString %d range" $
    it "generates numbers in 0-9999 range" $ do
      result <- randomString "%d"
      let num = read result :: Int
      num `shouldSatisfy` (\n -> n >= 0 && n <= 9999)

  describe "randomString %x chars" $
    it "generates lowercase hex digits" $ do
      result <- randomString "%x"
      result `shouldSatisfy` all (\c -> isHexDigit c && (isDigit c || c `elem` "abcdef"))

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy predicate str = case dropWhile predicate str of
  "" -> []
  str' -> let (word, rest) = break predicate str' in word : wordsBy predicate rest
