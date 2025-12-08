-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Must module that provides constraint specification
for rewriting rules with exact counts and ranges.
-}
module MustSpec where

import Control.Monad (forM_)
import Must (Must (..), exceedsUpperBound, inRange, validateMust)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Read (readMaybe)

spec :: Spec
spec = do
  describe "Show instance renders MtDisabled" $
    it "displays zero" $
      show MtDisabled `shouldBe` "0"

  describe "Show instance renders MtExact" $
    forM_
      [ ("positive integer", MtExact 42, "42")
      , ("large integer", MtExact 999999, "999999")
      , ("one", MtExact 1, "1")
      ]
      ( \(desc, must, expected) ->
          it desc $ show must `shouldBe` expected
      )

  describe "Show instance renders MtRange with both bounds" $
    forM_
      [ ("small range", MtRange (Just 1) (Just 5), "1..5")
      , ("same bounds", MtRange (Just 3) (Just 3), "3..3")
      , ("large range", MtRange (Just 0) (Just 1000), "0..1000")
      ]
      ( \(desc, must, expected) ->
          it desc $ show must `shouldBe` expected
      )

  describe "Show instance renders MtRange with only minimum" $
    forM_
      [ ("minimum only", MtRange (Just 5) Nothing, "5..")
      , ("zero minimum", MtRange (Just 0) Nothing, "0..")
      ]
      ( \(desc, must, expected) ->
          it desc $ show must `shouldBe` expected
      )

  describe "Show instance renders MtRange with only maximum" $
    forM_
      [ ("maximum only", MtRange Nothing (Just 10), "..10")
      , ("zero maximum", MtRange Nothing (Just 0), "..0")
      ]
      ( \(desc, must, expected) ->
          it desc $ show must `shouldBe` expected
      )

  describe "Show instance renders MtRange with no bounds" $
    it "displays empty range" $
      show (MtRange Nothing Nothing) `shouldBe` ".."

  describe "Read instance parses zero as MtDisabled" $
    it "parses disabled" $
      (readMaybe "0" :: Maybe Must) `shouldBe` Just MtDisabled

  describe "Read instance parses positive integers as MtExact" $
    forM_
      [ ("single digit", "5", Just (MtExact 5))
      , ("multi digit", "123", Just (MtExact 123))
      , ("large number", "999999", Just (MtExact 999999))
      ]
      ( \(desc, input, expected) ->
          it desc $ (readMaybe input :: Maybe Must) `shouldBe` expected
      )

  describe "Read instance rejects negative integers" $
    forM_
      [ ("negative one", "-1")
      , ("negative large", "-999")
      ]
      ( \(desc, input) ->
          it desc $ (readMaybe input :: Maybe Must) `shouldBe` Nothing
      )

  describe "Read instance rejects non-numeric input" $
    forM_
      [ ("alphabetic", "abc")
      , ("mixed", "12abc")
      , ("empty", "")
      , ("unicode", "日本語")
      ]
      ( \(desc, input) ->
          it desc $ (readMaybe input :: Maybe Must) `shouldBe` Nothing
      )

  describe "Read instance parses full range" $
    forM_
      [ ("simple range", "1..5", Just (MtRange (Just 1) (Just 5)))
      , ("same bounds", "3..3", Just (MtRange (Just 3) (Just 3)))
      , ("zero start", "0..10", Just (MtRange (Just 0) (Just 10)))
      ]
      ( \(desc, input, expected) ->
          it desc $ (readMaybe input :: Maybe Must) `shouldBe` expected
      )

  describe "Read instance parses minimum-only range" $
    forM_
      [ ("with minimum", "5..", Just (MtRange (Just 5) Nothing))
      , ("zero minimum", "0..", Just (MtRange (Just 0) Nothing))
      ]
      ( \(desc, input, expected) ->
          it desc $ (readMaybe input :: Maybe Must) `shouldBe` expected
      )

  describe "Read instance parses maximum-only range" $
    forM_
      [ ("with maximum", "..10", Just (MtRange Nothing (Just 10)))
      , ("zero maximum", "..0", Just (MtRange Nothing (Just 0)))
      ]
      ( \(desc, input, expected) ->
          it desc $ (readMaybe input :: Maybe Must) `shouldBe` expected
      )

  describe "Read instance rejects empty range" $
    it "fails on dots only" $
      (readMaybe ".." :: Maybe Must) `shouldBe` Nothing

  describe "Read instance rejects invalid range with negative minimum" $
    it "fails on negative min" $
      (readMaybe "-1..5" :: Maybe Must) `shouldBe` Nothing

  describe "Read instance rejects invalid range with negative maximum" $
    it "fails on negative max" $
      (readMaybe "1..-5" :: Maybe Must) `shouldBe` Nothing

  describe "Read instance rejects range where min exceeds max" $
    it "fails on inverted range" $
      (readMaybe "10..5" :: Maybe Must) `shouldBe` Nothing

  describe "Read instance rejects non-numeric range parts" $
    forM_
      [ ("alphabetic min", "abc..5")
      , ("alphabetic max", "5..abc")
      , ("both alphabetic", "abc..xyz")
      ]
      ( \(desc, input) ->
          it desc $ (readMaybe input :: Maybe Must) `shouldBe` Nothing
      )

  describe "Eq instance compares MtDisabled" $
    it "equals itself" $
      MtDisabled == MtDisabled `shouldBe` True

  describe "Eq instance compares MtExact" $
    forM_
      [ ("same values equal", MtExact 5, MtExact 5, True)
      , ("different values not equal", MtExact 5, MtExact 10, False)
      ]
      ( \(desc, lhs, rhs, expected) ->
          it desc $ (lhs == rhs) `shouldBe` expected
      )

  describe "Eq instance compares MtRange" $
    forM_
      [ ("same ranges equal", MtRange (Just 1) (Just 5), MtRange (Just 1) (Just 5), True)
      , ("different min not equal", MtRange (Just 1) (Just 5), MtRange (Just 2) (Just 5), False)
      , ("different max not equal", MtRange (Just 1) (Just 5), MtRange (Just 1) (Just 6), False)
      ]
      ( \(desc, lhs, rhs, expected) ->
          it desc $ (lhs == rhs) `shouldBe` expected
      )

  describe "Eq instance compares different types" $
    forM_
      [ ("disabled vs exact", MtDisabled, MtExact 0, False)
      , ("exact vs range", MtExact 5, MtRange (Just 5) (Just 5), False)
      ]
      ( \(desc, lhs, rhs, expected) ->
          it desc $ (lhs == rhs) `shouldBe` expected
      )

  describe "inRange with MtDisabled accepts any value" $
    forM_
      [ ("zero", 0)
      , ("large positive", 999999)
      , ("negative", -42)
      ]
      ( \(desc, val) ->
          it desc $ inRange MtDisabled val `shouldBe` True
      )

  describe "inRange with MtExact checks equality" $
    forM_
      [ ("exact match", MtExact 5, 5, True)
      , ("below exact", MtExact 5, 4, False)
      , ("above exact", MtExact 5, 6, False)
      ]
      ( \(desc, must, val, expected) ->
          it desc $ inRange must val `shouldBe` expected
      )

  describe "inRange with MtRange checks bounds" $
    forM_
      [ ("within range", MtRange (Just 1) (Just 10), 5, True)
      , ("at minimum", MtRange (Just 1) (Just 10), 1, True)
      , ("at maximum", MtRange (Just 1) (Just 10), 10, True)
      , ("below minimum", MtRange (Just 5) (Just 10), 4, False)
      , ("above maximum", MtRange (Just 1) (Just 5), 6, False)
      ]
      ( \(desc, must, val, expected) ->
          it desc $ inRange must val `shouldBe` expected
      )

  describe "inRange with minimum-only range" $
    forM_
      [ ("at minimum", MtRange (Just 5) Nothing, 5, True)
      , ("above minimum", MtRange (Just 5) Nothing, 100, True)
      , ("below minimum", MtRange (Just 5) Nothing, 4, False)
      ]
      ( \(desc, must, val, expected) ->
          it desc $ inRange must val `shouldBe` expected
      )

  describe "inRange with maximum-only range" $
    forM_
      [ ("at maximum", MtRange Nothing (Just 10), 10, True)
      , ("below maximum", MtRange Nothing (Just 10), 0, True)
      , ("above maximum", MtRange Nothing (Just 10), 11, False)
      ]
      ( \(desc, must, val, expected) ->
          it desc $ inRange must val `shouldBe` expected
      )

  describe "inRange with unbounded range" $
    forM_
      [ ("zero", 0)
      , ("large positive", 999999)
      , ("negative", -42)
      ]
      ( \(desc, val) ->
          it desc $ inRange (MtRange Nothing Nothing) val `shouldBe` True
      )

  describe "exceedsUpperBound with MtDisabled" $
    forM_
      [ ("zero", 0)
      , ("large positive", 999999)
      ]
      ( \(desc, val) ->
          it desc $ exceedsUpperBound MtDisabled val `shouldBe` False
      )

  describe "exceedsUpperBound with MtExact" $
    forM_
      [ ("at bound", MtExact 5, 5, False)
      , ("below bound", MtExact 5, 4, False)
      , ("above bound", MtExact 5, 6, True)
      ]
      ( \(desc, must, val, expected) ->
          it desc $ exceedsUpperBound must val `shouldBe` expected
      )

  describe "exceedsUpperBound with MtRange with maximum" $
    forM_
      [ ("at maximum", MtRange (Just 0) (Just 10), 10, False)
      , ("below maximum", MtRange (Just 0) (Just 10), 5, False)
      , ("above maximum", MtRange (Just 0) (Just 10), 11, True)
      ]
      ( \(desc, must, val, expected) ->
          it desc $ exceedsUpperBound must val `shouldBe` expected
      )

  describe "exceedsUpperBound with MtRange without maximum" $
    forM_
      [ ("zero", 0)
      , ("large positive", 999999)
      ]
      ( \(desc, val) ->
          it desc $ exceedsUpperBound (MtRange (Just 0) Nothing) val `shouldBe` False
      )

  describe "validateMust with MtDisabled" $
    it "returns nothing" $
      validateMust MtDisabled `shouldBe` Nothing

  describe "validateMust with valid MtExact" $
    it "returns nothing for positive" $
      validateMust (MtExact 5) `shouldBe` Nothing

  describe "validateMust with valid MtRange" $
    forM_
      [ ("both bounds", MtRange (Just 1) (Just 10))
      , ("minimum only", MtRange (Just 5) Nothing)
      , ("maximum only", MtRange Nothing (Just 10))
      , ("no bounds", MtRange Nothing Nothing)
      ]
      ( \(desc, must) ->
          it desc $ validateMust must `shouldBe` Nothing
      )

  describe "validateMust with inverted MtRange" $
    it "returns error message" $
      validateMust (MtRange (Just 10) (Just 5)) `shouldSatisfy` present

  describe "validateMust with zero MtExact" $
    it "returns error for zero" $
      validateMust (MtExact 0) `shouldSatisfy` present

  describe "validateMust with negative minimum in range" $
    it "returns error for negative min" $
      validateMust (MtRange (Just (-1)) (Just 5)) `shouldSatisfy` present

  describe "validateMust with negative maximum in range" $
    it "returns error for negative max" $
      validateMust (MtRange (Just 0) (Just (-1))) `shouldSatisfy` present
  where
    present (Just _) = True
    present Nothing = False
