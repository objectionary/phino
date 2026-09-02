-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Must module that provides constraint specification
for rewriting rules with exact counts and ranges.
-}
module MustSpec where

import Control.Monad (forM_)
import Must (Must (..), exceedsUpperBound, inRange, validateMust)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Read (readMaybe)

spec :: Spec
spec = do
  describe "Show instance" $
    forM_
      [ ("displays a disabled must as zero", MtDisabled, "0")
      , ("displays a positive exact value", MtExact 42, "42")
      , ("displays a large exact value", MtExact 999999, "999999")
      , ("displays an exact value of one", MtExact 1, "1")
      , ("displays a range with both bounds", MtRange (Just 1) (Just 5), "1..5")
      , ("displays a range with equal bounds", MtRange (Just 3) (Just 3), "3..3")
      , ("displays a large range", MtRange (Just 0) (Just 1000), "0..1000")
      , ("displays a range with only a minimum", MtRange (Just 5) Nothing, "5..")
      , ("displays a range with only a zero minimum", MtRange (Just 0) Nothing, "0..")
      , ("displays a range with only a maximum", MtRange Nothing (Just 10), "..10")
      , ("displays a range with only a zero maximum", MtRange Nothing (Just 0), "..0")
      , ("displays a range with no bounds", MtRange Nothing Nothing, "..")
      ]
      (\(desc, must, expected) -> it desc (show must `shouldBe` expected))

  describe "Read instance" $
    forM_
      [ ("parses zero as MtDisabled", "0", Just MtDisabled)
      , ("parses a single-digit exact value", "5", Just (MtExact 5))
      , ("parses a multi-digit exact value", "123", Just (MtExact 123))
      , ("parses a large exact value", "999999", Just (MtExact 999999))
      , ("rejects a negative exact value", "-1", Nothing)
      , ("rejects a large negative exact value", "-999", Nothing)
      , ("rejects alphabetic input", "abc", Nothing)
      , ("rejects mixed alphanumeric input", "12abc", Nothing)
      , ("rejects empty input", "", Nothing)
      , ("rejects unicode input", "日本語", Nothing)
      , ("parses a simple range", "1..5", Just (MtRange (Just 1) (Just 5)))
      , ("parses a range with equal bounds", "3..3", Just (MtRange (Just 3) (Just 3)))
      , ("parses a range with a zero start", "0..10", Just (MtRange (Just 0) (Just 10)))
      , ("parses a minimum-only range", "5..", Just (MtRange (Just 5) Nothing))
      , ("parses a minimum-only range with a zero minimum", "0..", Just (MtRange (Just 0) Nothing))
      , ("parses a maximum-only range", "..10", Just (MtRange Nothing (Just 10)))
      , ("parses a maximum-only range with a zero maximum", "..0", Just (MtRange Nothing (Just 0)))
      , ("round-trips an empty range with dots only", "..", Just (MtRange Nothing Nothing))
      , ("rejects a range with a negative minimum", "-1..5", Nothing)
      , ("rejects a range with a negative maximum", "1..-5", Nothing)
      , ("rejects an inverted range where min exceeds max", "10..5", Nothing)
      , ("rejects a range with an alphabetic minimum", "abc..5", Nothing)
      , ("rejects a range with an alphabetic maximum", "5..abc", Nothing)
      , ("rejects a range with both parts alphabetic", "abc..xyz", Nothing)
      , ("rejects a range with more than one '..' separator", "3.4..5", Nothing)
      , ("rejects a maximum-only range with a negative bound", "..-5", Nothing)
      , ("rejects a minimum-only range with a negative bound", "-5..", Nothing)
      ]
      (\(desc, input, expected) -> it desc ((readMaybe input :: Maybe Must) `shouldBe` expected))

  describe "Eq instance" $
    forM_
      [ ("MtDisabled equals itself", MtDisabled, MtDisabled, True)
      , ("equal MtExact values are equal", MtExact 5, MtExact 5, True)
      , ("different MtExact values are not equal", MtExact 5, MtExact 10, False)
      , ("equal MtRange values are equal", MtRange (Just 1) (Just 5), MtRange (Just 1) (Just 5), True)
      , ("MtRange values with different minimums are not equal", MtRange (Just 1) (Just 5), MtRange (Just 2) (Just 5), False)
      , ("MtRange values with different maximums are not equal", MtRange (Just 1) (Just 5), MtRange (Just 1) (Just 6), False)
      , ("MtDisabled is not equal to MtExact", MtDisabled, MtExact 0, False)
      , ("MtExact is not equal to MtRange", MtExact 5, MtRange (Just 5) (Just 5), False)
      ]
      (\(desc, lhs, rhs, expected) -> it desc ((lhs == rhs) `shouldBe` expected))

  describe "inRange" $
    forM_
      [ ("MtDisabled accepts zero", MtDisabled, 0, True)
      , ("MtDisabled accepts a large positive value", MtDisabled, 999999, True)
      , ("MtDisabled accepts a negative value", MtDisabled, -42, True)
      , ("MtExact accepts the exact value", MtExact 5, 5, True)
      , ("MtExact rejects a value below it", MtExact 5, 4, False)
      , ("MtExact rejects a value above it", MtExact 5, 6, False)
      , ("MtRange accepts a value within bounds", MtRange (Just 1) (Just 10), 5, True)
      , ("MtRange accepts the minimum bound", MtRange (Just 1) (Just 10), 1, True)
      , ("MtRange accepts the maximum bound", MtRange (Just 1) (Just 10), 10, True)
      , ("MtRange rejects a value below the minimum", MtRange (Just 5) (Just 10), 4, False)
      , ("MtRange rejects a value above the maximum", MtRange (Just 1) (Just 5), 6, False)
      , ("a minimum-only range accepts the minimum", MtRange (Just 5) Nothing, 5, True)
      , ("a minimum-only range accepts a value above the minimum", MtRange (Just 5) Nothing, 100, True)
      , ("a minimum-only range rejects a value below the minimum", MtRange (Just 5) Nothing, 4, False)
      , ("a maximum-only range accepts the maximum", MtRange Nothing (Just 10), 10, True)
      , ("a maximum-only range accepts a value below the maximum", MtRange Nothing (Just 10), 0, True)
      , ("a maximum-only range rejects a value above the maximum", MtRange Nothing (Just 10), 11, False)
      , ("an unbounded range accepts zero", MtRange Nothing Nothing, 0, True)
      , ("an unbounded range accepts a large positive value", MtRange Nothing Nothing, 999999, True)
      , ("an unbounded range accepts a negative value", MtRange Nothing Nothing, -42, True)
      ]
      (\(desc, must, value, expected) -> it desc (inRange must value `shouldBe` expected))

  describe "exceedsUpperBound" $
    forM_
      [ ("MtDisabled never exceeds for zero", MtDisabled, 0, False)
      , ("MtDisabled never exceeds for a large positive value", MtDisabled, 999999, False)
      , ("MtExact at the bound does not exceed", MtExact 5, 5, False)
      , ("MtExact below the bound does not exceed", MtExact 5, 4, False)
      , ("MtExact above the bound exceeds", MtExact 5, 6, True)
      , ("MtRange at the maximum does not exceed", MtRange (Just 0) (Just 10), 10, False)
      , ("MtRange below the maximum does not exceed", MtRange (Just 0) (Just 10), 5, False)
      , ("MtRange above the maximum exceeds", MtRange (Just 0) (Just 10), 11, True)
      , ("a maximum-less MtRange never exceeds for zero", MtRange (Just 0) Nothing, 0, False)
      , ("a maximum-less MtRange never exceeds for a large positive value", MtRange (Just 0) Nothing, 999999, False)
      ]
      (\(desc, must, value, expected) -> it desc (exceedsUpperBound must value `shouldBe` expected))

  describe "validateMust" $
    forM_
      [ ("MtDisabled is always valid", MtDisabled, Nothing)
      , ("a positive MtExact is valid", MtExact 5, Nothing)
      , ("a range with both bounds is valid", MtRange (Just 1) (Just 10), Nothing)
      , ("a minimum-only range is valid", MtRange (Just 5) Nothing, Nothing)
      , ("a maximum-only range is valid", MtRange Nothing (Just 10), Nothing)
      , ("an unbounded range is valid", MtRange Nothing Nothing, Nothing)
      , ("an inverted range is rejected", MtRange (Just 10) (Just 5), Just "--must range invalid: minimum (10) is greater than maximum (5)")
      , ("a zero MtExact is rejected", MtExact 0, Just "--must exact value must be positive")
      , ("a negative MtExact is rejected", MtExact (-3), Just "--must exact value must be positive")
      , ("a negative minimum is rejected", MtRange (Just (-1)) (Just 5), Just "--must minimum must be non-negative")
      , ("a negative maximum is rejected", MtRange (Just 0) (Just (-1)), Just "--must maximum must be non-negative")
      ]
      (\(desc, must, expected) -> it desc (validateMust must `shouldBe` expected))
