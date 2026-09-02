{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module BytesSpec where

import AST
import Bytes
  ( btsAnd
  , btsConcat
  , btsEqual
  , btsNot
  , btsOr
  , btsShift
  , btsSize
  , btsSlice
  , btsToNum
  , btsToStr
  , btsToUnescapedStr
  , bytesToBts
  , numToBts
  , strToBts
  )
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldSatisfy, shouldThrow)

spec :: Spec
spec = do
  describe "numToBts" $
    forM_
      [ ("0.0", 0.0 :: Double, BtMany ["00", "00", "00", "00", "00", "00", "00", "00"])
      , ("42", 42, BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
      , ("-0.25", -0.25, BtMany ["BF", "D0", "00", "00", "00", "00", "00", "00"])
      , ("5", 5, BtMany ["40", "14", "00", "00", "00", "00", "00", "00"])
      ]
      ( \(desc, num, bts) ->
          it desc $ numToBts num `shouldBe` bts
      )

  describe "numToBts/btsToNum round trip" $
    forM_
      [ ("normal integer", 42, (== Left 42))
      , ("normal fraction", 3.5, (== Right 3.5))
      , ("zero", 0.0, (== Left 0))
      , ("negative integer", -2, (== Left (-2)))
      , ("negative fraction", -0.25, (== Right (-0.25)))
      , ("NaN", 0 / 0, either (const False) isNaN)
      , ("positive infinity", 1 / 0, either (const False) (\num -> isInfinite num && num > 0))
      , ("negative infinity", -(1 / 0), either (const False) (\num -> isInfinite num && num < 0))
      , ("negative zero", -0.0, either (const False) isNegativeZero)
      ]
      (\(desc, num, predicate) -> it desc (btsToNum (numToBts num) `shouldSatisfy` predicate))

  describe "btsToNum with a byte array that is not 8 bytes long" $
    it "errors out" $
      evaluate (btsToNum (BtMany ["40", "45"])) `shouldThrow` anyErrorCall

  describe "strToBts" $
    forM_
      [ ("", BtEmpty)
      , ("h", BtOne "68")
      , ("hello", BtMany ["68", "65", "6C", "6C", "6F"])
      , ("\"", BtOne "22")
      , ("\\", BtOne "5C")
      , ("\n", BtOne "0A")
      , ("\t", BtOne "09")
      , ("\x01", BtOne "01")
      ]
      ( \(str, bts) ->
          it (show str) $ strToBts str `shouldBe` bts
      )

  describe "btsToStr" $
    forM_
      [ ("empty", BtEmpty, "")
      , ("single char", BtOne "68", "h")
      , ("multi byte", BtMany ["68", "65", "6C", "6C", "6F"], "hello")
      , ("escapes double quote", BtOne "22", "\\\"")
      , ("escapes backslash", BtOne "5C", "\\\\")
      , ("escapes newline", BtOne "0A", "\\n")
      , ("escapes tab", BtOne "09", "\\t")
      , ("escapes non-printable", BtOne "01", "\\x01")
      , ("mixed printable and quote", BtMany ["61", "22", "62"], "a\\\"b")
      ]
      ( \(desc, bts, str) ->
          it desc $ btsToStr bts `shouldBe` str
      )

  describe "btsToUnescapedStr" $
    forM_
      [ ("non-printable", BtMany ["01", "02"], "\SOH\STX")
      , ("multi byte word", BtMany ["77", "6F", "72", "6C", "64"], "world")
      , ("double quote", BtMany ["68", "22"], "h\"")
      , ("single hex digit padded", BtOne "35", "5")
      ]
      ( \(desc, bts, str) ->
          it desc $ btsToUnescapedStr bts `shouldBe` str
      )

  describe "bytesToBts" $
    forM_
      [ ("empty", "--", BtEmpty)
      , ("single byte with trailing dash", "01-", BtOne "01")
      , ("multi byte", "77-6F", BtMany ["77", "6F"])
      ]
      ( \(desc, str, bts) ->
          it desc $ bytesToBts str `shouldBe` bts
      )

  describe "btsAnd and btsOr" $
    forM_
      [
        ( "btsAnd: matching lengths"
        , btsAnd (BtMany ["02", "EF"]) (BtMany ["12", "33"])
        , Just (BtMany ["02", "23"])
        )
      , ("btsAnd: mismatched lengths", btsAnd (BtOne "20") (BtMany ["CA", "FE"]), Nothing)
      ,
        ( "btsOr: matching lengths"
        , btsOr (BtMany ["02", "EF"]) (BtMany ["12", "33"])
        , Just (BtMany ["12", "FF"])
        )
      , ("btsOr: mismatched lengths", btsOr (BtOne "20") (BtMany ["CA", "FE"]), Nothing)
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "btsNot" $
    it "negates every byte" $
      btsNot (BtMany ["CA", "FE", "BE", "BE"]) `shouldBe` BtMany ["35", "01", "41", "41"]

  describe "btsConcat" $
    forM_
      [ ("with BtEmpty on the right", BtMany ["05", "5E"], BtEmpty, BtMany ["05", "5E"])
      , ("two BtEmpty", BtEmpty, BtEmpty, BtEmpty)
      , ("two single bytes", BtOne "01", BtOne "02", BtMany ["01", "02"])
      ]
      ( \(desc, left, right, result) ->
          it desc $ btsConcat left right `shouldBe` result
      )

  describe "btsEqual" $ do
    it "same value via different constructors" $
      btsEqual (BtOne "01") (BtMany ["01"]) `shouldBe` True
    it "different values" $
      btsEqual (BtMany ["01", "02"]) (BtMany ["01", "03"]) `shouldBe` False

  describe "btsSize" $
    forM_
      [ ("empty", BtEmpty, 0)
      , ("three bytes", BtMany ["F1", "20", "5F"], 3)
      ]
      ( \(desc, bts, size) ->
          it desc $ btsSize bts `shouldBe` size
      )

  describe "btsSize on meta bytes" $
    it "errors out since meta bytes cannot be converted to actual bytes" $
      evaluate (btsSize (BtMeta "alpha")) `shouldThrow` anyErrorCall

  describe "hex byte decoding" $
    forM_
      [
        ( "accepts lowercase hex digits the same way as uppercase ones"
        , btsEqual (BtOne "bf") (BtOne "BF") `shouldBe` True
        )
      ,
        ( "decodes a single hex character via the fallback numeric reader"
        , btsEqual (BtOne "5") (BtOne "05") `shouldBe` True
        )
      ,
        ( "errors out on a hex digit that isn't 0-9, a-f or A-F"
        , evaluate (btsToUnescapedStr (BtOne "G1")) `shouldThrow` anyErrorCall
        )
      ,
        ( "errors out when the fallback numeric reader can't parse the byte at all"
        , evaluate (btsToUnescapedStr (BtOne "")) `shouldThrow` anyErrorCall
        )
      ]
      (uncurry it)

  describe "btsSlice" $
    forM_
      [
        ( "in range"
        , btsSlice 1 3 (BtMany ["20", "1F", "EE", "B5", "90"])
        , Just (BtMany ["1F", "EE", "B5"])
        )
      , ("out of range", btsSlice 3 10 (BtMany ["20", "1F", "EE", "B5", "90"]), Nothing)
      , ("negative start", btsSlice (-1) 2 (BtMany ["20", "1F", "EE"]), Nothing)
      , ("zero length slice", btsSlice 0 0 (BtMany ["20", "1F"]), Just BtEmpty)
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "btsShift" $
    forM_
      [
        ( "positive shift crossing a bit boundary"
        , btsShift 1 (BtMany ["C0", "43", "00"])
        , BtMany ["60", "21", "80"]
        )
      , ("negative shift crossing a bit boundary", btsShift (-1) (BtMany ["01", "80"]), BtMany ["03", "00"])
      , ("shift by zero is identity", btsShift 0 (BtMany ["FF", "00"]), BtMany ["FF", "00"])
      ,
        ( "positive shift crossing a byte boundary"
        , btsShift 8 (BtMany ["01", "02", "03"])
        , BtMany ["00", "01", "02"]
        )
      ,
        ( "negative shift crossing a byte boundary"
        , btsShift (-8) (BtMany ["01", "02", "03"])
        , BtMany ["02", "03", "00"]
        )
      , ("large negative shift empties out", btsShift (-2147483648) (BtMany ["BF", "F0"]), BtMany ["00", "00"])
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))
