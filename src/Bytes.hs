{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module is a codec between 'Bytes' and the values they encode:
-- IEEE-754 doubles, UTF-8 strings and raw hex.
module Bytes
  ( numToBts
  , strToBts
  , bytesToBts
  , btsToStr
  , btsToNum
  , btsToUnescapedStr
  )
where

import AST
import Data.Binary.IEEE754
import Data.Bits (Bits (shiftL, shiftR), (.&.), (.|.))
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString, word64BE)
import Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (chr, isDigit, isPrint, ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word64, Word8)
import Numeric (readHex)
import Text.Printf (printf)

-- >>> btsToWord8 BtEmpty
-- []
-- >>> btsToWord8 (BtOne "01")
-- [1]
-- >>> btsToWord8 (BtMany [])
-- []
-- >>> btsToWord8 (BtMany ["40", "14", "00", "00", "00", "00", "00", "00"])
-- [64,20,0,0,0,0,0,0]
btsToWord8 :: Bytes -> [Word8]
btsToWord8 BtEmpty = []
btsToWord8 (BtOne bt) = [hexByte bt]
btsToWord8 (BtMany bts) = map hexByte bts
btsToWord8 (BtMeta mt) = error $ "Cannot convert meta bytes to Word8; " ++ T.unpack mt

hexByte :: String -> Word8
hexByte [hi, lo] = (nibble hi `shiftL` 4) .|. nibble lo
  where
    nibble :: Char -> Word8
    nibble c
      | isDigit c = fromIntegral (ord c - ord '0')
      | c >= 'A' && c <= 'F' = fromIntegral (ord c - ord 'A' + 10)
      | c >= 'a' && c <= 'f' = fromIntegral (ord c - ord 'a' + 10)
      | otherwise = error ("Invalid hex digit: " ++ [c])
hexByte bt = case readHex bt of
  [(hex, "")] -> fromIntegral (hex :: Integer)
  _ -> error $ "Invalid hex byte; " ++ bt

-- >>> word8ToBytes [64, 20, 0]
-- BtMany ["40","14","00"]
word8ToBytes :: [Word8] -> Bytes
word8ToBytes [] = BtEmpty
word8ToBytes [w8] = BtOne (toHex w8)
word8ToBytes bts = BtMany (map toHex bts)

toHex :: Word8 -> String
toHex w = [digit (w `shiftR` 4), digit (w .&. 0x0F)]
  where
    digit :: Word8 -> Char
    digit n
      | n < 10 = chr (fromIntegral n + ord '0')
      | otherwise = chr (fromIntegral n + ord 'A' - 10)

-- Convert Bytes back to Double
-- >>> btsToNum (BtMany ["40", "14", "00", "00", "00", "00", "00", "00"])
-- Left 5
-- >>> btsToNum (BtMany ["BF", "D0", "00", "00", "00", "00", "00", "00"])
-- Right (-0.25)
-- >>> btsToNum (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
-- Left 42
-- >>> btsToNum (BtMany ["40", "45"])
-- Expected 8 bytes for conversion, got 2
-- >>> btsToNum (BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"])
-- Right NaN
-- >>> btsToNum (BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"])
-- Right Infinity
-- >>> btsToNum (BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"])
-- Right (-Infinity)
-- >>> btsToNum (BtMany ["80", "00", "00", "00", "00", "00", "00", "00"])
-- Right (-0.0)
btsToNum :: Bytes -> Either Int Double
btsToNum hx =
  let bytes = btsToWord8 hx
   in if length bytes /= 8
        then error $ "Expected 8 bytes for conversion, got " ++ show (length bytes)
        else
          let word = toWord64BE bytes
              val = wordToDouble word
           in if isNaN val || isInfinite val || isNegativeZero val
                then Right val
                else case properFraction val of
                  (n, 0.0) -> Left n
                  _ -> Right val
  where
    toWord64BE :: [Word8] -> Word64
    toWord64BE [a, b, c, d, e, f, g, h] =
      fromIntegral a `shiftL` 56
        .|. fromIntegral b `shiftL` 48
        .|. fromIntegral c `shiftL` 40
        .|. fromIntegral d `shiftL` 32
        .|. fromIntegral e `shiftL` 24
        .|. fromIntegral f `shiftL` 16
        .|. fromIntegral g `shiftL` 8
        .|. fromIntegral h
    toWord64BE _ = error "Expected 8 bytes for Double"

-- >>> numToBts 0.0
-- BtMany ["00","00","00","00","00","00","00","00"]
-- >>> numToBts 42
-- BtMany ["40","45","00","00","00","00","00","00"]
-- >>> numToBts (-0.25)
-- BtMany ["BF","D0","00","00","00","00","00","00"]
-- >>> numToBts 5
-- BtMany ["40","14","00","00","00","00","00","00"]
numToBts :: Double -> Bytes
numToBts num = word8ToBytes (unpack (toLazyByteString (word64BE (doubleToWord num))))

-- >>> strToBts "hello"
-- BtMany ["68","65","6C","6C","6F"]
-- >>> strToBts "world"
-- BtMany ["77","6F","72","6C","64"]
-- >>> strToBts ""
-- BtEmpty
-- >>> strToBts "h"
-- BtOne "68"
-- >>> strToBts "h\""
-- BtMany ["68","22"]
-- >>> strToBts "\x01\x01"
-- BtMany ["01","01"]
-- >>> strToBts "Hey"
-- BtMany ["48","65","79"]
strToBts :: String -> Bytes
strToBts "" = BtEmpty
strToBts [ch] = word8ToBytes (unpack (U.fromString [ch]))
strToBts str = word8ToBytes (unpack (U.fromString str))

-- >>> bytesToBts "--"
-- BtEmpty
-- >>> bytesToBts "77-6F"
-- BtMany ["77","6F"]
-- >>> bytesToBts "01-"
-- BtOne "01"
bytesToBts :: String -> Bytes
bytesToBts "--" = BtEmpty
bytesToBts str =
  if length str == 3 && last str == '-'
    then BtOne (init str)
    else BtMany (map T.unpack (T.splitOn "-" (T.pack str)))

-- Convert hex string like "68-65-6C-6C-6F" to "hello"
-- >>> btsToStr (BtMany ["68", "65", "6C", "6C", "6F"])
-- "hello"
-- >>> btsToStr (BtOne "68")
-- "h"
-- >>> btsToStr (BtOne "35")
-- "5"
-- >>> btsToStr (BtMany ["77", "6F", "72", "6C", "64"])
-- "world"
-- >>> btsToStr BtEmpty
-- ""
-- >>> btsToStr (BtMany ["68", "22"])
-- "h\\\""
-- >>> btsToStr (BtMany ["01", "02"])
-- "\\x01\\x02"
btsToStr :: Bytes -> String
btsToStr BtEmpty = ""
btsToStr bytes = escapeStr (btsToUnescapedStr bytes)
  where
    escapeStr :: String -> String
    escapeStr = concatMap escapeChar
      where
        escapeChar :: Char -> String
        escapeChar '"' = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar '\n' = "\\n"
        escapeChar '\t' = "\\t"
        escapeChar c
          | isPrint c && c /= '\\' && c /= '"' = [c]
          | otherwise = printf "\\x%02x" (ord c)

-- >>> btsToUnescapedStr (BtMany ["01", "02"])
-- "\SOH\STX"
-- >>> btsToUnescapedStr (BtMany ["77", "6F", "72", "6C", "64"])
-- "world"
-- >>> btsToUnescapedStr (BtMany ["68", "22"])
-- "h\""
-- >>> btsToUnescapedStr (BtOne "35")
-- "5"
btsToUnescapedStr :: Bytes -> String
btsToUnescapedStr bytes = T.unpack (T.decodeUtf8 (B.pack (btsToWord8 bytes)))
