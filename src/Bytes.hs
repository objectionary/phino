{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module is a codec between 'Bytes' and the values they encode:
-- IEEE-754 doubles, UTF-8 strings and raw hex. It also owns the byte-array
-- operations that EO's 'bytes' atoms are built on, since only this module knows
-- how a 'Bytes' maps onto the octets underneath it.
module Bytes
  ( numToBts
  , strToBts
  , bytesToBts
  , btsToStr
  , unescapeStr
  , btsToNum
  , btsToUnescapedStr
  , btsAnd
  , btsOr
  , btsNot
  , btsConcat
  , btsEqual
  , btsSize
  , btsSlice
  , btsShift
  , nonFinites
  , nonFiniteName
  , nonFiniteBts
  , btsToNonFinite
  , nonFiniteOf
  , NonFinite (..)
  )
where

import AST
import Data.Binary.IEEE754
import Data.Bits (Bits (complement, shiftL, shiftR), (.&.), (.|.))
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString, word64BE)
import Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (chr, isDigit, isPrint, ord)
import Data.List (find)
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

-- The three IEEE-754 doubles that are not finite numbers. None of them has a
-- numeric literal to be written with, so the printer spells each one as a
-- dispatch off the root — 'Φ.nan', 'Φ.pinf', 'Φ.ninf' — and the parser reads
-- those names back into the very bytes they stand for (see #1065)
data NonFinite = NfNan | NfPinf | NfNinf
  deriving (Eq, Show)

-- All the non-finite doubles, in the order they are documented in
nonFinites :: [NonFinite]
nonFinites = [NfNan, NfPinf, NfNinf]

-- The attribute name the value is dispatched on
-- >>> nonFiniteName NfPinf
-- "pinf"
nonFiniteName :: NonFinite -> T.Text
nonFiniteName NfNan = "nan"
nonFiniteName NfPinf = "pinf"
nonFiniteName NfNinf = "ninf"

-- The canonical byte form of a non-finite double. The patterns are spelled out
-- instead of being derived from '0 / 0' and '1 / 0' because the sign bit and
-- the payload of a computed NaN are platform-dependent, while the printer and
-- the parser have to agree on one exact pattern
-- >>> nonFiniteBts NfNan
-- BtMany ["7F","F8","00","00","00","00","00","00"]
nonFiniteBts :: NonFinite -> Bytes
nonFiniteBts NfNan = BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"]
nonFiniteBts NfPinf = BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"]
nonFiniteBts NfNinf = BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"]

-- Which non-finite double the given bytes encode, if they encode one at all.
-- Only the three canonical patterns qualify: a NaN carrying a payload, or the
-- negative quiet NaN, has no name of its own and keeps its byte form, so that
-- printing never drops a bit
-- >>> btsToNonFinite (BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"])
-- Just NfNinf
-- >>> btsToNonFinite (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
-- Nothing
-- >>> btsToNonFinite (BtMany ["7F", "F8", "00", "00", "00", "00", "00", "01"])
-- Nothing
btsToNonFinite :: Bytes -> Maybe NonFinite
btsToNonFinite (BtMeta _) = Nothing
btsToNonFinite bts = find (btsEqual bts . nonFiniteBts) nonFinites

-- The non-finite double the given name stands for, if it names one at all
-- >>> nonFiniteOf "ninf"
-- Just NfNinf
-- >>> nonFiniteOf "number"
-- Nothing
nonFiniteOf :: T.Text -> Maybe NonFinite
nonFiniteOf name = find ((== name) . nonFiniteName) nonFinites

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

-- The inverse of the escaping that 'btsToStr' applies, so that a sweet string
-- literal can be turned back into the very bytes it was printed from. A
-- backslash that starts no escape 'btsToStr' can produce is kept as it stands,
-- together with the character behind it
-- >>> unescapeStr "hello"
-- "hello"
-- >>> unescapeStr "h\\\""
-- "h\""
-- >>> unescapeStr "e\\ne"
-- "e\ne"
-- >>> unescapeStr "\\\\"
-- "\\"
-- >>> unescapeStr "\\t"
-- "\t"
-- >>> unescapeStr "\\x01"
-- "\SOH"
unescapeStr :: String -> String
unescapeStr = go
  where
    go :: String -> String
    go "" = ""
    go ('\\' : 'x' : high : low : rest)
      | Just code <- hexPair high low = chr code : go rest
    go ('\\' : escaped : rest)
      | Just unescaped <- lookup escaped escapes = unescaped : go rest
    go (char : rest) = char : go rest
    hexPair :: Char -> Char -> Maybe Int
    hexPair high low = case readHex [high, low] of
      [(code, "")] -> Just code
      _ -> Nothing
    escapes :: [(Char, Char)]
    escapes = [('"', '"'), ('\\', '\\'), ('n', '\n'), ('t', '\t')]

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

-- Bitwise conjunction of two byte arrays, byte by byte. EO's 'BytesRaw.and'
-- refuses operands of different lengths, so there is nothing to yield for them
-- >>> btsAnd (BtMany ["02", "EF"]) (BtMany ["12", "33"])
-- Just (BtMany ["02","23"])
-- >>> btsAnd (BtOne "20") (BtMany ["CA", "FE"])
-- Nothing
btsAnd :: Bytes -> Bytes -> Maybe Bytes
btsAnd = zipBytes (.&.)

-- Bitwise disjunction of two byte arrays, under the same length rule as 'btsAnd'
-- >>> btsOr (BtMany ["02", "EF"]) (BtMany ["12", "33"])
-- Just (BtMany ["12","FF"])
btsOr :: Bytes -> Bytes -> Maybe Bytes
btsOr = zipBytes (.|.)

zipBytes :: (Word8 -> Word8 -> Word8) -> Bytes -> Bytes -> Maybe Bytes
zipBytes op left right
  | length lefts /= length rights = Nothing
  | otherwise = Just (word8ToBytes (zipWith op lefts rights))
  where
    lefts :: [Word8]
    lefts = btsToWord8 left
    rights :: [Word8]
    rights = btsToWord8 right

-- Bitwise negation of every byte
-- >>> btsNot (BtMany ["CA", "FE", "BE", "BE"])
-- BtMany ["35","01","41","41"]
btsNot :: Bytes -> Bytes
btsNot = word8ToBytes . map complement . btsToWord8

-- >>> btsConcat (BtMany ["05", "5E"]) BtEmpty
-- BtMany ["05","5E"]
-- >>> btsConcat BtEmpty BtEmpty
-- BtEmpty
btsConcat :: Bytes -> Bytes -> Bytes
btsConcat left right = word8ToBytes (btsToWord8 left ++ btsToWord8 right)

-- EO's 'bytes.eq' compares the two arrays octet by octet, so two spellings of
-- the same single byte are equal even though their constructors differ
-- >>> btsEqual (BtOne "01") (BtMany ["01"])
-- True
btsEqual :: Bytes -> Bytes -> Bool
btsEqual left right = btsToWord8 left == btsToWord8 right

-- >>> btsSize (BtMany ["F1", "20", "5F"])
-- 3
btsSize :: Bytes -> Int
btsSize = length . btsToWord8

-- Take 'len' bytes starting at 'start'. A window reaching past the end of the
-- array has no answer, which is the case EO's 'cant-slice' fallback exists for
-- >>> btsSlice 1 3 (BtMany ["20", "1F", "EE", "B5", "90"])
-- Just (BtMany ["1F","EE","B5"])
-- >>> btsSlice 3 10 (BtMany ["20", "1F", "EE", "B5", "90"])
-- Nothing
btsSlice :: Int -> Int -> Bytes -> Maybe Bytes
btsSlice start len bts
  | start < 0 || len < 0 || start + len > length octets = Nothing
  | otherwise = Just (word8ToBytes (take len (drop start octets)))
  where
    octets :: [Word8]
    octets = btsToWord8 bts

-- Shift a byte array right by 'bits' bit positions, or left when 'bits' is
-- negative, the way EO's 'BytesRaw.shift' does it. The array keeps its length:
-- bits pushed past either end are dropped and the vacated positions read zero
-- >>> btsShift 1 (BtMany ["C0", "43", "00"])
-- BtMany ["60","21","80"]
-- >>> btsShift (-2147483648) (BtMany ["BF", "F0"])
-- BtMany ["00","00"]
btsShift :: Int -> Bytes -> Bytes
btsShift bits bts
  | bits < 0 = word8ToBytes (map leftwards indices)
  | otherwise = word8ToBytes (map rightwards indices)
  where
    octets :: [Word8]
    octets = btsToWord8 bts
    size :: Int
    size = length octets
    indices :: [Int]
    indices = [0 .. size - 1]
    modulo :: Int
    modulo = abs bits `mod` 8
    offset :: Int
    offset = abs bits `div` 8
    octet :: Int -> Word8
    octet index = octets !! index
    rightwards :: Int -> Word8
    rightwards index
      | source < 0 = 0
      | source > 0 = shifted .|. ((octet (source - 1) `shiftL` (8 - modulo)) .&. carry)
      | otherwise = shifted
      where
        source :: Int
        source = index - offset
        shifted :: Word8
        shifted = octet source `shiftR` modulo
        carry :: Word8
        carry = 0xFF `shiftL` (8 - modulo)
    leftwards :: Int -> Word8
    leftwards index
      | source >= size = 0
      | source + 1 < size = shifted .|. ((octet (source + 1) `shiftR` (8 - modulo)) .&. carry)
      | otherwise = shifted
      where
        source :: Int
        source = index + offset
        shifted :: Word8
        shifted = octet source `shiftL` modulo
        carry :: Word8
        carry = (0x01 `shiftL` modulo) - 1
