{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Bytes
  ( numToBts
  , strToBts
  , bytesToBts
  , btsToStr
  , unescapeStr
  , btsToNum
  , btsToUnescapedStr
  , btsIsUtf8
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
  , BytesException (..)
  )
where

import AST
import Control.Exception (Exception, throw)
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

newtype BytesException = InvalidNumberLength Int
  deriving (Eq, Show)

instance Exception BytesException

btsToWord8 :: Bytes -> [Word8]
btsToWord8 BtEmpty = []
btsToWord8 (BtOne bt) = [hexByte bt]
btsToWord8 (BtMany bts) = map hexByte bts
btsToWord8 (BtMeta mt) = error $ "Cannot convert meta bytes to Word8; " ++ T.unpack mt
btsToWord8 (BtAny _) = error "Cannot convert anonymous meta bytes to Word8"

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

btsToNum :: Bytes -> Either Int Double
btsToNum hx =
  let bytes = btsToWord8 hx
   in if length bytes /= 8
        then throw (InvalidNumberLength (length bytes))
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

numToBts :: Double -> Bytes
numToBts num = word8ToBytes (unpack (toLazyByteString (word64BE (doubleToWord num))))

data NonFinite = NfNan | NfPinf | NfNinf
  deriving (Eq, Show)

nonFinites :: [NonFinite]
nonFinites = [NfNan, NfPinf, NfNinf]

nonFiniteName :: NonFinite -> T.Text
nonFiniteName NfNan = "nan"
nonFiniteName NfPinf = "pinf"
nonFiniteName NfNinf = "ninf"

nonFiniteBts :: NonFinite -> Bytes
nonFiniteBts NfNan = BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"]
nonFiniteBts NfPinf = BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"]
nonFiniteBts NfNinf = BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"]

btsToNonFinite :: Bytes -> Maybe NonFinite
btsToNonFinite (BtMeta _) = Nothing
btsToNonFinite (BtAny _) = Nothing
btsToNonFinite bts = find (btsEqual bts . nonFiniteBts) nonFinites

nonFiniteOf :: T.Text -> Maybe NonFinite
nonFiniteOf name = find ((== name) . nonFiniteName) nonFinites

strToBts :: String -> Bytes
strToBts "" = BtEmpty
strToBts [ch] = word8ToBytes (unpack (U.fromString [ch]))
strToBts str = word8ToBytes (unpack (U.fromString str))

bytesToBts :: String -> Bytes
bytesToBts "--" = BtEmpty
bytesToBts str
  | length str == 3 && last str == '-' = BtOne (init str)
  | not (null str) && last str == '-' = error $ "Invalid trailing separator in byte string; " ++ str
  | otherwise = BtMany (map T.unpack (T.splitOn "-" (T.pack str)))

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
          | ord c <= 0xFF = printf "\\x%02x" (ord c)
          | ord c <= 0xFFFF = printf "\\u%04x" (ord c)
          | otherwise = surrogates (ord c)
        surrogates :: Int -> String
        surrogates code =
          let rest = code - 0x10000
              high = 0xD800 + rest `div` 0x400
              low = 0xDC00 + rest `mod` 0x400
           in printf "\\u%04x\\u%04x" high low

unescapeStr :: String -> String
unescapeStr = go
  where
    go :: String -> String
    go "" = ""
    go ('\\' : 'u' : digits) = goUnicode digits
    go ('\\' : 'x' : high : low : rest)
      | Just code <- hexPair high low = chr code : go rest
    go ('\\' : escaped : rest)
      | Just unescaped <- lookup escaped escapes = unescaped : go rest
    go (char : rest) = char : go rest
    goUnicode :: String -> String
    goUnicode (h1 : h2 : h3 : h4 : rest)
      | Just code <- hexQuad h1 h2 h3 h4 =
          if code >= 0xD800 && code <= 0xDBFF
            then case rest of
              ('\\' : 'u' : l1 : l2 : l3 : l4 : rest')
                | Just low <- hexQuad l1 l2 l3 l4
                , low >= 0xDC00 && low <= 0xDFFF ->
                    chr (0x10000 + (code - 0xD800) * 0x400 + (low - 0xDC00)) : go rest'
              _ -> chr code : go rest
            else chr code : go rest
    goUnicode rest = go rest
    hexQuad :: Char -> Char -> Char -> Char -> Maybe Int
    hexQuad a b c d = case readHex [a, b, c, d] of
      [(code, "")] -> Just code
      _ -> Nothing
    hexPair :: Char -> Char -> Maybe Int
    hexPair high low = case readHex [high, low] of
      [(code, "")] -> Just code
      _ -> Nothing
    escapes :: [(Char, Char)]
    escapes = [('"', '"'), ('\\', '\\'), ('n', '\n'), ('t', '\t'), ('r', '\r'), ('b', '\b'), ('f', '\f')]

btsToUnescapedStr :: Bytes -> String
btsToUnescapedStr bytes = T.unpack (T.decodeUtf8 (B.pack (btsToWord8 bytes)))

btsIsUtf8 :: Bytes -> Bool
btsIsUtf8 bytes =
  case T.decodeUtf8' (B.pack (btsToWord8 bytes)) of
    Left _ -> False
    Right _ -> True

btsAnd :: Bytes -> Bytes -> Maybe Bytes
btsAnd = zipBytes (.&.)

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

btsNot :: Bytes -> Bytes
btsNot = word8ToBytes . map complement . btsToWord8

btsConcat :: Bytes -> Bytes -> Bytes
btsConcat left right = word8ToBytes (btsToWord8 left ++ btsToWord8 right)

btsEqual :: Bytes -> Bytes -> Bool
btsEqual left right = btsToWord8 left == btsToWord8 right

btsSize :: Bytes -> Int
btsSize = length . btsToWord8

btsSlice :: Int -> Int -> Bytes -> Maybe Bytes
btsSlice start len bts
  | start < 0 || len < 0 || start + len > length octets = Nothing
  | otherwise = Just (word8ToBytes (take len (drop start octets)))
  where
    octets :: [Word8]
    octets = btsToWord8 bts

btsShift :: Int -> Bytes -> Bytes
btsShift bits bts
  | magnitude >= toInteger size * 8 = word8ToBytes (replicate size 0)
  | bits < 0 = word8ToBytes (map leftwards indices)
  | otherwise = word8ToBytes (map rightwards indices)
  where
    magnitude :: Integer
    magnitude = abs (toInteger bits)
    octets :: [Word8]
    octets = btsToWord8 bts
    size :: Int
    size = length octets
    indices :: [Int]
    indices = [0 .. size - 1]
    modulo :: Int
    modulo = fromInteger (magnitude `mod` 8)
    offset :: Int
    offset = fromInteger (magnitude `div` 8)
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
