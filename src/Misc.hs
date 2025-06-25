{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module provides commonly used helper functions for other modules
module Misc where

import Ast
import Control.Exception
import Control.Monad
import Data.Binary.IEEE754
import Data.Bits (Bits (shiftL), (.|.))
import qualified Data.Bits as IOArray
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString, word64BE)
import Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (chr)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Data.Word (Word64, Word8)
import Numeric (readHex)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.Random.Stateful
import Text.Printf (printf)

data FsException
  = FileDoesNotExist {file :: FilePath}
  | DirectoryDoesNotExist {dir :: FilePath}
  deriving (Exception)

instance Show FsException where
  show FileDoesNotExist {..} = printf "File '%s' does not exist" file
  show DirectoryDoesNotExist {..} = printf "Directory '%s' does not exist" dir

-- Add void rho binding to the end of the list of any rho binding is not present
withVoidRho :: [Binding] -> [Binding]
withVoidRho bds = withVoidRho' bds False
  where
    withVoidRho' :: [Binding] -> Bool -> [Binding]
    withVoidRho' [] hasRho = [BiVoid AtRho | not hasRho]
    withVoidRho' (bd : bds) hasRho =
      case bd of
        BiMeta _ -> bd : bds
        BiVoid (AtMeta _) -> bd : bds
        BiTau (AtMeta _) _ -> bd : bds
        BiVoid AtRho -> bd : withVoidRho' bds True
        BiTau AtRho _ -> bd : withVoidRho' bds True
        _ -> bd : withVoidRho' bds hasRho

ensuredFile :: FilePath -> IO FilePath
ensuredFile pth = do
  exists <- doesFileExist pth
  if exists then pure pth else throwIO (FileDoesNotExist pth)

-- Recursively collect all file paths in provided directory
allPathsIn :: FilePath -> IO [FilePath]
allPathsIn dir = do
  exists <- doesDirectoryExist dir
  names <- if exists then listDirectory dir else throwIO (DirectoryDoesNotExist dir)
  let nested = map (dir </>) names
  paths <-
    forM
      nested
      ( \path -> do
          isDir <- doesDirectoryExist path
          if isDir
            then allPathsIn path
            else return [path]
      )
  return (concat paths)

-- >>> hexToBts "40-14-00-00-00-00-00-00"
-- [64,20,0,0,0,0,0,0]
-- >>> hexToBts "68-65-6C-6C-6F"
-- [104,101,108,108,111]
hexToBts :: String -> [Word8]
hexToBts = map readHexByte . splitOnDash
  where
    splitOnDash = words . map (\c -> if c == '-' then ' ' else c)
    readHexByte hx = case readHex hx of
      [(v, "")] -> fromIntegral v
      _ -> error $ "Invalid hex byte: " ++ hx

btsToHex :: [Word8] -> String
btsToHex bts = intercalate "-" (map (printf "%02X") bts)

-- Convert hex string back to Double
-- >>> hexToNum "40-14-00-00-00-00-00-00"
-- Left 5
-- >>> hexToNum "BF-D0-00-00-00-00-00-00"
-- Right (-0.25)
-- >>> hexToNum "40-45-00-00-00-00-00-00"
-- Left 42
hexToNum :: String -> Either Integer Double
hexToNum hx =
  let bytes = hexToBts hx
      word = toWord64BE bytes
      val = wordToDouble word
   in case properFraction val of
        (n, 0.0) -> Left n
        _ -> Right val
  where
    toWord64BE :: [Word8] -> Word64
    toWord64BE bts =
      case bts of
        [a, b, c, d, e, f, g, h] ->
          fromIntegral a `shiftL` 56
            .|. fromIntegral b `shiftL` 48
            .|. fromIntegral c `shiftL` 40
            .|. fromIntegral d `shiftL` 32
            .|. fromIntegral e `shiftL` 24
            .|. fromIntegral f `shiftL` 16
            .|. fromIntegral g `shiftL` 8
            .|. fromIntegral h
        _ -> error "Expected 8 bytes for Double"

-- >>> numToHex 0.0
-- "00-00-00-00-00-00-00-00"
-- >>> numToHex 42
-- "40-45-00-00-00-00-00-00"
-- >>> numToHex (-0.25)
-- "BF-D0-00-00-00-00-00-00"
-- >>> numToHex 5
-- "40-14-00-00-00-00-00-00"
numToHex :: Double -> String
numToHex num = btsToHex (unpack (toLazyByteString (word64BE (doubleToWord num))))

-- >>> strToHex "hello"
-- "68-65-6C-6C-6F"
-- >>> strToHex "world"
-- "77-6F-72-6C-64"
-- >>> strToHex ""
-- "--"
-- >>> strToHex "h"
-- "68-"
strToHex :: String -> String
strToHex "" = "--"
strToHex [ch] = btsToHex (unpack (U.fromString [ch])) ++ "-"
strToHex str = btsToHex (unpack (U.fromString str))

-- Convert hex string like "68-65-6C-6C-6F" to "hello"
-- >>> hexToStr "68-65-6C-6C-6F"
-- "hello"
-- >>> hexToStr "--"
-- ""
-- >>> hexToStr "68-"
-- "h"
-- >>> hexToStr "77-6F-72-6C-64"
-- "world"
hexToStr :: String -> String
hexToStr "--" = ""
hexToStr hx = T.unpack $ T.decodeUtf8 $ B.pack (hexToBts cleaned)
  where
    -- Remove trailing dash if present (from single-char case)
    cleaned = if last hx == '-' then init hx else hx

-- Fast Fisher-Yates with mutable vectors.
-- The function is generated by ChatGPT and claimed as
-- fastest approach comparing to usage IOArray.
-- >>> shuffle [1..20]
-- [20,5,6,1,12,10,7,8,9,11,14,15,13,2,4,16,3,19,18,17]
shuffle :: [a] -> IO [a]
shuffle xs = do
  gen <- newIOGenM =<< newStdGen
  let n = length xs
  v <- V.thaw (V.fromList xs) -- Mutable copy
  forM_ [n - 1, n - 2 .. 1] $ \i -> do
    j <- uniformRM (0, i) gen
    M.swap v i j
  V.toList <$> V.freeze v
