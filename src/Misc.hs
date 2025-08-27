{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module provides commonly used helper functions for other modules
module Misc
  ( numToBts,
    strToBts,
    bytesToBts,
    btsToStr,
    btsToNum,
    withVoidRho,
    allPathsIn,
    ensuredFile,
    shuffle,
    btsToUnescapedStr,
    attributesFromBindings,
    uniqueBindings,
    uniqueBindings',
    pattern DataObject,
    pattern DataString,
    pattern DataNumber,
  )
where

import Ast
import Control.Exception
import Control.Monad
import Data.Binary.IEEE754
import Data.Bits (Bits (shiftL), (.|.))
import qualified Data.Bits as IOArray
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString, word64BE, word8)
import Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Char (chr, isPrint, ord)
import Data.List (intercalate)
import qualified Data.Set as Set
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

-- Minimal matcher function (required for view pattern)
matchDataoObject :: Expression -> Maybe (String, Bytes)
matchDataoObject
  ( ExApplication
      (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel label))
      ( BiTau
          (AtAlpha 0)
          ( ExApplication
              (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes"))
              ( BiTau
                  (AtAlpha 0)
                  (ExFormation [BiDelta bts, BiVoid AtRho])
                )
            )
        )
    ) = Just (label, bts)
matchDataoObject _ = Nothing

pattern DataString :: Bytes -> Expression
pattern DataString bts = DataObject "string" bts

pattern DataNumber :: Bytes -> Expression
pattern DataNumber bts = DataObject "number" bts

pattern DataObject :: String -> Bytes -> Expression
pattern DataObject label bts <- (matchDataoObject -> Just (label, bts))
  where
    DataObject label bts =
      ExApplication
        (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel label))
        ( BiTau
            (AtAlpha 0)
            ( ExApplication
                (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes"))
                ( BiTau
                    (AtAlpha 0)
                    (ExFormation [BiDelta bts, BiVoid AtRho])
                )
            )
        )

-- Extract attributes from bindings
attributesFromBindings :: [Binding] -> [Attribute]
attributesFromBindings [] = []
attributesFromBindings (bd : bds) =
  let attr = case bd of
        BiTau attr _ -> Just attr
        BiDelta _ -> Just AtDelta
        BiLambda _ -> Just AtLambda
        BiVoid attr -> Just attr
        BiMeta _ -> Nothing
        BiMetaLambda _ -> Just AtLambda
   in case attr of
        Just attr' -> attr' : attributesFromBindings bds
        _ -> attributesFromBindings bds

uniqueBindings' :: [Binding] -> IO [Binding]
uniqueBindings' bds = case uniqueBindings bds of
  Left msg -> throwIO (userError msg)
  Right _ -> pure bds

-- Check if given binding list consists of unique attributes
uniqueBindings :: [Binding] -> Either String [Binding]
uniqueBindings bds = case maybeDuplicatedAttribute bds Set.empty of
  Just attr ->
    Left
      ( printf
          "Duplicated attribute '%s' found in %s"
          (show attr)
          (intercalate ", " (map show (attributesFromBindings bds)))
      )
  _ -> Right bds
  where
    maybeDuplicatedAttribute :: [Binding] -> Set.Set Attribute -> Maybe Attribute
    maybeDuplicatedAttribute [] = const Nothing
    maybeDuplicatedAttribute ((BiTau attr _) : rest) = checkAttr attr rest
    maybeDuplicatedAttribute (BiVoid attr : rest) = checkAttr attr rest
    maybeDuplicatedAttribute (BiLambda _ : rest) = checkAttr AtLambda rest
    maybeDuplicatedAttribute (BiMetaLambda _ : rest) = checkAttr AtLambda rest
    maybeDuplicatedAttribute (BiDelta _ : rest) = checkAttr AtDelta rest
    maybeDuplicatedAttribute (BiMeta _ : rest) = maybeDuplicatedAttribute rest

    checkAttr :: Attribute -> [Binding] -> Set.Set Attribute -> Maybe Attribute
    checkAttr attr rest acc
      | attr `Set.member` acc = Just attr
      | otherwise = maybeDuplicatedAttribute rest (Set.insert attr acc)

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
btsToWord8 (BtOne bt) = case readHex bt of
  [(hex, "")] -> [fromIntegral hex]
  _ -> error $ "Invalid hex byte; " ++ bt
btsToWord8 (BtMany []) = []
btsToWord8 (BtMany (bt : bts)) =
  let [next] = btsToWord8 (BtOne bt)
   in next : btsToWord8 (BtMany bts)

-- >>> word8ToBytes [64, 20, 0]
-- BtMany ["40","14","00"]
word8ToBytes :: [Word8] -> Bytes
word8ToBytes [] = BtEmpty
word8ToBytes [w8] = BtOne (printf "%02X" w8)
word8ToBytes bts = BtMany (map (printf "%02X") bts)

-- Convert Bytes back to Double
-- >>> btsToNum (BtMany ["40", "14", "00", "00", "00", "00", "00", "00"])
-- Left 5
-- >>> btsToNum (BtMany ["BF", "D0", "00", "00", "00", "00", "00", "00"])
-- Right (-0.25)
-- >>> btsToNum (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
-- Left 42
-- >>> btsToNum (BtMany ["40", "45"])
-- Expected 8 bytes for conversion, got 2
btsToNum :: Bytes -> Either Integer Double
btsToNum hx =
  let bytes = btsToWord8 hx
   in if length bytes /= 8
        then error $ "Expected 8 bytes for conversion, got " ++ show (length bytes)
        else
          let word = toWord64BE bytes
              val = wordToDouble word
           in case properFraction val of
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

-- Fast Fisher-Yates with mutable vectors.
-- The function is generated by ChatGPT and claimed as
-- fastest approach comparing to usage IOArray.
-- >>> shuffle [1..20]
-- [7,15,5,18,13,19,3,11,20,2,1,8,14,16,17,12,9,10,6,4]
shuffle :: [a] -> IO [a]
shuffle xs = do
  gen <- newIOGenM =<< newStdGen
  let n = length xs
  v <- V.thaw (V.fromList xs) -- Mutable copy
  forM_ [n - 1, n - 2 .. 1] $ \i -> do
    j <- uniformRM (0, i) gen
    M.swap v i j
  V.toList <$> V.freeze v
