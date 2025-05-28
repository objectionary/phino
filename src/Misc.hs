{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module provides commonly used helper functions for other modules
module Misc where

import Ast
import Control.Exception
import Control.Monad
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import Data.Binary.IEEE754  
import Data.ByteString.Builder (word64BE, toLazyByteString)
import Data.List (intercalate)
import Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Word (Word8)

data FsException
  = FileDoesNotExist {file :: FilePath}
  | DirectoryDoesNotExist {dir :: FilePath}
  deriving (Exception)

instance Show FsException where
  show FileDoesNotExist {..} = printf "File '%s' does not exist" file
  show DirectoryDoesNotExist {..} = printf "Directory '%s' does not exist" dir

-- List without element by given index
withoutAt :: Int -> [a] -> [a]
withoutAt i xs = take i xs ++ drop (i + 1) xs

-- Returns True if given binding is BiMeta (!B)
isMetaBinding :: Binding -> Bool
isMetaBinding (BiMeta _) = True
isMetaBinding _ = False

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

btsToHex :: [Word8] -> String
btsToHex bts = intercalate "-" (map (printf "%02X") bts)

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
strToHex :: String -> String
strToHex str = btsToHex (unpack (U.fromString str))
