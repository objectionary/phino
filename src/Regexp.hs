-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Regexp where

import Control.Exception
import Data.Array (bounds, (!))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Text.Regex.PCRE.ByteString
import qualified Text.Regex.PCRE.ByteString as R

compile :: B.ByteString -> IO Regex
compile pat = do
  compiled <- R.compile compBlank execBlank pat
  case compiled of
    Left (_, err) -> throwIO (userError ("Regex compilation failed: " ++ err))
    Right regex -> pure regex

extractGroups :: Regex -> B.ByteString -> IO [B.ByteString]
extractGroups regex input = do
  result <- execute regex input
  case result of
    Left _ -> pure []
    Right Nothing -> pure []
    Right (Just arr) ->
      let (start, end) = bounds arr
          groups =
            [ let (off, len) = arr ! i
               in if off == -1 then B.empty else B.take len (B.drop off input)
              | i <- [start .. end]
            ]
       in pure groups

substituteGroups :: B.ByteString -> [B.ByteString] -> B.ByteString
substituteGroups rep groups = B.concat (go (B.unpack rep))
  where
    go [] = []
    go ('$' : rest) =
      let (digits, afterDigits) = span isDigit rest
       in if null digits
            then B.singleton '$' : go rest
            else
              let idx = read digits
                  val = fromMaybe (B.pack ('$' : digits)) (safeIndex idx groups)
               in val : go afterDigits
    go (c : rest) = B.singleton c : go rest
    safeIndex i xs
      | i >= 0 && i < length xs = Just (xs !! i)
      | otherwise = Nothing

replaceFirst :: Regex -> B.ByteString -> B.ByteString -> IO B.ByteString
replaceFirst regex rep input = do
  result <- execute regex input
  case result of
    Left _ -> return input
    Right Nothing -> return input
    Right (Just arr) -> do
      groups <- extractGroups regex input
      let (off, len) = arr ! 0
          (before, rest) = B.splitAt off input
          (_, after) = B.splitAt len rest
          replacement = substituteGroups rep groups
      return $ B.concat [before, replacement, after]

replaceAll :: Regex -> B.ByteString -> B.ByteString -> IO B.ByteString
replaceAll regex rep input = go input B.empty
  where
    go bs acc = do
      result <- execute regex bs
      case result of
        Left _ -> return $ B.append acc bs
        Right Nothing -> return $ B.append acc bs
        Right (Just arr) -> do
          let (off, len) = arr ! 0
              (before, rest1) = B.splitAt off bs
              (_, rest2) = B.splitAt len rest1
          groups <- extractGroups regex bs
          let replacement = substituteGroups rep groups
          go rest2 (B.concat [acc, before, replacement])
