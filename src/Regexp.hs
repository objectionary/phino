-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Regexp where

import Control.Exception
import Data.Array (bounds, (!))
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit, ord)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Text.Regex.PCRE.ByteString as R

data Regex = Regex R.Regex R.Regex

compile :: B.ByteString -> IO Regex
compile pat = do
  first <- compileWith R.execBlank
  continuation <- compileWith R.execNotBOL
  pure (Regex first continuation)
  where
    compileWith option = do
      compiled <- R.compile R.compUTF8 option pat
      case compiled of
        Left (_, err) -> throwIO (userError ("Regex compilation failed: " ++ err))
        Right regex -> pure regex

match :: B.ByteString -> B.ByteString -> IO Bool
match pattern input = do
  Regex regex _ <- compile pattern
  res <- R.execute regex input
  case res of
    Left _ -> pure False
    Right Nothing -> pure False
    Right (Just _) -> pure True

extractGroups :: Regex -> B.ByteString -> IO [B.ByteString]
extractGroups (Regex regex _) input = extractGroupsWith regex input

extractGroupsWith :: R.Regex -> B.ByteString -> IO [B.ByteString]
extractGroupsWith regex input = do
  result <- R.execute regex input
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
    go :: String -> [B.ByteString]
    go [] = []
    go ('$' : rest) =
      let (digits, afterDigits) = span isDigit rest
       in if null digits
            then B.singleton '$' : go rest
            else
              let val = case readMaybe digits of
                    Nothing -> B.pack ('$' : digits)
                    Just idx -> fromMaybe (B.pack ('$' : digits)) (safeIndex idx groups)
               in val : go afterDigits
    go (c : rest) = B.singleton c : go rest
    safeIndex :: Int -> [B.ByteString] -> Maybe B.ByteString
    safeIndex i xs
      | i >= 0 && i < length xs = Just (xs !! i)
      | otherwise = Nothing

replaceFirst :: Regex -> B.ByteString -> B.ByteString -> IO B.ByteString
replaceFirst (Regex regex _) rep input = do
  result <- R.execute regex input
  case result of
    Left _ -> return input
    Right Nothing -> return input
    Right (Just arr) -> do
      groups <- extractGroupsWith regex input
      let (off, len) = arr ! 0
          (before, rest) = B.splitAt off input
          (_, after) = B.splitAt len rest
          replacement = substituteGroups rep groups
      return $ B.concat [before, replacement, after]

replaceAll :: Regex -> B.ByteString -> B.ByteString -> IO B.ByteString
replaceAll (Regex first continuation) rep input = go True input B.empty
  where
    go :: Bool -> B.ByteString -> B.ByteString -> IO B.ByteString
    go initial bs acc = do
      let regex = if initial then first else continuation
      result <- R.execute regex bs
      case result of
        Left _ -> return $ B.append acc bs
        Right Nothing -> return $ B.append acc bs
        Right (Just arr) -> do
          let (off, len) = arr ! 0
              (before, rest1) = B.splitAt off bs
              (_, rest2) = B.splitAt len rest1
          groups <- extractGroupsWith regex bs
          let replacement = substituteGroups rep groups
          if len == 0
            then
              let next = B.take (utf8Width rest2) rest2
               in if B.null next
                    then return $ B.concat [acc, before, replacement]
                    else go False (B.drop (B.length next) rest2) (B.concat [acc, before, replacement, next])
            else go False rest2 (B.concat [acc, before, replacement])
    utf8Width :: B.ByteString -> Int
    utf8Width bytes =
      case B.uncons bytes of
        Nothing -> 0
        Just (lead, rest)
          | ord lead < 0xC0 -> 1
          | ord lead < 0xE0 -> continuationWidth 2 rest
          | ord lead < 0xF0 -> continuationWidth 3 rest
          | ord lead < 0xF8 -> continuationWidth 4 rest
          | otherwise -> 1
    continuationWidth :: Int -> B.ByteString -> Int
    continuationWidth width rest =
      if B.length rest >= width - 1 && B.all isContinuation (B.take (width - 1) rest)
        then width
        else 1
    isContinuation :: Char -> Bool
    isContinuation byte = ord byte >= 0x80 && ord byte < 0xC0
