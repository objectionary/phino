-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Must (Must(..), inRange, exceedsUpperBound) where

import Data.List (isInfixOf)
import Text.Read (readMaybe)

data Must
  = MustDisabled
  | MustExact Integer
  | MustRange (Maybe Integer) (Maybe Integer)
  deriving (Eq)

instance Show Must where
  show MustDisabled = "disabled"
  show (MustExact n) = show n
  show (MustRange Nothing Nothing) = ".."
  show (MustRange Nothing (Just max)) = ".." ++ show max
  show (MustRange (Just min) Nothing) = show min ++ ".."
  show (MustRange (Just min) (Just max)) = show min ++ ".." ++ show max

instance Read Must where
  readsPrec _ "0" = [(MustDisabled, "")]
  readsPrec _ s 
    | ".." `isInfixOf` s = parseRange s
    | otherwise = parseExact s
    where
      parseRange :: String -> [(Must, String)]
      parseRange str = case break (== '.') str of
        (minStr, '.':'.':maxStr) ->
          let minPart = if null minStr then Nothing else readMaybe minStr
              maxPart = if null maxStr then Nothing else readMaybe maxStr
          in case (minPart, maxPart, null minStr, null maxStr) of
            (Nothing, Nothing, False, False) -> [] -- Invalid range: non-numeric values
            (Nothing, Nothing, True, True) -> [] -- Invalid range: empty range '..'
            (Nothing, Just max, True, False) ->
              [(MustRange Nothing (Just max), "") | max >= 0]
            (Just min, Nothing, False, True) ->
              [(MustRange (Just min) Nothing, "") | min >= 0]
            (Just min, Just max, False, False) ->
              [(MustRange (Just min) (Just max), "") | min >= 0 && max >= 0 && min <= max]
            _ -> [] -- Invalid range format
        _ -> [] -- Invalid range: expected format like '3..5', '3..', or '..5'
      
      parseExact :: String -> [(Must, String)]
      parseExact str = case readMaybe str of
        Just n | n >= 0 -> [(if n == 0 then MustDisabled else MustExact n, "")]
        Just _ -> [] -- Invalid value: must be non-negative
        Nothing -> [] -- Invalid value: expected integer

inRange :: Must -> Integer -> Bool
inRange MustDisabled _ = True
inRange (MustExact expected) actual = actual == expected
inRange (MustRange minVal maxVal) actual =
  checkMin && checkMax
  where
    checkMin = maybe True (<= actual) minVal
    checkMax = maybe True (>= actual) maxVal

-- | Check if a value exceeds the upper bound of the range
exceedsUpperBound :: Must -> Integer -> Bool
exceedsUpperBound MustDisabled _ = False
exceedsUpperBound (MustExact n) current = current > n
exceedsUpperBound (MustRange _ (Just max)) current = current > max
exceedsUpperBound (MustRange _ Nothing) _ = False
