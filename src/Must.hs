-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Must (Must (..), inRange, exceedsUpperBound, validateMust) where

import Data.List (isInfixOf)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Must
  = MtDisabled
  | MtExact Integer
  | MtRange (Maybe Integer) (Maybe Integer)
  deriving (Eq)

instance Show Must where
  show MtDisabled = "0"
  show (MtExact n) = show n
  show (MtRange Nothing Nothing) = ".."
  show (MtRange Nothing (Just max)) = ".." ++ show max
  show (MtRange (Just min) Nothing) = show min ++ ".."
  show (MtRange (Just min) (Just max)) = show min ++ ".." ++ show max

instance Read Must where
  readsPrec _ "0" = [(MtDisabled, "")]
  readsPrec _ s
    | ".." `isInfixOf` s = parseRange s
    | otherwise = parseExact s
   where
    parseRange :: String -> [(Must, String)]
    parseRange str = case break (== '.') str of
      (minStr, '.' : '.' : maxStr) ->
        let minPart = if null minStr then Nothing else readMaybe minStr
            maxPart = if null maxStr then Nothing else readMaybe maxStr
         in case (minPart, maxPart, null minStr, null maxStr) of
              (Nothing, Nothing, False, False) -> [] -- Invalid range: non-numeric values
              (Nothing, Nothing, True, True) -> [] -- Invalid range: empty range '..'
              (Nothing, Just max, True, False) ->
                [(MtRange Nothing (Just max), "") | max >= 0]
              (Just min, Nothing, False, True) ->
                [(MtRange (Just min) Nothing, "") | min >= 0]
              (Just min, Just max, False, False) ->
                [(MtRange (Just min) (Just max), "") | min >= 0 && max >= 0 && min <= max]
              _ -> [] -- Invalid range format
      _ -> [] -- Invalid range: expected format like '3..5', '3..', or '..5'
    parseExact :: String -> [(Must, String)]
    parseExact str = case readMaybe str of
      Just n | n >= 0 -> [(if n == 0 then MtDisabled else MtExact n, "")]
      Just _ -> [] -- Invalid value: must be non-negative
      Nothing -> [] -- Invalid value: expected integer

inRange :: Must -> Integer -> Bool
inRange MtDisabled _ = True
inRange (MtExact expected) actual = actual == expected
inRange (MtRange minVal maxVal) actual =
  checkMin && checkMax
 where
  checkMin = maybe True (<= actual) minVal
  checkMax = maybe True (>= actual) maxVal

-- | Check if a value exceeds the upper bound of the range
exceedsUpperBound :: Must -> Integer -> Bool
exceedsUpperBound MtDisabled _ = False
exceedsUpperBound (MtExact n) current = current > n
exceedsUpperBound (MtRange _ (Just max)) current = current > max
exceedsUpperBound (MtRange _ Nothing) _ = False

validateMust :: Must -> Maybe String
validateMust MtDisabled = Nothing
validateMust (MtExact n) | n <= 0 = Just "--must exact value must be positive"
validateMust (MtRange (Just minVal) _) | minVal < 0 = Just "--must minimum must be non-negative"
validateMust (MtRange _ (Just maxVal)) | maxVal < 0 = Just "--must maximum must be non-negative"
validateMust (MtRange Nothing _) = Nothing
validateMust (MtRange _ Nothing) = Nothing
validateMust (MtRange (Just min) (Just max))
  | min > max = Just (printf "--must range invalid: minimum (%d) is greater than maximum (%d)" min max)
  | otherwise = Nothing
validateMust _ = Nothing
