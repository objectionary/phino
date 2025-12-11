-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Must (Must (..), inRange, exceedsUpperBound, validateMust) where

import Data.List (isInfixOf)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Must
  = MtDisabled
  | MtExact Int
  | MtRange (Maybe Int) (Maybe Int)
  deriving (Eq)

instance Show Must where
  show MtDisabled = "0"
  show (MtExact n) = show n
  show (MtRange Nothing Nothing) = ".."
  show (MtRange Nothing (Just hi)) = ".." ++ show hi
  show (MtRange (Just lo) Nothing) = show lo ++ ".."
  show (MtRange (Just lo) (Just hi)) = show lo ++ ".." ++ show hi

instance Read Must where
  readsPrec _ "0" = [(MtDisabled, "")]
  readsPrec _ s
    | ".." `isInfixOf` s = parseRange s
    | otherwise = parseExact s
    where
      parseRange :: String -> [(Must, String)]
      parseRange str = case break (== '.') str of
        (loStr, '.' : '.' : hiStr) ->
          let loPart = if null loStr then Nothing else readMaybe loStr
              hiPart = if null hiStr then Nothing else readMaybe hiStr
           in case (loPart, hiPart, null loStr, null hiStr) of
                (Nothing, Nothing, False, False) -> [] -- Invalid range: non-numeric values
                (Nothing, Nothing, True, True) -> [] -- Invalid range: empty range '..'
                (Nothing, Just hi, True, False) ->
                  [(MtRange Nothing (Just hi), "") | hi >= 0]
                (Just lo, Nothing, False, True) ->
                  [(MtRange (Just lo) Nothing, "") | lo >= 0]
                (Just lo, Just hi, False, False) ->
                  [(MtRange (Just lo) (Just hi), "") | lo >= 0 && hi >= 0 && lo <= hi]
                _ -> [] -- Invalid range format
        _ -> [] -- Invalid range: expected format like '3..5', '3..', or '..5'
      parseExact :: String -> [(Must, String)]
      parseExact str = case readMaybe str of
        Just n | n >= 0 -> [(if n == 0 then MtDisabled else MtExact n, "")]
        Just _ -> [] -- Invalid value: must be non-negative
        Nothing -> [] -- Invalid value: expected integer

inRange :: Must -> Int -> Bool
inRange MtDisabled _ = True
inRange (MtExact expected) actual = actual == expected
inRange (MtRange minVal maxVal) actual =
  checkMin && checkMax
  where
    checkMin = maybe True (<= actual) minVal
    checkMax = maybe True (>= actual) maxVal

-- | Check if a value exceeds the upper bound of the range
exceedsUpperBound :: Must -> Int -> Bool
exceedsUpperBound MtDisabled _ = False
exceedsUpperBound (MtExact n) current = current > n
exceedsUpperBound (MtRange _ (Just hi)) current = current > hi
exceedsUpperBound (MtRange _ Nothing) _ = False

validateMust :: Must -> Maybe String
validateMust MtDisabled = Nothing
validateMust (MtExact n) | n <= 0 = Just "--must exact value must be positive"
validateMust (MtRange (Just lo) _) | lo < 0 = Just "--must minimum must be non-negative"
validateMust (MtRange _ (Just hi)) | hi < 0 = Just "--must maximum must be non-negative"
validateMust (MtRange Nothing _) = Nothing
validateMust (MtRange _ Nothing) = Nothing
validateMust (MtRange (Just lo) (Just hi))
  | lo > hi = Just (printf "--must range invalid: minimum (%d) is greater than maximum (%d)" lo hi)
  | otherwise = Nothing
validateMust _ = Nothing
