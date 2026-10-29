{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Margin (defaultMargin, withMargin, withLines, WithMargin (..)) where

import CST
import Data.Char (GeneralCategory (..), generalCategory)
import qualified Data.Text as T
import Lining (LineFormat (..), ToSingleLine (..))
import Render (Render (..))

defaultMargin :: Int
defaultMargin = 80

withMargin :: (WithMargin a) => Int -> a -> a
withMargin margin = withMargin' (0, margin)

withLines :: (WithMargin a, ToSingleLine a) => LineFormat -> Int -> a -> a
withLines SINGLELINE _ = toSingleLine
withLines MULTILINE margin = withMargin margin

class WithMargin a where
  withMargin' :: (Int, Int) -> a -> a

instance WithMargin EXPRESSION where
  withMargin' _ ex@EX_FORMATION{binding = BI_EMPTY{}} = ex
  withMargin' (extra, margin) ex@EX_FORMATION{tab = tab@(TAB indent), ..} =
    let single = toSingleLine ex
        ex' = EX_FORMATION lsb EOL tab (withMargin' (2 * indent, margin) binding) EOL tab' rsb
     in if lengthOf single + extra <= margin then single else ex'
  withMargin' _ num@EX_NUMBER{} = num
  withMargin' _ str@EX_STRING{} = str
  withMargin' (extra, margin) EX_DISPATCH{..} = EX_DISPATCH (withMargin' (extra, margin - lengthOf attr - 1) expr) space attr
  withMargin' cfg EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (withMargin' cfg expr)
  withMargin' _ EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toSingleLine expr)
  withMargin' cfg@(extra, margin) ex@EX_SINGLE{pair = PA_TAU{..}, ..} =
    let single = toSingleLine ex
        asset = withMargin' (extra, margin - lengthOf attr - 1) expr
     in if lengthOf single + extra <= margin then single else EX_SINGLE (PA_TAU attr arrow asset) space (withMargin' cfg formation)
  withMargin' cfg@(extra, margin) ex@EX_APPLICATION{tab = tab@(TAB indt), ..} =
    let single = toSingleLine ex
        main = withMargin' cfg expr
        singleMain = toSingleLine main
        extra' = displayWidth (last (T.lines (render main))) + 4
        arg' = withMargin' (2 * indt, margin) argument
        singleArg = toSingleLine arg'
     in if
          | lengthOf single + extra <= margin -> single
          | lengthOf singleMain + extra + 1 <= margin -> EX_APPLICATION singleMain space EOL tab arg' EOL tab' indent
          | lengthOf singleArg + extra' <= margin -> EX_APPLICATION main space NO_EOL TAB' singleArg NO_EOL TAB' indent
          | otherwise -> EX_APPLICATION main space EOL tab arg' EOL tab' indent
  withMargin' _ ex = ex

instance WithMargin APP_ARGUMENT where
  withMargin' cfg (AA_TAU tau) = AA_TAU (withMargin' cfg tau)
  withMargin' cfg (AA_TAUS taus) = AA_TAUS (withMargin' cfg taus)
  withMargin' cfg (AA_EXPRS args) = AA_EXPRS (withMargin' cfg args)

instance WithMargin APP_BINDING where
  withMargin' cfg APP_BINDING{..} = APP_BINDING (withMargin' cfg pair)

instance WithMargin APP_ARG where
  withMargin' cfg@(extra, margin) arg@APP_ARG{..} =
    let single = toSingleLine arg
     in if lengthOf single + extra <= margin then single else APP_ARG (withMargin' cfg expr) (withMargin' cfg args)

instance WithMargin APP_ARGS where
  withMargin' cfg AAS_EXPR{..} = AAS_EXPR EOL tab (withMargin' cfg expr) (withMargin' cfg args)
  withMargin' _ args = args

instance WithMargin BINDING where
  withMargin' cfg BI_PAIR{..} = BI_PAIR (withMargin' (commas cfg) pair) (withMargin' cfg bindings) tab
    where
      commas (extra, margin) = case bindings of
        BDS_EMPTY{} -> (extra, margin)
        _ -> (extra + 1, margin)
  withMargin' cfg BI_META{..} = BI_META meta (withMargin' cfg bindings) tab
  withMargin' _ bd = bd

instance WithMargin BINDINGS where
  withMargin' cfg BDS_PAIR{..} = BDS_PAIR EOL tab (withMargin' (commas cfg) pair) (withMargin' cfg bindings)
    where
      commas (extra, margin) = case bindings of
        BDS_EMPTY{} -> (extra, margin)
        _ -> (extra + 1, margin)
  withMargin' cfg BDS_META{..} = BDS_META EOL tab meta (withMargin' cfg bindings)
  withMargin' _ bds = bds

instance WithMargin PAIR where
  withMargin' (extra, margin) pa@PA_TAU{..} =
    let single = toSingleLine pa
        extra' = extra + lengthOf attr + lengthOf arrow + 2
        pa' = PA_TAU attr arrow (withMargin' (extra', margin) expr)
     in if lengthOf single + extra <= margin then single else pa'
  withMargin' (extra, margin) pa@PA_ALPHA{..} =
    let single = toSingleLine pa
        extra' = extra + lengthOf alpha + lengthOf arrow + 2
        pa' = PA_ALPHA alpha arrow (withMargin' (extra', margin) expr)
     in if lengthOf single + extra <= margin then single else pa'
  withMargin' (extra, margin) pa@PA_FORMATION{..} =
    let single = toSingleLine pa
        extra' = extra + lengthOf attr + lengthOf voids + lengthOf arrow + 4
        pa' = PA_FORMATION attr voids arrow (withMargin' (extra', margin) expr)
     in if lengthOf single + extra <= margin then single else pa'
  withMargin' _ pa = pa

instance WithMargin CONDITION where
  withMargin' _ co = co

instance WithMargin EXTRA where
  withMargin' _ = id

lengthOf :: Render a => a -> Int
lengthOf renderable = displayWidth (render renderable)

displayWidth :: T.Text -> Int
displayWidth = T.foldl' addWidth 0
  where
    addWidth :: Int -> Char -> Int
    addWidth acc character = acc + charWidth character

charWidth :: Char -> Int
charWidth character
  | fromEnum character < 0x300 = 1
  | blank = 0
  | isWideCodePoint (fromEnum character) = 2
  | otherwise = 1
  where
    blank :: Bool
    blank = generalCategory character `elem` [NonSpacingMark, EnclosingMark, Format]

    isWideCodePoint :: Int -> Bool
    isWideCodePoint codePoint = any (inRange codePoint) wideRanges

    inRange :: Int -> (Int, Int) -> Bool
    inRange codePoint (lo, hi) = codePoint >= lo && codePoint <= hi

    wideRanges :: [(Int, Int)]
    wideRanges =
      [ (0x1100, 0x115F)
      , (0x231A, 0x231B)
      , (0x2329, 0x232A)
      , (0x23E9, 0x23EC)
      , (0x23F0, 0x23F0)
      , (0x23F3, 0x23F3)
      , (0x25FD, 0x25FE)
      , (0x2614, 0x2615)
      , (0x2648, 0x2653)
      , (0x267F, 0x267F)
      , (0x2693, 0x2693)
      , (0x26A1, 0x26A1)
      , (0x26AA, 0x26AB)
      , (0x26BD, 0x26BE)
      , (0x26C4, 0x26C5)
      , (0x26CE, 0x26CE)
      , (0x26D4, 0x26D4)
      , (0x26EA, 0x26EA)
      , (0x26F2, 0x26F3)
      , (0x26F5, 0x26F5)
      , (0x26FA, 0x26FA)
      , (0x26FD, 0x26FD)
      , (0x2705, 0x2705)
      , (0x270A, 0x270B)
      , (0x2728, 0x2728)
      , (0x274C, 0x274C)
      , (0x274E, 0x274E)
      , (0x2753, 0x2755)
      , (0x2757, 0x2757)
      , (0x2795, 0x2797)
      , (0x27B0, 0x27B0)
      , (0x27BF, 0x27BF)
      , (0x2B1B, 0x2B1C)
      , (0x2B50, 0x2B50)
      , (0x2B55, 0x2B55)
      , (0x2E80, 0x303E)
      , (0x3041, 0x33FF)
      , (0x3400, 0x4DBF)
      , (0x4E00, 0x9FFF)
      , (0xA000, 0xA4CF)
      , (0xA960, 0xA97C)
      , (0xAC00, 0xD7A3)
      , (0xF900, 0xFAFF)
      , (0xFE10, 0xFE19)
      , (0xFE30, 0xFE4F)
      , (0xFE50, 0xFE52)
      , (0xFE54, 0xFE66)
      , (0xFE68, 0xFE6B)
      , (0xFF00, 0xFF60)
      , (0xFFE0, 0xFFE6)
      , (0x16FE0, 0x16FE4)
      , (0x16FF0, 0x16FF1)
      , (0x17000, 0x187F7)
      , (0x18800, 0x18CD5)
      , (0x18D00, 0x18D08)
      , (0x1B000, 0x1B11E)
      , (0x1B150, 0x1B152)
      , (0x1B164, 0x1B167)
      , (0x1B170, 0x1B2FB)
      , (0x1F004, 0x1F004)
      , (0x1F0CF, 0x1F0CF)
      , (0x1F18E, 0x1F18E)
      , (0x1F191, 0x1F19A)
      , (0x1F200, 0x1F202)
      , (0x1F210, 0x1F23B)
      , (0x1F240, 0x1F248)
      , (0x1F250, 0x1F251)
      , (0x1F260, 0x1F265)
      , (0x1F300, 0x1FAFF)
      , (0x20000, 0x3FFFD)
      ]
