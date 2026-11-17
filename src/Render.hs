{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Render (Render (..), Rendering, union, displayWidth, lastLineWidthAtMost, widthAtMost) where

import Bytes (nonFiniteName)
import CST
import Data.Char (GeneralCategory (..), generalCategory)
import Data.List (intersperse)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T

data Rendering = Rendering
  { forwards :: [Text] -> [Text]
  , backwards :: [Text] -> [Text]
  }

instance Semigroup Rendering where
  left <> right = Rendering (forwards left . forwards right) (backwards right . backwards left)

instance Monoid Rendering where
  mempty = Rendering id id

instance IsString Rendering where
  fromString = text . T.pack

text :: Text -> Rendering
text value = Rendering (value :) (value :)

intercalate :: Rendering -> [Rendering] -> Rendering
intercalate separator = mconcat . intersperse separator

class Render a where
  render :: a -> Text
  render = T.concat . flip (forwards . rendering) []

  rendering :: a -> Rendering
  rendering = text . render

  {-# MINIMAL render | rendering #-}

widthAtMost :: Render a => Int -> a -> Int
widthAtMost limit = go 0 . flip (backwards . rendering) []
  where
    go :: Int -> [Text] -> Int
    go width [] = width
    go width (chunk : rest) = case T.unsnoc chunk of
      Nothing -> go width rest
      Just (prefix, character)
        | width + charWidth character > limit -> limit + 1
        | otherwise -> go (width + charWidth character) (prefix : rest)

lastLineWidthAtMost :: Render a => Int -> a -> Int
lastLineWidthAtMost limit = go True 0 . flip (backwards . rendering) []
  where
    go :: Bool -> Int -> [Text] -> Int
    go _ width [] = width
    go atEnd width (chunk : rest) = case T.unsnoc chunk of
      Nothing -> go atEnd width rest
      Just (prefix, character)
        | character == '\n' -> if atEnd then go False width (prefix : rest) else width
        | width + charWidth character > limit -> limit + 1
        | otherwise -> go False (width + charWidth character) (prefix : rest)

instance Render String where
  render = T.pack

instance Render Text where
  render = id

instance Render Int where
  render = T.pack . show

instance Render Char where
  render = T.singleton

instance Render LCB where
  render LCB = "{"
  render BIG_LCB = "\\Big\\{"

instance Render RCB where
  render RCB = "}"
  render BIG_RCB = "\\Big\\}"

instance Render LSB where
  render LSB = "⟦"
  render LSB' = "[["

instance Render RSB where
  render RSB = "⟧"
  render RSB' = "]]"

instance Render COMMA where
  render COMMA = ","
  render NO_COMMA = ""

instance Render ARROW where
  render ARROW = "↦"
  render ARROW' = "->"

instance Render DASHED_ARROW where
  render DASHED_ARROW = "⤍"

instance Render VOID where
  render EMPTY = "∅"
  render QUESTION = "?"

instance Render PHI where
  render PHI = "φ"
  render AT = "@"

instance Render RHO where
  render RHO = "ρ"
  render CARET = "^"
  render RHO' = "\\phiTerminal{\\rho}"

instance Render DELTA where
  render DELTA = "Δ"
  render DELTA' = "D"

instance Render XI where
  render XI = "ξ"
  render DOLLAR = "$"
  render XI' = "\\phiTerminal{\\xi}"

instance Render LAMBDA where
  render LAMBDA = "λ"
  render LAMBDA' = "L"

instance Render GLOBAL where
  render Φ = "Φ"
  render Q = "Q"

instance Render TERMINATION where
  render DEAD = "⊥"
  render T = "T"

instance Render SPACE where
  render SPACE = " "
  render NO_SPACE = ""

instance Render EOL where
  render EOL = "\n"
  render NO_EOL = ""

instance Render DOTS where
  render DOTS = "..."
  render DOTS' = "\\dots"

instance Render BYTES where
  rendering BT_EMPTY = "--"
  rendering (BT_ONE bte) = rendering bte <> "-"
  rendering (BT_MANY bts) = intercalate "-" (map rendering bts)
  rendering (BT_META mt) = rendering mt
  rendering (BT_PIPED bts) = "|" <> rendering bts <> "|"
  rendering (BT_CUT opening omitted closing) = intercalate "-" (map rendering opening) <> "-..(" <> rendering omitted <> "b)..-" <> intercalate "-" (map rendering closing)

instance Render EXCLAMATION where
  render EXCL = "!"
  render NO_EXCL = ""

instance Render META_HEAD where
  render E = "𝑒"
  render E' = "e"
  render N = "𝑛"
  render N' = "n"
  render K = "𝑘"
  render K' = "k"
  render A = "t"
  render TAU = "𝜏"
  render TAU' = "\\tau"
  render I = "𝑖"
  render I' = "i"
  render B = "𝐵"
  render B' = "B"
  render D = "𝛿"
  render D' = "\\delta"
  render D'' = "d"
  render F = "𝑓"
  render F' = "F"
  render F'' = "f"
  render S = "𝜎"
  render S' = "S"
  render S'' = "\\sigma"

instance Render META where
  rendering META{..} = rendering excl <> rendering hd <> rendering rest

instance Render ALPHA' where
  render ALPHA = "α"
  render ALPHA' = "~"

instance Render ALPHA where
  rendering AL_IDX{..} = rendering sym <> rendering idx
  rendering AL_META{..} = rendering sym <> rendering meta

instance Render TAB where
  render TAB{..} = T.replicate indent "  "
  render TAB' = " "
  render NO_TAB = ""

instance Render PAIR where
  rendering PA_TAU{..} = rendering attr <> rendering SPACE <> rendering arrow <> rendering SPACE <> rendering expr
  rendering PA_ALPHA{..} = rendering alpha <> rendering SPACE <> rendering arrow <> rendering SPACE <> rendering expr
  rendering PA_FORMATION{voids = [], attr, arrow, expr} = rendering (PA_TAU attr arrow expr)
  rendering PA_FORMATION{..} = rendering attr <> "(" <> rendering voids <> ")" <> rendering SPACE <> rendering arrow <> rendering SPACE <> rendering expr
  rendering PA_LAMBDA{..} = rendering LAMBDA <> rendering SPACE <> rendering DASHED_ARROW <> rendering SPACE <> rendering func
  rendering PA_LAMBDA'{..} = "L> " <> rendering func
  rendering PA_VOID{..} = rendering attr <> rendering SPACE <> rendering arrow <> rendering SPACE <> rendering void
  rendering PA_DELTA{..} = rendering DELTA <> rendering SPACE <> rendering DASHED_ARROW <> rendering SPACE <> rendering bytes
  rendering PA_DELTA'{..} = "D> " <> rendering bytes
  rendering PA_META_LAMBDA{..} = rendering LAMBDA <> rendering SPACE <> rendering DASHED_ARROW <> rendering SPACE <> rendering meta
  rendering PA_META_LAMBDA'{..} = "L> " <> rendering meta
  rendering PA_META_DELTA{..} = rendering DELTA <> rendering SPACE <> rendering DASHED_ARROW <> rendering SPACE <> rendering meta
  rendering PA_META_DELTA'{..} = "D> " <> rendering meta
  rendering PA_FOLDED{..} = "+" <> rendering count

instance Render BINDINGS where
  rendering BDS_EMPTY{} = mempty
  rendering BDS_PAIR{..} = rendering COMMA <> rendering eol <> rendering tab <> rendering pair <> rendering bindings
  rendering BDS_META{..} = rendering COMMA <> rendering eol <> rendering tab <> rendering meta <> rendering bindings

instance Render APP_BINDING where
  rendering APP_BINDING{..} = rendering pair

instance Render BINDING where
  rendering BI_EMPTY{} = ""
  rendering BI_PAIR{..} = rendering pair <> rendering bindings
  rendering BI_META{..} = rendering meta <> rendering bindings

instance Render APP_ARGUMENT where
  rendering (AA_TAU tau) = rendering tau
  rendering (AA_TAUS taus) = rendering taus
  rendering (AA_EXPRS args) = rendering args

instance Render APP_ARG where
  rendering APP_ARG{..} = rendering expr <> rendering args

instance Render APP_ARGS where
  rendering AAS_EMPTY = mempty
  rendering AAS_EXPR{..} = rendering COMMA <> rendering eol <> rendering tab <> rendering expr <> rendering args

instance Render EXPRESSION where
  rendering EX_GLOBAL{..} = rendering global
  rendering EX_XI{..} = rendering xi
  rendering EX_ATTR{..} = rendering attr
  rendering EX_TERMINATION{..} = rendering termination
  rendering EX_FORMATION{..} = rendering lsb <> rendering eol <> rendering tab <> rendering binding <> rendering eol' <> rendering tab' <> rendering rsb
  rendering EX_DISPATCH{..} = rendering expr <> rendering space <> "." <> rendering space <> rendering attr
  rendering EX_APPLICATION{..} = rendering expr <> rendering space <> "(" <> rendering eol <> rendering tab <> rendering argument <> rendering eol' <> rendering tab' <> ")"
  rendering EX_STRING{..} = "\"" <> rendering str <> "\""
  rendering EX_NUMBER{..} = case properFraction num :: (Int, Double) of
    (whole, 0.0) | abs num < 1.0e7 && not (isNegativeZero num) -> fromString (show whole) <> ".0"
    _ -> fromString (show num)
  rendering EX_NONFINITE{..} = rendering global <> "." <> text (nonFiniteName nonfinite)
  rendering EX_META{..} = rendering meta
  rendering EX_PHI_MEET{..} = "\\phinoMeet{" <> maybe "" (\p -> fromString p <> ":") prefix <> rendering idx <> "}{ " <> rendering expr <> " }"
  rendering EX_PHI_AGAIN{..} = "\\phinoAgain{" <> maybe "" (\p -> fromString p <> ":") prefix <> rendering idx <> "}"
  rendering EX_BYTES{..} = rendering bytes
  rendering EX_SINGLE{..} = case pair of
    PA_TAU{..} -> rendering expr <> colon <> rendering attr
    PA_FORMATION{voids = [], ..} -> rendering expr <> colon <> rendering attr
    PA_VOID{..} -> rendering void <> colon <> rendering attr
    PA_DELTA{..} -> rendering bytes <> colon <> rendering DELTA
    PA_DELTA'{..} -> rendering bytes <> colon <> rendering DELTA'
    PA_META_DELTA{..} -> rendering meta <> colon <> rendering DELTA
    PA_META_DELTA'{..} -> rendering meta <> colon <> rendering DELTA'
    PA_LAMBDA{..} -> rendering func <> colon <> rendering LAMBDA
    PA_LAMBDA'{..} -> rendering func <> colon <> rendering LAMBDA'
    PA_META_LAMBDA{..} -> rendering meta <> colon <> rendering LAMBDA
    PA_META_LAMBDA'{..} -> rendering meta <> colon <> rendering LAMBDA'
    _ -> rendering formation
    where
      colon :: Rendering
      colon = rendering space <> ":" <> rendering space

instance Render [ATTRIBUTE] where
  rendering attrs = intercalate ", " (map rendering attrs)

instance Render ATTRIBUTE where
  rendering AT_LABEL{..} = rendering label
  rendering AT_RHO{..} = rendering rho
  rendering AT_PHI{..} = rendering phi
  rendering AT_LAMBDA{..} = rendering lambda
  rendering AT_DELTA{..} = rendering delta
  rendering AT_META{..} = rendering meta
  rendering AT_REST{..} = rendering dots

instance Render BELONGING where
  render IN = "\\in"
  render NOT_IN = "\\notin"

instance Render SET where
  rendering ST_BINDING{..} = rendering binding
  rendering ST_ATTRIBUTES{..} = "[ " <> intercalate " \\char44{} " (map rendering attrs) <> " ]"

instance Render LOGIC_OPERATOR where
  render AND = "\\phiText{and}"
  render OR = "\\phiText{or}"

instance Render NUMBER where
  rendering IDX_META{..} = rendering meta
  rendering LENGTH{..} = "\\vert " <> rendering binding <> " \\vert"
  rendering DOMAIN{..} = "\\vert \\overline{ " <> rendering binding <> " } \\vert"
  rendering LITERAL{..} = fromString (show num)

instance Render COMPARABLE where
  rendering CMP_ATTR{..} = rendering attr
  rendering CMP_EXPR{..} = rendering expr
  rendering CMP_NUM{..} = rendering num

instance Render EQUAL where
  render EQUAL = "="
  render NOT_EQUAL = "\\not="
  render GREATER = ">"
  render NOT_GREATER = "\\leq"

instance Render CONDITION where
  rendering CO_BELONGS{..} = rendering attr <> " " <> rendering belongs <> " " <> rendering set
  rendering CO_LOGIC{conditions = [cond]} = rendering cond
  rendering CO_LOGIC{..} = intercalate (" " <> rendering operator <> " ") (map renderingWrapped conditions)
    where
      renderingWrapped :: CONDITION -> Rendering
      renderingWrapped CO_LOGIC{conditions = [cond]} = rendering cond
      renderingWrapped cond@CO_LOGIC{} = "\\lparen " <> rendering cond <> " \\rparen"
      renderingWrapped cond = rendering cond
  rendering CO_NF{..} = "\\isnormal{ " <> rendering expr <> " }"
  rendering CO_ABSOLUTE{belongs = NOT_IN, ..} = "\\phinoNotAbsolute{ " <> rendering expr <> " }"
  rendering CO_ABSOLUTE{..} = "\\phinoAbsolute{ " <> rendering expr <> " }"
  rendering CO_NOT{condition = CO_FORMATION{..}} = "\\phinoNotFormation{ " <> rendering expr <> " }"
  rendering CO_NOT{..} = renderingFunc "not" condition
    where
      renderingFunc :: Render a => Rendering -> a -> Rendering
      renderingFunc func renderable = func <> "\\lparen " <> rendering renderable <> " \\rparen"
  rendering CO_COMPARE{..} = rendering left <> " " <> rendering equal <> " " <> rendering right
  rendering CO_MATCHES{..} = "matches\\lparen " <> fromString regex <> ", " <> rendering expr <> " \\rparen"
  rendering CO_PART_OF{..} = "part-of\\lparen " <> rendering expr <> ", " <> rendering binding <> " \\rparen"
  rendering CO_FORMATION{..} = "\\phinoIsFormation{ " <> rendering expr <> " }"
  rendering CO_DISJOINT{..} = rendering (ST_ATTRIBUTES attrs) <> " \\cap " <> renderingUnion groups <> " = \\emptyset"
  rendering CO_SUBSET{attrs = [attr], ..} = rendering attr <> " " <> rendering belongs <> " " <> renderingUnion groups
  rendering CO_SUBSET{belongs = NOT_IN, ..} = rendering (ST_ATTRIBUTES attrs) <> " \\not\\subseteq " <> renderingUnion groups
  rendering CO_SUBSET{..} = rendering (ST_ATTRIBUTES attrs) <> " \\subseteq " <> renderingUnion groups
  rendering CO_EMPTY = ""

union :: [BINDING] -> Text
union = T.concat . flip (forwards . renderingUnion) []

renderingUnion :: [BINDING] -> Rendering
renderingUnion [group] = rendering group
renderingUnion groups = "\\lparen " <> intercalate " \\cup " (map rendering groups) <> " \\rparen"

instance Render EXTRA_ARG where
  rendering ARG_ATTR{..} = rendering attr
  rendering ARG_EXPR{..} = rendering expr
  rendering ARG_BINDING{..} = rendering binding
  rendering ARG_BYTES{..} = rendering bytes

instance Render EXTRA where
  rendering EXTRA{func = "contextualize", args = arg : rest, ..} = "\\phinoContextualize{ " <> rendering arg <> " }{ " <> intercalate ", " (map rendering rest) <> " }{ " <> rendering meta <> " }"
  rendering EXTRA{func = "morph", ..} = rendering meta <> " \\coloneqq \\phinoMorph{ " <> intercalate ", " (map rendering args) <> " }{ e }{ s_1 }"
  rendering EXTRA{func = "named", args = [form], ..} = "\\phinoNamed{ " <> rendering meta <> " }{ " <> rendering form <> " }"
  rendering EXTRA{..} = rendering meta <> " \\coloneqq " <> macro func <> "{ " <> intercalate ", " (map rendering args) <> " }"
    where
      macro :: String -> Rendering
      macro "evaluate" = "\\phinoEvaluate"
      macro "number" = "\\phinoNumber"
      macro "string" = "\\phinoString"
      macro "sum" = "\\phinoSum"
      macro "tau" = "\\phinoTau"
      macro name = "\\" <> text (T.concat (zipWith camel [0 :: Int ..] (T.splitOn "-" (T.pack name))))
      camel :: Int -> Text -> Text
      camel 0 part = part
      camel _ part = T.toTitle part

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
