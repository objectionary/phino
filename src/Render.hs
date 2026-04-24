{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Render where

import CST
import Data.Text (Text)
import qualified Data.Text as T

class Render a where
  render :: a -> Text

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

instance Render DELTA where
  render DELTA = "Δ"

instance Render XI where
  render XI = "ξ"
  render DOLLAR = "$"

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
  render BT_EMPTY = "--"
  render (BT_ONE bte) = render bte <> "-"
  render (BT_MANY bts) = T.intercalate "-" (map render bts)
  render (BT_META mt) = render mt

instance Render EXCLAMATION where
  render EXCL = "!"
  render NO_EXCL = ""

instance Render META_HEAD where
  render E = "𝑒"
  render E' = "e"
  render A = "a"
  render TAU = "𝜏"
  render TAU' = "\\tau"
  render B = "𝐵"
  render B' = "B"
  render D = "δ"
  render D' = "d"
  render TAIL = "t"
  render F = "F"

instance Render META where
  render META{..} = render excl <> render hd <> render rest

instance Render ALPHA where
  render ALPHA = "α"
  render ALPHA' = "~"

instance Render TAB where
  render TAB{..} = T.replicate indent "  "
  render TAB' = " "
  render NO_TAB = ""

instance Render PROGRAM where
  render PR_SWEET{..} = render lcb <> render space <> render expr <> render space <> render rcb
  render PR_SALTY{..} = render global <> render SPACE <> render arrow <> render SPACE <> render expr

instance Render PAIR where
  render PA_TAU{..} = render attr <> render SPACE <> render arrow <> render SPACE <> render expr
  render PA_FORMATION{voids = [], attr, arrow, expr} = render (PA_TAU attr arrow expr)
  render PA_FORMATION{..} = render attr <> "(" <> render voids <> ")" <> render SPACE <> render arrow <> render SPACE <> render expr
  render PA_LAMBDA{..} = render LAMBDA <> render SPACE <> render DASHED_ARROW <> render SPACE <> render func
  render PA_LAMBDA'{..} = "L> " <> render func
  render PA_VOID{..} = render attr <> render SPACE <> render arrow <> render SPACE <> render void
  render PA_DELTA{..} = render DELTA <> render SPACE <> render DASHED_ARROW <> render SPACE <> render bytes
  render PA_DELTA'{..} = "D> " <> render bytes
  render PA_META_LAMBDA{..} = render LAMBDA <> render SPACE <> render DASHED_ARROW <> render SPACE <> render meta
  render PA_META_LAMBDA'{..} = "L> " <> render meta
  render PA_META_DELTA{..} = render DELTA <> render SPACE <> render DASHED_ARROW <> render SPACE <> render meta
  render PA_META_DELTA'{..} = "D> " <> render meta

instance Render BINDINGS where
  render BDS_EMPTY{} = ""
  render BDS_PAIR{..} = render COMMA <> render eol <> render tab <> render pair <> render bindings
  render BDS_META{..} = render COMMA <> render eol <> render tab <> render meta <> render bindings

instance Render APP_BINDING where
  render APP_BINDING{..} = render pair

instance Render BINDING where
  render BI_EMPTY{} = ""
  render BI_PAIR{..} = render pair <> render bindings
  render BI_META{..} = render meta <> render bindings

instance Render APP_ARG where
  render APP_ARG{..} = render expr <> render args

instance Render APP_ARGS where
  render AAS_EMPTY = ""
  render AAS_EXPR{..} = render COMMA <> render eol <> render tab <> render expr <> render args

instance Render EXPRESSION where
  render EX_GLOBAL{..} = render global
  render EX_XI{..} = render xi
  render EX_ATTR{..} = render attr
  render EX_TERMINATION{..} = render termination
  render EX_FORMATION{..} = render lsb <> render eol <> render tab <> render binding <> render eol' <> render tab' <> render rsb
  render EX_DISPATCH{..} = render expr <> render space <> "." <> render space <> render attr
  render EX_APPLICATION{..} = render expr <> render space <> "(" <> render eol <> render tab <> render tau <> render eol' <> render tab' <> ")"
  render EX_APPLICATION_TAUS{..} = render expr <> render space <> "(" <> render eol <> render tab <> render taus <> render eol' <> render tab' <> ")"
  render EX_APPLICATION_EXPRS{..} = render expr <> render space <> "(" <> render eol <> render tab <> render args <> render eol' <> render tab' <> ")"
  render EX_STRING{..} = "\"" <> render str <> "\""
  render EX_NUMBER{..} = either (T.pack . show) (T.pack . show) num
  render EX_META{..} = render meta
  render EX_META_TAIL{..} = render expr <> " * " <> render meta
  render EX_PHI_MEET{..} = "\\phiMeet{" <> maybe "" (\p -> T.pack p <> ":") prefix <> render idx <> "}{ " <> render expr <> " }"
  render EX_PHI_AGAIN{..} = "\\phiAgain{" <> maybe "" (\p -> T.pack p <> ":") prefix <> render idx <> "}"

instance Render [ATTRIBUTE] where
  render attrs = T.intercalate ", " (map render attrs)

instance Render ATTRIBUTE where
  render AT_LABEL{..} = render label
  render AT_ALPHA{..} = render alpha <> render idx
  render AT_RHO{..} = render rho
  render AT_PHI{..} = render phi
  render AT_LAMBDA{..} = render lambda
  render AT_DELTA{..} = render delta
  render AT_META{..} = render meta
  render AT_REST{..} = render dots

instance Render BELONGING where
  render IN = "\\in"
  render NOT_IN = "\\notin"

instance Render SET where
  render ST_BINDING{..} = render binding
  render ST_ATTRIBUTES{..} = "[ " <> T.intercalate ", " (map render attrs) <> " ]"

instance Render LOGIC_OPERATOR where
  render AND = "\\;\\text{and}\\;"
  render OR = "\\;\\text{or}\\;"

instance Render NUMBER where
  render INDEX{..} = "\\indexof{ " <> render attr <> " }"
  render LENGTH{..} = "\\vert " <> render binding <> " \\vert"
  render LITERAL{..} = T.pack (show num)

instance Render COMPARABLE where
  render CMP_ATTR{..} = render attr
  render CMP_EXPR{..} = render expr
  render CMP_NUM{..} = render num

instance Render EQUAL where
  render EQUAL = "="
  render NOT_EQUAL = "\\not="

instance Render CONDITION where
  render CO_BELONGS{..} = render attr <> " " <> render belongs <> " " <> render set
  render CO_LOGIC{conditions = [cond]} = render cond
  render CO_LOGIC{..} = T.intercalate (" " <> render operator <> " ") (map renderWrapped conditions)
    where
      renderWrapped :: CONDITION -> Text
      renderWrapped CO_LOGIC{conditions = [cond]} = render cond
      renderWrapped cond@CO_LOGIC{} = "( " <> render cond <> " )"
      renderWrapped cond = render cond
  render CO_NF{..} = "\\isnormal{ " <> render expr <> " }"
  render CO_NOT{..} = renderFunc "not" condition
  render CO_COMPARE{..} = render left <> " " <> render equal <> " " <> render right
  render CO_MATCHES{..} = "matches( " <> T.pack regex <> ", " <> render expr <> " )"
  render CO_PART_OF{..} = "part-of( " <> render expr <> ", " <> render binding <> " )"
  render CO_EMPTY = ""

renderFunc :: Render a => Text -> a -> Text
renderFunc func renderable = func <> "( " <> render renderable <> " )"

instance Render EXTRA_ARG where
  render ARG_ATTR{..} = render attr
  render ARG_EXPR{..} = render expr
  render ARG_BINDING{..} = render binding
  render ARG_BYTES{..} = render bytes

instance Render EXTRA where
  render EXTRA{func = "contextualize", args = arg : rest, ..} = "$ " <> render meta <> " \\coloneqq \\ctx{ " <> render arg <> " }{ " <> T.intercalate ", " (map render rest) <> " } $"
  render EXTRA{func = "scope", args = arg : _, ..} = "$ " <> render meta <> " \\coloneqq \\scopeof{ " <> render arg <> " } $"
  render EXTRA{..} = "$ " <> render meta <> " \\coloneqq " <> T.pack func <> "( " <> T.intercalate ", " (map render args) <> " ) $"
