{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Render where

import AST
import CST
import Data.List

class Render a where
  render :: a -> String

instance Render String where
  render str = str

instance Render Integer where
  render = show

instance Render Double where
  render = show

instance Render Char where
  render ch = [ch]

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

instance Render GLOBAL where
  render Φ = "Φ"
  render Q = "Q"

instance Render DEF_PACKAGE where
  render Φ̇ = "Φ̇"
  render QQ = "QQ"

instance Render TERMINATION where
  render DEAD = "⊥"
  render T = "T"

instance Render SPACE where
  render SPACE = " "

instance Render EOL where
  render EOL = "\n"
  render NO_EOL = ""

instance Render BYTES where
  render BT_EMPTY = "--"
  render (BT_ONE bte) = render bte <> "-"
  render (BT_MANY bts) = intercalate "-" bts

instance Render ALPHA where
  render ALPHA = "α"
  render ALPHA' = "~"

instance Render TAB where
  render TAB {..} = intercalate "" (replicate (fromIntegral indent) "  ")
  render TAB' = " "
  render NO_TAB = ""

instance Render PROGRAM where
  render PR_SWEET {..} = "{" <> render expr <> "}"
  render PR_SALTY {..} = render global <> render SPACE <> render arrow <> render SPACE <> render expr

instance Render PAIR where
  render PA_TAU {..} = render attr <> render SPACE <> render arrow <> render SPACE <> render expr
  render PA_LAMBDA {..} = render LAMBDA <> render SPACE <> render DASHED_ARROW <> render SPACE <> render func
  render PA_LAMBDA' {..} = "L> " <> func
  render PA_VOID {..} = render attr <> render SPACE <> render arrow <> render SPACE <> render void
  render PA_DELTA {..} = render DELTA <> render SPACE <> render DASHED_ARROW <> render SPACE <> render bytes
  render PA_DELTA' {..} = "D> " <> render bytes

instance Render BINDINGS where
  render BDS_EMPTY {..} = ""
  render BDS_PAIR {..} = render COMMA <> render eol <> render tab <> render pair <> render bindings

instance Render BINDING where
  render BI_PAIR {..} = render pair <> render bindings
  render BI_EMPTY {..} = ""

instance Render APP_ARG where
  render APP_ARG {..} = render expr <> render args

instance Render APP_ARGS where
  render AAS_EMPTY = ""
  render AAS_EXPR {..} = render COMMA <> render eol <> render tab <> render expr <> render args

-- @todo #163:30min Introduce node for formation with inlined voids.
--  We need to be able to print formation with inlined void attributes:
--  x(a, b) -> [[ y -> 1 ]] => x -> [[ a -> ?, b -> ?, y -> 1 ]]
--  Don't forget to extend toSalty instance so such sugar.
instance Render EXPRESSION where
  render EX_GLOBAL {..} = render global
  render EX_DEF_PACKAGE {..} = render pckg
  render EX_XI {..} = render xi
  render EX_ATTR {..} = render attr
  render EX_TERMINATION {..} = render termination
  render EX_FORMATION {..} = render lsb <> render eol <> render tab <> render binding <> render eol' <> render tab' <> render rsb
  render EX_DISPATCH {..} = render expr <> "." <> render attr
  render EX_APPLICATION {..} = render expr <> "(" <> render eol <> render tab <> render bindings <> render eol' <> render tab' <> ")"
  render EX_APPLICATION' {..} = render expr <> "(" <> render eol <> render tab <> render args <> render eol' <> render tab' <> ")"
  render EX_STRING {..} = '"' : render str <> "\""
  render EX_NUMBER {..} = either show show num

instance Render ATTRIBUTE where
  render AT_LABEL {..} = label
  render AT_ALPHA {..} = render alpha <> render idx
  render AT_RHO {..} = render rho
  render AT_PHI {..} = render phi
  render AT_LAMBDA {..} = render lambda
  render AT_DELTA {..} = render delta
