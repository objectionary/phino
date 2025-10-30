{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
-- This module represents concrete syntax tree for phi-calculus program
module CST where

import AST
import Data.List (intercalate)
import Misc

data PrintMode = SWEET | SALTY
  deriving (Eq, Show)

instance Pretty PrintMode where
  pretty SWEET = "sweet"
  pretty SALTY = "salty"

data LSB = LSB | LSB'
  deriving (Eq, Show)

data RSB = RSB | RSB'
  deriving (Eq, Show)

data COMMA = COMMA | NO_COMMA
  deriving (Eq, Show)

data ARROW = ARROW | ARROW'
  deriving (Eq, Show)

data DASHED_ARROW = DASHED_ARROW
  deriving (Eq, Show)

data VOID = EMPTY | QUESTION
  deriving (Eq, Show)

data PHI = PHI | AT | CHAR_64
  deriving (Eq, Show)

data RHO = RHO | CARET | CHAR_94
  deriving (Eq, Show)

data DELTA = DELTA
  deriving (Eq, Show)

data XI = XI | DOLLAR | CHAR_36
  deriving (Eq, Show)

data LAMBDA = LAMBDA
  deriving (Eq, Show)

data GLOBAL = Φ | Q
  deriving (Eq, Show)

data DEF_PACKAGE = Φ̇ | QQ
  deriving (Eq, Show)

data TERMINATION = DEAD | T
  deriving (Eq, Show)

data SPACE = SPACE
  deriving (Eq, Show)

data EOL = EOL | NO_EOL
  deriving (Eq, Show)

data BYTES = BT_EMPTY | BT_ONE String | BT_MANY [String]
  deriving (Eq, Show)

data TAB
  = TAB {indent :: Integer}
  | TAB'
  | NO_TAB
  deriving (Eq, Show)

data ALPHA = ALPHA | ALPHA'
  deriving (Eq, Show)

data PROGRAM
  = PR_SWEET {expr :: EXPRESSION}
  | PR_SALTY {global :: GLOBAL, arrow :: ARROW, expr :: EXPRESSION}
  deriving (Eq, Show)

data PAIR
  = PA_TAU {attr :: ATTRIBUTE, arrow :: ARROW, expr :: EXPRESSION}
  | PA_VOID {attr :: ATTRIBUTE, arrow :: ARROW, void :: VOID}
  | PA_LAMBDA {func :: String}
  | PA_LAMBDA' {func :: String} -- ASCII version of PA_LAMBDA
  | PA_DELTA {bytes :: BYTES}
  | PA_DELTA' {bytes :: BYTES} -- ASCII version of PA_DELTA
  deriving (Eq, Show)

data BINDING
  = BI_PAIR {pair :: PAIR, bindings :: BINDINGS, tab :: TAB}
  | BI_EMPTY {tab :: TAB}
  deriving (Eq, Show)

data BINDINGS
  = BDS_PAIR {eol :: EOL, tab :: TAB, pair :: PAIR, bindings :: BINDINGS}
  | BDS_EMPTY {tab :: TAB}
  deriving (Eq, Show)

-- Arguments for application with default α attributes
-- which are not necessary to be printed
data APP_ARG = APP_ARG {expr :: EXPRESSION, args :: APP_ARGS}
  deriving (Eq, Show)

data APP_ARGS
  = AAS_EXPR {eol :: EOL, tab :: TAB, expr :: EXPRESSION, args :: APP_ARGS}
  | AAS_EMPTY
  deriving (Eq, Show)

data EXPRESSION
  = EX_GLOBAL {global :: GLOBAL}
  | EX_DEF_PACKAGE {pckg :: DEF_PACKAGE}
  | EX_XI {xi :: XI}
  | EX_ATTR {attr :: ATTRIBUTE} -- sugar for $.x -> just x
  | EX_TERMINATION {termination :: TERMINATION}
  | EX_FORMATION {lsb :: LSB, eol :: EOL, tab :: TAB, binding :: BINDING, eol' :: EOL, tab' :: TAB, rsb :: RSB}
  | EX_DISPATCH {expr :: EXPRESSION, attr :: ATTRIBUTE}
  | EX_APPLICATION {expr :: EXPRESSION, eol :: EOL, tab :: TAB, bindings :: BINDING, eol' :: EOL, tab' :: TAB}
  | EX_APPLICATION' {expr :: EXPRESSION, eol :: EOL, tab :: TAB, args :: APP_ARG, eol' :: EOL, tab' :: TAB}
  | EX_STRING {str :: String, tab :: TAB}
  | EX_NUMBER {num :: Either Integer Double, tab :: TAB}
  deriving (Eq, Show)

data ATTRIBUTE
  = AT_LABEL {label :: String}
  | AT_ALPHA {alpha :: ALPHA, idx :: Integer}
  | AT_RHO {rho :: RHO}
  | AT_PHI {phi :: PHI}
  | AT_LAMBDA {lambda :: LAMBDA}
  | AT_DELTA {delta :: DELTA}
  deriving (Eq, Show)

class Pretty a where
  pretty :: a -> String

instance Pretty String where
  pretty str = str

instance Pretty Integer where
  pretty = show

instance Pretty Double where
  pretty = show

instance Pretty Char where
  pretty ch = [ch]

instance Pretty LSB where
  pretty LSB = "⟦"
  pretty LSB' = "[["

instance Pretty RSB where
  pretty RSB = "⟧"
  pretty RSB' = "]]"

instance Pretty COMMA where
  pretty COMMA = ","
  pretty NO_COMMA = ""

instance Pretty ARROW where
  pretty ARROW = "↦"
  pretty ARROW' = "->"

instance Pretty DASHED_ARROW where
  pretty DASHED_ARROW = "⤍"

instance Pretty VOID where
  pretty EMPTY = "∅"
  pretty QUESTION = "?"

instance Pretty PHI where
  pretty :: PHI -> String
  pretty PHI = "φ"
  pretty AT = "@"
  pretty CHAR_64 = "\\char64{}"

instance Pretty RHO where
  pretty RHO = "ρ"
  pretty CARET = "^"
  pretty CHAR_94 = "\\char94{}"

instance Pretty DELTA where
  pretty DELTA = "Δ"

instance Pretty XI where
  pretty XI = "ξ"
  pretty DOLLAR = "$"
  pretty CHAR_36 = "\\char36{}"

instance Pretty LAMBDA where
  pretty LAMBDA = "λ"

instance Pretty GLOBAL where
  pretty Φ = "Φ"
  pretty Q = "Q"

instance Pretty DEF_PACKAGE where
  pretty Φ̇ = "Φ̇"
  pretty QQ = "QQ"

instance Pretty TERMINATION where
  pretty DEAD = "⊥"
  pretty T = "T"

instance Pretty SPACE where
  pretty SPACE = " "

instance Pretty EOL where
  pretty EOL = "\n"
  pretty NO_EOL = ""

instance Pretty BYTES where
  pretty BT_EMPTY = "--"
  pretty (BT_ONE bte) = pretty bte <> "-"
  pretty (BT_MANY bts) = intercalate "-" bts

instance Pretty ALPHA where
  pretty ALPHA = "α"
  pretty ALPHA' = "~"

instance Pretty TAB where
  pretty TAB {..} = intercalate "" (replicate (fromIntegral indent) "  ")
  pretty TAB' = " "
  pretty NO_TAB = ""

instance Pretty PROGRAM where
  pretty PR_SWEET {..} = "{" <> pretty expr <> "}"
  pretty PR_SALTY {..} = pretty global <> pretty SPACE <> pretty arrow <> pretty SPACE <> pretty expr

instance Pretty PAIR where
  pretty PA_TAU {..} = pretty attr <> pretty SPACE <> pretty arrow <> pretty SPACE <> pretty expr
  pretty PA_LAMBDA {..} = pretty LAMBDA <> pretty SPACE <> pretty DASHED_ARROW <> pretty SPACE <> pretty func
  pretty PA_LAMBDA' {..} = "L> " <> func
  pretty PA_VOID {..} = pretty attr <> pretty SPACE <> pretty arrow <> pretty SPACE <> pretty void
  pretty PA_DELTA {..} = pretty DELTA <> pretty SPACE <> pretty DASHED_ARROW <> pretty SPACE <> pretty bytes
  pretty PA_DELTA' {..} = "D> " <> pretty bytes

instance Pretty BINDINGS where
  pretty BDS_EMPTY {..} = ""
  pretty BDS_PAIR {..} = pretty COMMA <> pretty eol <> pretty tab <> pretty pair <> pretty bindings

instance Pretty BINDING where
  pretty BI_PAIR {..} = pretty pair <> pretty bindings
  pretty BI_EMPTY {..} = ""

instance Pretty APP_ARG where
  pretty APP_ARG {..} = pretty expr <> pretty args

instance Pretty APP_ARGS where
  pretty AAS_EMPTY = ""
  pretty AAS_EXPR {..} = pretty COMMA <> pretty eol <> pretty tab <> pretty expr <> pretty args

-- @todo #163:30min Introduce node for formation with inlined voids.
--  We need to be able to print formation with inlined void attributes:
--  x(a, b) -> [[ y -> 1 ]] => x -> [[ a -> ?, b -> ?, y -> 1 ]]
--  Don't forget to extend toSalty instance so such sugar.
instance Pretty EXPRESSION where
  pretty EX_GLOBAL {..} = pretty global
  pretty EX_DEF_PACKAGE {..} = pretty pckg
  pretty EX_XI {..} = pretty xi
  pretty EX_ATTR {..} = pretty attr
  pretty EX_TERMINATION {..} = pretty termination
  pretty EX_FORMATION {..} = pretty lsb <> pretty eol <> pretty tab <> pretty binding <> pretty eol' <> pretty tab' <> pretty rsb
  pretty EX_DISPATCH {..} = pretty expr <> "." <> pretty attr
  pretty EX_APPLICATION {..} = pretty expr <> "(" <> pretty eol <> pretty tab <> pretty bindings <> pretty eol' <> pretty tab' <> ")"
  pretty EX_APPLICATION' {..} = pretty expr <> "(" <> pretty eol <> pretty tab <> pretty args <> pretty eol' <> pretty tab' <> ")"
  pretty EX_STRING {..} = show str
  pretty EX_NUMBER {..} = either show show num

instance Pretty ATTRIBUTE where
  pretty AT_LABEL {..} = label
  pretty AT_ALPHA {..} = pretty alpha <> pretty idx
  pretty AT_RHO {..} = pretty rho
  pretty AT_PHI {..} = pretty phi
  pretty AT_LAMBDA {..} = pretty lambda
  pretty AT_DELTA {..} = pretty delta

astToCst :: Program -> PROGRAM
astToCst prog = toCST prog 0

-- This class is used to convert AST to CST
-- CST is created with sugar and unicode
-- All further transformations much consider that
class ToCST a b where
  toCST :: a -> Integer -> b

instance ToCST Program PROGRAM where
  toCST (Program expr) tabs = PR_SWEET (toCST expr tabs)

instance ToCST Expression EXPRESSION where
  toCST ExGlobal _ = EX_GLOBAL Φ
  toCST ExThis _ = EX_XI XI
  toCST ExTermination _ = EX_TERMINATION DEAD
  toCST (ExFormation bds) tabs =
    let next = tabs + 1
        bds' = toCST (withoutLastVoidRho bds) next :: BINDING
     in EX_FORMATION
          LSB
          EOL
          (TAB next)
          bds'
          EOL
          (TAB tabs)
          RSB
    where
      withoutLastVoidRho :: [Binding] -> [Binding]
      withoutLastVoidRho [] = []
      withoutLastVoidRho (bd : [BiVoid AtRho]) = [bd]
      withoutLastVoidRho (bd : bds') = bd : withoutLastVoidRho bds'
  toCST (DataString bts) tabs = EX_STRING (btsToStr bts) (TAB tabs)
  toCST (DataNumber bts) tabs = EX_NUMBER (btsToNum bts) (TAB tabs)
  toCST (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) _ = EX_DEF_PACKAGE Φ̇
  toCST (ExDispatch ExThis attr) tabs = EX_ATTR (toCST attr tabs)
  toCST (ExDispatch expr attr) tabs = EX_DISPATCH (toCST expr tabs) (toCST attr tabs)
  toCST app@(ExApplication (ExApplication expr tau) tau') tabs =
    let (expr', taus, exprs) = complexApplication app
        next = tabs + 1
        expr'' = toCST expr' tabs :: EXPRESSION
     in if null exprs
          then
            EX_APPLICATION
              (toCST expr' tabs)
              EOL
              (TAB next)
              (toCST (reverse taus) next)
              EOL
              (TAB tabs)
          else
            EX_APPLICATION'
              (toCST expr' tabs)
              EOL
              (TAB next)
              (toCST (reverse exprs) next)
              EOL
              (TAB tabs)
    where
      -- Here we unroll nested application sequence into flat structure
      -- The returned tuple consists of:
      -- 1. deepest start expression
      -- 2. list of tau bindings which are applied to start expression
      -- 3. list of expressions which are applied to start expression with default
      --    alpha attributes (~0 -> e1, ~1 -> e2, ...)
      complexApplication :: Expression -> (Expression, [Binding], [Expression])
      complexApplication (ExApplication (ExApplication expr tau) tau') =
        let (before, taus, exprs) = complexApplication (ExApplication expr tau)
            taus' = tau' : taus
         in if null exprs
              then (before, taus', [])
              else case tau' of
                BiTau (AtAlpha idx) expr' ->
                  if idx == fromIntegral (length exprs)
                    then (before, taus', expr' : exprs)
                    else (before, taus', [])
                _ -> (before, taus', [])
      complexApplication (ExApplication expr (BiTau (AtAlpha 0) expr')) = (expr, [BiTau (AtAlpha 0) expr'], [expr'])
      complexApplication (ExApplication expr tau) = (expr, [tau], [])
  toCST (ExApplication expr tau) tabs =
    let next = tabs + 1
     in EX_APPLICATION (toCST expr tabs) EOL (TAB next) (toCST [tau] next) EOL (TAB tabs)

instance ToCST [Expression] APP_ARG where
  toCST (expr : exprs) tabs = APP_ARG (toCST expr tabs) (toCST exprs tabs)

instance ToCST [Expression] APP_ARGS where
  toCST [] _ = AAS_EMPTY
  toCST (expr : exprs) tabs = AAS_EXPR EOL (TAB tabs) (toCST expr tabs) (toCST exprs tabs)

instance ToCST [Binding] BINDING where
  toCST [] tabs = BI_EMPTY (TAB tabs)
  toCST (bd : bds) tabs = BI_PAIR (toCST bd tabs) (toCST bds tabs) (TAB tabs)

instance ToCST [Binding] BINDINGS where
  toCST [] tabs = BDS_EMPTY (TAB tabs)
  toCST (bd : bds) tabs = BDS_PAIR EOL (TAB tabs) (toCST bd tabs) (toCST bds tabs)

instance ToCST Binding PAIR where
  toCST (BiTau attr exp) tabs = PA_TAU (toCST attr tabs) ARROW (toCST exp tabs)
  toCST (BiVoid attr) tabs = PA_VOID (toCST attr tabs) ARROW EMPTY
  toCST (BiDelta bts) tabs = PA_DELTA (toCST bts tabs)
  toCST (BiLambda func) _ = PA_LAMBDA func

instance ToCST Bytes BYTES where
  toCST BtEmpty _ = BT_EMPTY
  toCST (BtOne byte) _ = BT_ONE byte
  toCST (BtMany bts) _ = BT_MANY bts

instance ToCST Attribute ATTRIBUTE where
  toCST (AtLabel label) _ = AT_LABEL label
  toCST (AtAlpha idx) _ = AT_ALPHA ALPHA idx
  toCST AtPhi _ = AT_PHI PHI
  toCST AtRho _ = AT_RHO RHO

class HasEOL a where
  hasEOL :: a -> Bool

instance HasEOL EXPRESSION where
  hasEOL EX_FORMATION {eol = NO_EOL, binding, eol' = NO_EOL} = hasEOL binding
  hasEOL EX_FORMATION {..} = True
  hasEOL EX_DISPATCH {..} = hasEOL expr
  hasEOL EX_APPLICATION {eol = NO_EOL, expr, bindings, eol' = NO_EOL} = hasEOL expr || hasEOL bindings
  hasEOL EX_APPLICATION {..} = True
  hasEOL EX_APPLICATION' {eol = NO_EOL, expr, args, eol' = NO_EOL} = hasEOL expr || hasEOL args
  hasEOL EX_APPLICATION' {..} = True
  hasEOL _ = False

instance HasEOL BINDING where
  hasEOL BI_EMPTY {..} = False
  hasEOL BI_PAIR {..} = hasEOL pair || hasEOL bindings

instance HasEOL BINDINGS where
  hasEOL BDS_PAIR {eol = NO_EOL, pair, bindings} = hasEOL pair || hasEOL bindings
  hasEOL BDS_PAIR {..} = True
  hasEOL BDS_EMPTY {..} = False

instance HasEOL PAIR where
  hasEOL PA_TAU {..} = hasEOL expr
  hasEOL _ = False

instance HasEOL APP_ARG where
  hasEOL APP_ARG {..} = hasEOL expr || hasEOL args

instance HasEOL APP_ARGS where
  hasEOL AAS_EMPTY = False
  hasEOL AAS_EXPR {eol = NO_EOL, expr, args} = hasEOL expr || hasEOL args
  hasEOL AAS_EXPR {..} = True

withPrintMode :: PrintMode -> PROGRAM -> PROGRAM
withPrintMode SWEET prog = prog
withPrintMode SALTY prog = toSalty prog

-- By default CST is generated with all possible syntax sugar
-- The main purpose of this class is to get rid of syntax sugar

--  |----------------------------|-------------------------------------------------------|
--  | sugar                      | verbose version                                       |
--  |----------------------------|-------------------------------------------------------|
--  | {e}                        | Q -> e                                                |
--  | QQ                         | Q.org.eolang                                          |
--  | a1 -> a2                   | a1 ↦ $.a2                                             |
--  | a -> 42                    | QQ.number(QQ.bytes([[ D> 40-45-00-00-00-00-00-00 ]])) |
--  | a -> "Hey"                 | QQ.number(QQ.bytes([[ D> 48-65-79 ]]))                |
--  | [[ B ]]                    | [[ B, ^ -> ? ]], if rho is absent in 'B'              |
--  | a1(a2, a3, ...) -> [[ B ]] | a1 -> [[ a2 -> ?, a3 -> ?, ..., B ]]                  |
--  | e(e0, e1, ...)             | e(~0 -> e0, ~1 -> e1, ...)                            |
--  | e(a1 -> e1, a2 -> e2, ...) | e(a1 -> e1)(a2 -> e2)...                              |
--  |----------------------------|-------------------------------------------------------|
class ToSalty a where
  toSalty :: a -> a

instance ToSalty PROGRAM where
  toSalty :: PROGRAM -> PROGRAM
  toSalty PR_SWEET {..} = PR_SALTY Φ ARROW (toSalty expr)
  toSalty prog = prog

instance ToSalty EXPRESSION where
  toSalty EX_DEF_PACKAGE {..} = EX_DISPATCH (EX_DISPATCH (EX_GLOBAL Φ) (AT_LABEL "org")) (AT_LABEL "eolang")
  toSalty EX_ATTR {..} = EX_DISPATCH (EX_XI XI) attr
  toSalty EX_DISPATCH {..} = EX_DISPATCH (toSalty expr) attr
  toSalty EX_FORMATION {..} = EX_FORMATION lsb eol tab (toSalty (bdWithVoidRho binding)) eol' tab' rsb
    where
      voidRho :: PAIR
      voidRho = PA_VOID (AT_RHO RHO) ARROW EMPTY
      bdWithVoidRho :: BINDING -> BINDING
      bdWithVoidRho BI_EMPTY {..} = BI_PAIR voidRho (BDS_EMPTY tab) tab
      bdWithVoidRho bd@BI_PAIR {pair = PA_VOID {attr = AT_RHO _}} = bd
      bdWithVoidRho bd@BI_PAIR {pair = PA_TAU {attr = AT_RHO _}} = bd
      bdWithVoidRho BI_PAIR {..} = BI_PAIR pair (bdsWithVoidRho bindings) tab
      bdsWithVoidRho :: BINDINGS -> BINDINGS
      bdsWithVoidRho BDS_EMPTY {..} = BDS_PAIR EOL tab voidRho (BDS_EMPTY tab)
      bdsWithVoidRho bds@BDS_PAIR {pair = PA_VOID {attr = AT_RHO _}} = bds
      bdsWithVoidRho bds@BDS_PAIR {pair = PA_TAU {attr = AT_RHO _}} = bds
      bdsWithVoidRho BDS_PAIR {..} = BDS_PAIR eol tab pair (bdsWithVoidRho bindings)
  toSalty EX_APPLICATION {..} = EX_APPLICATION (toSalty expr) eol tab (toSalty bindings) eol' tab'
  toSalty EX_APPLICATION' {..} = EX_APPLICATION (toSalty expr) eol tab (toSalty (argToBinding args tab)) eol' tab'
    where
      argToBinding :: APP_ARG -> TAB -> BINDING
      argToBinding APP_ARG {..} =
        BI_PAIR
          (PA_TAU (AT_ALPHA ALPHA 0) ARROW expr)
          (argsToBindings args 1 tab)
      argsToBindings :: APP_ARGS -> Integer -> TAB -> BINDINGS
      argsToBindings AAS_EMPTY _ tab = BDS_EMPTY tab
      argsToBindings AAS_EXPR {..} idx tb = BDS_PAIR eol tb (PA_TAU (AT_ALPHA ALPHA idx) ARROW expr) (argsToBindings args (idx + 1) tb)
  toSalty EX_NUMBER {num, tab = tb@TAB {..}} =
    let number = ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number")
        bytes = ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes")
        number' = toCST number (indent + 1) :: EXPRESSION
        bytes' = toCST bytes (indent + 2) :: EXPRESSION
        data' = toCST (ExFormation [BiDelta (numToBts (either toDouble id num))]) (indent + 2) :: EXPRESSION
     in toSalty
          ( EX_APPLICATION'
              number'
              EOL
              (TAB (indent + 1))
              ( APP_ARG
                  ( EX_APPLICATION'
                      bytes'
                      EOL
                      (TAB (indent + 2))
                      (APP_ARG data' AAS_EMPTY)
                      EOL
                      (TAB (indent + 1))
                  )
                  AAS_EMPTY
              )
              EOL
              tb
          )
  toSalty EX_STRING {str, tab = tb@TAB {..}} =
    let string = ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "string")
        bytes = ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes")
        string' = toCST string (indent + 1) :: EXPRESSION
        bytes' = toCST bytes (indent + 2) :: EXPRESSION
        data' = toCST (ExFormation [BiDelta (strToBts str)]) (indent + 2) :: EXPRESSION
     in toSalty
          ( EX_APPLICATION'
              string'
              EOL
              (TAB (indent + 1))
              ( APP_ARG
                  ( EX_APPLICATION'
                      bytes'
                      EOL
                      (TAB (indent + 2))
                      (APP_ARG data' AAS_EMPTY)
                      EOL
                      (TAB (indent + 1))
                  )
                  AAS_EMPTY
              )
              EOL
              tb
          )
  toSalty expr = expr

instance ToSalty BINDING where
  toSalty BI_PAIR {..} = BI_PAIR (toSalty pair) (toSalty bindings) tab
  toSalty bd = bd

instance ToSalty BINDINGS where
  toSalty BDS_PAIR {..} = BDS_PAIR eol tab (toSalty pair) (toSalty bindings)
  toSalty bds = bds

instance ToSalty PAIR where
  toSalty PA_TAU {..} = PA_TAU attr arrow (toSalty expr)
  toSalty pair = pair
