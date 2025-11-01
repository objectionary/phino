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

programToCST :: Program -> PROGRAM
programToCST prog = toCST prog 0

expressionToCST :: Expression -> EXPRESSION
expressionToCST expr = toCST expr 0

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
  toCST (ExFormation []) _ = EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY NO_TAB) NO_EOL NO_TAB RSB
  toCST (ExFormation [BiVoid AtRho]) _ = toCST (ExFormation []) 0
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
      withoutLastVoidRho [BiVoid AtRho] = []
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
  toCST AtDelta _ = AT_DELTA DELTA
  toCST AtLambda _ = AT_LAMBDA LAMBDA

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
