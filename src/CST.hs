{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
-- This module represents concrete syntax tree for phi-calculus program
module CST where

import AST
import Data.List (intercalate)
import Data.Maybe (isJust)
import Misc

data LCB = LCB | BIG_LCB
  deriving (Eq, Show)

data RCB = RCB | BIG_RCB
  deriving (Eq, Show)

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

data PHI = PHI | AT
  deriving (Eq, Show)

data RHO = RHO | CARET
  deriving (Eq, Show)

data DELTA = DELTA
  deriving (Eq, Show)

data XI = XI | DOLLAR
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

data BYTES
  = BT_EMPTY
  | BT_ONE String
  | BT_MANY [String]
  | BT_META META
  deriving (Eq, Show)

data META
  = MT_EXPRESSION {rest :: String}
  | MT_EXPRESSION' {rest :: String}
  | MT_ATTRIBUTE {rest :: String}
  | MT_ATTRIBUTE' {rest :: String}
  | MT_BINDING {rest :: String}
  | MT_BINDING' {rest :: String}
  | MT_BYTES {rest :: String}
  | MT_BYTES' {rest :: String}
  | MT_TAIL {rest :: String}
  | MT_FUNCTION {rest :: String}
  deriving (Eq, Show)

data TAB
  = TAB {indent :: Integer}
  | TAB'
  | NO_TAB
  deriving (Eq, Show)

data ALPHA = ALPHA | ALPHA'
  deriving (Eq, Show)

data PROGRAM
  = PR_SWEET {lcb :: LCB, expr :: EXPRESSION, rcb :: RCB}
  | PR_SALTY {global :: GLOBAL, arrow :: ARROW, expr :: EXPRESSION}
  deriving (Eq, Show)

data PAIR
  = PA_TAU {attr :: ATTRIBUTE, arrow :: ARROW, expr :: EXPRESSION}
  | PA_FORMATION {attr :: ATTRIBUTE, voids :: [ATTRIBUTE], arrow :: ARROW, expr :: EXPRESSION}
  | PA_VOID {attr :: ATTRIBUTE, arrow :: ARROW, void :: VOID}
  | PA_LAMBDA {func :: String}
  | PA_LAMBDA' {func :: String} -- ASCII version of PA_LAMBDA
  | PA_META_LAMBDA {meta :: META}
  | PA_META_LAMBDA' {meta :: META} -- ASCII version of PA_META_LAMBDA'
  | PA_DELTA {bytes :: BYTES}
  | PA_DELTA' {bytes :: BYTES} -- ASCII version of PA_DELTA
  | PA_META_DELTA {meta :: META}
  | PA_META_DELTA' {meta :: META} -- ASCII version of PA_META_DELTA
  deriving (Eq, Show)

newtype APP_BINDING = APP_BINDING {pair :: PAIR}
  deriving (Eq, Show)

data BINDING
  = BI_PAIR {pair :: PAIR, bindings :: BINDINGS, tab :: TAB}
  | BI_EMPTY {tab :: TAB}
  | BI_META {meta :: META, bindings :: BINDINGS, tab :: TAB}
  deriving (Eq, Show)

data BINDINGS
  = BDS_PAIR {eol :: EOL, tab :: TAB, pair :: PAIR, bindings :: BINDINGS}
  | BDS_EMPTY {tab :: TAB}
  | BDS_META {eol :: EOL, tab :: TAB, meta :: META, bindings :: BINDINGS}
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
  | EX_DEF_PACKAGE {pckg :: DEF_PACKAGE} -- sugar for Q.org.eolang
  | EX_XI {xi :: XI}
  | EX_ATTR {attr :: ATTRIBUTE} -- sugar for $.x -> just x
  | EX_TERMINATION {termination :: TERMINATION}
  | EX_FORMATION {lsb :: LSB, eol :: EOL, tab :: TAB, binding :: BINDING, eol' :: EOL, tab' :: TAB, rsb :: RSB}
  | EX_DISPATCH {expr :: EXPRESSION, attr :: ATTRIBUTE}
  | EX_APPLICATION {expr :: EXPRESSION, eol :: EOL, tab :: TAB, tau :: APP_BINDING, eol' :: EOL, tab' :: TAB} -- e(a1 -> e1)
  | EX_APPLICATION_TAUS {expr :: EXPRESSION, eol :: EOL, tab :: TAB, taus :: BINDING, eol' :: EOL, tab' :: TAB} -- e(a1 -> e1)(a2 -> e2)(...)
  | EX_APPLICATION_EXPRS {expr :: EXPRESSION, eol :: EOL, tab :: TAB, args :: APP_ARG, eol' :: EOL, tab' :: TAB} -- e(e1, e2, ...)
  | EX_STRING {str :: String, tab :: TAB, rhos :: [Binding]}
  | EX_NUMBER {num :: Either Integer Double, tab :: TAB, rhos :: [Binding]}
  | EX_META {meta :: META}
  | EX_META_TAIL {expr :: EXPRESSION, meta :: META}
  deriving (Eq, Show)

data ATTRIBUTE
  = AT_LABEL {label :: String}
  | AT_ALPHA {alpha :: ALPHA, idx :: Integer}
  | AT_RHO {rho :: RHO}
  | AT_PHI {phi :: PHI}
  | AT_LAMBDA {lambda :: LAMBDA}
  | AT_DELTA {delta :: DELTA}
  | AT_META {meta :: META}
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
  toCST (Program expr) tabs = PR_SWEET LCB (toCST expr tabs) RCB

instance ToCST Expression EXPRESSION where
  toCST ExGlobal _ = EX_GLOBAL Φ
  toCST ExThis _ = EX_XI XI
  toCST (ExMeta mt) _ = EX_META (MT_EXPRESSION (tail mt))
  toCST (ExMetaTail expr mt) tabs = EX_META_TAIL (toCST expr tabs) (MT_TAIL (tail mt))
  toCST ExTermination _ = EX_TERMINATION DEAD
  toCST (ExFormation [BiVoid AtRho]) _ = toCST (ExFormation []) 0
  toCST (ExFormation []) _ = EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY NO_TAB) NO_EOL NO_TAB RSB
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
  toCST (DataString bts) tabs = EX_STRING (btsToStr bts) (TAB tabs) []
  toCST (DataNumber bts) tabs = EX_NUMBER (btsToNum bts) (TAB tabs) []
  toCST (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) _ = EX_DEF_PACKAGE Φ̇
  toCST (ExDispatch ExThis attr) tabs = EX_ATTR (toCST attr tabs)
  toCST (ExDispatch expr attr) tabs = EX_DISPATCH (toCST expr tabs) (toCST attr tabs)
  -- Since we convert AST to CST in sweet notation, here we're trying to get rid of unnecessary rho bindings
  -- in primitives (more details here: https://github.com/objectionary/phino/issues/451)
  -- If we find something similar to:
  -- `QQ.number(~0 -> QQ.bytes(...), ^ -> ..., ^ -> ...)`
  -- We remove unnecessary rho bindings and save them to EX_STRING or EX_NUMBER so they can be successfully
  -- converted to salty notation without losing information.
  -- In the end we just get CST with data primitive which is printed correctly.
  -- If given application is not such primitive - we just convert it to one of the applications:
  -- 1. either with pure expression with arguments, which means there are incremented only alpha bindings
  -- 2. or with just bindings
  toCST app@(ExApplication exp tau) tabs =
    let (expr, taus, exprs) = complexApplication app
        expr' = toCST expr tabs :: EXPRESSION
        next = tabs + 1
        (taus', rhos) = withoutRhosInPrimitives expr taus
        obj = ExApplication expr (head taus')
     in if length taus' == 1 && isJust (matchDataObject obj)
          then applicationToPrimitive obj tabs rhos
          else
            if null exprs
              then
                EX_APPLICATION_TAUS
                  expr'
                  EOL
                  (TAB next)
                  (toCST taus next :: BINDING)
                  EOL
                  (TAB tabs)
              else
                EX_APPLICATION_EXPRS
                  expr'
                  EOL
                  (TAB next)
                  (toCST exprs next)
                  EOL
                  (TAB tabs)
    where
      primitives :: [String]
      primitives = ["number", "string"]
      withoutRhosInPrimitives :: Expression -> [Binding] -> ([Binding], [Binding])
      withoutRhosInPrimitives _ [] = ([], [])
      withoutRhosInPrimitives obj@(BaseObject label) bds@(rho@(BiTau AtRho _) : rest)
        | label `elem` primitives =
            let (bds', rhos) = withoutRhosInPrimitives obj rest
             in (bds', rho : rhos)
        | otherwise = (bds, [])
      withoutRhosInPrimitives obj@(BaseObject label) bds@(bd : rest)
        | label `elem` primitives =
            let (bds', rhos) = withoutRhosInPrimitives obj rest
             in (bd : bds', rhos)
        | otherwise = (bds, [])
      withoutRhosInPrimitives _ bds = (bds, [])
      applicationToPrimitive :: Expression -> Integer -> [Binding] -> EXPRESSION
      applicationToPrimitive (DataNumber bts) tabs = EX_NUMBER (btsToNum bts) (TAB tabs)
      applicationToPrimitive (DataString bts) tabs = EX_STRING (btsToStr bts) (TAB tabs)
      -- Here we unroll nested application sequence into flat structure
      -- The returned tuple consists of:
      -- 1. deepest start expression
      -- 2. list of tau bindings which are applied to start expression
      -- 3. list of expressions which are applied to start expression with default
      --    alpha attributes (~0 -> e1, ~1 -> e2, ...)
      complexApplication :: Expression -> (Expression, [Binding], [Expression])
      complexApplication expr =
        let (expr', taus', exprs') = complexApplication' expr
         in (expr', reverse taus', reverse exprs')
        where
          complexApplication' :: Expression -> (Expression, [Binding], [Expression])
          complexApplication' (ExApplication (ExApplication expr tau) tau') =
            let (before, taus, exprs) = complexApplication' (ExApplication expr tau)
                taus' = tau' : taus
             in if null exprs
                  then (before, taus', [])
                  else case tau' of
                    BiTau (AtAlpha idx) expr' ->
                      if idx == fromIntegral (length exprs)
                        then (before, taus', expr' : exprs)
                        else (before, taus', [])
                    _ -> (before, taus', [])
          complexApplication' (ExApplication expr (BiTau (AtAlpha 0) expr')) = (expr, [BiTau (AtAlpha 0) expr'], [expr'])
          complexApplication' (ExApplication expr tau) = (expr, [tau], [])

instance ToCST [Expression] APP_ARG where
  toCST (expr : exprs) tabs = APP_ARG (toCST expr tabs) (toCST exprs tabs)

instance ToCST [Expression] APP_ARGS where
  toCST [] _ = AAS_EMPTY
  toCST (expr : exprs) tabs = AAS_EXPR EOL (TAB tabs) (toCST expr tabs) (toCST exprs tabs)

instance ToCST [Binding] BINDING where
  toCST [] tabs = BI_EMPTY (TAB tabs)
  toCST (BiMeta mt : bds) tabs = BI_META (MT_BINDING (tail mt)) (toCST bds tabs) (TAB tabs)
  toCST (bd : bds) tabs = BI_PAIR (toCST bd tabs) (toCST bds tabs) (TAB tabs)

instance ToCST [Binding] BINDINGS where
  toCST [] tabs = BDS_EMPTY (TAB tabs)
  toCST (BiMeta mt : bds) tabs = BDS_META EOL (TAB tabs) (MT_BINDING (tail mt)) (toCST bds tabs)
  toCST (bd : bds) tabs = BDS_PAIR EOL (TAB tabs) (toCST bd tabs) (toCST bds tabs)

instance ToCST Binding PAIR where
  toCST (BiTau attr exp@(ExFormation bds)) tabs =
    let voids' = voids bds
        attr' = toCST attr tabs
     in if null voids'
          then PA_TAU attr' ARROW (toCST exp tabs)
          else
            let (_voids, _bds) = if length voids' == length bds && last voids' == AtRho then (init voids', []) else (voids', drop (length voids') bds)
             in PA_FORMATION
                  attr'
                  (map (`toCST` tabs) _voids)
                  ARROW
                  (toCST (ExFormation _bds) tabs)
    where
      voids :: [Binding] -> [Attribute]
      voids [] = []
      voids (bd : bds) = case bd of
        BiVoid attr -> attr : voids bds
        _ -> []
  toCST (BiTau attr exp) tabs = PA_TAU (toCST attr tabs) ARROW (toCST exp tabs)
  toCST (BiVoid attr) tabs = PA_VOID (toCST attr tabs) ARROW EMPTY
  toCST (BiDelta bts) tabs = PA_DELTA (toCST bts tabs)
  toCST (BiLambda func) _ = PA_LAMBDA func
  toCST (BiMetaLambda mt) _ = PA_META_LAMBDA (MT_FUNCTION (tail mt))

instance ToCST Binding APP_BINDING where
  toCST bd@(BiTau _ _) tabs = APP_BINDING (toCST bd tabs :: PAIR)

instance ToCST Bytes BYTES where
  toCST BtEmpty _ = BT_EMPTY
  toCST (BtOne byte) _ = BT_ONE byte
  toCST (BtMany bts) _ = BT_MANY bts
  toCST (BtMeta mt) _ = BT_META (MT_BYTES (tail mt))

instance ToCST Attribute ATTRIBUTE where
  toCST (AtLabel label) _ = AT_LABEL label
  toCST (AtAlpha idx) _ = AT_ALPHA ALPHA idx
  toCST AtPhi _ = AT_PHI PHI
  toCST AtRho _ = AT_RHO RHO
  toCST AtDelta _ = AT_DELTA DELTA
  toCST AtLambda _ = AT_LAMBDA LAMBDA
  toCST (AtMeta mt) _ = AT_META (MT_ATTRIBUTE (tail mt))
