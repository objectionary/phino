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
  | EX_APPLICATION {expr :: EXPRESSION, eol :: EOL, tab :: TAB, tau :: APP_BINDING, eol' :: EOL, tab' :: TAB, indent :: Integer} -- e(a1 -> e1)
  | EX_APPLICATION_TAUS {expr :: EXPRESSION, eol :: EOL, tab :: TAB, taus :: BINDING, eol' :: EOL, tab' :: TAB, indent :: Integer} -- e(a1 -> e1)(a2 -> e2)(...)
  | EX_APPLICATION_EXPRS {expr :: EXPRESSION, eol :: EOL, tab :: TAB, args :: APP_ARG, eol' :: EOL, tab' :: TAB, indent :: Integer} -- e(e1, e2, ...)
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
programToCST prog = toCST prog 0 EOL

expressionToCST :: Expression -> EXPRESSION
expressionToCST expr = toCST expr 0 EOL

-- This class is used to convert AST to CST
-- CST is created with sugar and unicode
-- All further transformations much consider that
class ToCST a b where
  toCST :: a -> Integer -> EOL -> b

instance ToCST Program PROGRAM where
  toCST (Program expr) tabs eol = PR_SWEET LCB (toCST expr tabs eol) RCB

instance ToCST Expression EXPRESSION where
  toCST ExGlobal _ _ = EX_GLOBAL Φ
  toCST ExThis _ _ = EX_XI XI
  toCST (ExMeta mt) _ _ = EX_META (MT_EXPRESSION (tail mt))
  toCST (ExMetaTail expr mt) tabs eol = EX_META_TAIL (toCST expr tabs eol) (MT_TAIL (tail mt))
  toCST ExTermination _ _ = EX_TERMINATION DEAD
  toCST (ExFormation [BiVoid AtRho]) _ eol = toCST (ExFormation []) 0 eol
  toCST (ExFormation []) _ _ = EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY NO_TAB) NO_EOL NO_TAB RSB
  toCST (ExFormation bds) tabs eol =
    let next = tabs + 1
        bds' = toCST (withoutLastVoidRho bds) next eol :: BINDING
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
  toCST (DataString bts) tabs _ = EX_STRING (btsToStr bts) (TAB tabs) []
  toCST (DataNumber bts) tabs _ = EX_NUMBER (btsToNum bts) (TAB tabs) []
  toCST (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) _ _ = EX_DEF_PACKAGE Φ̇
  toCST (ExDispatch ExThis attr) tabs eol = EX_ATTR (toCST attr tabs eol)
  toCST (ExDispatch expr attr) tabs eol = EX_DISPATCH (toCST expr tabs eol) (toCST attr tabs eol)
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
  toCST app@(ExApplication exp tau) tabs eol =
    let (expr, taus, exprs) = complexApplication app
        expr' = toCST expr tabs eol :: EXPRESSION
        next = tabs + 1
        (taus', rhos) = withoutRhosInPrimitives expr taus
        obj = ExApplication expr (head taus')
     in if length taus' == 1 && isJust (matchDataObject obj)
          then applicationToPrimitive obj tabs rhos
          else
            if null exprs
              then
                let eol' = inlinedEOL (not (hasEOL taus))
                 in EX_APPLICATION_TAUS
                      expr'
                      eol'
                      (tabOfEOL eol' next)
                      (toCST taus next eol' :: BINDING)
                      eol'
                      (tabOfEOL eol' tabs)
                      next
              else
                let eol' = inlinedEOL (not (hasEOL exprs))
                 in EX_APPLICATION_EXPRS
                      expr'
                      eol'
                      (tabOfEOL eol' next)
                      (toCST exprs next eol')
                      eol'
                      (tabOfEOL eol' tabs)
                      next
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
  toCST (expr : exprs) tabs eol = APP_ARG (toCST expr tabs eol) (toCST exprs tabs eol)

instance ToCST [Expression] APP_ARGS where
  toCST [] _ _ = AAS_EMPTY
  toCST (expr : exprs) tabs eol = AAS_EXPR eol (tabOfEOL eol tabs) (toCST expr tabs eol) (toCST exprs tabs eol)

instance ToCST [Binding] BINDING where
  toCST [] tabs _ = BI_EMPTY (TAB tabs)
  toCST (BiMeta mt : bds) tabs eol = BI_META (MT_BINDING (tail mt)) (toCST bds tabs eol) (tabOfEOL eol tabs)
  toCST (bd : bds) tabs eol = BI_PAIR (toCST bd tabs eol) (toCST bds tabs eol) (tabOfEOL eol tabs)

instance ToCST [Binding] BINDINGS where
  toCST [] tabs _ = BDS_EMPTY (TAB tabs)
  toCST (BiMeta mt : bds) tabs eol = BDS_META eol (tabOfEOL eol tabs) (MT_BINDING (tail mt)) (toCST bds tabs eol)
  toCST (bd : bds) tabs eol = BDS_PAIR eol (tabOfEOL eol tabs) (toCST bd tabs eol) (toCST bds tabs eol)

instance ToCST Binding PAIR where
  toCST (BiTau attr exp@(ExFormation bds)) tabs eol =
    let voids' = voids bds
        attr' = toCST attr tabs eol
     in if null voids'
          then PA_TAU attr' ARROW (toCST exp tabs eol)
          else
            let (_voids, _bds) = if length voids' == length bds && last voids' == AtRho then (init voids', []) else (voids', drop (length voids') bds)
             in PA_FORMATION
                  attr'
                  (map (\at -> toCST at tabs eol) _voids)
                  ARROW
                  (toCST (ExFormation _bds) tabs eol)
    where
      voids :: [Binding] -> [Attribute]
      voids [] = []
      voids (bd : bds) = case bd of
        BiVoid attr -> attr : voids bds
        _ -> []
  toCST (BiTau attr exp) tabs eol = PA_TAU (toCST attr tabs eol) ARROW (toCST exp tabs eol)
  toCST (BiVoid attr) tabs eol = PA_VOID (toCST attr tabs eol) ARROW EMPTY
  toCST (BiDelta bts) tabs eol = PA_DELTA (toCST bts tabs eol)
  toCST (BiLambda func) _ _ = PA_LAMBDA func
  toCST (BiMetaLambda mt) _ _ = PA_META_LAMBDA (MT_FUNCTION (tail mt))

instance ToCST Binding APP_BINDING where
  toCST bd@(BiTau _ _) tabs eol = APP_BINDING (toCST bd tabs eol :: PAIR)

instance ToCST Bytes BYTES where
  toCST BtEmpty _ _ = BT_EMPTY
  toCST (BtOne byte) _ _ = BT_ONE byte
  toCST (BtMany bts) _ _ = BT_MANY bts
  toCST (BtMeta mt) _ _ = BT_META (MT_BYTES (tail mt))

instance ToCST Attribute ATTRIBUTE where
  toCST (AtLabel label) _ _ = AT_LABEL label
  toCST (AtAlpha idx) _ _ = AT_ALPHA ALPHA idx
  toCST AtPhi _ _ = AT_PHI PHI
  toCST AtRho _ _ = AT_RHO RHO
  toCST AtDelta _ _ = AT_DELTA DELTA
  toCST AtLambda _ _ = AT_LAMBDA LAMBDA
  toCST (AtMeta mt) _ _ = AT_META (MT_ATTRIBUTE (tail mt))

inlinedEOL :: Bool -> EOL
inlinedEOL True = NO_EOL
inlinedEOL False = EOL

tabOfEOL :: EOL -> Integer -> TAB
tabOfEOL EOL indent = TAB indent
tabOfEOL NO_EOL _ = TAB'

class HasEOL a where
  hasEOL :: a -> Bool

instance HasEOL [Binding] where
  hasEOL [] = False
  hasEOL (bd : rest) = hasEOL bd || hasEOL rest

instance HasEOL Binding where
  hasEOL (BiTau _ expr) = hasEOL expr
  hasEOL bd = False

instance HasEOL [Expression] where
  hasEOL [] = False
  hasEOL (expr : rest) = hasEOL expr || hasEOL rest

instance HasEOL Expression where
  hasEOL (ExFormation []) = False
  hasEOL (ExFormation _) = True
  hasEOL (DataNumber _) = False
  hasEOL (DataString _) = False
  hasEOL (BaseObject _) = False
  hasEOL (ExDispatch expr _) = hasEOL expr
  hasEOL (ExApplication expr tau) = hasEOL expr || hasEOL tau
  hasEOL expr = False
