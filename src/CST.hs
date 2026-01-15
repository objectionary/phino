{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
-- This module represents concrete syntax tree for phi-calculus program
module CST where

import AST
import Data.Maybe (isJust)
import Misc
import qualified Yaml as Y

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

data LAMBDA = LAMBDA | LAMBDA'
  deriving (Eq, Show)

data GLOBAL = Φ | Q
  deriving (Eq, Show)

data TERMINATION = DEAD | T
  deriving (Eq, Show)

data SPACE = SPACE
  deriving (Eq, Show)

data EOL = EOL | NO_EOL
  deriving (Eq, Show)

data DOTS = DOTS | DOTS'
  deriving (Eq, Show)

data BYTES
  = BT_EMPTY
  | BT_ONE String
  | BT_MANY [String]
  | BT_META META
  deriving (Eq, Show)

data META_HEAD
  = E -- 𝑒
  | E' -- e
  | A -- a
  | TAU -- 𝜏
  | TAU' -- \tau
  | B -- 𝐵
  | B' -- B
  | D -- δ
  | D' -- d
  | TAIL -- t
  | F -- F
  deriving (Eq, Show)

data EXCLAMATION = EXCL | NO_EXCL
  deriving (Eq, Show)

data META = META {excl :: EXCLAMATION, hd :: META_HEAD, rest :: String}
  deriving (Eq, Show)

data TAB
  = TAB {indent :: Int}
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
  | EX_XI {xi :: XI}
  | EX_ATTR {attr :: ATTRIBUTE} -- sugar for $.x -> just x
  | EX_TERMINATION {termination :: TERMINATION}
  | EX_FORMATION {lsb :: LSB, eol :: EOL, tab :: TAB, binding :: BINDING, eol' :: EOL, tab' :: TAB, rsb :: RSB}
  | EX_DISPATCH {expr :: EXPRESSION, attr :: ATTRIBUTE}
  | EX_APPLICATION {expr :: EXPRESSION, eol :: EOL, tab :: TAB, tau :: APP_BINDING, eol' :: EOL, tab' :: TAB, indent :: Int} -- e(a1 -> e1)
  | EX_APPLICATION_TAUS {expr :: EXPRESSION, eol :: EOL, tab :: TAB, taus :: BINDING, eol' :: EOL, tab' :: TAB, indent :: Int} -- e(a1 -> e1)(a2 -> e2)(...)
  | EX_APPLICATION_EXPRS {expr :: EXPRESSION, eol :: EOL, tab :: TAB, args :: APP_ARG, eol' :: EOL, tab' :: TAB, indent :: Int} -- e(e1, e2, ...)
  | EX_STRING {str :: String, tab :: TAB, rhos :: [Binding]}
  | EX_NUMBER {num :: Either Int Double, tab :: TAB, rhos :: [Binding]}
  | EX_META {meta :: META}
  | EX_META_TAIL {expr :: EXPRESSION, meta :: META}
  | EX_PHI_MEET {prefix :: Maybe String, idx :: Int, expr :: EXPRESSION}
  | EX_PHI_AGAIN {prefix :: Maybe String, idx :: Int, expr :: EXPRESSION}
  deriving (Eq, Show)

data ATTRIBUTE
  = AT_LABEL {label :: String}
  | AT_ALPHA {alpha :: ALPHA, idx :: Int}
  | AT_RHO {rho :: RHO}
  | AT_PHI {phi :: PHI}
  | AT_LAMBDA {lambda :: LAMBDA}
  | AT_DELTA {delta :: DELTA}
  | AT_META {meta :: META}
  | AT_REST {dots :: DOTS}
  deriving (Eq, Show)

data BELONGING
  = IN
  | NOT_IN
  deriving (Eq, Show)

data SET
  = ST_BINDING {binding :: BINDING}
  | ST_ATTRIBUTES {attrs :: [ATTRIBUTE]}
  deriving (Eq, Show)

data LOGIC_OPERATOR
  = AND
  | OR
  deriving (Eq, Show)

data EQUAL
  = EQUAL
  | NOT_EQUAL
  deriving (Eq, Show)

data NUMBER
  = ORDINAL {attr :: ATTRIBUTE}
  | LENGTH {binding :: BINDING}
  | LITERAL {num :: Int}
  deriving (Eq, Show)

data COMPARABLE
  = CMP_ATTR {attr :: ATTRIBUTE}
  | CMP_EXPR {expr :: EXPRESSION}
  | CMP_NUM {num :: NUMBER}
  deriving (Eq, Show)

data CONDITION
  = CO_EMPTY
  | CO_BELONGS {attr :: ATTRIBUTE, belongs :: BELONGING, set :: SET}
  | CO_LOGIC {conditions :: [CONDITION], operator :: LOGIC_OPERATOR}
  | CO_NF {expr :: EXPRESSION}
  | CO_NOT {condition :: CONDITION}
  | CO_COMPARE {left :: COMPARABLE, equal :: EQUAL, right :: COMPARABLE}
  | CO_MATCHES {regex :: String, expr :: EXPRESSION}
  | CO_PART_OF {expr :: EXPRESSION, binding :: BINDING}
  deriving (Eq, Show)

data EXTRA_ARG
  = ARG_EXPR {expr :: EXPRESSION}
  | ARG_ATTR {attr :: ATTRIBUTE}
  | ARG_BINDING {binding :: BINDING}
  | ARG_BYTES {bytes :: BYTES}
  deriving (Eq, Show)

data EXTRA = EXTRA {meta :: EXTRA_ARG, func :: String, args :: [EXTRA_ARG]}
  deriving (Eq, Show)

programToCST :: Program -> PROGRAM
programToCST = toCST'

expressionToCST :: Expression -> EXPRESSION
expressionToCST = toCST'

attributeToCST :: Attribute -> ATTRIBUTE
attributeToCST = toCST'

bindingsToCST :: [Binding] -> BINDING
bindingsToCST = toCST'

conditionToCST :: Y.Condition -> CONDITION
conditionToCST = toCST'

comparableToCST :: Y.Comparable -> COMPARABLE
comparableToCST = toCST'

numberToCST :: Y.Number -> NUMBER
numberToCST = toCST'

extraToCST :: Y.Extra -> EXTRA
extraToCST = toCST'

toCST' :: ToCST a b => a -> b
toCST' = (`toCST` (0, EOL))

metaTail :: String -> String
metaTail = tail

-- This class is used to convert AST to CST
-- CST is created with sugar and unicode
-- All further transformations must consider that
class ToCST a b where
  toCST :: a -> (Int, EOL) -> b

instance ToCST Program PROGRAM where
  toCST (Program expr) ctx = PR_SWEET LCB (toCST expr ctx) RCB

instance ToCST Expression EXPRESSION where
  toCST ExGlobal _ = EX_GLOBAL Φ
  toCST ExThis _ = EX_XI XI
  toCST (ExMeta mt) _ = EX_META (META NO_EXCL E (metaTail mt))
  toCST (ExMetaTail expr mt) ctx = EX_META_TAIL (toCST expr ctx) (META EXCL TAIL (metaTail mt))
  toCST ExTermination _ = EX_TERMINATION DEAD
  toCST (ExPhiMeet prefix idx expr) ctx = EX_PHI_MEET prefix idx (toCST expr ctx)
  toCST (ExPhiAgain prefix idx expr) ctx = EX_PHI_AGAIN prefix idx (toCST expr ctx)
  toCST (ExFormation [BiVoid AtRho]) ctx = toCST (ExFormation []) ctx
  toCST (ExFormation []) _ = EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY NO_TAB) NO_EOL NO_TAB RSB
  toCST (ExFormation bds) (tabs, eol) =
    let next = tabs + 1
        bds' = toCST (withoutLastVoidRho bds) (next, eol) :: BINDING
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
      withoutLastVoidRho (bd : bds') = bd : withoutLastVoidRho bds'
  toCST (DataString bts) (tabs, _) = EX_STRING (btsToStr bts) (TAB tabs) []
  toCST (DataNumber bts) (tabs, _) = EX_NUMBER (btsToNum bts) (TAB tabs) []
  toCST (ExDispatch ExThis attr) ctx = EX_ATTR (toCST attr ctx)
  toCST (ExDispatch expr attr) ctx = EX_DISPATCH (toCST expr ctx) (toCST attr ctx)
  -- Since we convert AST to CST in sweet notation, here we're trying to get rid of unnecessary rho bindings
  -- in primitives (more details here: https://github.com/objectionary/phino/issues/451)
  -- If we find something similar to:
  -- `Q.number(~0 -> Q.bytes(...), ^ -> ..., ^ -> ...)`
  -- We remove unnecessary rho bindings and save them to EX_STRING or EX_NUMBER so they can be successfully
  -- converted to salty notation without losing information.
  -- In the end we just get CST with data primitive which is printed correctly.
  -- If given application is not such primitive - we just convert it to one of the applications:
  -- 1. either with pure expression with arguments, which means there are incremented only alpha bindings
  -- 2. or with just bindings
  toCST app@(ExApplication _ _) ctx@(tabs, eol) =
    let (ex, ts, exs) = complexApplication app
        ex' = toCST ex ctx :: EXPRESSION
        next = tabs + 1
        (ts', rs) = withoutRhosInPrimitives ex ts
        obj = ExApplication ex (head ts')
     in if length ts' == 1 && isJust (matchDataObject obj)
          then applicationToPrimitive obj tabs rs
          else
            if null exs
              then
                EX_APPLICATION_TAUS
                  ex'
                  eol
                  (TAB next)
                  (toCST ts (next, eol) :: BINDING)
                  eol
                  (TAB tabs)
                  next
              else
                EX_APPLICATION_EXPRS
                  ex'
                  eol
                  (TAB next)
                  (toCST exs (next, eol))
                  eol
                  (TAB tabs)
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
      applicationToPrimitive :: Expression -> Int -> [Binding] -> EXPRESSION
      applicationToPrimitive (DataNumber bts) tabs = EX_NUMBER (btsToNum bts) (TAB tabs)
      applicationToPrimitive (DataString bts) tabs = EX_STRING (btsToStr bts) (TAB tabs)
      applicationToPrimitive _ _ = error "applicationToPrimitive expects DataNumber or DataString"
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
                      if idx == length exprs
                        then (before, taus', expr' : exprs)
                        else (before, taus', [])
                    _ -> (before, taus', [])
          complexApplication' (ExApplication expr (BiTau (AtAlpha 0) expr')) = (expr, [BiTau (AtAlpha 0) expr'], [expr'])
          complexApplication' (ExApplication expr tau) = (expr, [tau], [])
          complexApplication' expr = (expr, [], [])

instance ToCST [Expression] APP_ARG where
  toCST (expr : exprs) ctx = APP_ARG (toCST expr ctx) (toCST exprs ctx)
  toCST [] _ = error "toCST APP_ARG requires non-empty expression list"

instance ToCST [Expression] APP_ARGS where
  toCST [] _ = AAS_EMPTY
  toCST (expr : exprs) ctx@(tabs, eol) = AAS_EXPR eol (TAB tabs) (toCST expr ctx) (toCST exprs ctx)

instance ToCST [Binding] BINDING where
  toCST [] (tabs, _) = BI_EMPTY (TAB tabs)
  toCST (BiMeta mt : bds) ctx@(tabs, _) = BI_META (META NO_EXCL B (metaTail mt)) (toCST bds ctx) (TAB tabs)
  toCST (bd : bds) ctx@(tabs, _) = BI_PAIR (toCST bd ctx) (toCST bds ctx) (TAB tabs)

instance ToCST [Binding] BINDINGS where
  toCST [] (tabs, _) = BDS_EMPTY (TAB tabs)
  toCST (BiMeta mt : bds) ctx@(tabs, eol) = BDS_META eol (TAB tabs) (META NO_EXCL B (metaTail mt)) (toCST bds ctx)
  toCST (bd : bds) ctx@(tabs, eol) = BDS_PAIR eol (TAB tabs) (toCST bd ctx) (toCST bds ctx)

instance ToCST Binding PAIR where
  toCST (BiTau attr exp@(ExFormation bds)) ctx =
    let voids' = voids bds
        attr' = toCST attr ctx
     in if null voids'
          then PA_TAU attr' ARROW (toCST exp ctx)
          else
            let (_voids, _bds) = if length voids' == length bds && last voids' == AtRho then (init voids', []) else (voids', drop (length voids') bds)
             in PA_FORMATION
                  attr'
                  (map (`toCST` ctx) _voids)
                  ARROW
                  (toCST (ExFormation _bds) ctx)
    where
      voids :: [Binding] -> [Attribute]
      voids [] = []
      voids (bd : bds) = case bd of
        BiVoid attr -> attr : voids bds
        _ -> []
  toCST (BiTau attr exp) ctx = PA_TAU (toCST attr ctx) ARROW (toCST exp ctx)
  toCST (BiVoid attr) ctx = PA_VOID (toCST attr ctx) ARROW EMPTY
  toCST (BiDelta bts) ctx = PA_DELTA (toCST bts ctx)
  toCST (BiLambda func) _ = PA_LAMBDA func
  toCST (BiMetaLambda mt) _ = PA_META_LAMBDA (META EXCL F (metaTail mt))
  toCST (BiMeta mt) _ = error $ "BiMeta binding " ++ mt ++ " cannot be converted to PAIR"

instance ToCST Binding APP_BINDING where
  toCST bd@(BiTau _ _) ctx = APP_BINDING (toCST bd ctx :: PAIR)
  toCST bd _ = error $ "Only BiTau binding can be converted to APP_BINDING, got: " ++ show bd

instance ToCST Bytes BYTES where
  toCST BtEmpty _ = BT_EMPTY
  toCST (BtOne byte) _ = BT_ONE byte
  toCST (BtMany bts) _ = BT_MANY bts
  toCST (BtMeta mt) _ = BT_META (META NO_EXCL D (metaTail mt))

instance ToCST Attribute ATTRIBUTE where
  toCST (AtLabel label) _ = AT_LABEL label
  toCST (AtAlpha idx) _ = AT_ALPHA ALPHA idx
  toCST AtPhi _ = AT_PHI PHI
  toCST AtRho _ = AT_RHO RHO
  toCST AtDelta _ = AT_DELTA DELTA
  toCST AtLambda _ = AT_LAMBDA LAMBDA
  toCST (AtMeta mt) _ = AT_META (META NO_EXCL TAU (metaTail mt))

withoutXi :: [Y.Condition] -> [Y.Condition]
withoutXi conds = [cond | cond <- conds, not (singleXi cond)]
  where
    singleXi :: Y.Condition -> Bool
    singleXi (Y.Xi _) = True
    singleXi (Y.Not (Y.Xi _)) = True
    singleXi (Y.And [cond]) = singleXi cond
    singleXi (Y.Or [cond]) = singleXi cond
    singleXi _ = False

instance ToCST Y.Condition CONDITION where
  toCST (Y.Not (Y.In attr binding)) _ = CO_BELONGS (attributeToCST attr) NOT_IN (ST_BINDING (bindingsToCST [binding]))
  toCST (Y.Not (Y.Eq left right)) _ = CO_COMPARE (comparableToCST left) NOT_EQUAL (comparableToCST right)
  toCST (Y.Not (Y.Alpha attr)) _ = CO_BELONGS (attributeToCST attr) NOT_IN (ST_ATTRIBUTES [attributeToCST (AtAlpha 0), attributeToCST (AtAlpha 1), AT_REST DOTS])
  toCST (Y.In attr binding) _ = CO_BELONGS (attributeToCST attr) IN (ST_BINDING (bindingsToCST [binding]))
  toCST (Y.And conds) _ = case withoutXi conds of
    [] -> CO_EMPTY
    conds' -> CO_LOGIC (map toCST' (withoutXi conds')) AND
  toCST (Y.Or conds) _ = case withoutXi conds of
    [] -> CO_EMPTY
    conds' -> CO_LOGIC (map toCST' (withoutXi conds')) OR
  toCST (Y.Alpha attr) _ = CO_BELONGS (attributeToCST attr) IN (ST_ATTRIBUTES [attributeToCST (AtAlpha 0), attributeToCST (AtAlpha 1), AT_REST DOTS])
  toCST (Y.NF expr) _ = CO_NF (expressionToCST expr)
  toCST (Y.Not cond) _ = CO_NOT (conditionToCST cond)
  toCST (Y.Eq left right) _ = CO_COMPARE (comparableToCST left) EQUAL (comparableToCST right)
  toCST (Y.Matches regex expr) _ = CO_MATCHES regex (expressionToCST expr)
  toCST (Y.PartOf expr binding) _ = CO_PART_OF (expressionToCST expr) (bindingsToCST [binding])
  toCST (Y.Xi _) _ = CO_EMPTY

instance ToCST Y.Comparable COMPARABLE where
  toCST (Y.CmpAttr attr) _ = CMP_ATTR (attributeToCST attr)
  toCST (Y.CmpExpr expr) _ = CMP_EXPR (expressionToCST expr)
  toCST (Y.CmpNum num) _ = CMP_NUM (numberToCST num)

instance ToCST Y.Number NUMBER where
  toCST (Y.Ordinal attr) _ = ORDINAL (attributeToCST attr)
  toCST (Y.Length binding) _ = LENGTH (bindingsToCST [binding])
  toCST (Y.Literal num) _ = LITERAL num

instance ToCST Y.ExtraArgument EXTRA_ARG where
  toCST (Y.ArgAttribute attr) _ = ARG_ATTR (attributeToCST attr)
  toCST (Y.ArgExpression expr) _ = ARG_EXPR (expressionToCST expr)
  toCST (Y.ArgBinding binding) _ = ARG_BINDING (bindingsToCST [binding])
  toCST (Y.ArgBytes bytes) _ = ARG_BYTES (toCST' bytes)

instance ToCST Y.Extra EXTRA where
  toCST Y.Extra{..} _ = EXTRA (toCST' meta) function (map toCST' args)
