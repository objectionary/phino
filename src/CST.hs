{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
module CST where

import AST
import Bytes (NonFinite, btsIsUtf8, btsSize, btsToNum, btsToStr)
import qualified Data.Text as T
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

data RHO = RHO | CARET | RHO'
  deriving (Eq, Show)

data DELTA = DELTA | DELTA'
  deriving (Eq, Show)

data XI = XI | DOLLAR | XI'
  deriving (Eq, Show)

data LAMBDA = LAMBDA | LAMBDA'
  deriving (Eq, Show)

data GLOBAL = Φ | Q
  deriving (Eq, Show)

data TERMINATION = DEAD | T
  deriving (Eq, Show)

data SPACE = SPACE | NO_SPACE
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
  | BT_PIPED BYTES
  | BT_CUT [String] Int [String]
  deriving (Eq, Show)

data META_HEAD
  = E
  | E'
  | N
  | N'
  | K
  | K'
  | A
  | TAU
  | TAU'
  | I
  | I'
  | B
  | B'
  | D
  | D'
  | D''
  | F
  | F'
  | F''
  | S
  | S'
  | S''
  deriving (Eq, Show)

data EXCLAMATION = EXCL | NO_EXCL
  deriving (Eq, Show)

data META = META {excl :: EXCLAMATION, hd :: META_HEAD, rest :: T.Text}
  deriving (Eq, Show)

data TAB
  = TAB {indent :: Int}
  | TAB'
  | NO_TAB
  deriving (Eq, Show)

data ALPHA' = ALPHA | ALPHA'
  deriving (Eq, Show)

data ALPHA
  = AL_IDX {sym :: ALPHA', idx :: Int}
  | AL_META {sym :: ALPHA', meta :: META}
  deriving (Eq, Show)

data PAIR
  = PA_TAU {attr :: ATTRIBUTE, arrow :: ARROW, expr :: EXPRESSION}
  | PA_ALPHA {alpha :: ALPHA, arrow :: ARROW, expr :: EXPRESSION}
  | PA_FORMATION {attr :: ATTRIBUTE, voids :: [ATTRIBUTE], arrow :: ARROW, expr :: EXPRESSION}
  | PA_VOID {attr :: ATTRIBUTE, arrow :: ARROW, void :: VOID}
  | PA_LAMBDA {func :: T.Text}
  | PA_LAMBDA' {func :: T.Text}
  | PA_META_LAMBDA {meta :: META}
  | PA_META_LAMBDA' {meta :: META}
  | PA_DELTA {bytes :: BYTES}
  | PA_DELTA' {bytes :: BYTES}
  | PA_META_DELTA {meta :: META}
  | PA_META_DELTA' {meta :: META}
  | PA_FOLDED {count :: Int}
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

data APP_ARG = APP_ARG {expr :: EXPRESSION, args :: APP_ARGS}
  deriving (Eq, Show)

data APP_ARGS
  = AAS_EXPR {eol :: EOL, tab :: TAB, expr :: EXPRESSION, args :: APP_ARGS}
  | AAS_EMPTY
  deriving (Eq, Show)

data APP_ARGUMENT
  = AA_TAU APP_BINDING
  | AA_TAUS BINDING
  | AA_EXPRS APP_ARG
  deriving (Eq, Show)

data EXPRESSION
  = EX_GLOBAL {global :: GLOBAL}
  | EX_XI {xi :: XI}
  | EX_ATTR {attr :: ATTRIBUTE}
  | EX_TERMINATION {termination :: TERMINATION}
  | EX_FORMATION {lsb :: LSB, eol :: EOL, tab :: TAB, binding :: BINDING, eol' :: EOL, tab' :: TAB, rsb :: RSB}
  | EX_DISPATCH {expr :: EXPRESSION, space :: SPACE, attr :: ATTRIBUTE}
  | EX_APPLICATION {expr :: EXPRESSION, space :: SPACE, eol :: EOL, tab :: TAB, argument :: APP_ARGUMENT, eol' :: EOL, tab' :: TAB, indent :: Int}
  | EX_STRING {str :: String, tab :: TAB, rhos :: [Argument]}
  | EX_NUMBER {num :: Either Int Double, tab :: TAB, rhos :: [Argument]}
  | EX_NONFINITE {global :: GLOBAL, nonfinite :: NonFinite, tab :: TAB, rhos :: [Argument]} -- @todo #1427:30min drop EX_NONFINITE with its Render, Sugar and Encoding clauses and the NonFinite helpers of Bytes, since nothing builds it any more
  | EX_META {meta :: META}
  | EX_PHI_MEET {prefix :: Maybe String, idx :: Int, expr :: EXPRESSION}
  | EX_PHI_AGAIN {prefix :: Maybe String, idx :: Int, expr :: EXPRESSION}
  | EX_BYTES {bytes :: BYTES}
  | EX_SINGLE {pair :: PAIR, space :: SPACE, formation :: EXPRESSION}
  deriving (Eq, Show)

data ATTRIBUTE
  = AT_LABEL {label :: T.Text}
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
  | GREATER
  | NOT_GREATER
  deriving (Eq, Show)

data NUMBER
  = IDX_META {meta :: META}
  | LENGTH {binding :: BINDING}
  | DOMAIN {binding :: BINDING}
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
  | CO_ABSOLUTE {expr :: EXPRESSION, belongs :: BELONGING}
  | CO_NOT {condition :: CONDITION}
  | CO_COMPARE {left :: COMPARABLE, equal :: EQUAL, right :: COMPARABLE}
  | CO_MATCHES {regex :: String, expr :: EXPRESSION}
  | CO_PART_OF {expr :: EXPRESSION, binding :: BINDING}
  | CO_DISJOINT {attrs :: [ATTRIBUTE], groups :: [BINDING]}
  | CO_SUBSET {attrs :: [ATTRIBUTE], belongs :: BELONGING, groups :: [BINDING]}
  | CO_FORMATION {expr :: EXPRESSION}
  deriving (Eq, Show)

data EXTRA_ARG
  = ARG_EXPR {expr :: EXPRESSION}
  | ARG_ATTR {attr :: ATTRIBUTE}
  | ARG_BINDING {binding :: BINDING}
  | ARG_BYTES {bytes :: BYTES}
  deriving (Eq, Show)

data EXTRA = EXTRA {meta :: EXTRA_ARG, func :: String, args :: [EXTRA_ARG]}
  deriving (Eq, Show)

expressionToCST :: Expression -> EXPRESSION
expressionToCST = toCST'

expressionToCSTFrom :: Int -> Expression -> EXPRESSION
expressionToCSTFrom tabs expr = toCST expr (tabs, EOL)

-- A number can be rendered in sweet form when it is finite, and so has a
-- numeric literal. Every non-finite pattern is kept in its byte form, since
-- `Φ.nan`, `Φ.pinf` and `Φ.ninf` are ordinary root dispatches (see #1427).
sweetNumber :: Bytes -> Bool
sweetNumber bts
  | btsSize bts /= 8 = False
sweetNumber bts = case btsToNum bts of
  Right dbl | isNaN dbl || isInfinite dbl -> False
  _ -> True

sweetString :: Bytes -> Bool
sweetString = btsIsUtf8

sweetCollapsible :: Expression -> Bool
sweetCollapsible (DataNumber bts) = sweetNumber bts
sweetCollapsible (DataString bts) = sweetString bts
sweetCollapsible _ = True

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

metaTail :: T.Text -> T.Text
metaTail = T.drop 1

anyMeta :: META_HEAD -> META
anyMeta hd' = META NO_EXCL hd' T.empty

exMetaHead :: T.Text -> META_HEAD
exMetaHead mt
  | T.isPrefixOf "n" mt = N
  | T.isPrefixOf "k" mt = K
  | otherwise = E

class ToCST a b where
  toCST :: a -> (Int, EOL) -> b

instance ToCST Expression EXPRESSION where
  toCST ExRoot _ = EX_GLOBAL Φ
  toCST ExXi _ = EX_XI XI
  toCST (ExMeta mt) _ = EX_META (META NO_EXCL (exMetaHead mt) (metaTail mt))
  toCST (ExAny (Slot kind _)) _ = EX_META (anyMeta (exMetaHead kind))
  toCST ExTermination _ = EX_TERMINATION DEAD
  toCST (ExBytes bts) ctx = EX_BYTES (toCST bts ctx)
  toCST (ExPhiMeet prefix idx expr) ctx = EX_PHI_MEET prefix idx (toCST expr ctx)
  toCST (ExPhiAgain prefix idx expr) ctx = EX_PHI_AGAIN prefix idx (toCST expr ctx)
  toCST (ExFormation []) _ = EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY NO_TAB) NO_EOL NO_TAB RSB
  toCST (ExFormation bds) ctx@(tabs, eol) =
    maybe full (\sole -> EX_SINGLE sole NO_SPACE full) (single bds)
    where
      full :: EXPRESSION
      full =
        let next = tabs + 1
            bds' = toCST bds (next, eol) :: BINDING
         in EX_FORMATION
              LSB
              EOL
              (TAB next)
              bds'
              EOL
              (TAB tabs)
              RSB
      single :: [Binding] -> Maybe PAIR
      single [BiTau AtDelta _] = Nothing
      single [BiTau AtLambda _] = Nothing
      single [bd@(BiTau _ ExFormation{})] | inlined (toCST bd ctx) = Nothing
      single [BiTau attr expr] = Just (PA_TAU (toCST attr ctx) ARROW (toCST expr ctx))
      single [BiVoid AtRho] = Nothing
      single [bd@(BiVoid _)] = Just (toCST bd ctx)
      single [bd@(BiDelta _)] = Just (toCST bd ctx)
      single [bd@(BiLambda _)] = Just (toCST bd ctx)
      single _ = Nothing
      inlined :: PAIR -> Bool
      inlined PA_FORMATION{voids = _ : _} = True
      inlined _ = False
  toCST (DataString bts) (tabs, _) | sweetString bts = EX_STRING (btsToStr bts) (TAB tabs) []
  toCST (DataNumber bts) (tabs, _) | sweetNumber bts = EX_NUMBER (btsToNum bts) (TAB tabs) []
  toCST (ExDispatch ExXi attr) ctx = EX_ATTR (toCST attr ctx)
  toCST (ExDispatch expr attr) ctx = EX_DISPATCH (toCST expr ctx) NO_SPACE (toCST attr ctx)
  toCST app@(ExApplication _ _) ctx@(tabs, eol) =
    let (ex, ts, exs) = complexApplication app
        ex' = toCST ex ctx :: EXPRESSION
        next = tabs + 1
        (ts', rs) = withoutRhosInPrimitives ex ts
        obj = ExApplication ex (head' ts')
     in if length ts' == 1 && dataPrimitive obj && sweetCollapsible obj
          then applicationToPrimitive obj tabs rs
          else
            if null exs
              then
                EX_APPLICATION
                  ex'
                  NO_SPACE
                  eol
                  (TAB next)
                  (AA_TAUS (toCST ts (next, eol) :: BINDING))
                  eol
                  (TAB tabs)
                  next
              else
                EX_APPLICATION
                  ex'
                  NO_SPACE
                  eol
                  (TAB next)
                  (AA_EXPRS (toCST exs (next, eol)))
                  eol
                  (TAB tabs)
                  next
    where
      primitives :: [T.Text]
      primitives = ["number", "string"]
      dataPrimitive :: Expression -> Bool
      dataPrimitive obj' = case matchDataObject obj' of
        Just (label, _) -> label `elem` primitives
        Nothing -> False
      withoutRhosInPrimitives :: Expression -> [Argument] -> ([Argument], [Argument])
      withoutRhosInPrimitives _ [] = ([], [])
      withoutRhosInPrimitives obj@(BaseObject label) bds@(rho@(ArTau AtRho _) : rest)
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
      applicationToPrimitive :: Expression -> Int -> [Argument] -> EXPRESSION
      applicationToPrimitive (DataNumber bts) tabs rhos = EX_NUMBER (btsToNum bts) (TAB tabs) rhos
      applicationToPrimitive (DataString bts) tabs rhos = EX_STRING (btsToStr bts) (TAB tabs) rhos
      applicationToPrimitive _ _ _ = error "applicationToPrimitive expects DataNumber or DataString"
      complexApplication :: Expression -> (Expression, [Argument], [Expression])
      complexApplication expr =
        let (expr', taus', exprs') = complexApplication' expr
         in (expr', reverse taus', reverse exprs')
        where
          complexApplication' :: Expression -> (Expression, [Argument], [Expression])
          complexApplication' (ExApplication (ExApplication expr tau) tau') =
            let (before, taus, exprs) = complexApplication' (ExApplication expr tau)
                taus' = tau' : taus
             in if null exprs
                  then (before, taus', [])
                  else case tau' of
                    ArAlpha (Alpha idx) expr' ->
                      if idx == length exprs
                        then (before, taus', expr' : exprs)
                        else (before, taus', [])
                    _ -> (before, taus', [])
          complexApplication' (ExApplication expr (ArAlpha (Alpha 0) expr')) = (expr, [ArAlpha (Alpha 0) expr'], [expr'])
          complexApplication' (ExApplication expr tau) = (expr, [tau], [])
          complexApplication' expr = (expr, [], [])
      head' :: [a] -> a
      head' [] = error "Should never be called"
      head' (x : _) = x

instance ToCST [Expression] APP_ARG where
  toCST (expr : exprs) ctx = APP_ARG (toCST expr ctx) (toCST exprs ctx)
  toCST [] _ = error "toCST APP_ARG requires non-empty expression list"

instance ToCST [Expression] APP_ARGS where
  toCST [] _ = AAS_EMPTY
  toCST (expr : exprs) ctx@(tabs, eol) = AAS_EXPR eol (TAB tabs) (toCST expr ctx) (toCST exprs ctx)

instance ToCST [Binding] BINDING where
  toCST [] (tabs, _) = BI_EMPTY (TAB tabs)
  toCST (BiMeta mt : bds) ctx@(tabs, _) = BI_META (META NO_EXCL B (metaTail mt)) (toCST bds ctx) (TAB tabs)
  toCST (BiAny _ : bds) ctx@(tabs, _) = BI_META (anyMeta B) (toCST bds ctx) (TAB tabs)
  toCST (bd : bds) ctx@(tabs, _) = BI_PAIR (toCST bd ctx) (toCST bds ctx) (TAB tabs)

instance ToCST [Binding] BINDINGS where
  toCST [] (tabs, _) = BDS_EMPTY (TAB tabs)
  toCST (BiMeta mt : bds) ctx@(tabs, eol) = BDS_META eol (TAB tabs) (META NO_EXCL B (metaTail mt)) (toCST bds ctx)
  toCST (BiAny _ : bds) ctx@(tabs, eol) = BDS_META eol (TAB tabs) (anyMeta B) (toCST bds ctx)
  toCST (bd : bds) ctx@(tabs, eol) = BDS_PAIR eol (TAB tabs) (toCST bd ctx) (toCST bds ctx)

instance ToCST Binding PAIR where
  toCST (BiTau attr exp@(ExFormation bds)) ctx =
    let (head', rest) = span positionless bds
        voids' = [void | BiVoid void <- head']
        others = filter (not . isVoid) head'
        attr' = toCST attr ctx
     in if null voids'
          then PA_TAU attr' ARROW (toCST exp ctx)
          else
            PA_FORMATION
              attr'
              (map (`toCST` ctx) voids')
              ARROW
              (toCST (ExFormation (others ++ rest)) ctx)
    where
      positionless :: Binding -> Bool
      positionless BiVoid{} = True
      positionless BiLambda{} = True
      positionless BiDelta{} = True
      positionless _ = False
      isVoid :: Binding -> Bool
      isVoid BiVoid{} = True
      isVoid _ = False
  toCST (BiTau attr exp) ctx = PA_TAU (toCST attr ctx) ARROW (toCST exp ctx)
  toCST (BiVoid attr) ctx = PA_VOID (toCST attr ctx) ARROW EMPTY
  toCST (BiDelta bts) ctx = PA_DELTA (toCST bts ctx)
  toCST (BiLambda (Function name)) _ = PA_LAMBDA name
  toCST (BiLambda (FnMeta mt)) _ = PA_META_LAMBDA (META NO_EXCL F (metaTail mt))
  toCST (BiLambda (FnAny _)) _ = PA_META_LAMBDA (anyMeta F)
  toCST (BiLambda (FnSymbol idx)) _ = PA_META_LAMBDA (META NO_EXCL S (T.pack (show idx)))
  toCST (BiLambda (FnFresh _)) _ = PA_META_LAMBDA (anyMeta S)
  toCST (BiMeta mt) _ = error $ "BiMeta binding " ++ T.unpack mt ++ " cannot be converted to PAIR"
  toCST (BiAny _) _ = error "An anonymous meta binding cannot be converted to PAIR"

instance ToCST Argument PAIR where
  toCST (ArTau attr exp) ctx = toCST (BiTau attr exp) ctx
  toCST (ArAlpha alpha exp) ctx = PA_ALPHA (toCST alpha ctx) ARROW (toCST exp ctx)

instance ToCST [Argument] BINDING where
  toCST [] (tabs, _) = BI_EMPTY (TAB tabs)
  toCST (arg : args) ctx@(tabs, _) = BI_PAIR (toCST arg ctx) (toCST args ctx) (TAB tabs)

instance ToCST [Argument] BINDINGS where
  toCST [] (tabs, _) = BDS_EMPTY (TAB tabs)
  toCST (arg : args) ctx@(tabs, eol) = BDS_PAIR eol (TAB tabs) (toCST arg ctx) (toCST args ctx)

instance ToCST Binding APP_BINDING where
  toCST bd@(BiTau _ _) ctx = APP_BINDING (toCST bd ctx :: PAIR)
  toCST bd _ = error $ "Only BiTau binding can be converted to APP_BINDING, got: " ++ show bd

instance ToCST Bytes BYTES where
  toCST BtEmpty _ = BT_EMPTY
  toCST (BtOne byte) _ = BT_ONE byte
  toCST (BtMany bts) _ = BT_MANY bts
  toCST (BtMeta mt) _ = BT_META (META NO_EXCL D (metaTail mt))
  toCST (BtAny _) _ = BT_META (anyMeta D)

instance ToCST Attribute ATTRIBUTE where
  toCST (AtLabel label) _ = AT_LABEL label
  toCST AtPhi _ = AT_PHI PHI
  toCST AtRho _ = AT_RHO RHO
  toCST AtDelta _ = AT_DELTA DELTA
  toCST AtLambda _ = AT_LAMBDA LAMBDA
  toCST (AtMeta mt) _ = AT_META (META NO_EXCL TAU (metaTail mt))
  toCST (AtAny _) _ = AT_META (anyMeta TAU)

instance ToCST Alpha ALPHA where
  toCST (Alpha idx) _ = AL_IDX ALPHA idx
  toCST (AlMeta mt) _ = AL_META ALPHA (META NO_EXCL I (metaTail mt))
  toCST (AlAny _) _ = AL_META ALPHA (anyMeta I)

instance ToCST Y.Condition CONDITION where
  toCST (Y.Not (Y.In [attr] [binding])) _ = CO_BELONGS (attributeToCST attr) NOT_IN (ST_BINDING (bindingsToCST [binding]))
  toCST (Y.Not (Y.In attrs groups)) _ = CO_SUBSET (map attributeToCST attrs) NOT_IN (map (\bd -> bindingsToCST [bd]) groups)
  toCST (Y.Not (Y.Eq left right)) _ = CO_COMPARE (comparableToCST left) NOT_EQUAL (comparableToCST right)
  toCST (Y.Not (Y.Gt left right)) _ = CO_COMPARE (comparableToCST left) NOT_GREATER (comparableToCST right)
  toCST (Y.Not (Y.Absolute expr)) _ = CO_ABSOLUTE (expressionToCST expr) NOT_IN
  toCST (Y.Absolute expr) _ = CO_ABSOLUTE (expressionToCST expr) IN
  toCST (Y.Disjoint attrs groups) _ = CO_DISJOINT (map attributeToCST attrs) (map (\bd -> bindingsToCST [bd]) groups)
  toCST (Y.In [attr] [binding]) _ = CO_BELONGS (attributeToCST attr) IN (ST_BINDING (bindingsToCST [binding]))
  toCST (Y.In attrs groups) _ = CO_SUBSET (map attributeToCST attrs) IN (map (\bd -> bindingsToCST [bd]) groups)
  toCST (Y.And conds) _ = case conds of
    [] -> CO_EMPTY
    _ -> CO_LOGIC (map toCST' conds) AND
  toCST (Y.Or conds) _ = case conds of
    [] -> CO_EMPTY
    _ -> CO_LOGIC (map toCST' conds) OR
  toCST (Y.NF expr) _ = CO_NF (expressionToCST expr)
  toCST (Y.Not cond) _ = CO_NOT (conditionToCST cond)
  toCST (Y.Eq left right) _ = CO_COMPARE (comparableToCST left) EQUAL (comparableToCST right)
  toCST (Y.Gt left right) _ = CO_COMPARE (comparableToCST left) GREATER (comparableToCST right)
  toCST (Y.Matches regex expr) _ = CO_MATCHES regex (expressionToCST expr)
  toCST (Y.PartOf expr binding) _ = CO_PART_OF (expressionToCST expr) (bindingsToCST [binding])
  toCST (Y.IsFormation expr) _ = CO_FORMATION (expressionToCST expr)

instance ToCST Y.Comparable COMPARABLE where
  toCST (Y.CmpAttr attr) _ = CMP_ATTR (attributeToCST attr)
  toCST (Y.CmpExpr expr) _ = CMP_EXPR (expressionToCST expr)
  toCST (Y.CmpNum num) _ = CMP_NUM (numberToCST num)

instance ToCST Y.Number NUMBER where
  toCST (Y.MetaIndex mt) _ = IDX_META (META NO_EXCL I (metaTail mt))
  toCST (Y.AnyIndex _) _ = IDX_META (anyMeta I)
  toCST (Y.Length binding) _ = LENGTH (bindingsToCST [binding])
  toCST (Y.Domain binding) _ = DOMAIN (bindingsToCST [binding])
  toCST (Y.Literal num) _ = LITERAL num

instance ToCST Y.ExtraArgument EXTRA_ARG where
  toCST (Y.ArgAttribute attr) _ = ARG_ATTR (attributeToCST attr)
  toCST (Y.ArgExpression expr) _ = ARG_EXPR (expressionToCST expr)
  toCST (Y.ArgBinding binding) _ = ARG_BINDING (bindingsToCST [binding])
  toCST (Y.ArgBytes bytes) _ = ARG_BYTES (toCST' bytes)

instance ToCST Y.Extra EXTRA where
  toCST Y.Extra{..} _ = EXTRA (toCST' meta) function (map toCST' args)
