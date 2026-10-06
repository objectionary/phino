{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Abridge (abridged) where

import CST
import qualified Data.Text as T
import Lining (toSingleLine)
import Render (render)

abridged :: Bool -> Int -> EXPRESSION -> EXPRESSION
abridged cut width = goExpr
  where
    goExpr :: EXPRESSION -> EXPRESSION
    goExpr expr@EX_FORMATION{..}
      | short expr = EX_FORMATION lsb eol tab (goIntact binding) eol' tab' rsb
      | otherwise = EX_FORMATION lsb eol tab (goBinding binding) eol' tab' rsb
    goExpr expr@EX_SINGLE{..}
      | short expr || salient pair = EX_SINGLE (goPair pair) space (goExpr formation)
      | otherwise = goExpr formation
    goExpr EX_DISPATCH{..} = EX_DISPATCH (goExpr expr) space attr
    goExpr EX_APPLICATION{..} = EX_APPLICATION (goExpr expr) space eol tab (goArgument argument) eol' tab' indent
    goExpr EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (goExpr expr)
    goExpr EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (goExpr expr)
    goExpr EX_BYTES{..} = EX_BYTES (goBytes bytes)
    goExpr expr = expr
    goBinding :: BINDING -> BINDING
    goBinding empty@BI_EMPTY{} = empty
    goBinding binding = headed (goBindings 0 (tail' binding))
      where
        tail' :: BINDING -> BINDINGS
        tail' BI_PAIR{..} = BDS_PAIR EOL tab pair bindings
        tail' BI_META{..} = BDS_META EOL tab meta bindings
        tail' BI_EMPTY{..} = BDS_EMPTY tab
        headed :: BINDINGS -> BINDING
        headed BDS_PAIR{..} = BI_PAIR pair bindings tab
        headed BDS_META{..} = BI_META meta bindings tab
        headed BDS_EMPTY{..} = BI_EMPTY tab
    goBindings :: Int -> BINDINGS -> BINDINGS
    goBindings folded BDS_PAIR{..}
      | salient pair = BDS_PAIR eol tab (goPair pair) (goBindings folded bindings)
      | otherwise = goBindings (folded + 1) bindings
    goBindings folded BDS_META{..} = BDS_META eol tab meta (goBindings folded bindings)
    goBindings 0 empty@BDS_EMPTY{} = empty
    goBindings folded empty@BDS_EMPTY{..} = BDS_PAIR EOL tab (PA_FOLDED folded) empty
    goPair :: PAIR -> PAIR
    goPair PA_TAU{..} = PA_TAU attr arrow (goExpr expr)
    goPair PA_ALPHA{..} = PA_ALPHA alpha arrow (goExpr expr)
    goPair PA_FORMATION{..} = PA_FORMATION attr voids arrow (goExpr expr)
    goPair PA_DELTA{..} = PA_DELTA (goBytes bytes)
    goPair pair = pair
    goArgument :: APP_ARGUMENT -> APP_ARGUMENT
    goArgument (AA_TAU APP_BINDING{..}) = AA_TAU (APP_BINDING (goPair pair))
    goArgument (AA_TAUS binding) = AA_TAUS (goIntact binding)
    goArgument (AA_EXPRS APP_ARG{..}) = AA_EXPRS (APP_ARG (goExpr expr) (goAppArgs args))
    goIntact :: BINDING -> BINDING
    goIntact BI_PAIR{..} = BI_PAIR (goPair pair) (goIntacts bindings) tab
    goIntact BI_META{..} = BI_META meta (goIntacts bindings) tab
    goIntact empty = empty
    goIntacts :: BINDINGS -> BINDINGS
    goIntacts BDS_PAIR{..} = BDS_PAIR eol tab (goPair pair) (goIntacts bindings)
    goIntacts BDS_META{..} = BDS_META eol tab meta (goIntacts bindings)
    goIntacts empty = empty
    goAppArgs :: APP_ARGS -> APP_ARGS
    goAppArgs AAS_EXPR{..} = AAS_EXPR eol tab (goExpr expr) (goAppArgs args)
    goAppArgs AAS_EMPTY = AAS_EMPTY
    goBytes :: BYTES -> BYTES
    goBytes (BT_MANY bts)
      | cut && length bts > 8 = BT_CUT (take 2 bts) (length bts - 4) (drop (length bts - 2) bts)
    goBytes bts = bts
    short :: EXPRESSION -> Bool
    short expr = T.length (render (toSingleLine expr)) <= width
    salient :: PAIR -> Bool
    salient PA_TAU{attr = AT_PHI{}} = True
    salient PA_FORMATION{attr = AT_PHI{}} = True
    salient PA_LAMBDA{} = True
    salient PA_META_LAMBDA{} = True
    salient PA_DELTA{} = True
    salient PA_META_DELTA{} = True
    salient _ = False
