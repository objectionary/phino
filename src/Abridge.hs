{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The spelling a term takes in a protocol written under '--abridged' (#1465).
-- A formation carrying a whole standard object flattens into a line tens of
-- thousands of characters long, and every short line of the protocol ends up
-- between two walls of text. So a formation whose flat spelling runs past
-- sixty characters keeps its salient bindings — φ, Δ and λ, the ones saying
-- what the object decorates, holds and fires — and folds the rest into a
-- count, '+34 attrs'; a shorter one says little enough to keep them all. A
-- byte string past eight bytes keeps its first four and its length, however
-- short the formation holding it, so a wide Δ never blows a line either. The
-- metas of a rule are kept, since they stand for bindings and are none. The
-- arguments of an application are never folded, since they are what the
-- object is applied to, not what it carries.
module Abridge (abridged) where

import CST
import qualified Data.Text as T
import Lining (toSingleLine)
import Render (render)

abridged :: EXPRESSION -> EXPRESSION
abridged = goExpr
  where
    goExpr :: EXPRESSION -> EXPRESSION
    goExpr expr@EX_FORMATION{..}
      | short expr = EX_FORMATION lsb eol tab (goIntact binding) eol' tab' rsb
      | otherwise = EX_FORMATION lsb eol tab (goBinding binding) eol' tab' rsb
    goExpr expr@EX_SINGLE{..}
      | short expr || salient pair = EX_SINGLE (goPair pair) (goExpr formation)
      | otherwise = goExpr formation
    goExpr EX_DISPATCH{..} = EX_DISPATCH (goExpr expr) space attr
    goExpr EX_APPLICATION{..} = EX_APPLICATION (goExpr expr) space eol tab (goArgument argument) eol' tab' indent
    goExpr EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (goExpr expr)
    goExpr EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (goExpr expr)
    goExpr EX_BYTES{..} = EX_BYTES (goBytes bytes)
    goExpr expr = expr
    -- The bindings of a long formation: the salient ones and the metas kept in
    -- their order, the rest counted into one folded pair closing the list.
    goBinding :: BINDING -> BINDING
    goBinding empty@BI_EMPTY{} = empty
    goBinding binding = headed (goBindings 0 (tail' binding))
      where
        -- The whole chain as a tail, so the head folds the same way every
        -- other binding does, and the tail made a head again once folded.
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
    -- The bindings of a short formation or of an application, every one kept.
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
      | length bts > 8 = BT_CUT (take 4 bts) (length bts)
    goBytes bts = bts
    -- Whether a formation spelled flat fits in sixty characters.
    short :: EXPRESSION -> Bool
    short expr = T.length (render (toSingleLine expr)) <= 60
    -- Whether a binding says what the object decorates, holds or fires.
    salient :: PAIR -> Bool
    salient PA_TAU{attr = AT_PHI{}} = True
    salient PA_FORMATION{attr = AT_PHI{}} = True
    salient PA_LAMBDA{} = True
    salient PA_META_LAMBDA{} = True
    salient PA_DELTA{} = True
    salient PA_META_DELTA{} = True
    salient _ = False
