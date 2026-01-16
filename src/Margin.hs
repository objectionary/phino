{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Margin (defaultMargin, withMargin, WithMargin) where

import CST
import Lining (ToSingleLine (..))
import Render (Render (..))

defaultMargin :: Int
defaultMargin = 80

withMargin :: (WithMargin a) => Int -> a -> a
withMargin margin = withMargin' (0, margin)

class WithMargin a where
  withMargin' :: (Int, Int) -> a -> a

instance WithMargin PROGRAM where
  withMargin' (_, margin) PR_SWEET{..} = PR_SWEET lcb (withMargin' (2, margin) expr) rcb space
  withMargin' (_, margin) PR_SALTY{..} =
    let before = lengthOf global + lengthOf arrow + 2 -- 'Q -> '
     in PR_SALTY global arrow (withMargin' (before, margin) expr)

instance WithMargin EXPRESSION where
  withMargin' _ ex@EX_FORMATION{binding = BI_EMPTY{}} = ex
  withMargin' (extra, margin) ex@EX_FORMATION{tab = tab@(TAB indent), ..} =
    let single = toSingleLine ex
        ex' = EX_FORMATION lsb EOL tab (withMargin' (indent, margin) binding) EOL tab' rsb
     in if lengthOf single + extra <= margin then single else ex'
  withMargin' _ num@EX_NUMBER{} = num
  withMargin' _ str@EX_STRING{} = str
  withMargin' cfg EX_DISPATCH{..} = EX_DISPATCH (withMargin' cfg expr) space attr
  withMargin' cfg EX_META_TAIL{..} = EX_META_TAIL (withMargin' cfg expr) meta
  withMargin' cfg EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (withMargin' cfg expr)
  withMargin' cfg EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (withMargin' cfg expr)
  withMargin' cfg@(extra, margin) ex@EX_APPLICATION{tab = tab@(TAB indt), ..} =
    let single = toSingleLine ex
        main = withMargin' cfg expr
        singleMain = toSingleLine main
        extra' = length (last (lines (render main))) + 4 -- 2 spaces + 2 braces around argument
        arg = withMargin' (indt, margin) tau
        singleArg = toSingleLine arg
     in if
          | lengthOf single + extra <= margin -> single
          | lengthOf singleMain + extra <= margin -> EX_APPLICATION singleMain space EOL tab arg EOL tab' indent
          | lengthOf singleArg + extra' <= margin -> EX_APPLICATION main space NO_EOL TAB' singleArg NO_EOL TAB' indent
          | otherwise -> EX_APPLICATION main space EOL tab arg EOL tab' indent
  withMargin' cfg@(extra, margin) ex@EX_APPLICATION_EXPRS{tab = tab@(TAB indt), ..} =
    let single = toSingleLine ex
        main = withMargin' cfg expr
        singleMain = toSingleLine main
        extra' = length (last (lines (render main))) + 4 -- 2 spaces + 2 braces around arguments
        exprs = withMargin' (indt, margin) args
        singleExprs = toSingleLine exprs
     in if
          | lengthOf single + extra <= margin -> single
          | lengthOf singleMain + extra <= margin -> EX_APPLICATION_EXPRS singleMain space EOL tab exprs EOL tab' indent
          | lengthOf singleExprs + extra' <= margin -> EX_APPLICATION_EXPRS main space NO_EOL TAB' singleExprs NO_EOL TAB' indent
          | otherwise -> EX_APPLICATION_EXPRS main space EOL tab exprs EOL tab' indent
  withMargin' cfg@(extra, margin) ex@EX_APPLICATION_TAUS{tab = tab@(TAB indt), ..} =
    let single = toSingleLine ex
        main = withMargin' cfg expr
        singleMain = toSingleLine main
        extra' = length (last (lines (render main))) + 4 -- 2 spaces + 2 braces around arguments
        taus' = withMargin' (indt, margin) taus
        singleTaus = toSingleLine taus'
     in if
          | lengthOf single + extra <= margin -> single
          | lengthOf singleMain + extra <= margin -> EX_APPLICATION_TAUS singleMain space EOL tab taus' EOL tab' indent
          | lengthOf singleTaus + extra' <= margin -> EX_APPLICATION_TAUS main space NO_EOL TAB' singleTaus NO_EOL TAB' indent
          | otherwise -> EX_APPLICATION_TAUS main space EOL tab taus' EOL tab' indent
  withMargin' _ ex = ex

instance WithMargin APP_BINDING where
  withMargin' cfg APP_BINDING{..} = APP_BINDING (withMargin' cfg pair)

instance WithMargin APP_ARG where
  withMargin' cfg@(extra, margin) arg@APP_ARG{..} =
    let single = toSingleLine arg
     in if lengthOf single + extra <= margin then single else APP_ARG (withMargin' cfg expr) (withMargin' cfg args)

instance WithMargin APP_ARGS where
  withMargin' cfg AAS_EXPR{..} = AAS_EXPR EOL tab (withMargin' cfg expr) (withMargin' cfg args)
  withMargin' _ args = args

instance WithMargin BINDING where
  withMargin' cfg BI_PAIR{..} = BI_PAIR (withMargin' cfg pair) (withMargin' cfg bindings) tab
  withMargin' cfg BI_META{..} = BI_META meta (withMargin' cfg bindings) tab
  withMargin' _ bd = bd

instance WithMargin BINDINGS where
  withMargin' cfg BDS_PAIR{..} = BDS_PAIR EOL tab (withMargin' cfg pair) (withMargin' cfg bindings)
  withMargin' cfg BDS_META{..} = BDS_META EOL tab meta (withMargin' cfg bindings)
  withMargin' _ bds = bds

instance WithMargin PAIR where
  withMargin' (extra, margin) pa@PA_TAU{..} =
    let single = toSingleLine pa
        extra' = extra + lengthOf attr + lengthOf arrow + 2 -- indent + attr + arrow + 2 spaces
        pa' = PA_TAU attr arrow (withMargin' (extra', margin) expr)
     in if lengthOf single + extra <= margin then single else pa'
  withMargin' (extra, margin) pa@PA_FORMATION{..} =
    let single = toSingleLine pa
        extra' = extra + lengthOf attr + lengthOf voids + lengthOf arrow + 4 -- indent + 2 braces + 2 spaces + voids
        pa' = PA_FORMATION attr voids arrow (withMargin' (extra', margin) expr)
     in if lengthOf single + extra <= margin then single else pa'
  withMargin' _ pa = pa

instance WithMargin CONDITION where
  withMargin' _ co = co

instance WithMargin EXTRA where
  withMargin' _ = id

lengthOf :: Render a => a -> Int
lengthOf renderable = length (render renderable)
