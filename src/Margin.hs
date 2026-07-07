{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Margin (defaultMargin, withMargin, WithMargin) where

import CST
import qualified Data.Text as T
import Lining (ToSingleLine (..))
import Render (Render (..))

defaultMargin :: Int
defaultMargin = 80

withMargin :: (WithMargin a) => Int -> a -> a
withMargin margin = withMargin' (0, margin)

class WithMargin a where
  withMargin' :: (Int, Int) -> a -> a

instance WithMargin EXPRESSION where
  withMargin' _ ex@EX_FORMATION{binding = BI_EMPTY{}} = ex
  withMargin' (extra, margin) ex@EX_FORMATION{tab = tab@(TAB indent), ..} =
    let single = toSingleLine ex
        ex' = EX_FORMATION lsb EOL tab (withMargin' (indent, margin) binding) EOL tab' rsb
     in if lengthOf single + extra <= margin then single else ex'
  withMargin' _ num@EX_NUMBER{} = num
  withMargin' _ str@EX_STRING{} = str
  withMargin' cfg EX_DISPATCH{..} = EX_DISPATCH (withMargin' cfg expr) space attr
  withMargin' cfg EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (withMargin' cfg expr)
  withMargin' _ EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toSingleLine expr)
  withMargin' cfg@(extra, margin) ex@EX_APPLICATION{tab = tab@(TAB indt), ..} =
    let single = toSingleLine ex
        main = withMargin' cfg expr
        singleMain = toSingleLine main
        extra' = T.length (last (T.lines (render main))) + 4 -- 2 spaces + 2 braces around argument
        arg' = withMargin' (indt, margin) argument
        singleArg = toSingleLine arg'
     in if
          | lengthOf single + extra <= margin -> single
          | lengthOf singleMain + extra <= margin -> EX_APPLICATION singleMain space EOL tab arg' EOL tab' indent
          | lengthOf singleArg + extra' <= margin -> EX_APPLICATION main space NO_EOL TAB' singleArg NO_EOL TAB' indent
          | otherwise -> EX_APPLICATION main space EOL tab arg' EOL tab' indent
  withMargin' _ ex = ex

instance WithMargin APP_ARGUMENT where
  withMargin' cfg (AA_TAU tau) = AA_TAU (withMargin' cfg tau)
  withMargin' cfg (AA_TAUS taus) = AA_TAUS (withMargin' cfg taus)
  withMargin' cfg (AA_EXPRS args) = AA_EXPRS (withMargin' cfg args)

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
  withMargin' (extra, margin) pa@PA_ALPHA{..} =
    let single = toSingleLine pa
        extra' = extra + lengthOf alpha + lengthOf arrow + 2 -- indent + alpha + arrow + 2 spaces
        pa' = PA_ALPHA alpha arrow (withMargin' (extra', margin) expr)
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
lengthOf renderable = T.length (render renderable)
