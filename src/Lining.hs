{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Lining (toSingleLine, LineFormat (..), withLineFormat, ToSingleLine) where

import CST

withLineFormat :: (ToSingleLine a) => LineFormat -> a -> a
withLineFormat MULTILINE node = node
withLineFormat SINGLELINE node = toSingleLine node

data LineFormat = SINGLELINE | MULTILINE
  deriving (Show, Eq)

class ToSingleLine a where
  toSingleLine :: a -> a

instance ToSingleLine PROGRAM where
  toSingleLine PR_SALTY{..} = PR_SALTY global arrow (toSingleLine expr)
  toSingleLine PR_SWEET{..} = PR_SWEET lcb (toSingleLine expr) rcb

instance ToSingleLine EXPRESSION where
  toSingleLine EX_FORMATION{lsb, binding = bd@BI_EMPTY{}, rsb} = EX_FORMATION lsb NO_EOL NO_TAB bd NO_EOL NO_TAB rsb
  toSingleLine EX_FORMATION{..} = EX_FORMATION lsb NO_EOL TAB' (toSingleLine binding) NO_EOL TAB' rsb
  toSingleLine EX_DISPATCH{..} = EX_DISPATCH (toSingleLine expr) attr
  toSingleLine EX_APPLICATION{..} = EX_APPLICATION (toSingleLine expr) NO_EOL TAB' (toSingleLine tau) NO_EOL TAB' indent
  toSingleLine EX_APPLICATION_TAUS{..} = EX_APPLICATION_TAUS (toSingleLine expr) NO_EOL TAB' (toSingleLine taus) NO_EOL TAB' indent
  toSingleLine EX_APPLICATION_EXPRS{..} = EX_APPLICATION_EXPRS (toSingleLine expr) NO_EOL TAB' (toSingleLine args) NO_EOL TAB' indent
  toSingleLine EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toSingleLine expr)
  toSingleLine EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (toSingleLine expr)
  toSingleLine expr = expr

instance ToSingleLine APP_BINDING where
  toSingleLine APP_BINDING{..} = APP_BINDING (toSingleLine pair)

instance ToSingleLine BINDING where
  toSingleLine BI_PAIR{..} = BI_PAIR (toSingleLine pair) (toSingleLine bindings) TAB'
  toSingleLine BI_META{..} = BI_META meta (toSingleLine bindings) TAB'
  toSingleLine bd = bd

instance ToSingleLine BINDINGS where
  toSingleLine BDS_PAIR{..} = BDS_PAIR NO_EOL TAB' (toSingleLine pair) (toSingleLine bindings)
  toSingleLine BDS_META{..} = BDS_META NO_EOL TAB' meta (toSingleLine bindings)
  toSingleLine bds = bds

instance ToSingleLine PAIR where
  toSingleLine PA_TAU{..} = PA_TAU attr arrow (toSingleLine expr)
  toSingleLine PA_FORMATION{..} = PA_FORMATION attr voids arrow (toSingleLine expr)
  toSingleLine pair = pair

instance ToSingleLine APP_ARG where
  toSingleLine APP_ARG{..} = APP_ARG (toSingleLine expr) (toSingleLine args)

instance ToSingleLine APP_ARGS where
  toSingleLine AAS_EXPR{..} = AAS_EXPR NO_EOL TAB' (toSingleLine expr) (toSingleLine args)
  toSingleLine args = args

instance ToSingleLine SET where
  toSingleLine ST_BINDING{..} = ST_BINDING (toSingleLine binding)
  toSingleLine st = st

instance ToSingleLine NUMBER where
  toSingleLine LENGTH{..} = LENGTH (toSingleLine binding)
  toSingleLine num = num

instance ToSingleLine COMPARABLE where
  toSingleLine comp@CMP_ATTR{} = comp
  toSingleLine CMP_EXPR{..} = CMP_EXPR (toSingleLine expr)
  toSingleLine CMP_NUM{..} = CMP_NUM (toSingleLine num)

instance ToSingleLine CONDITION where
  toSingleLine CO_BELONGS{..} = CO_BELONGS attr belongs (toSingleLine set)
  toSingleLine CO_LOGIC{..} = CO_LOGIC (map toSingleLine conditions) operator
  toSingleLine CO_NF{..} = CO_NF (toSingleLine expr)
  toSingleLine CO_NOT{..} = CO_NOT (toSingleLine condition)
  toSingleLine CO_COMPARE{..} = CO_COMPARE (toSingleLine left) equal (toSingleLine right)
  toSingleLine CO_MATCHES{..} = CO_MATCHES regex (toSingleLine expr)
  toSingleLine CO_PART_OF{..} = CO_PART_OF (toSingleLine expr) (toSingleLine binding)
  toSingleLine CO_EMPTY = CO_EMPTY

instance ToSingleLine EXTRA_ARG where
  toSingleLine ARG_EXPR{..} = ARG_EXPR (toSingleLine expr)
  toSingleLine ARG_BINDING{..} = ARG_BINDING (toSingleLine binding)
  toSingleLine at@ARG_ATTR{} = at
  toSingleLine bts@ARG_BYTES{} = bts

instance ToSingleLine EXTRA where
  toSingleLine EXTRA{..} = EXTRA (toSingleLine meta) func (map toSingleLine args)
