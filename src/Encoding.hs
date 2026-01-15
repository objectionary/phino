{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Encoding (toASCII, withEncoding, Encoding (..), ToASCII) where

import CST

data Encoding = ASCII | UNICODE
  deriving (Eq, Show)

withEncoding :: (ToASCII a) => Encoding -> a -> a
withEncoding UNICODE node = node
withEncoding ASCII node = toASCII node

class ToASCII a where
  toASCII :: a -> a

instance ToASCII PROGRAM where
  toASCII PR_SALTY{..} = PR_SALTY Q ARROW' (toASCII expr)
  toASCII PR_SWEET{..} = PR_SWEET lcb (toASCII expr) rcb

instance ToASCII EXPRESSION where
  toASCII EX_GLOBAL{} = EX_GLOBAL Q
  toASCII EX_XI{} = EX_XI DOLLAR
  toASCII EX_ATTR{..} = EX_ATTR (toASCII attr)
  toASCII EX_TERMINATION{} = EX_TERMINATION T
  toASCII EX_FORMATION{..} = EX_FORMATION LSB' eol tab (toASCII binding) eol' tab' RSB'
  toASCII EX_DISPATCH{..} = EX_DISPATCH (toASCII expr) (toASCII attr)
  toASCII EX_APPLICATION{..} = EX_APPLICATION (toASCII expr) eol tab (toASCII tau) eol' tab' indent
  toASCII EX_APPLICATION_TAUS{..} = EX_APPLICATION_TAUS (toASCII expr) eol tab (toASCII taus) eol' tab' indent
  toASCII EX_APPLICATION_EXPRS{..} = EX_APPLICATION_EXPRS (toASCII expr) eol tab (toASCII args) eol' tab' indent
  toASCII EX_META{..} = EX_META (META EXCL E' (rest meta))
  toASCII EX_META_TAIL{..} = EX_META_TAIL (toASCII expr) meta
  toASCII EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toASCII expr)
  toASCII EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (toASCII expr)
  toASCII expr = expr

instance ToASCII APP_BINDING where
  toASCII APP_BINDING{..} = APP_BINDING (toASCII pair)

instance ToASCII BINDING where
  toASCII BI_PAIR{..} = BI_PAIR (toASCII pair) (toASCII bindings) tab
  toASCII BI_META{meta = META{..}, ..} = BI_META (META EXCL B' rest) (toASCII bindings) tab
  toASCII bd = bd

instance ToASCII BINDINGS where
  toASCII BDS_PAIR{..} = BDS_PAIR eol tab (toASCII pair) (toASCII bindings)
  toASCII BDS_META{..} = BDS_META eol tab (META EXCL B' (rest meta)) (toASCII bindings)
  toASCII bds = bds

instance ToASCII APP_ARG where
  toASCII APP_ARG{..} = APP_ARG (toASCII expr) (toASCII args)

instance ToASCII APP_ARGS where
  toASCII AAS_EXPR{..} = AAS_EXPR eol tab (toASCII expr) (toASCII args)
  toASCII args = args

instance ToASCII PAIR where
  toASCII PA_TAU{..} = PA_TAU (toASCII attr) ARROW' (toASCII expr)
  toASCII PA_FORMATION{..} = PA_FORMATION (toASCII attr) (map toASCII voids) ARROW' (toASCII expr)
  toASCII PA_VOID{..} = PA_VOID (toASCII attr) ARROW' QUESTION
  toASCII PA_LAMBDA{..} = PA_LAMBDA' func
  toASCII PA_DELTA{..} = PA_DELTA' bytes
  toASCII PA_META_LAMBDA{..} = PA_META_LAMBDA' meta
  toASCII PA_META_DELTA{..} = PA_META_DELTA' (META EXCL D' (rest meta))
  toASCII pair = pair

instance ToASCII ATTRIBUTE where
  toASCII AT_ALPHA{..} = AT_ALPHA ALPHA' idx
  toASCII AT_PHI{} = AT_PHI AT
  toASCII AT_RHO{} = AT_RHO CARET
  toASCII AT_META{..} = AT_META (META EXCL A (rest meta))
  toASCII attr = attr

instance ToASCII SET where
  toASCII ST_BINDING{..} = ST_BINDING (toASCII binding)
  toASCII ST_ATTRIBUTES{..} = ST_ATTRIBUTES (map toASCII attrs)

instance ToASCII NUMBER where
  toASCII ORDINAL{..} = ORDINAL (toASCII attr)
  toASCII LENGTH{..} = LENGTH (toASCII binding)
  toASCII literal@LITERAL{} = literal

instance ToASCII COMPARABLE where
  toASCII CMP_ATTR{..} = CMP_ATTR (toASCII attr)
  toASCII CMP_EXPR{..} = CMP_EXPR (toASCII expr)
  toASCII CMP_NUM{..} = CMP_NUM (toASCII num)

instance ToASCII CONDITION where
  toASCII CO_BELONGS{..} = CO_BELONGS (toASCII attr) belongs (toASCII set)
  toASCII CO_LOGIC{..} = CO_LOGIC (map toASCII conditions) operator
  toASCII CO_NF{..} = CO_NF (toASCII expr)
  toASCII CO_NOT{..} = CO_NOT (toASCII condition)
  toASCII CO_COMPARE{..} = CO_COMPARE (toASCII left) equal (toASCII right)
  toASCII CO_MATCHES{..} = CO_MATCHES regex (toASCII expr)
  toASCII CO_PART_OF{..} = CO_PART_OF (toASCII expr) (toASCII binding)
  toASCII CO_EMPTY = CO_EMPTY

instance ToASCII EXTRA_ARG where
  toASCII ARG_ATTR{..} = ARG_ATTR (toASCII attr)
  toASCII ARG_EXPR{..} = ARG_EXPR (toASCII expr)
  toASCII ARG_BINDING{..} = ARG_BINDING (toASCII binding)
  toASCII bts@ARG_BYTES{} = bts

instance ToASCII EXTRA where
  toASCII EXTRA{..} = EXTRA (toASCII meta) func (map toASCII args)
