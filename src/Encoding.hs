{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Encoding (toASCII, withEncoding, Encoding (..)) where

import CST

data Encoding = ASCII | UNICODE
  deriving (Eq, Show)

withEncoding :: (ToASCII a) => Encoding -> a -> a
withEncoding UNICODE prog = prog
withEncoding ASCII prog = toASCII prog

class ToASCII a where
  toASCII :: a -> a

instance ToASCII PROGRAM where
  toASCII PR_SALTY{..} = PR_SALTY Q ARROW' (toASCII expr)
  toASCII PR_SWEET{..} = PR_SWEET lcb (toASCII expr) rcb

instance ToASCII EXPRESSION where
  toASCII EX_GLOBAL{..} = EX_GLOBAL Q
  toASCII EX_DEF_PACKAGE{..} = EX_DEF_PACKAGE QQ
  toASCII EX_XI{..} = EX_XI DOLLAR
  toASCII EX_ATTR{..} = EX_ATTR (toASCII attr)
  toASCII EX_TERMINATION{..} = EX_TERMINATION T
  toASCII EX_FORMATION{..} = EX_FORMATION LSB' eol tab (toASCII binding) eol' tab' RSB'
  toASCII EX_DISPATCH{..} = EX_DISPATCH (toASCII expr) (toASCII attr)
  toASCII EX_APPLICATION{..} = EX_APPLICATION (toASCII expr) eol tab (toASCII tau) eol' tab' indent
  toASCII EX_APPLICATION_TAUS{..} = EX_APPLICATION_TAUS (toASCII expr) eol tab (toASCII taus) eol' tab' indent
  toASCII EX_APPLICATION_EXPRS{..} = EX_APPLICATION_EXPRS (toASCII expr) eol tab (toASCII args) eol' tab' indent
  toASCII EX_META{..} = EX_META (MT_EXPRESSION' (rest meta))
  toASCII EX_META_TAIL{..} = EX_META_TAIL (toASCII expr) (MT_TAIL (rest meta))
  toASCII EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toASCII expr)
  toASCII EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (toASCII expr)
  toASCII expr = expr

instance ToASCII APP_BINDING where
  toASCII APP_BINDING{..} = APP_BINDING (toASCII pair)

instance ToASCII BINDING where
  toASCII BI_PAIR{..} = BI_PAIR (toASCII pair) (toASCII bindings) tab
  toASCII BI_META{..} = BI_META (MT_BINDING' (rest meta)) (toASCII bindings) tab
  toASCII bd = bd

instance ToASCII BINDINGS where
  toASCII BDS_PAIR{..} = BDS_PAIR eol tab (toASCII pair) (toASCII bindings)
  toASCII BDS_META{..} = BDS_META eol tab (MT_BINDING' (rest meta)) (toASCII bindings)
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
  toASCII PA_META_DELTA{..} = PA_META_DELTA' (MT_BYTES' (rest meta))
  toASCII pair = pair

instance ToASCII ATTRIBUTE where
  toASCII AT_ALPHA{..} = AT_ALPHA ALPHA' idx
  toASCII AT_PHI{..} = AT_PHI AT
  toASCII AT_RHO{..} = AT_RHO CARET
  toASCII AT_META{..} = AT_META (MT_ATTRIBUTE' (rest meta))
  toASCII attr = attr
