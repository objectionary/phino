{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Encoding where

import CST

data Encoding = ASCII | UNICODE
  deriving (Eq, Show)

withEncoding :: Encoding -> PROGRAM -> PROGRAM
withEncoding UNICODE prog = prog
withEncoding ASCII prog = toASCII prog

class ToASCII a where
  toASCII :: a -> a

instance ToASCII PROGRAM where
  toASCII PR_SALTY {..} = PR_SALTY Q ARROW' (toASCII expr)
  toASCII PR_SWEET {..} = PR_SWEET (toASCII expr)

-- @todo #404:30min Convert EX_STRING to ASCII. We need to decide what to do with EX_STRING when
--  converting to ASCII because string itself may contain unicode characters and we need to deal with
--  them somehow.
instance ToASCII EXPRESSION where
  toASCII EX_GLOBAL {..} = EX_GLOBAL Q
  toASCII EX_DEF_PACKAGE {..} = EX_DEF_PACKAGE QQ
  toASCII EX_XI {..} = EX_XI DOLLAR
  toASCII EX_ATTR {..} = EX_ATTR (toASCII attr)
  toASCII EX_TERMINATION {..} = EX_TERMINATION T
  toASCII EX_FORMATION {..} = EX_FORMATION LSB' eol tab (toASCII binding) eol' tab' RSB'
  toASCII EX_DISPATCH {..} = EX_DISPATCH (toASCII expr) (toASCII attr)
  toASCII EX_APPLICATION {..} = EX_APPLICATION (toASCII expr) eol tab (toASCII bindings) eol' tab'
  toASCII EX_APPLICATION' {..} = EX_APPLICATION' (toASCII expr) eol tab (toASCII args) eol' tab'
  toASCII expr = expr

instance ToASCII BINDING where
  toASCII BI_PAIR {..} = BI_PAIR (toASCII pair) (toASCII bindings) tab
  toASCII bd = bd

instance ToASCII BINDINGS where
  toASCII BDS_PAIR {..} = BDS_PAIR eol tab (toASCII pair) (toASCII bindings)
  toASCII bds = bds

instance ToASCII APP_ARG where
  toASCII APP_ARG {..} = APP_ARG (toASCII expr) (toASCII args)

instance ToASCII APP_ARGS where
  toASCII AAS_EXPR {..} = AAS_EXPR eol tab (toASCII expr) (toASCII args)
  toASCII args = args

instance ToASCII PAIR where
  toASCII PA_TAU {..} = PA_TAU (toASCII attr) ARROW' (toASCII expr)
  toASCII PA_VOID {..} = PA_VOID (toASCII attr) ARROW' QUESTION
  toASCII PA_LAMBDA {..} = PA_LAMBDA' func
  toASCII PA_DELTA {..} = PA_DELTA' bytes
  toASCII pair = pair

-- @todo #404:30min Convert AT_LABEL to ASCII. We need to decide what to do with AT_LABEL when
--  converting to ASCII because attribute itself may contain unicode characters and we need to deal with
--  them somehow.
instance ToASCII ATTRIBUTE where
  toASCII AT_ALPHA {..} = AT_ALPHA ALPHA' idx
  toASCII AT_PHI {..} = AT_PHI AT
  toASCII AT_RHO {..} = AT_RHO CARET
  toASCII attr = attr
