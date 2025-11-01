{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Lining where

import CST

data LineFormat = SINGLELINE | MULTILINE
  deriving (Show, Eq)

class ToSingleLine a where
  toSingleLine :: a -> a

instance ToSingleLine PROGRAM where
  toSingleLine PR_SALTY {..} = PR_SALTY global arrow (toSingleLine expr)
  toSingleLine PR_SWEET {..} = PR_SWEET (toSingleLine expr)

instance ToSingleLine EXPRESSION where
  toSingleLine EX_FORMATION {..} = EX_FORMATION lsb NO_EOL TAB' (toSingleLine binding) NO_EOL TAB' rsb
  toSingleLine EX_DISPATCH {..} = EX_DISPATCH (toSingleLine expr) attr
  toSingleLine EX_APPLICATION {..} = EX_APPLICATION (toSingleLine expr) NO_EOL TAB' (toSingleLine bindings) NO_EOL TAB'
  toSingleLine EX_APPLICATION' {..} = EX_APPLICATION' (toSingleLine expr) NO_EOL TAB' (toSingleLine args) NO_EOL TAB'
  toSingleLine expr = expr

instance ToSingleLine BINDING where
  toSingleLine BI_PAIR {..} = BI_PAIR (toSingleLine pair) (toSingleLine bindings) TAB'
  toSingleLine bd = bd

instance ToSingleLine BINDINGS where
  toSingleLine BDS_PAIR {..} = BDS_PAIR NO_EOL TAB' (toSingleLine pair) (toSingleLine bindings)
  toSingleLine bds = bds

instance ToSingleLine PAIR where
  toSingleLine PA_TAU {..} = PA_TAU attr arrow (toSingleLine expr)
  toSingleLine pair = pair

instance ToSingleLine APP_ARG where
  toSingleLine APP_ARG {..} = APP_ARG (toSingleLine expr) (toSingleLine args)

instance ToSingleLine APP_ARGS where
  toSingleLine AAS_EXPR {..} = AAS_EXPR NO_EOL TAB' (toSingleLine expr) (toSingleLine args)
  toSingleLine args = args