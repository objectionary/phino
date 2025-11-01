{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Sugar where

import AST
import CST
import Misc

voidRho :: PAIR
voidRho = PA_VOID (AT_RHO RHO) ARROW EMPTY

bdWithVoidRho :: BINDING -> BINDING
bdWithVoidRho BI_EMPTY {..} = BI_PAIR voidRho (BDS_EMPTY tab) tab
bdWithVoidRho bd@BI_PAIR {pair = PA_VOID {attr = AT_RHO _}} = bd
bdWithVoidRho bd@BI_PAIR {pair = PA_TAU {attr = AT_RHO _}} = bd
bdWithVoidRho BI_PAIR {..} = BI_PAIR pair (bdsWithVoidRho bindings) tab
  where
    bdsWithVoidRho :: BINDINGS -> BINDINGS
    bdsWithVoidRho BDS_EMPTY {..} = BDS_PAIR EOL tab voidRho (BDS_EMPTY tab)
    bdsWithVoidRho bds@BDS_PAIR {pair = PA_VOID {attr = AT_RHO _}} = bds
    bdsWithVoidRho bds@BDS_PAIR {pair = PA_TAU {attr = AT_RHO _}} = bds
    bdsWithVoidRho BDS_PAIR {..} = BDS_PAIR eol tab pair (bdsWithVoidRho bindings)

data SugarType = SWEET | SALTY
  deriving (Eq)

-- By default CST is generated with all possible syntax sugar
-- The main purpose of this class is to get rid of syntax sugar
--  |----------------------------|-------------------------------------------------------|
--  | sugar                      | verbose version                                       |
--  |----------------------------|-------------------------------------------------------|
--  | {e}                        | Q -> e                                                |
--  | QQ                         | Q.org.eolang                                          |
--  | a1 -> a2                   | a1 ↦ $.a2                                             |
--  | a -> 42                    | QQ.number(QQ.bytes([[ D> 40-45-00-00-00-00-00-00 ]])) |
--  | a -> "Hey"                 | QQ.number(QQ.bytes([[ D> 48-65-79 ]]))                |
--  | [[ B ]]                    | [[ B, ^ -> ? ]], if rho is absent in 'B'              |
--  | a1(a2, a3, ...) -> [[ B ]] | a1 -> [[ a2 -> ?, a3 -> ?, ..., B ]]                  |
--  | e(e0, e1, ...)             | e(~0 -> e0, ~1 -> e1, ...)                            |
--  | e(a1 -> e1, a2 -> e2, ...) | e(a1 -> e1)(a2 -> e2)...                              |
--  |----------------------------|-------------------------------------------------------|
class ToSalty a where
  toSalty :: a -> a

instance ToSalty PROGRAM where
  toSalty PR_SWEET {..} = PR_SALTY Φ ARROW (toSalty expr)
  toSalty prog = prog

instance ToSalty EXPRESSION where
  toSalty EX_DEF_PACKAGE {..} = EX_DISPATCH (EX_DISPATCH (EX_GLOBAL Φ) (AT_LABEL "org")) (AT_LABEL "eolang")
  toSalty EX_ATTR {..} = EX_DISPATCH (EX_XI XI) attr
  toSalty EX_DISPATCH {..} = EX_DISPATCH (toSalty expr) attr
  toSalty EX_FORMATION {lsb, binding = bd@BI_EMPTY{..}, rsb} = EX_FORMATION lsb NO_EOL TAB' (toSalty (bdWithVoidRho bd)) NO_EOL TAB' rsb
  toSalty EX_FORMATION {..} = EX_FORMATION lsb eol tab (toSalty (bdWithVoidRho binding)) eol' tab' rsb
  toSalty EX_APPLICATION {..} = EX_APPLICATION (toSalty expr) eol tab (toSalty bindings) eol' tab'
  toSalty EX_APPLICATION' {..} = EX_APPLICATION (toSalty expr) eol tab (toSalty (argToBinding args tab)) eol' tab'
    where
      argToBinding :: APP_ARG -> TAB -> BINDING
      argToBinding APP_ARG {..} =
        BI_PAIR
          (PA_TAU (AT_ALPHA ALPHA 0) ARROW expr)
          (argsToBindings args 1 tab)
      argsToBindings :: APP_ARGS -> Integer -> TAB -> BINDINGS
      argsToBindings AAS_EMPTY _ tab = BDS_EMPTY tab
      argsToBindings AAS_EXPR {..} idx tb = BDS_PAIR eol tb (PA_TAU (AT_ALPHA ALPHA idx) ARROW expr) (argsToBindings args (idx + 1) tb)
  toSalty EX_NUMBER {num, tab = tb@TAB {..}} =
    let number = ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number")
        bytes = ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes")
        number' = toCST number (indent + 1) :: EXPRESSION
        bytes' = toCST bytes (indent + 2) :: EXPRESSION
        data' = toCST (ExFormation [BiDelta (numToBts (either toDouble id num))]) (indent + 2) :: EXPRESSION
     in toSalty
          ( EX_APPLICATION'
              number'
              EOL
              (TAB (indent + 1))
              ( APP_ARG
                  ( EX_APPLICATION'
                      bytes'
                      EOL
                      (TAB (indent + 2))
                      (APP_ARG data' AAS_EMPTY)
                      EOL
                      (TAB (indent + 1))
                  )
                  AAS_EMPTY
              )
              EOL
              tb
          )
  toSalty EX_STRING {str, tab = tb@TAB {..}} =
    let string = ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "string")
        bytes = ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes")
        string' = toCST string (indent + 1) :: EXPRESSION
        bytes' = toCST bytes (indent + 2) :: EXPRESSION
        data' = toCST (ExFormation [BiDelta (strToBts str)]) (indent + 2) :: EXPRESSION
     in toSalty
          ( EX_APPLICATION'
              string'
              EOL
              (TAB (indent + 1))
              ( APP_ARG
                  ( EX_APPLICATION'
                      bytes'
                      EOL
                      (TAB (indent + 2))
                      (APP_ARG data' AAS_EMPTY)
                      EOL
                      (TAB (indent + 1))
                  )
                  AAS_EMPTY
              )
              EOL
              tb
          )
  toSalty expr = expr

instance ToSalty BINDING where
  toSalty BI_PAIR {..} = BI_PAIR (toSalty pair) (toSalty bindings) tab
  toSalty bd = bd

instance ToSalty BINDINGS where
  toSalty BDS_PAIR {..} = BDS_PAIR eol tab (toSalty pair) (toSalty bindings)
  toSalty bds = bds

instance ToSalty PAIR where
  toSalty PA_TAU {..} = PA_TAU attr arrow (toSalty expr)
  toSalty pair = pair
