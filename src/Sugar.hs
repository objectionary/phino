{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Sugar (toSalty, withSugarType, SugarType (..), ToSalty) where

import AST
import CST
import Misc (numToBts, strToBts, toDouble, pattern BaseObject)

withSugarType :: (ToSalty a) => SugarType -> a -> a
withSugarType SWEET prog = prog
withSugarType SALTY prog = toSalty prog

voidRho :: PAIR
voidRho = PA_VOID (AT_RHO RHO) ARROW EMPTY

bdWithVoidRho :: BINDING -> BINDING
bdWithVoidRho BI_EMPTY{..} = BI_PAIR voidRho (BDS_EMPTY tab) tab
bdWithVoidRho bd@BI_PAIR{pair = PA_VOID{attr = AT_RHO _}} = bd
bdWithVoidRho bd@BI_PAIR{pair = PA_TAU{attr = AT_RHO _}} = bd
bdWithVoidRho BI_PAIR{..} = BI_PAIR pair (bdsWithVoidRho bindings) tab
  where
    bdsWithVoidRho :: BINDINGS -> BINDINGS
    bdsWithVoidRho BDS_EMPTY{..} = BDS_PAIR EOL tab voidRho (BDS_EMPTY tab)
    bdsWithVoidRho bds@BDS_PAIR{pair = PA_VOID{attr = AT_RHO _}} = bds
    bdsWithVoidRho bds@BDS_PAIR{pair = PA_TAU{attr = AT_RHO _}} = bds
    bdsWithVoidRho BDS_PAIR{..} = BDS_PAIR eol tab pair (bdsWithVoidRho bindings)
    bdsWithVoidRho bds@BDS_META{} = bds
bdWithVoidRho bd@BI_META{} = bd

data SugarType = SWEET | SALTY
  deriving (Eq, Show)

-- By default CST is generated with all possible syntax sugar
-- The main purpose of this class is to get rid of syntax sugar
--  |----------------------------|-------------------------------------------------------|
--  | sugar                      | verbose version                                       |
--  |----------------------------|-------------------------------------------------------|
--  | {e}                        | Q -> e                                                |
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
  toSalty PR_SWEET{..} = PR_SALTY Φ ARROW (toSalty expr)
  toSalty prog = prog

instance ToSalty EXPRESSION where
  toSalty EX_ATTR{..} = EX_DISPATCH (EX_XI XI) attr
  toSalty EX_DISPATCH{..} = EX_DISPATCH (toSalty expr) attr
  toSalty EX_FORMATION{lsb, binding = bd@BI_EMPTY{}, rsb} = EX_FORMATION lsb NO_EOL TAB' (toSalty (bdWithVoidRho bd)) NO_EOL TAB' rsb
  toSalty EX_FORMATION{..} = EX_FORMATION lsb eol tab (toSalty (bdWithVoidRho binding)) eol' tab' rsb
  toSalty EX_APPLICATION{..} = EX_APPLICATION (toSalty expr) EOL (TAB indent) (toSalty tau) EOL (TAB (indent - 1)) indent
  toSalty EX_APPLICATION_TAUS{..} =
    foldl
      toApplication
      expr
      (tauToPairs taus)
    where
      toApplication :: EXPRESSION -> PAIR -> EXPRESSION
      toApplication exp pair =
        EX_APPLICATION (toSalty exp) EOL (TAB indent) (APP_BINDING (toSalty pair)) EOL (TAB (indent - 1)) indent
      tauToPairs :: BINDING -> [PAIR]
      tauToPairs BI_PAIR{..} = pair : tausToPairs bindings
      tauToPairs BI_EMPTY{} = []
      tauToPairs (BI_META mt _ _) = error $ "BI_META " ++ show mt ++ " unexpected in tauToPairs"
      tausToPairs :: BINDINGS -> [PAIR]
      tausToPairs BDS_EMPTY{} = []
      tausToPairs BDS_PAIR{..} = pair : tausToPairs bindings
      tausToPairs (BDS_META _ _ mt _) = error $ "BDS_META " ++ show mt ++ " unexpected in tausToPairs"
  toSalty EX_APPLICATION_EXPRS{..} = toSalty (EX_APPLICATION_TAUS expr EOL (TAB indent) (argToBinding args tab) EOL (TAB (indent - 1)) indent)
    where
      argToBinding :: APP_ARG -> TAB -> BINDING
      argToBinding APP_ARG{..} =
        BI_PAIR
          (PA_TAU (AT_ALPHA ALPHA 0) ARROW expr)
          (argsToBindings args 1 tab)
      argsToBindings :: APP_ARGS -> Int -> TAB -> BINDINGS
      argsToBindings AAS_EMPTY _ tab = BDS_EMPTY tab
      argsToBindings AAS_EXPR{..} idx tb = BDS_PAIR eol tb (PA_TAU (AT_ALPHA ALPHA idx) ARROW expr) (argsToBindings args (idx + 1) tb)
  toSalty EX_NUMBER{num, tab = tab@TAB{..}, rhos} =
    saltifyPrimitive
      (toCST (BaseObject "number") (indent + 1) EOL)
      (toCST (BaseObject "bytes") (indent + 2) EOL)
      (toCST (ExFormation [BiDelta (numToBts (either toDouble id num))]) (indent + 2) EOL)
      tab
      rhos
  toSalty EX_STRING{str, tab = tab@TAB{..}, rhos} =
    saltifyPrimitive
      (toCST (BaseObject "string") (indent + 1) EOL)
      (toCST (BaseObject "bytes") (indent + 2) EOL)
      (toCST (ExFormation [BiDelta (strToBts str)]) (indent + 2) EOL)
      tab
      rhos
  toSalty EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toSalty expr)
  toSalty EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (toSalty expr)
  toSalty expr = expr

saltifyPrimitive :: EXPRESSION -> EXPRESSION -> EXPRESSION -> TAB -> [Binding] -> EXPRESSION
saltifyPrimitive base bytes data' tb@TAB{..} rhos =
  let next = TAB (indent + 1)
   in toSalty
        ( EX_APPLICATION_TAUS
            base
            EOL
            next
            ( BI_PAIR
                ( PA_TAU
                    (AT_ALPHA ALPHA 0)
                    ARROW
                    ( EX_APPLICATION_EXPRS
                        bytes
                        EOL
                        (TAB (indent + 2))
                        (APP_ARG data' AAS_EMPTY)
                        EOL
                        next
                        (indent + 2)
                    )
                )
                (toCST rhos (indent + 1) EOL)
                next
            )
            EOL
            tb
            (indent + 1)
        )
saltifyPrimitive _ _ _ TAB' _ = error "saltifyPrimitive requires TAB with indent, got TAB'"
saltifyPrimitive _ _ _ NO_TAB _ = error "saltifyPrimitive requires TAB with indent, got NO_TAB"

instance ToSalty BINDING where
  toSalty BI_PAIR{..} = BI_PAIR (toSalty pair) (toSalty bindings) tab
  toSalty bd = bd

instance ToSalty APP_BINDING where
  toSalty APP_BINDING{..} = APP_BINDING (toSalty pair)

instance ToSalty BINDINGS where
  toSalty BDS_PAIR{..} = BDS_PAIR eol tab (toSalty pair) (toSalty bindings)
  toSalty bds = bds

instance ToSalty PAIR where
  toSalty PA_TAU{..} = PA_TAU attr arrow (toSalty expr)
  toSalty PA_FORMATION{voids, attr, arrow, expr = EX_FORMATION{..}} =
    PA_TAU attr arrow (toSalty (EX_FORMATION lsb eol tab (joinToBinding voids binding) eol' tab' rsb))
    where
      joinToBinding :: [ATTRIBUTE] -> BINDING -> BINDING
      joinToBinding [] bd = bd
      joinToBinding (attr : rest) bd = BI_PAIR (PA_VOID attr arrow EMPTY) (joinToBindings rest bd) tab
      joinToBindings :: [ATTRIBUTE] -> BINDING -> BINDINGS
      joinToBindings [] BI_EMPTY{..} = BDS_EMPTY tab
      joinToBindings [] BI_PAIR{..} = BDS_PAIR eol tab pair bindings
      joinToBindings [] BI_META{} = error "BI_META unexpected in joinToBindings"
      joinToBindings (attr : rest) bd = BDS_PAIR eol tab (PA_VOID attr arrow EMPTY) (joinToBindings rest bd)
  toSalty pair = pair
