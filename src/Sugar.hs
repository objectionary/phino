{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Sugar (toSalty, withSugarType, withoutRho, SugarType (..), ToSalty) where

import AST
import Bytes (numToBts, strToBts, unescapeStr)
import CST
import Misc (toDouble)

withSugarType :: (ToSalty a) => SugarType -> a -> a
withSugarType SWEET node = node
withSugarType SALTY node = toSalty node

voidRho :: PAIR
voidRho = PA_VOID (AT_RHO RHO) ARROW EMPTY

bdWithVoidRho :: BINDING -> BINDING
bdWithVoidRho BI_EMPTY{..} = BI_PAIR voidRho (BDS_EMPTY tab) tab
bdWithVoidRho bd@BI_PAIR{pair = PA_VOID{attr = AT_RHO _}} = bd
bdWithVoidRho bd@BI_PAIR{pair = PA_TAU{attr = AT_RHO _}} = bd
bdWithVoidRho bd@BI_PAIR{pair = PA_FORMATION{attr = AT_RHO _}} = bd
bdWithVoidRho BI_PAIR{..} = BI_PAIR pair (bdsWithVoidRho bindings) tab
  where
    bdsWithVoidRho :: BINDINGS -> BINDINGS
    bdsWithVoidRho BDS_EMPTY{..} = BDS_PAIR EOL tab voidRho (BDS_EMPTY tab)
    bdsWithVoidRho bds@BDS_PAIR{pair = PA_VOID{attr = AT_RHO _}} = bds
    bdsWithVoidRho bds@BDS_PAIR{pair = PA_TAU{attr = AT_RHO _}} = bds
    bdsWithVoidRho bds@BDS_PAIR{pair = PA_FORMATION{attr = AT_RHO _}} = bds
    bdsWithVoidRho BDS_PAIR{..} = BDS_PAIR eol tab pair (bdsWithVoidRho bindings)
    bdsWithVoidRho bds@BDS_META{} = bds
bdWithVoidRho bd@BI_META{} = bd

data SugarType = SWEET | SALTY
  deriving (Eq, Show)

-- Drop every ρ binding (ρ ↦ ∅, ρ ↦ e and ρ(…) ↦ e) from a rendered CST, the
-- effect of the '--hide-rho' switch. It runs after 'withSugarType', so it also
-- removes the ρ ↦ ∅ that 'bdWithVoidRho' re-inserts into every formation on the
-- SALTY path. Both formation bindings and application arguments are stripped;
-- dispatches such as ξ.ρ are left untouched. A formation left empty by the
-- strip collapses to the compact '⟦⟧' layout, and an application left with no
-- argument collapses to its bare callee (no leftover 'e()').
withoutRho :: EXPRESSION -> EXPRESSION
withoutRho = goExpr
  where
    goExpr :: EXPRESSION -> EXPRESSION
    goExpr EX_FORMATION{..} = case goBinding binding of
      empty@BI_EMPTY{} -> EX_FORMATION lsb NO_EOL NO_TAB empty NO_EOL NO_TAB rsb
      binding' -> EX_FORMATION lsb eol tab binding' eol' tab' rsb
    goExpr EX_DISPATCH{..} = EX_DISPATCH (goExpr expr) space attr
    goExpr EX_APPLICATION{..} = case goArgument argument of
      Nothing -> goExpr expr
      Just argument' -> EX_APPLICATION (goExpr expr) space eol tab argument' eol' tab' indent
    goExpr EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (goExpr expr)
    goExpr EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (goExpr expr)
    goExpr expr = expr
    -- Formation bindings: drop the ρ pairs, recurse into whatever remains.
    goBinding :: BINDING -> BINDING
    goBinding empty@BI_EMPTY{} = empty
    goBinding BI_META{..} = BI_META meta (goBindings bindings) tab
    goBinding BI_PAIR{..}
      | isRho pair = promote tab (goBindings bindings)
      | otherwise = BI_PAIR (goPair pair) (goBindings bindings) tab
    goBindings :: BINDINGS -> BINDINGS
    goBindings empty@BDS_EMPTY{} = empty
    goBindings BDS_META{..} = BDS_META eol tab meta (goBindings bindings)
    goBindings BDS_PAIR{..}
      | isRho pair = goBindings bindings
      | otherwise = BDS_PAIR eol tab (goPair pair) (goBindings bindings)
    -- Turn the tail chain back into a head binding once its leading pair was
    -- dropped; the promoted pair is already stripped and recursed by 'goBindings'.
    promote :: TAB -> BINDINGS -> BINDING
    promote tab (BDS_EMPTY _) = BI_EMPTY tab
    promote tab (BDS_PAIR _ _ pair bindings) = BI_PAIR pair bindings tab
    promote tab (BDS_META _ _ meta bindings) = BI_META meta bindings tab
    -- Application arguments: drop the ρ pairs too, the way 'goBinding' does for
    -- formations. 'Nothing' means nothing survived the strip, so 'goExpr'
    -- collapses the whole application to its bare callee instead of leaving an
    -- empty 'e()'. Positional arguments ('AA_EXPRS') carry no ρ, so they stay.
    goArgument :: APP_ARGUMENT -> Maybe APP_ARGUMENT
    goArgument (AA_TAU (APP_BINDING pair))
      | isRho pair = Nothing
      | otherwise = Just (AA_TAU (APP_BINDING (goPair pair)))
    goArgument (AA_TAUS binding) = case goArgBinding binding of
      BI_EMPTY{} -> Nothing
      binding' -> Just (AA_TAUS binding')
    goArgument (AA_EXPRS args) = Just (AA_EXPRS (goAppArg args))
    goArgBinding :: BINDING -> BINDING
    goArgBinding empty@BI_EMPTY{} = empty
    goArgBinding BI_META{..} = BI_META meta (goArgBindings bindings) tab
    goArgBinding BI_PAIR{..}
      | isRho pair = promote tab (goArgBindings bindings)
      | otherwise = BI_PAIR (goPair pair) (goArgBindings bindings) tab
    goArgBindings :: BINDINGS -> BINDINGS
    goArgBindings empty@BDS_EMPTY{} = empty
    goArgBindings BDS_META{..} = BDS_META eol tab meta (goArgBindings bindings)
    goArgBindings BDS_PAIR{..}
      | isRho pair = goArgBindings bindings
      | otherwise = BDS_PAIR eol tab (goPair pair) (goArgBindings bindings)
    goAppArg :: APP_ARG -> APP_ARG
    goAppArg APP_ARG{..} = APP_ARG (goExpr expr) (goAppArgs args)
    goAppArgs :: APP_ARGS -> APP_ARGS
    goAppArgs AAS_EMPTY = AAS_EMPTY
    goAppArgs AAS_EXPR{..} = AAS_EXPR eol tab (goExpr expr) (goAppArgs args)
    goPair :: PAIR -> PAIR
    goPair PA_TAU{..} = PA_TAU attr arrow (goExpr expr)
    goPair PA_ALPHA{..} = PA_ALPHA alpha arrow (goExpr expr)
    goPair PA_FORMATION{..} = PA_FORMATION attr voids arrow (goExpr expr)
    goPair pair = pair
    isRho :: PAIR -> Bool
    isRho PA_VOID{attr = AT_RHO _} = True
    isRho PA_TAU{attr = AT_RHO _} = True
    isRho PA_FORMATION{attr = AT_RHO _} = True
    isRho _ = False

-- By default CST is generated with all possible syntax sugar
-- The main purpose of this class is to get rid of syntax sugar
--  |----------------------------|-----------------------------------------------------|
--  | sugar                      | verbose version                                     |
--  |----------------------------|-----------------------------------------------------|
--  | a1 -> a2                   | a1 ↦ $.a2                                           |
--  | a -> 42                    | Q.number(Q.bytes([[ D> 40-45-00-00-00-00-00-00 ]])) |
--  | a -> "Hey"                 | Q.number(Q.bytes([[ D> 48-65-79 ]]))                |
--  | [[ B ]]                    | [[ B, ^ -> ? ]], if rho is absent in 'B'            |
--  | a1(a2, a3, ...) -> [[ B ]] | a1 -> [[ a2 -> ?, a3 -> ?, ..., B ]]                |
--  | e(e0, e1, ...)             | e(~0 -> e0, ~1 -> e1, ...)                          |
--  | e(a1 -> e1, a2 -> e2, ...) | e(a1 -> e1)(a2 -> e2)...                            |
--  |----------------------------|-----------------------------------------------------|
class ToSalty a where
  toSalty :: a -> a

instance ToSalty EXPRESSION where
  toSalty EX_ATTR{..} = EX_DISPATCH (EX_XI XI) NO_SPACE attr
  toSalty EX_DISPATCH{..} = EX_DISPATCH (toSalty expr) space attr
  toSalty EX_FORMATION{lsb, binding = bd@BI_EMPTY{}, rsb} = EX_FORMATION lsb NO_EOL TAB' (toSalty (bdWithVoidRho bd)) NO_EOL TAB' rsb
  toSalty EX_FORMATION{..} = EX_FORMATION lsb eol tab (toSalty (bdWithVoidRho binding)) eol' tab' rsb
  toSalty EX_APPLICATION{argument = AA_TAU tau, ..} = EX_APPLICATION (toSalty expr) space EOL (TAB indent) (AA_TAU (toSalty tau)) EOL (TAB (indent - 1)) indent
  toSalty EX_APPLICATION{argument = AA_TAUS taus, ..} =
    foldl
      toApplication
      expr
      (tauToPairs taus)
    where
      toApplication :: EXPRESSION -> PAIR -> EXPRESSION
      toApplication exp pair =
        EX_APPLICATION (toSalty exp) space EOL (TAB indent) (AA_TAU (APP_BINDING (toSalty pair))) EOL (TAB (indent - 1)) indent
      tauToPairs :: BINDING -> [PAIR]
      tauToPairs BI_PAIR{..} = pair : tausToPairs bindings
      tauToPairs BI_EMPTY{} = []
      tauToPairs (BI_META mt _ _) = error $ "BI_META " ++ show mt ++ " unexpected in tauToPairs"
      tausToPairs :: BINDINGS -> [PAIR]
      tausToPairs BDS_EMPTY{} = []
      tausToPairs BDS_PAIR{..} = pair : tausToPairs bindings
      tausToPairs (BDS_META _ _ mt _) = error $ "BDS_META " ++ show mt ++ " unexpected in tausToPairs"
  toSalty EX_APPLICATION{argument = AA_EXPRS args, ..} = toSalty (EX_APPLICATION expr space EOL (TAB indent) (AA_TAUS (argToBinding args tab)) EOL (TAB (indent - 1)) indent)
    where
      argToBinding :: APP_ARG -> TAB -> BINDING
      argToBinding APP_ARG{..} =
        BI_PAIR
          (PA_ALPHA (AL_IDX ALPHA 0) ARROW expr)
          (argsToBindings args 1 tab)
      argsToBindings :: APP_ARGS -> Int -> TAB -> BINDINGS
      argsToBindings AAS_EMPTY _ tab = BDS_EMPTY tab
      argsToBindings AAS_EXPR{..} idx tb = BDS_PAIR eol tb (PA_ALPHA (AL_IDX ALPHA idx) ARROW expr) (argsToBindings args (idx + 1) tb)
  toSalty EX_NUMBER{num, tab = tab@TAB{..}, rhos} =
    saltifyPrimitive
      (toCST (BaseObject "number") (indent + 1, EOL))
      (toCST (BaseObject "bytes") (indent + 2, EOL))
      (toCST (ExFormation [BiDelta (numToBts (either toDouble id num))]) (indent + 2, EOL))
      tab
      rhos
  toSalty EX_STRING{str, tab = tab@TAB{..}, rhos} =
    saltifyPrimitive
      (toCST (BaseObject "string") (indent + 1, EOL))
      (toCST (BaseObject "bytes") (indent + 2, EOL))
      (toCST (ExFormation [BiDelta (strToBts (unescapeStr str))]) (indent + 2, EOL))
      tab
      rhos
  toSalty EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toSalty expr)
  toSalty EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (toSalty expr)
  toSalty expr = expr

saltifyPrimitive :: EXPRESSION -> EXPRESSION -> EXPRESSION -> TAB -> [Argument] -> EXPRESSION
saltifyPrimitive base bytes data' tb@TAB{..} rhos =
  let next = TAB (indent + 1)
   in toSalty
        ( EX_APPLICATION
            base
            NO_SPACE
            EOL
            next
            ( AA_TAUS
                ( BI_PAIR
                    ( PA_TAU
                        (AT_LABEL "as-bytes")
                        ARROW
                        ( EX_APPLICATION
                            bytes
                            NO_SPACE
                            EOL
                            (TAB (indent + 2))
                            ( AA_TAUS
                                ( BI_PAIR
                                    (PA_TAU (AT_LABEL "data") ARROW data')
                                    (BDS_EMPTY (TAB (indent + 2)))
                                    (TAB (indent + 2))
                                )
                            )
                            EOL
                            next
                            (indent + 2)
                        )
                    )
                    (toCST rhos (indent + 1, EOL))
                    next
                )
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
  toSalty PA_ALPHA{..} = PA_ALPHA alpha arrow (toSalty expr)
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

instance ToSalty SET where
  toSalty ST_BINDING{..} = ST_BINDING (toSalty binding)
  toSalty st = st

instance ToSalty NUMBER where
  toSalty LENGTH{..} = LENGTH (toSalty binding)
  toSalty DOMAIN{..} = DOMAIN (toSalty binding)
  toSalty num = num

instance ToSalty COMPARABLE where
  toSalty comp@CMP_ATTR{} = comp
  toSalty CMP_EXPR{..} = CMP_EXPR (toSalty expr)
  toSalty CMP_NUM{..} = CMP_NUM (toSalty num)

instance ToSalty CONDITION where
  toSalty CO_BELONGS{..} = CO_BELONGS attr belongs (toSalty set)
  toSalty CO_LOGIC{..} = CO_LOGIC (map toSalty conditions) operator
  toSalty CO_NF{..} = CO_NF (toSalty expr)
  toSalty CO_ABSOLUTE{..} = CO_ABSOLUTE (toSalty expr) belongs
  toSalty CO_NOT{..} = CO_NOT (toSalty condition)
  toSalty CO_COMPARE{..} = CO_COMPARE (toSalty left) equal (toSalty right)
  toSalty CO_MATCHES{..} = CO_MATCHES regex (toSalty expr)
  toSalty CO_PART_OF{..} = CO_PART_OF (toSalty expr) (toSalty binding)
  toSalty CO_DISJOINT{..} = CO_DISJOINT attrs (map toSalty groups)
  toSalty CO_FORMATION{..} = CO_FORMATION (toSalty expr)
  toSalty CO_EMPTY = CO_EMPTY

instance ToSalty EXTRA_ARG where
  toSalty ARG_EXPR{..} = ARG_EXPR (toSalty expr)
  toSalty ARG_BINDING{..} = ARG_BINDING (toSalty binding)
  toSalty at@ARG_ATTR{} = at
  toSalty bts@ARG_BYTES{} = bts

instance ToSalty EXTRA where
  toSalty EXTRA{..} = EXTRA (toSalty meta) func (map toSalty args)
