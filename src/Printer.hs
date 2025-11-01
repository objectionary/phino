{-# HLINT ignore "Avoid restricted module" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Printer
  ( printProgram,
    printProgram',
    printExpression,
    printExpression',
    printAttribute,
    printAttribute',
    printBinding,
    printBinding',
    printBytes,
    printExtraArg,
    printExtraArg',
    printSubsts,
    printSubsts',
  )
where

import AST
import CST
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Encoding
import Lining
import Matcher
import Render
import Sugar
import Yaml (ExtraArgument (ArgAttribute, ArgBinding, ArgBytes, ArgExpression))
import Prelude hiding (print)

type PrintConfig = (SugarType, Encoding, LineFormat)

defConfig :: PrintConfig
defConfig = (SWEET, UNICODE, MULTILINE)

withSugarType :: (ToSalty a) => SugarType -> a -> a
withSugarType SWEET prog = prog
withSugarType SALTY prog = toSalty prog

withLineFormat :: (ToSingleLine a) => LineFormat -> a -> a
withLineFormat MULTILINE prog = prog
withLineFormat SINGLELINE prog = toSingleLine prog

withEncoding :: (ToASCII a) => Encoding -> a -> a
withEncoding UNICODE prog = prog
withEncoding ASCII prog = toASCII prog

printProgram :: Program -> PrintConfig -> Prelude.String
printProgram prog (sugar, encoding, line) = render (withLineFormat line Prelude.$ withEncoding encoding Prelude.$ withSugarType sugar Prelude.$ programToCST prog)

printProgram' :: Program -> Prelude.String
printProgram' prog = printProgram prog defConfig

printExpression :: Expression -> PrintConfig -> Prelude.String
printExpression expr (sugar, encoding, line) = render (withLineFormat line Prelude.$ withEncoding encoding Prelude.$ withSugarType sugar Prelude.$ expressionToCST expr)

printExpression' :: Expression -> Prelude.String
printExpression' expr = printExpression expr defConfig

printAttribute :: Attribute -> Encoding -> Prelude.String
printAttribute attr encoding = render (withEncoding encoding (toCST attr 0 :: ATTRIBUTE))

printAttribute' :: Attribute -> Prelude.String
printAttribute' attr = printAttribute attr UNICODE

printBinding :: Binding -> PrintConfig -> Prelude.String
printBinding bd = printExpression (ExFormation [bd])

printBinding' :: Binding -> Prelude.String
printBinding' bd = printBinding bd defConfig

printBytes :: Bytes -> Prelude.String
printBytes bts = render (toCST bts 0 :: BYTES)

printExtraArg :: ExtraArgument -> PrintConfig -> Prelude.String
printExtraArg (ArgAttribute attr) (_, encoding, _) = printAttribute attr encoding
printExtraArg (ArgBinding bd) config = printBinding bd config
printExtraArg (ArgExpression expr) config = printExpression expr config
printExtraArg (ArgBytes bts) _ = printBytes bts

printExtraArg' :: ExtraArgument -> Prelude.String
printExtraArg' arg = printExtraArg arg defConfig

printTail :: Tail -> PrintConfig -> Prelude.String
printTail (TaApplication tau) config = "(" Prelude.<> printBinding tau config Prelude.<> ")"
printTail (TaDispatch attr) (_, encoding, _) = "." Prelude.<> printAttribute attr encoding

printMetaValue :: MetaValue -> PrintConfig -> Prelude.String
printMetaValue (MvAttribute attr) (_, encoding, _) = printAttribute attr encoding
printMetaValue (MvExpression expr _) config = printExpression expr config
printMetaValue (MvBytes bts) _ = printBytes bts
printMetaValue (MvBindings bds) config = printExpression (ExFormation bds) config
printMetaValue (MvFunction func) _ = func
printMetaValue (MvTail tails) config = intercalate "," (Prelude.map (`printTail` config) tails)

printSubst :: Subst -> PrintConfig -> Prelude.String
printSubst (Subst mp) config =
  "  "
    Prelude.<> intercalate
      "\n  "
      (Prelude.map (\(key, value) -> key Prelude.<> " >> " Prelude.<> printMetaValue value config) (Map.toList mp))

printSubsts :: [Subst] -> PrintConfig -> Prelude.String
printSubsts substs config = intercalate "\n---\n" (Prelude.map (`printSubst` config) substs)

printSubsts' :: [Subst] -> Prelude.String
printSubsts' substs = printSubsts substs defConfig
