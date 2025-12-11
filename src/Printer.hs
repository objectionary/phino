{-# HLINT ignore "Avoid restricted module" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Printer
  ( printProgram
  , printProgram'
  , printExpression
  , printExpression'
  , printAttribute
  , printBinding
  , printBytes
  , printExtraArg
  , printSubsts
  , printSubsts'
  , PrintConfig
  , logPrintConfig
  )
where

import AST
import qualified CST
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

defaultPrintConfig :: PrintConfig
defaultPrintConfig = (SWEET, UNICODE, MULTILINE)

logPrintConfig :: (SugarType, Encoding, LineFormat)
logPrintConfig = (SWEET, UNICODE, SINGLELINE)

printProgram' :: Program -> PrintConfig -> String
printProgram' prog (sugar, encoding, line) = render (withLineFormat line $ withEncoding encoding $ withSugarType sugar $ CST.programToCST prog)

printProgram :: Program -> String
printProgram prog = printProgram' prog defaultPrintConfig

printExpression' :: Expression -> PrintConfig -> String
printExpression' expr (sugar, encoding, line) = render (withLineFormat line $ withEncoding encoding $ withSugarType sugar $ CST.expressionToCST expr)

printExpression :: Expression -> String
printExpression expr = printExpression' expr defaultPrintConfig

printAttribute' :: Attribute -> Encoding -> String
printAttribute' attr encoding = render (withEncoding encoding (CST.toCST attr 0 CST.NO_EOL :: CST.ATTRIBUTE))

printAttribute :: Attribute -> String
printAttribute attr =
  let (_, encoding, _) = defaultPrintConfig
   in printAttribute' attr encoding

printBinding' :: Binding -> PrintConfig -> String
printBinding' bd = printExpression' (ExFormation [bd])

printBinding :: Binding -> String
printBinding bd = printBinding' bd defaultPrintConfig

printBytes :: Bytes -> String
printBytes bts = render (CST.toCST bts 0 CST.NO_EOL :: CST.BYTES)

printExtraArg' :: ExtraArgument -> PrintConfig -> String
printExtraArg' (ArgAttribute attr) (_, encoding, _) = printAttribute' attr encoding
printExtraArg' (ArgBinding bd) config = printBinding' bd config
printExtraArg' (ArgExpression expr) config = printExpression' expr config
printExtraArg' (ArgBytes bts) _ = printBytes bts

printExtraArg :: ExtraArgument -> String
printExtraArg arg = printExtraArg' arg defaultPrintConfig

printTail :: Tail -> PrintConfig -> String
printTail (TaApplication tau) config = "(" <> printBinding' tau config <> ")"
printTail (TaDispatch attr) (_, encoding, _) = "." <> printAttribute' attr encoding

printMetaValue :: MetaValue -> PrintConfig -> String
printMetaValue (MvAttribute attr) (_, encoding, _) = printAttribute' attr encoding
printMetaValue (MvExpression expr _) config = printExpression' expr config
printMetaValue (MvBytes bts) _ = printBytes bts
printMetaValue (MvBindings bds) config = printExpression' (ExFormation bds) config
printMetaValue (MvFunction func) _ = func
printMetaValue (MvTail tails) config = intercalate "," (map (`printTail` config) tails)

printSubst :: Subst -> PrintConfig -> String
printSubst (Subst mp) config =
  intercalate
    "\n"
    (map (\(key, value) -> key <> " >> " <> printMetaValue value config) (Map.toList mp))

printSubsts' :: [Subst] -> PrintConfig -> String
printSubsts' [] _ = "------"
printSubsts' substs config = intercalate "\n------\n" (map (`printSubst` config) substs)

printSubsts :: [Subst] -> String
printSubsts substs = printSubsts' substs defaultPrintConfig
