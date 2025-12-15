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

defaultPrintConfig :: PrintConfig
defaultPrintConfig = (SWEET, UNICODE, MULTILINE)

logPrintConfig :: (SugarType, Encoding, LineFormat)
logPrintConfig = (SWEET, UNICODE, SINGLELINE)

printProgram' :: Program -> PrintConfig -> String
printProgram' prog (sugar, encoding, line) = render (withLineFormat line $ withEncoding encoding $ withSugarType sugar $ programToCST prog)

printProgram :: Program -> String
printProgram prog = printProgram' prog defaultPrintConfig

printExpression' :: Expression -> PrintConfig -> String
printExpression' ex (sugar, encoding, line) = render (withLineFormat line $ withEncoding encoding $ withSugarType sugar $ expressionToCST ex)

printExpression :: Expression -> String
printExpression ex = printExpression' ex defaultPrintConfig

printAttribute' :: Attribute -> Encoding -> String
printAttribute' att encoding = render (withEncoding encoding (toCST att 0 NO_EOL :: ATTRIBUTE))

printAttribute :: Attribute -> String
printAttribute att =
  let (_, encoding, _) = defaultPrintConfig
   in printAttribute' att encoding

printBinding' :: Binding -> PrintConfig -> String
printBinding' bd = printExpression' (ExFormation [bd])

printBinding :: Binding -> String
printBinding bd = printBinding' bd defaultPrintConfig

printBytes :: Bytes -> String
printBytes bts = render (toCST bts 0 NO_EOL :: BYTES)

printExtraArg' :: ExtraArgument -> PrintConfig -> String
printExtraArg' (ArgAttribute att) (_, encoding, _) = printAttribute' att encoding
printExtraArg' (ArgBinding bd) config = printBinding' bd config
printExtraArg' (ArgExpression ex) config = printExpression' ex config
printExtraArg' (ArgBytes bts) _ = printBytes bts

printExtraArg :: ExtraArgument -> String
printExtraArg arg = printExtraArg' arg defaultPrintConfig

printTail :: Tail -> PrintConfig -> String
printTail (TaApplication bd) config = "(" <> printBinding' bd config <> ")"
printTail (TaDispatch att) (_, encoding, _) = "." <> printAttribute' att encoding

printMetaValue :: MetaValue -> PrintConfig -> String
printMetaValue (MvAttribute att) (_, encoding, _) = printAttribute' att encoding
printMetaValue (MvExpression ex _) config = printExpression' ex config
printMetaValue (MvBytes bts) _ = printBytes bts
printMetaValue (MvBindings bds) config = printExpression' (ExFormation bds) config
printMetaValue (MvFunction fun) _ = fun
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
