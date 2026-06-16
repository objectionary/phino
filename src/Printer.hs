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
  , printAlpha
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
import qualified Data.Text as T
import Encoding
import Lining
import Margin (defaultMargin, withMargin)
import Matcher
import Render
import Sugar
import Yaml (ExtraArgument (ArgAttribute, ArgBinding, ArgBytes, ArgExpression))
import Prelude hiding (print)

type PrintConfig = (SugarType, Encoding, LineFormat, Int)

defaultPrintConfig :: PrintConfig
defaultPrintConfig = (SWEET, UNICODE, MULTILINE, defaultMargin)

logPrintConfig :: (SugarType, Encoding, LineFormat, Int)
logPrintConfig = (SWEET, UNICODE, SINGLELINE, defaultMargin)

printProgram' :: Program -> PrintConfig -> String
printProgram' prog (sugar, encoding, line, margin) = T.unpack $ render (withLineFormat line $ withMargin margin $ withEncoding encoding $ withSugarType sugar $ programToCST prog)

printProgram :: Program -> String
printProgram prog = printProgram' prog defaultPrintConfig

printExpression' :: Expression -> PrintConfig -> String
printExpression' ex (sugar, encoding, line, margin) = T.unpack $ render (withLineFormat line $ withMargin margin $ withEncoding encoding $ withSugarType sugar $ expressionToCST ex)

printExpression :: Expression -> String
printExpression ex = printExpression' ex defaultPrintConfig

printAttribute' :: Attribute -> Encoding -> String
printAttribute' att encoding = T.unpack $ render (withEncoding encoding (toCST att (0, NO_EOL) :: ATTRIBUTE))

printAttribute :: Attribute -> String
printAttribute att =
  let (_, encoding, _, _) = defaultPrintConfig
   in printAttribute' att encoding

printAlpha' :: Alpha -> Encoding -> String
printAlpha' a encoding = T.unpack $ render (withEncoding encoding (toCST a (0, NO_EOL) :: ATTRIBUTE))

printAlpha :: Alpha -> String
printAlpha a =
  let (_, encoding, _, _) = defaultPrintConfig
   in printAlpha' a encoding

printBinding' :: Binding -> PrintConfig -> String
printBinding' bd = printExpression' (ExFormation [bd])

printBinding :: Binding -> String
printBinding bd = printBinding' bd defaultPrintConfig

printArgument' :: Argument -> PrintConfig -> String
printArgument' arg (_, encoding, _, _) = T.unpack $ render (withEncoding encoding (toCST arg (0, NO_EOL) :: PAIR))

printBytes :: Bytes -> String
printBytes bts = T.unpack $ render (toCST bts (0, NO_EOL) :: BYTES)

printExtraArg' :: ExtraArgument -> PrintConfig -> String
printExtraArg' (ArgAttribute att) (_, encoding, _, _) = printAttribute' att encoding
printExtraArg' (ArgBinding bd) config = printBinding' bd config
printExtraArg' (ArgExpression ex) config = printExpression' ex config
printExtraArg' (ArgBytes bts) _ = printBytes bts

printExtraArg :: ExtraArgument -> String
printExtraArg arg = printExtraArg' arg defaultPrintConfig

printTail :: Tail -> PrintConfig -> String
printTail (TaApplication arg) config = "(" <> printArgument' arg config <> ")"
printTail (TaDispatch att) (_, encoding, _, _) = "." <> printAttribute' att encoding

printMetaValue :: MetaValue -> PrintConfig -> String
printMetaValue (MvAttribute att) (_, encoding, _, _) = printAttribute' att encoding
printMetaValue (MvAlpha a) (_, encoding, _, _) = printAlpha' a encoding
printMetaValue (MvExpression ex) config = printExpression' ex config
printMetaValue (MvBytes bts) _ = printBytes bts
printMetaValue (MvBindings bds) config = printExpression' (ExFormation bds) config
printMetaValue (MvFunction fun) _ = T.unpack fun
printMetaValue (MvTail tails) config = intercalate "," (map (`printTail` config) tails)

printSubst :: Subst -> PrintConfig -> String
printSubst (Subst mp) config =
  intercalate
    "\n"
    (map (\(key, value) -> T.unpack key <> " >> " <> printMetaValue value config) (Map.toList mp))

printSubsts' :: [Subst] -> PrintConfig -> String
printSubsts' [] _ = "------"
printSubsts' substs config = intercalate "\n------\n" (map (`printSubst` config) substs)

printSubsts :: [Subst] -> String
printSubsts substs = printSubsts' substs defaultPrintConfig
