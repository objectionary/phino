{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI.Types where

import AST
import Control.Exception (Exception)
import Lining (LineFormat)
import Logger (LogLevel)
import Must (Must)
import Sugar (SugarType)
import Text.Printf (printf)
import XMIR (XmirContext)

data PrintProgramContext = PrintProgCtx
  { _sugar :: SugarType
  , _line :: LineFormat
  , _margin :: Int
  , _xmirCtx :: XmirContext
  , _nonumber :: Bool
  , _compress :: Bool
  , _sequence :: Bool
  , _meetPopularity :: Int
  , _meetLength :: Int
  , _focus :: Expression
  , _expression :: Maybe String
  , _label :: Maybe String
  , _meetPrefix :: Maybe String
  , _outputFormat :: IOFormat
  }

data CmdException
  = InvalidCLIArguments String
  | CouldNotReadFromStdin String
  | CouldNotDataize
  | CouldNotPrintExpressionInXMIR
  deriving (Exception)

instance Show CmdException where
  show (InvalidCLIArguments msg) = printf "Invalid set of arguments: %s" msg
  show (CouldNotReadFromStdin msg) = printf "Could not read input from stdin\nReason: %s" msg
  show CouldNotDataize = "Could not dataize given program"
  show CouldNotPrintExpressionInXMIR = "Could not print expression with --output=xmir, only program printing is allowed"

data Command
  = CmdRewrite OptsRewrite
  | CmdDataize OptsDataize
  | CmdExplain OptsExplain
  | CmdMerge OptsMerge
  | CmdMatch OptsMatch

data IOFormat = XMIR | PHI | LATEX
  deriving (Eq)

instance Show IOFormat where
  show XMIR = "xmir"
  show PHI = "phi"
  show LATEX = "latex"

data OptsDataize = OptsDataize
  { _logLevel :: LogLevel
  , _logLines :: Int
  , _inputFormat :: IOFormat
  , _outputFormat :: IOFormat
  , _sugarType :: SugarType
  , _flat :: LineFormat
  , _omitListing :: Bool
  , _omitComments :: Bool
  , _nonumber :: Bool
  , _sequence :: Bool
  , _canonize :: Bool
  , _depthSensitive :: Bool
  , _quiet :: Bool
  , _compress :: Bool
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _margin :: Int
  , _meetPopularity :: Maybe Int
  , _meetLength :: Maybe Int
  , _hide :: [String]
  , _show :: [String]
  , _locator :: String
  , _focus :: String
  , _expression :: Maybe String
  , _label :: Maybe String
  , _meetPrefix :: Maybe String
  , _stepsDir :: Maybe FilePath
  , _inputFile :: Maybe FilePath
  }

data OptsExplain = OptsExplain
  { _logLevel :: LogLevel
  , _logLines :: Int
  , _rules :: [FilePath]
  , _normalize :: Bool
  , _shuffle :: Bool
  , _targetFile :: Maybe FilePath
  }

data OptsRewrite = OptsRewrite
  { _logLevel :: LogLevel
  , _logLines :: Int
  , _inputFormat :: IOFormat
  , _outputFormat :: IOFormat
  , _sugarType :: SugarType
  , _flat :: LineFormat
  , _must :: Must
  , _normalize :: Bool
  , _shuffle :: Bool
  , _omitListing :: Bool
  , _omitComments :: Bool
  , _depthSensitive :: Bool
  , _nonumber :: Bool
  , _inPlace :: Bool
  , _sequence :: Bool
  , _canonize :: Bool
  , _compress :: Bool
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _margin :: Int
  , _meetPopularity :: Maybe Int
  , _meetLength :: Maybe Int
  , _rules :: [FilePath]
  , _hide :: [String]
  , _show :: [String]
  , _locator :: String
  , _focus :: String
  , _expression :: Maybe String
  , _label :: Maybe String
  , _meetPrefix :: Maybe String
  , _targetFile :: Maybe FilePath
  , _stepsDir :: Maybe FilePath
  , _inputFile :: Maybe FilePath
  }

data OptsMerge = OptsMerge
  { _logLevel :: LogLevel
  , _logLines :: Int
  , _inputFormat :: IOFormat
  , _outputFormat :: IOFormat
  , _sugarType :: SugarType
  , _flat :: LineFormat
  , _omitListing :: Bool
  , _omitComments :: Bool
  , _margin :: Int
  , _targetFile :: Maybe FilePath
  , _inputs :: [FilePath]
  }

data OptsMatch = OptsMatch
  { _logLevel :: LogLevel
  , _logLines :: Int
  , _sugarType :: SugarType
  , _flat :: LineFormat
  , _pattern :: Maybe String
  , _when :: Maybe String
  , _inputFile :: Maybe FilePath
  }
