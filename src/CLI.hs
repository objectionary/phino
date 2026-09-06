{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import CLI.Parsers
import CLI.Runners
import CLI.Types
import Control.Exception.Base (SomeException, fromException, handle, throwIO)
import Data.Version (showVersion)
import Logger
import Options.Applicative
import Paths_phino (version)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (hPutStrLn, stderr)

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  let parsed = execParserPure defaultPrefs parserInfo args
  CliArgs{_pin, _command} <- case parsed of
    Success opts -> pure opts
    Failure failure -> do
      let (msg, code) = renderFailure failure "phino"
      case code of
        ExitSuccess -> do
          putStrLn msg -- --version/--help output as-is
          exitWith code
        _ -> do
          -- Keep the full optparse message (including the Usage/synopsis
          -- block that follows a parse error), but without the GHC
          -- HasCallStack backtrace; prefix just the first line with [ERROR]:.
          hPutStrLn stderr (prefixFirstLine "[ERROR]: " msg)
          exitWith code
    CompletionInvoked _ -> handleParseResult parsed
  checkPin _pin
  setLogger _command
  case _command of
    CmdRewrite opts -> runRewrite opts
    CmdDataize opts -> runDataize opts
    CmdMorph opts -> runMorph opts
    CmdExplain opts -> runExplain opts
    CmdMerge opts -> runMerge opts
    CmdMatch opts -> runMatch opts
  where
    prefixFirstLine :: String -> String -> String
    prefixFirstLine _ "" = "Failure"
    prefixFirstLine prefix msg = prefix ++ msg
    handler :: SomeException -> IO ()
    handler e = case fromException e of
      Just ExitSuccess -> pure () -- prevent printing error on --version etc.
      Just (ExitFailure _) -> exitFailure -- already logged by the Failure branch above
      _ -> do
        logError (show e)
        exitFailure
    setLogger :: Command -> IO ()
    setLogger cmd =
      let (level, lns) = case cmd of
            CmdRewrite OptsRewrite{_logLevel, _logLines} -> (_logLevel, _logLines)
            CmdDataize OptsDataize{_logLevel, _logLines} -> (_logLevel, _logLines)
            CmdMorph OptsMorph{_logLevel, _logLines} -> (_logLevel, _logLines)
            CmdExplain OptsExplain{_logLevel, _logLines} -> (_logLevel, _logLines)
            CmdMerge OptsMerge{_logLevel, _logLines} -> (_logLevel, _logLines)
            CmdMatch OptsMatch{_logLevel, _logLines} -> (_logLevel, _logLines)
       in setLogConfig level lns
    checkPin :: Maybe String -> IO ()
    checkPin Nothing = pure ()
    checkPin (Just expected)
      | expected == actual = pure ()
      | otherwise = throwIO (VersionMismatch expected actual)
      where
        actual = showVersion version
