{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import CLI.Parsers
import CLI.Runners
import CLI.Types
import Control.Exception.Base (Exception (displayException), SomeException, fromException, handle, throwIO)
import Data.Version (showVersion)
import Logger
import Options.Applicative
import Paths_phino (version)
import System.Exit (ExitCode (..), exitFailure)

handler :: SomeException -> IO ()
handler e = case fromException e of
  Just ExitSuccess -> pure () -- prevent printing error on --version etc.
  _ -> do
    logError (displayException e)
    exitFailure

setLogger :: Command -> IO ()
setLogger cmd =
  let (level, lns) = case cmd of
        CmdRewrite OptsRewrite{_logLevel, _logLines} -> (_logLevel, _logLines)
        CmdDataize OptsDataize{_logLevel, _logLines} -> (_logLevel, _logLines)
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

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  CliArgs{_pin, _command} <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  checkPin _pin
  setLogger _command
  case _command of
    CmdRewrite opts -> runRewrite opts
    CmdDataize opts -> runDataize opts
    CmdExplain opts -> runExplain opts
    CmdMerge opts -> runMerge opts
    CmdMatch opts -> runMatch opts
