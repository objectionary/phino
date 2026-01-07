{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import CLI.Parsers
import CLI.Runners
import CLI.Types
import Control.Exception.Base (Exception (displayException), SomeException, fromException, handle)
import Logger
import Options.Applicative
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

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  cmd <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  setLogger cmd
  case cmd of
    CmdRewrite opts -> runRewrite opts
    CmdDataize opts -> runDataize opts
    CmdExplain opts -> runExplain opts
    CmdMerge opts -> runMerge opts
    CmdMatch opts -> runMatch opts
