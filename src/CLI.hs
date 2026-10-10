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
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Version (showVersion)
import Files (ensuredFile)
import GHC.IO.Exception (IOErrorType (ResourceVanished))
import Logger
import Options.Applicative
import Paths_phino (version)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType)

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  let parsed = execParserPure defaultPrefs parserInfo args
  CliArgs{_pin, _command} <- case parsed of
    Success opts -> pure opts
    Failure failure -> do
      let (msg, code) = renderFailure failure "phino"
      case code of
        ExitSuccess -> do
          putStrLn msg
          exitWith code
        _ -> do
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
    CmdCompile opts -> runCompile opts
  where
    prefixFirstLine :: String -> String -> String
    prefixFirstLine _ "" = "Failure"
    prefixFirstLine prefix msg = prefix ++ msg
    handler :: SomeException -> IO ()
    handler e = case fromException e of
      Just ExitSuccess -> pure ()
      Just (ExitFailure _) -> exitFailure
      _ -> case fromException e of
        Just (ioe :: IOError) | ioeGetErrorType ioe == ResourceVanished -> exitWith (ExitFailure 141)
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
            CmdCompile OptsCompile{_logLevel, _logLines} -> (_logLevel, _logLines)
       in setLogConfig level lns
    checkPin :: Maybe Pin -> IO ()
    checkPin Nothing = pure ()
    checkPin (Just (PinFile file)) = ensuredFile file >>= readFile >>= checkPin . Just . PinVersion . dropWhileEnd isSpace . dropWhile isSpace
    checkPin (Just (PinVersion expected))
      | expected == actual = pure ()
      | otherwise = throwIO (VersionMismatch expected actual)
      where
        actual = showVersion version
