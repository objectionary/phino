{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import Ast (Program (Program))
import Control.Exception (Exception (displayException), SomeException, handle, throw, throwIO)
import Control.Exception.Base
import Control.Monad (when)
import Data.List (intercalate)
import Data.Version (showVersion)
import Logger
import Misc (ensuredFile)
import qualified Misc
import Options.Applicative
import Parser (parseProgramThrows)
import Paths_phino (version)
import Printer (printProgram)
import Rewriter (rewrite)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (getContents', hPutStrLn, stderr)
import Text.Printf (printf)
import Yaml (normalizationRules)
import qualified Yaml as Y
import Data.Char (toLower)

data CmdException
  = InvalidRewriteArguments {message :: String}
  | CouldNotReadFromStdin {message :: String}
  deriving (Exception)

instance Show CmdException where
  show InvalidRewriteArguments {..} = printf "Invalid set of arguments for 'rewrite' command: %s" message
  show CouldNotReadFromStdin {..} = printf "Could not read 𝜑-expression from stdin\nReason: %s" message

data App = App
  { logLevel :: LogLevel,
    cmd :: Command
  }

newtype Command = CmdRewrite OptsRewrite

data OptsRewrite = OptsRewrite
  { rules :: [FilePath],
    phiInput :: Maybe FilePath,
    normalize :: Bool,
    nothing :: Bool,
    shuffle :: Bool,
    maxDepth :: Integer
  }

rewriteParser :: Parser Command
rewriteParser =
  CmdRewrite
    <$> ( OptsRewrite
            <$> many (strOption (long "rule" <> metavar "FILE" <> help "Path to custom rule"))
            <*> optional (strOption (long "phi-input" <> metavar "FILE" <> help "Path .phi file with 𝜑-expression"))
            <*> switch (long "normalize" <> help "Use built-in normalization rules")
            <*> switch (long "nothing" <> help "Desugar provided 𝜑-expression")
            <*> switch (long "shuffle" <> help "Shuffle rules before applying")
            <*> option auto (long "max-depth" <> metavar "DEPTH" <> help "Max amount of rewritng cycles" <> value 25 <> showDefault)
        )

commandParser :: Parser Command
commandParser = hsubparser (command "rewrite" (info rewriteParser (progDesc "Rewrite the expression")))

appParser :: Parser App
appParser =
  App
    <$> do
      let all = map show [DEBUG, INFO, WARNING, ERROR, NONE]
      option
        auto
        ( long "log-level"
            <> metavar "LEVEL"
            <> completeWith all
            <> help ("Log level (" <> intercalate ", " all <> ")")
            <> value INFO
            <> showDefault
        )
    <*> commandParser

parserInfo :: ParserInfo App
parserInfo =
  info
    (appParser <**> helper <**> simpleVersioner (showVersion version))
    (fullDesc <> header "Phino - CLI Manipulator of 𝜑-Calculus Expressions")

handler :: SomeException -> IO ()
handler e = case fromException e of
  Just ExitSuccess -> pure () -- prevent printing error on --version etc.
  _ -> do
    logError (displayException e)
    exitFailure

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  App{..} <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  setLogLevel logLevel
  case cmd of
    CmdRewrite OptsRewrite {..} -> do
      when (maxDepth < 0) $ throwIO (InvalidRewriteArguments "--max-depth must be non-negative")
      logDebug (printf "Amount of rewrigin cycles: %d" maxDepth)
      prog <- case phiInput of
        Just pth -> do
          logDebug (printf "Reading 𝜑-program from file: '%s'" pth)
          readFile =<< ensuredFile pth
        Nothing -> do
          logDebug "Reading 𝜑-program from stdin"
          getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))
      rules' <- do
        ordered <-
          if nothing
            then do
              logDebug "The --nothing option is provided, no rules are used"
              pure []
            else
              if normalize
                then do
                  logDebug "The --normalize option is provided, built-it normalization rules are used"
                  pure normalizationRules
                else
                  if null rules
                    then throwIO (InvalidRewriteArguments "no --rule, no --normalize, no --nothing are provided")
                    else do
                      logDebug (printf "Using rules from files: [%s]" (intercalate ", " rules))
                      yamls <- mapM ensuredFile rules
                      mapM Y.yamlRule yamls
        if shuffle
          then do
            logDebug "The --shuffle option is provided, rules are used in random order"
            Misc.shuffle ordered
          else pure ordered
      program <- parseProgramThrows prog
      rewritten <- rewrite' program rules' 1
      putStrLn (printProgram rewritten)
      where
        rewrite' :: Program -> [Y.Rule] -> Integer -> IO Program
        rewrite' prog rules count = do
          logDebug (printf "Starting rewriting cycle %d out of %d" count maxDepth)
          if count == maxDepth
            then do
              logDebug (printf "Max amount of rewriting cycles is reached, rewriting is stopped")
              pure prog
            else do
              rewritten <- rewrite prog rules
              if rewritten == prog
                then do
                  logDebug "Rewriting is stopped since it does not affect program anymore"
                  pure rewritten
                else rewrite' rewritten rules (count + 1)
