{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import Ast (Program (Program))
import Control.Exception (Exception (displayException), SomeException, handle, throw, throwIO)
import Control.Exception.Base
import Control.Monad (when)
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Version (showVersion)
import Logger
import Misc (ensuredFile)
import qualified Misc
import Options.Applicative
import Parser (parseProgramThrows)
import Paths_phino (version)
import Pretty (PrintMode (SALTY, SWEET), prettyProgram')
import Rewriter (rewrite')
import System.Exit (ExitCode (..), exitFailure)
import System.IO (getContents')
import Text.Printf (printf)
import XMIR (parseXMIRThrows, printXMIR, programToXMIR, xmirToPhi)
import Yaml (normalizationRules)
import qualified Yaml as Y

data CmdException
  = InvalidRewriteArguments {message :: String}
  | CouldNotReadFromStdin {message :: String}
  deriving (Exception)

instance Show CmdException where
  show InvalidRewriteArguments {..} = printf "Invalid set of arguments for 'rewrite' command: %s" message
  show CouldNotReadFromStdin {..} = printf "Could not read input from stdin\nReason: %s" message

data Command
  = CmdRewrite OptsRewrite
  | CmdDataize OptsDataize

data IOFormat = XMIR | PHI
  deriving (Eq)

instance Show IOFormat where
  show XMIR = "xmir"
  show PHI = "phi"

data OptsDataize = OptsDataize
  { logLevel :: LogLevel,
    inputFormat :: IOFormat,
    inputFile :: Maybe FilePath
  }

data OptsRewrite = OptsRewrite
  { logLevel :: LogLevel,
    rules :: [FilePath],
    inputFormat :: IOFormat,
    outputFormat :: IOFormat,
    printMode :: PrintMode,
    normalize :: Bool,
    nothing :: Bool,
    shuffle :: Bool,
    omitListing :: Bool,
    maxDepth :: Integer,
    inputFile :: Maybe FilePath
  }

parseIOFormat :: String -> ReadM IOFormat
parseIOFormat type' = eitherReader $ \format -> case map toLower format of
  "xmir" -> Right XMIR
  "phi" -> Right PHI
  _ -> Left (printf "invalid %s format: expected '%s' or '%s'" type' (show PHI) (show XMIR))

argInputFile :: Parser (Maybe FilePath)
argInputFile = optional (argument str (metavar "FILE" <> help "Path to input file"))

optInputFormat :: Parser IOFormat
optInputFormat = option (parseIOFormat "input") (long "input" <> metavar "FORMAT" <> help "Program input format (phi, xmir)" <> value PHI <> showDefault)

optLogLevel :: Parser LogLevel
optLogLevel =
  option
    parseLogLevel
    ( long "log-level"
        <> metavar "LEVEL"
        <> help ("Log level (" <> intercalate ", " (map show [DEBUG, INFO, WARNING, ERROR, NONE]) <> ")")
        <> value INFO
        <> showDefault
    )
  where
    parseLogLevel :: ReadM LogLevel
    parseLogLevel = eitherReader $ \lvl -> case map toUpper lvl of
      "DEBUG" -> Right DEBUG
      "INFO" -> Right INFO
      "WARNING" -> Right WARNING
      "WARN" -> Right WARNING
      "ERROR" -> Right ERROR
      "ERR" -> Right ERROR
      "NONE" -> Right NONE
      _ -> Left $ "unknown log-level: " <> lvl

dataizeParser :: Parser Command
dataizeParser =
  CmdDataize
    <$> ( OptsDataize
            <$> optLogLevel
            <*> optInputFormat
            <*> argInputFile
        )

rewriteParser :: Parser Command
rewriteParser =
  CmdRewrite
    <$> ( OptsRewrite
            <$> optLogLevel
            <*> many (strOption (long "rule" <> metavar "FILE" <> help "Path to custom rule"))
            <*> optInputFormat
            <*> option (parseIOFormat "output") (long "output" <> metavar "FORMAT" <> help "Program output format (phi, xmir)" <> value PHI <> showDefault)
            <*> flag SALTY SWEET (long "sweet" <> help "Print 𝜑-program using syntax sugar")
            <*> switch (long "normalize" <> help "Use built-in normalization rules")
            <*> switch (long "nothing" <> help "Just desugar provided 𝜑-program")
            <*> switch (long "shuffle" <> help "Shuffle rules before applying")
            <*> switch (long "omit-listing" <> help "Omit full program listing in XMIR output")
            <*> option auto (long "max-depth" <> metavar "DEPTH" <> help "Max amount of rewritng cycles" <> value 25 <> showDefault)
            <*> argInputFile
        )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "rewrite" (info (rewriteParser <**> helper) (progDesc "Rewrite the program"))
        <> command "dataize" (info (dataizeParser <**> helper) (progDesc "Dataize the program"))
    )

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper <**> simpleVersioner (showVersion version))
    (fullDesc <> header "Phino - CLI Manipulator of 𝜑-Calculus Expressions")

handler :: SomeException -> IO ()
handler e = case fromException e of
  Just ExitSuccess -> pure () -- prevent printing error on --version etc.
  _ -> do
    logError (displayException e)
    exitFailure

setLogLevel' :: Command -> IO ()
setLogLevel' cmd =
  let level = case cmd of
        CmdRewrite OptsRewrite {logLevel} -> logLevel
        CmdDataize OptsDataize {logLevel} -> logLevel
   in setLogLevel level

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  cmd <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  setLogLevel' cmd
  case cmd of
    CmdRewrite OptsRewrite {..} -> do
      when (maxDepth <= 0) $ throwIO (InvalidRewriteArguments "--max-depth must be positive")
      logDebug (printf "Amount of rewriting cycles: %d" maxDepth)
      input <- readInput inputFile
      rules' <- getRules
      program <- parseProgram input inputFormat
      rewritten <- rewrite' program rules' 1
      logDebug (printf "Printing rewritten 𝜑-program as %s" (show outputFormat))
      out <- printProgram rewritten outputFormat printMode
      putStrLn out
      where
        getRules :: IO [Y.Rule]
        getRules = do
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
        printProgram :: Program -> IOFormat -> PrintMode -> IO String
        printProgram prog PHI mode = pure (prettyProgram' prog mode)
        printProgram prog XMIR mode = do
          xmir <- programToXMIR prog mode omitListing
          pure (printXMIR xmir)
    CmdDataize OptsDataize {..} -> do
      input <- readInput inputFile
      prog <- parseProgram input inputFormat
      putStrLn "Dataization is not implemented yet"
  where
    readInput :: Maybe FilePath -> IO String
    readInput inputFile' = case inputFile' of
      Just pth -> do
        logDebug (printf "Reading from file: '%s'" pth)
        readFile =<< ensuredFile pth
      Nothing -> do
        logDebug "Reading from stdin"
        getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))
    parseProgram :: String -> IOFormat -> IO Program
    parseProgram phi PHI = parseProgramThrows phi
    parseProgram xmir XMIR = do
      doc <- parseXMIRThrows xmir
      xmirToPhi doc
