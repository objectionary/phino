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
import Data.Version (showVersion)
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

data CmdException
  = InvalidRewriteArguments {message :: String}
  | CouldNotReadFromStdin {message :: String}
  deriving (Exception)

instance Show CmdException where
  show InvalidRewriteArguments {..} = printf "Invalid set of arguments for 'rewrite' command: %s" message
  show CouldNotReadFromStdin {..} = printf "Could not read 𝜑-expression from stdin\nReason: %s" message

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

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper <**> simpleVersioner (showVersion version))
    (fullDesc <> header "Phino - CLI Manipulator of 𝜑-Calculus Expressions")

handler :: SomeException -> IO ()
handler e = case fromException e of
  Just ExitSuccess -> pure () -- prevent printing error on --version etc.
  _ -> do
    hPutStrLn stderr ("[error] " ++ displayException e)
    exitFailure

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  cmd <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  case cmd of
    CmdRewrite OptsRewrite {..} -> do
      when (maxDepth < 0) $ throwIO (InvalidRewriteArguments "--max-depth must be non-negative")
      prog <- case phiInput of
        Just pth -> readFile =<< ensuredFile pth
        Nothing -> getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))
      rules' <- do
        ordered <-
          if nothing
            then pure []
            else
              if normalize
                then pure normalizationRules
                else
                  if null rules
                    then throwIO (InvalidRewriteArguments "no --rule, no --normalize, no --nothing are provided")
                    else do
                      yamls <- mapM ensuredFile rules
                      mapM Y.yamlRule yamls
        if shuffle
          then Misc.shuffle ordered
          else pure ordered
      program <- parseProgramThrows prog
      rewritten <- rewrite' program rules' 0
      putStrLn (printProgram rewritten)
      where
        rewrite' :: Program -> [Y.Rule] -> Integer -> IO Program
        rewrite' prog rules count = do
          if count == maxDepth
            then pure prog
            else do
              rewritten <- rewrite prog rules
              if rewritten == prog
                then pure rewritten
                else rewrite' rewritten rules (count + 1)
