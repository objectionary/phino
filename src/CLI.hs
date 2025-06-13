{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import Control.Exception (Exception (displayException), SomeException, handle, throw, throwIO)
import Control.Exception.Base
import Data.Version (showVersion)
import Misc (ensuredFile)
import qualified Misc
import Options.Applicative
import Paths_phino (version)
import Rewriter (rewrite)
import System.Exit (exitFailure, ExitCode (..))
import System.IO (getContents', hPutStrLn, stderr)
import Text.Printf (printf)
import qualified Yaml as Y
import Printer (printProgram)
import Yaml (normalizationRules)
import Ast (Program(Program))
import Parser (parseProgramThrows)

data CmdException
  = InvalidRewriteArguments
  | CouldNotReadFromStdin {message :: String}
  deriving (Exception)

instance Show CmdException where
  show InvalidRewriteArguments = "Invalid set of arguments for 'rewrite' command: no --rule, no --normalize, no --nothing are provided"
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
  Just ExitSuccess -> pure ()  -- prevent printing error on --version etc.
  _ -> do
    hPutStrLn stderr ("[error] " ++ displayException e)
    exitFailure

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  cmd <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  case cmd of
    CmdRewrite OptsRewrite {..} -> do
      prog <- case phiInput of
        Just pth -> readFile =<< ensuredFile pth
        Nothing -> getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))
      rules' <- do
        ordered <- if nothing
          then pure []
          else
            if normalize
              then pure normalizationRules
              else
                if null rules
                  then throwIO InvalidRewriteArguments
                  else do
                    yamls <- mapM ensuredFile rules
                    mapM Y.yamlRule yamls
        if shuffle
          then Misc.shuffle ordered
          else pure ordered
      program <- parseProgramThrows prog
      rewritten <- rewriteAgain program rules' 0
      putStrLn (printProgram rewritten)
      where
        rewriteAgain :: Program -> [Y.Rule] -> Integer -> IO Program
        rewriteAgain prog rules count = do
          if count == maxDepth
            then pure prog
            else do
              rewritten <- rewrite prog rules
              if rewritten == prog
                then pure rewritten
                else rewriteAgain rewritten rules (count + 1)
