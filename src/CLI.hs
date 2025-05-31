{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import Data.Version (showVersion)
import Options.Applicative
import Paths_phino (version)
import Rewriter (rewrite)
import Misc (ensuredFile)
import qualified Yaml as Y
import System.IO (hPutStrLn, stderr, getContents')
import System.Exit (exitFailure)
import Control.Exception (handle, SomeException, Exception (displayException), throwIO)
import Control.Exception.Base
import Text.Printf (printf)

data CmdException
  = InvalidRewriteArguments
  | NormalizationIsNotSupported
  | CouldNotReadFromStdin { message :: String }
  deriving (Exception)

instance Show CmdException where
  show InvalidRewriteArguments = "Invalid set of arguments for 'rewrite' command: no --rules, no --normalize, no --nothing are provided"
  show NormalizationIsNotSupported = "Normalization is not supported yet..."
  show CouldNotReadFromStdin{..} = printf "Could not read 𝜑-expression from stdin\nReason: %s" message

newtype Command = CmdRewrite OptsRewrite

data OptsRewrite = OptsRewrite
  { rules :: Maybe FilePath,
    phiInput :: Maybe FilePath,
    normalize :: Bool,
    nothing :: Bool
  }

rewriteParser :: Parser Command
rewriteParser =
  CmdRewrite
    <$> ( OptsRewrite
            <$> optional (strOption (long "rules" <> metavar "FILENAME" <> help "Path to custom rules"))
            <*> optional (strOption (long "phi-input" <> metavar "FILENAME" <> help "Path .phi file with 𝜑-expression"))
            <*> switch (long "normalize" <> help "Use built-in normalization rules")
            <*> switch (long "nothing" <> help "Desugar provided 𝜑-expression")
        )

commandParser :: Parser Command
commandParser = hsubparser (command "rewrite" (info rewriteParser (progDesc "Rewrite the expression")))

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper <**> simpleVersioner (showVersion version))
    (fullDesc <> header "Phino - CLI Manipulator of 𝜑-Calculus Expressions")

handler :: SomeException -> IO ()
handler e = do
  hPutStrLn stderr ("[error] " ++ displayException e)
  exitFailure

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  cmd <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  case cmd of
    CmdRewrite OptsRewrite{..} -> do
      prog <- case phiInput of
        Just pth -> readFile =<< ensuredFile pth
        Nothing -> getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))
      rules' <- if nothing
        then pure Nothing
        else do
          path <- case rules of
            Nothing ->
              if normalize
                then throwIO NormalizationIsNotSupported
                else throwIO InvalidRewriteArguments
            Just pth -> ensuredFile pth
          ruleSet <- Y.yamlRuleSet path
          pure (Just ruleSet)
      rewritten <- rewrite prog rules'
      putStrLn rewritten
