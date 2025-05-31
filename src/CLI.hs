{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import Data.Version (showVersion)
import Options.Applicative
import Paths_phino (version)
import Rewriter (rewrite)
import Misc (ensuredFile)
import qualified Yaml as Y
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Control.Exception (handle, SomeException, Exception (displayException), throwIO)

data CmdException
  = InvalidRewriteArguments
  | NormalizationIsNotSupported
  deriving (Show, Exception)

newtype Command = CmdRewrite OptsRewrite

data OptsRewrite = OptsRewrite
  { rules :: Maybe FilePath,
    normalize :: Bool,
    nothing :: Bool,
    phi :: FilePath
  }

rewriteParser :: Parser Command
rewriteParser =
  CmdRewrite
    <$> ( OptsRewrite
            <$> optional (strOption (long "rules" <> metavar "FILENAME" <> help "Custom rule"))
            <*> switch (long "normalize" <> help "Use built-in rules")
            <*> switch (long "nothing" <> help "Just desugar given expression")
            <*> strArgument (metavar "FILENAME" <> help "Path to .phi file to process")
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
      prog <- readFile =<< ensuredFile phi
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
