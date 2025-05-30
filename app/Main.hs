{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import Data.Version (showVersion)
import Options.Applicative
import Paths_phino (version)
import Rewriter (rewrite)
import Control.Exception
import Misc (ensuredFile)
import qualified Yaml as Y

data CmdException
  = InvalidRewriteArguments
  | NormalizationIsNotSupported
  deriving (Exception)

instance Show CmdException where
  show InvalidRewriteArguments = "Invalid set of arguments for 'rewrite' command: no --rules, no --normalize, no --nothing are provided"
  show NormalizationIsNotSupported = "Normalization is not supported yet"

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

main :: IO ()
main = do
  cmd <- execParser parserInfo
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
