-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import Data.Version (showVersion)
import Options.Applicative
import Paths_phino (version)
import Rewriter (rewrite)

newtype Command = CmdRewrite OptsRewrite

data OptsRewrite = OptsRewrite
  { rules :: Maybe FilePath,
    normalize :: Bool,
    phi :: FilePath
  }

rewriteParser :: Parser Command
rewriteParser =
  CmdRewrite
    <$> ( OptsRewrite
            <$> optional (strOption (long "rules" <> metavar "FILENAME" <> help "Custom rule"))
            <*> switch (long "normalize" <> help "Use built-in rules")
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
    CmdRewrite (OptsRewrite rules' normalize' phi') -> do
      rewritten <- rewrite rules' normalize' phi'
      putStrLn rewritten
