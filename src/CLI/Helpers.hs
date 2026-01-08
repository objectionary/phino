-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Helpers where

import AST
import CLI.Types
import CLI.Validators (invalidCLIArguments)
import Control.Exception
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe
import Deps (SaveStepFunc, saveStep)
import Encoding
import LaTeX (LatexContext (..), defaultMeetLength, defaultMeetPopularity, expressionToLaTeX, programToLaTeX, rewrittensToLatex)
import Locator (locatedExpression)
import Logger
import qualified Misc as M
import Parser (parseProgramThrows)
import qualified Printer as P
import Rewriter (Rewrittens)
import System.IO (getContents')
import Text.Printf (printf)
import XMIR (parseXMIRThrows, printXMIR, programToXMIR, xmirToPhi)
import Yaml (normalizationRules)
import qualified Yaml as Y

justMeetPopularity :: Maybe Int -> Int
justMeetPopularity = fromMaybe defaultMeetPopularity

justMeetLength :: Maybe Int -> Int
justMeetLength = fromMaybe defaultMeetLength

-- Prepare saveStepFunc
saveStepFunc :: Maybe FilePath -> PrintProgramContext -> SaveStepFunc
saveStepFunc stepsDir ctx@PrintProgCtx{..} = saveStep stepsDir ioToExt (printProgram ctx)
  where
    ioToExt :: String
    ioToExt
      | _outputFormat == LATEX = "tex"
      | otherwise = show _outputFormat

-- Read input from file or stdin
readInput :: Maybe FilePath -> IO String
readInput inputFile' = case inputFile' of
  Just pth -> do
    logDebug (printf "Reading from file: '%s'" pth)
    readFile =<< M.ensuredFile pth
  Nothing -> do
    logDebug "Reading from stdin"
    getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))

-- Parse program from String input depending on input IO format
parseProgram :: String -> IOFormat -> IO Program
parseProgram phi PHI = parseProgramThrows phi
parseProgram xmir XMIR = parseXMIRThrows xmir >>= xmirToPhi
parseProgram _ LATEX = invalidCLIArguments "LaTeX cannot be used as input format"

printRewrittens :: PrintProgramContext -> Rewrittens -> IO String
printRewrittens ctx@PrintProgCtx{..} rewrittens@(chain, _)
  | _outputFormat == LATEX && _sequence = rewrittensToLatex rewrittens (printCtxToLatexCtx ctx)
  | _focus == ExGlobal = mapM (printProgram ctx . fst) chain <&> intercalate "\n"
  | otherwise = mapM (\(prog, _) -> locatedExpression _focus prog >>= printExpression ctx) chain <&> intercalate "\n"

printExpression :: PrintProgramContext -> Expression -> IO String
printExpression ctx@PrintProgCtx{..} ex = case _outputFormat of
  PHI -> pure (P.printExpression' ex (_sugar, UNICODE, _line, _margin))
  XMIR -> throwIO CouldNotPrintExpressionInXMIR
  LATEX -> pure (expressionToLaTeX ex (printCtxToLatexCtx ctx))

-- Convert
-- Convert program to corresponding String format
printProgram :: PrintProgramContext -> Program -> IO String
printProgram ctx@PrintProgCtx{..} prog = case _outputFormat of
  PHI -> pure (P.printProgram' prog (_sugar, UNICODE, _line, _margin))
  XMIR -> programToXMIR prog _xmirCtx <&> printXMIR
  LATEX -> pure (programToLaTeX prog (printCtxToLatexCtx ctx))

printCtxToLatexCtx :: PrintProgramContext -> LatexContext
printCtxToLatexCtx PrintProgCtx{..} =
  LatexContext _sugar _line _margin _nonumber _compress _meetPopularity _meetLength _focus _expression _label _meetPrefix

-- Get rules for rewriting depending on provided flags
getRules :: Bool -> Bool -> [FilePath] -> IO [Y.Rule]
getRules normalize shuffle rules = do
  ordered <-
    if normalize
      then do
        let rules' = normalizationRules
        logDebug (printf "The --normalize option is provided, %d built-it normalization rules are used" (length rules'))
        pure rules'
      else
        if null rules
          then do
            logDebug "No --rule and no --normalize options are provided, no rules are used"
            pure []
          else do
            logDebug (printf "Using rules from files: [%s]" (intercalate ", " rules))
            yamls <- mapM M.ensuredFile rules
            mapM Y.yamlRule yamls
  if shuffle
    then do
      logDebug "The --shuffle option is provided, rules are used in random order"
      M.shuffle ordered
    else pure ordered

-- Output content
printOut :: Maybe FilePath -> String -> IO ()
printOut target content = case target of
  Nothing -> do
    logDebug "The option '--target' is not specified, printing to console..."
    putStrLn content
  Just file -> do
    logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
    writeFile file content
    logDebug (printf "The command result was saved in '%s'" file)
