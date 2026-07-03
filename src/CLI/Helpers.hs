-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CLI.Helpers where

import AST
import CLI.Types
import CLI.Validators (invalidCLIArguments)
import Canonizer (canonize)
import Control.Exception
import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe
import Deps (SaveStepFunc, saveStep)
import Encoding
import Files (ensuredFile)
import Functions (execFunctions)
import LaTeX (LatexContext (..), defaultMeetLength, defaultMeetPopularity, expressionToLaTeX, rewrittensToLatex)
import Locator (locatedExpression)
import Logger
import Parser (parseExpressionThrows)
import qualified Printer as P
import qualified Random as R
import Rewriter (Rewritten, Rewrittens')
import System.IO (getContents')
import Text.Printf (printf)
import XMIR (expressionToXMIR, parseXMIRThrows, printXMIR, xmirToPhi)
import Yaml (normalizationRules)
import qualified Yaml as Y

justMeetPopularity :: Maybe Int -> Int
justMeetPopularity = fromMaybe defaultMeetPopularity

justMeetLength :: Maybe Int -> Int
justMeetLength = fromMaybe defaultMeetLength

-- Prepare saveStepFunc
saveStepFunc :: Maybe FilePath -> PrintContext -> SaveStepFunc
saveStepFunc stepsDir ctx@PrintCtx{..} = saveStep stepsDir ioToExt (printInFormat ctx)
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
    readFile =<< ensuredFile pth
  Nothing -> do
    logDebug "Reading from stdin"
    getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))

-- Parse expression from String input depending on input IO format
parseInput :: String -> IOFormat -> IO Expression
parseInput phi PHI = parseExpressionThrows phi
parseInput xmir XMIR = parseXMIRThrows xmir >>= xmirToPhi
parseInput _ LATEX = invalidCLIArguments "LaTeX cannot be used as input format"

-- The LaTeX sequence path canonizes inside 'rewrittensToLatex', after the meet
-- compression (see 'canonizedRewrittens' there); the remaining formats have no
-- meet pass, so canonization happens here right before rendering.
printRewrittens :: PrintContext -> Rewrittens' -> IO String
printRewrittens ctx@PrintCtx{..} rewrittens@(chain, _)
  | _outputFormat == LATEX && _sequence = rewrittensToLatex rewrittens (printCtxToLatexCtx ctx)
  | _focus == ExRoot = mapM (printInFormat ctx . fst) (canonized chain) <&> intercalate "\n"
  | otherwise = mapM (\(expr, _) -> locatedExpression _focus expr >>= printExpression ctx) (canonized chain) <&> intercalate "\n"
  where
    canonized :: [Rewritten] -> [Rewritten]
    canonized = if _canonize then canonize else id

printExpression :: PrintContext -> Expression -> IO String
printExpression ctx@PrintCtx{..} ex = case _outputFormat of
  PHI -> pure (P.printExpression' ex (_sugar, UNICODE, _line, _margin))
  XMIR -> throwIO CouldNotPrintExpressionInXMIR
  LATEX -> pure (expressionToLaTeX ex (printCtxToLatexCtx ctx))

-- Convert an expression to its corresponding String format
printInFormat :: PrintContext -> Expression -> IO String
printInFormat ctx@PrintCtx{..} expr = case _outputFormat of
  PHI -> pure (P.printExpression' expr (_sugar, UNICODE, _line, _margin))
  XMIR -> expressionToXMIR expr _xmirCtx <&> printXMIR
  LATEX -> pure (expressionToLaTeX expr (printCtxToLatexCtx ctx))

printCtxToLatexCtx :: PrintContext -> LatexContext
printCtxToLatexCtx PrintCtx{..} =
  LatexContext _sugar _line _margin _nonumber _compress _canonize _meetPopularity _meetLength _focus _expression _label _meetPrefix

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
            yamls <- mapM ensuredFile rules
            mapM (Y.yamlRule >=> validateRewriteRule) yamls
  if shuffle
    then do
      logDebug "The --shuffle option is provided, rules are used in random order"
      R.shuffle ordered
    else pure ordered

-- Pass a user-supplied rewriting rule through unchanged, or fail fast if it
-- references a build-term function which needs the dataization context: those
-- work only for dataization and morphing, never for plain rewriting.
validateRewriteRule :: Y.Rule -> IO Y.Rule
validateRewriteRule rule =
  let used = maybe [] (map Y.function) rule.where_
   in case filter (`elem` execFunctions) used of
        [] -> pure rule
        (fn : _) ->
          invalidCLIArguments
            (printf "Function '%s' in rule '%s' is available only for dataization and morphing, not for rewriting" fn rule.name)

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
