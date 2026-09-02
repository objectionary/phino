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
import Data.IORef
import Data.List (intercalate)
import Data.Maybe
import Deps (SaveEvalFunc, SaveStepFunc, dontSaveEval, saveEval, saveStep)
import Encoding
import Files (ensuredFile)
import Functions (execFunctions)
import LaTeX (LatexContext (LatexContext), defaultMeetLength, defaultMeetPopularity, expressionToLaTeX, rewrittensToLatex)
import Lining (LineFormat (SINGLELINE))
import Locator (locatedExpression)
import Logger
import Parser (parseExpressionThrows)
import qualified Printer as P
import qualified Random as R
import Rewriter (Rewritten, Rewrittens', stepHeaders)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (Handle, IOMode (WriteMode), getContents', hClose, hSetEncoding, openFile, utf8)
import Text.Printf (printf)
import XMIR (expressionToXMIR, parseXMIRThrows, printXMIR, xmirToPhi)
import Yaml (normalizationRules)
import qualified Yaml as Y

justMeetPopularity :: Maybe Int -> Int
justMeetPopularity = fromMaybe defaultMeetPopularity

justMeetLength :: Maybe Int -> Int
justMeetLength = fromMaybe defaultMeetLength

-- Prepare saveStepFunc
saveStepFunc :: Maybe FilePath -> PrintContext -> IO SaveStepFunc
saveStepFunc stepsDir ctx@PrintCtx{..} = do
  counter <- newIORef (0 :: Int)
  let ioToExt :: String
      ioToExt
        | _outputFormat == LATEX = "tex"
        | otherwise = show _outputFormat
      render = printInFormat ctx
      save :: SaveStepFunc
      save expr = do
        step <- atomicModifyIORef' counter (\value -> (value + 1, value + 1))
        saveStep stepsDir ioToExt render step expr
  pure save

-- Run the action with a function recording atom firings, holding the protocol
-- file open for the whole run. Opening it for writing truncates it, so that it
-- always holds the firings of exactly one run: a caller reading it back never
-- picks up records left over from the previous run, even when this run fires no
-- atom at all. The handle is closed on the way out, failure included, so the
-- last records reach the disk even when dataization gives up. Every record is
-- flattened into a single line, whatever '--flat' says about the main output,
-- since the file is a line-per-firing protocol. The encoding is pinned to UTF-8
-- rather than taken from the locale, since the file is read back by other
-- programs.
withEvalFunc :: Maybe FilePath -> PrintContext -> (SaveEvalFunc -> IO a) -> IO a
withEvalFunc Nothing _ action = action dontSaveEval
withEvalFunc (Just file) ctx action = do
  createDirectoryIfMissing True (takeDirectory file)
  logDebug (printf "The option '--evaluations' is specified, atom firings will be recorded in '%s'" file)
  bracket opened hClose $ \protocol ->
    action (saveEval protocol (printExpression ctx{_line = SINGLELINE}))
  where
    -- 'withFile' would do the same, except that it annotates whatever the action
    -- throws with the name of the file, and a dataization failure has to reach
    -- the user as it is
    opened :: IO Handle
    opened = do
      protocol <- openFile file WriteMode
      hSetEncoding protocol utf8
      pure protocol

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
  | otherwise = withHeaders <$> mapM (printFocused ctx . fst) (canonized chain)
  where
    canonized :: [Rewritten] -> [Rewritten]
    canonized = if _canonize then canonize else id
    -- Prefix every step with an empty line and its header (see 'stepHeaders')
    -- when '--headers' is on. Headers, like the other intermediate-output
    -- flags, are meaningful only together with '--sequence'. Node counts come
    -- from the original 'chain', not the canonized one, since canonization
    -- only renames functions and never changes the AST size.
    withHeaders :: [String] -> String
    withHeaders rendered
      | _headers && _sequence = intercalate "\n" (zipWith prefixed (stepHeaders chain) rendered)
      | otherwise = intercalate "\n" rendered
      where
        prefixed :: String -> String -> String
        prefixed = printf "\n%s\n%s"

-- Render one expression in the output format, narrowed to the '--focus'
-- sub-expression when one is given.
printFocused :: PrintContext -> Expression -> IO String
printFocused ctx@PrintCtx{..} expr
  | _focus == ExRoot = printInFormat ctx expr
  | otherwise = locatedExpression _focus expr >>= printExpression ctx

printExpression :: PrintContext -> Expression -> IO String
printExpression ctx@PrintCtx{..} ex = case _outputFormat of
  PHI -> pure (printPhi ctx ex)
  XMIR -> throwIO CouldNotPrintExpressionInXMIR
  LATEX -> pure (expressionToLaTeX ex (printCtxToLatexCtx ctx))

-- Convert an expression to its corresponding String format
printInFormat :: PrintContext -> Expression -> IO String
printInFormat ctx@PrintCtx{..} expr = case _outputFormat of
  PHI -> pure (printPhi ctx expr)
  XMIR -> expressionToXMIR expr _xmirCtx <&> printXMIR
  LATEX -> pure (expressionToLaTeX expr (printCtxToLatexCtx ctx))

-- Render an expression as PHI, dropping every ρ binding when '--hide-rho' is set.
printPhi :: PrintContext -> Expression -> String
printPhi PrintCtx{..} expr =
  (if _hideRho then P.printExpressionHidingRho' else P.printExpression') expr (_sugar, UNICODE, _line, _margin)

printCtxToLatexCtx :: PrintContext -> LatexContext
printCtxToLatexCtx PrintCtx{..} =
  LatexContext _sugar _line _margin _nonumber _compress _canonize _meetPopularity _meetLength _focus _expression _label _meetPrefix _headers

-- Get rules for rewriting depending on provided flags. Both flags may be given
-- together, in which case the user rules follow the built-in ones
getRules :: Bool -> Bool -> [FilePath] -> IO [Y.Rule]
getRules normalize shuffle rules = do
  ordered <- (++) <$> builtin <*> custom
  if shuffle
    then do
      logDebug "The --shuffle option is provided, rules are used in random order"
      R.shuffle ordered
    else pure ordered
  where
    builtin :: IO [Y.Rule]
    builtin
      | normalize = do
          logDebug (printf "The --normalize option is provided, %d built-it normalization rules are used" (length normalizationRules))
          pure normalizationRules
      | otherwise = pure []
    custom :: IO [Y.Rule]
    custom
      | null rules = do
          logDebug "No --rule option is provided, no user rules are used"
          pure []
      | otherwise = do
          logDebug (printf "Using rules from files: [%s]" (intercalate ", " rules))
          yamls <- mapM ensuredFile rules
          mapM (Y.yamlRule >=> validateRewriteRule) yamls

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
