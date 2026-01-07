{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI.Runners where

import AST
import CLI.Helpers
import CLI.Types
import CLI.Validators
import qualified Canonizer as C
import Condition (parseConditionThrows)
import Control.Exception
import Control.Monad (unless, when)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, isJust, isNothing)
import Dataize
import Encoding
import qualified Filter as F
import Functions (buildTerm)
import LaTeX (explainRules)
import Logger
import Margin (defaultMargin)
import Merge (merge)
import Parser (parseExpressionThrows)
import qualified Printer as P
import Rewriter
import Rule (RuleContext (..), matchProgramWithRule)
import Text.Printf (printf)
import XMIR
import qualified Yaml as Y

runRewrite :: OptsRewrite -> IO ()
runRewrite OptsRewrite{..} = do
  validateOpts
  excluded <- validatedDispatches "hide" _hide
  included <- validatedDispatches "show" _show
  [loc] <- validatedDispatches "locator" [_locator]
  [foc] <- validatedDispatches "focus" [_focus]
  logDebug (printf "Amount of rewriting cycles across all the rules: %d, per rule: %d" _maxCycles _maxDepth)
  input <- readInput _inputFile
  rules <- getRules _normalize _shuffle _rules
  program <- parseProgram input _inputFormat
  let listing = if null rules then const input else (\prog -> P.printProgram' prog (_sugarType, UNICODE, _flat, _margin))
      xmirCtx = XmirContext _omitListing _omitComments listing
      printCtx = printProgCtx xmirCtx foc
      canonize = if _canonize then C.canonize else id
      exclude = (`F.exclude` excluded)
      include = (`F.include` included)
  rewrittens <- rewrite program rules (context loc printCtx) <&> canonize . exclude . include
  let rewrittens' = if _sequence then rewrittens else [last rewrittens]
  logDebug (printf "Printing rewritten 𝜑-program as %s" (show _outputFormat))
  progs <- printRewrittens printCtx rewrittens'
  output _targetFile progs
  where
    validateOpts :: IO ()
    validateOpts = do
      when (_inPlace && isNothing _inputFile) (invalidCLIArguments "The option --in-place requires an input file")
      when (_inPlace && isJust _targetFile) (invalidCLIArguments "The options --in-place and --target cannot be used together")
      when (length _show > 1) (invalidCLIArguments "The option --show can be used only once")
      validateLatexOptions
        _outputFormat
        [(_nonumber, "nonumber"), (_compress, "compress")]
        [(_expression, "expression"), (_label, "label"), (_meetPrefix, "meet-prefix")]
        [(_meetPopularity, "meet-popularity"), (_meetLength, "meet-length")]
      validateMust' _must
      validateXmirOptions _outputFormat [(_omitListing, "omit-listing"), (_omitComments, "omit-comments")] _focus
    output :: Maybe FilePath -> String -> IO ()
    output target prog = case (_inPlace, target, _inputFile) of
      (True, _, Just file) -> do
        logDebug (printf "The option '--in-place' is specified, writing back to '%s'..." file)
        writeFile file prog
        logDebug (printf "The file '%s' was modified in-place" file)
      (True, _, Nothing) ->
        error "The option --in-place requires an input file"
      (False, Just file, _) -> do
        logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
        writeFile file prog
        logDebug (printf "The command result was saved in '%s'" file)
      (False, Nothing, _) -> do
        logDebug "The option '--target' is not specified, printing to console..."
        putStrLn prog
    context :: Expression -> PrintProgramContext -> RewriteContext
    context loc ctx = RewriteContext loc _maxDepth _maxCycles _depthSensitive buildTerm _must (saveStepFunc _stepsDir ctx)
    printProgCtx :: XmirContext -> Expression -> PrintProgramContext
    printProgCtx xmirCtx focus =
      PrintProgCtx
        _sugarType
        _flat
        _margin
        xmirCtx
        _nonumber
        _compress
        _sequence
        (justMeetPopularity _meetPopularity)
        (justMeetLength _meetLength)
        focus
        _expression
        _label
        _meetPrefix
        _outputFormat

runDataize :: OptsDataize -> IO ()
runDataize OptsDataize{..} = do
  validateOpts
  excluded <- validatedDispatches "hide" _hide
  included <- validatedDispatches "show" _show
  [loc] <- validatedDispatches "locator" [_locator]
  [foc] <- validatedDispatches "focus" [_focus]
  input <- readInput _inputFile
  prog <- parseProgram input _inputFormat
  let printCtx = printProgCtx foc
      canonize = if _canonize then C.canonize else id
      exclude = (`F.exclude` excluded)
      include = (`F.include` included)
  (maybeBytes, chain) <- dataize (context loc prog printCtx)
  when _sequence (printRewrittens printCtx (canonize $ exclude $ include chain) >>= putStrLn)
  unless _quiet (putStrLn (maybe (P.printExpression ExTermination) P.printBytes maybeBytes))
  where
    validateOpts :: IO ()
    validateOpts = do
      validateLatexOptions
        _outputFormat
        [(_nonumber, "nonumber"), (_compress, "compress")]
        [(_expression, "expression"), (_label, "label"), (_meetPrefix, "meet-prefix")]
        [(_meetPopularity, "meet-popularity"), (_meetLength, "meet-length")]
      validateXmirOptions _outputFormat [(_omitListing, "omit-listing"), (_omitComments, "omit-comments")] _focus
      when (length _show > 1) (invalidCLIArguments "The option --show can be used only once")
    context :: Expression -> Program -> PrintProgramContext -> DataizeContext
    context loc prog ctx = DataizeContext loc prog _maxDepth _maxCycles _depthSensitive buildTerm (saveStepFunc _stepsDir ctx)
    printProgCtx :: Expression -> PrintProgramContext
    printProgCtx focus =
      PrintProgCtx
        _sugarType
        _flat
        _margin
        defaultXmirContext
        _nonumber
        _compress
        _sequence
        (justMeetPopularity _meetPopularity)
        (justMeetLength _meetLength)
        focus
        _expression
        _label
        _meetPrefix
        _outputFormat

runExplain :: OptsExplain -> IO ()
runExplain OptsExplain{..} = do
  validateOpts
  rules <- getRules _normalize _shuffle _rules
  printOut _targetFile (explainRules rules)
  where
    validateOpts :: IO ()
    validateOpts = when (null _rules && not _normalize) (invalidCLIArguments "Either --rule or --normalize must be specified")

runMerge :: OptsMerge -> IO ()
runMerge OptsMerge{..} = do
  validateOpts
  inputs' <- traverse (readInput . Just) _inputs
  progs <- traverse (`parseProgram` _inputFormat) inputs'
  prog <- merge progs
  let listing = const (P.printProgram' prog (_sugarType, UNICODE, _flat, _margin))
      xmirCtx = XmirContext _omitListing _omitComments listing
      printCtx = printProgCtx xmirCtx
  prog' <- printProgram printCtx prog
  printOut _targetFile prog'
  where
    validateOpts :: IO ()
    validateOpts = do
      when (null _inputs) (throwIO (InvalidCLIArguments "At least one input file must be specified for 'merge' command"))
      validateXmirOptions _outputFormat [(_omitListing, "omit-listing"), (_omitComments, "omit-comments")] "Q"
    printProgCtx :: XmirContext -> PrintProgramContext
    printProgCtx xmirCtx =
      PrintProgCtx
        _sugarType
        _flat
        _margin
        xmirCtx
        False
        False
        False
        (justMeetPopularity Nothing)
        (justMeetLength Nothing)
        ExGlobal
        Nothing
        Nothing
        Nothing
        _outputFormat

runMatch :: OptsMatch -> IO ()
runMatch OptsMatch{..} = do
  input <- readInput _inputFile
  prog <- parseProgram input PHI
  if isNothing _pattern
    then logDebug "The --pattern is not provided, no substitutions are built"
    else do
      ptn <- parseExpressionThrows (fromJust _pattern)
      condition <- traverse parseConditionThrows _when
      substs <- matchProgramWithRule prog (rule ptn condition) (RuleContext buildTerm)
      if null substs
        then logDebug "Provided pattern was not matched, no substitutions are built"
        else putStrLn (P.printSubsts' substs (_sugarType, UNICODE, _flat, defaultMargin))
  where
    rule :: Expression -> Maybe Y.Condition -> Y.Rule
    rule ptn cnd = Y.Rule Nothing Nothing ptn ExGlobal cnd Nothing Nothing
