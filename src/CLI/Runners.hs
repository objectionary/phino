{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI.Runners where

import AST
import CLI.Helpers
import CLI.Types
import CLI.Validators
import Condition (parseConditionThrows)
import Control.Exception
import Control.Monad (unless, when)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isJust, isNothing)
import Dataize
import Encoding
import qualified Filter as F
import Functions (buildTerm)
import LaTeX (explainContextualizeRules, explainDataizeRules, explainMorphRules, explainRules)
import Logger
import Margin (defaultMargin)
import Merge (merge)
import Parser (parseExpressionThrows)
import qualified Printer as P
import Rewriter
import Rule (RuleContext (..), matchExpressionWithRule)
import System.Directory (doesFileExist, getModificationTime)
import System.Exit (exitSuccess)
import Tau (seedTaus)
import Text.Printf (printf)
import XMIR
import qualified Yaml as Y

runRewrite :: OptsRewrite -> IO ()
runRewrite OptsRewrite{..} = do
  validateOpts
  checkUpdate
  excluded <- validatedDispatches "hide" _hide
  included <- validatedDispatches "show" _show
  [loc] <- validatedDispatches "locator" [_locator]
  [foc] <- validatedDispatches "focus" [_focus]
  rules <- getRules _normalize _shuffle _rules
  validateBreakpoint _breakpoint rules
  input <- readInput _inputFile
  expr <- parseInput input _inputFormat
  seedTaus expr
  logDebug (printf "Amount of rewriting cycles across all the rules: %d, per rule: %d" _maxCycles _maxDepth)
  let listing = case (rules, _inputFormat, _outputFormat) of
        ([], XMIR, XMIR) -> (\_ -> escapeXML input)
        ([], _, _) -> const input
        (_, _, _) -> (\rewritten -> P.printExpression' rewritten (_sugarType, UNICODE, _flat, _margin))
      xmirCtx = XmirContext _omitListing _omitComments listing
      printCtx = toPrintCtx xmirCtx foc
      exclude = (`F.exclude` excluded)
      include = (`F.include` included)
  (rewrittens, exceeded) <- rewrite expr rules (context loc printCtx)
  let rewrittens' = exclude $ include (if _sequence then NE.toList rewrittens else [NE.last rewrittens])
  logDebug (printf "Printing rewritten 𝜑-expression as %s" (show _outputFormat))
  exprs <- printRewrittens printCtx (rewrittens', exceeded)
  output _targetFile exprs
  where
    validateOpts :: IO ()
    validateOpts = do
      when (_inPlace && isNothing _inputFile) (invalidCLIArguments "The option --in-place requires an input file")
      when (_inPlace && isJust _targetFile) (invalidCLIArguments "The options --in-place and --target cannot be used together")
      when (_update && _inPlace) (invalidCLIArguments "The options --update and --in-place cannot be used together")
      when (_update && isNothing _targetFile) (invalidCLIArguments "The option --update requires --target")
      when (_update && isNothing _inputFile) (invalidCLIArguments "The option --update requires an input file")
      when (length _show > 1) (invalidCLIArguments "The option --show can be used only once")
      validateLatexOptions
        _outputFormat
        [(_nonumber, "nonumber"), (_compress, "compress")]
        [(_expression, "expression"), (_label, "label"), (_meetPrefix, "meet-prefix")]
        [(_meetPopularity, "meet-popularity"), (_meetLength, "meet-length")]
      validateMust' _must
      validateXmirOptions _outputFormat [(_omitListing, "omit-listing"), (_omitComments, "omit-comments")] _focus
    checkUpdate :: IO ()
    checkUpdate = case (_update, _inputFile, _targetFile) of
      (True, Just src, Just tgt) -> do
        exists <- doesFileExist tgt
        when exists $ do
          newer <- (>) <$> getModificationTime tgt <*> getModificationTime src
          when newer $ do
            logDebug (printf "Target '%s' is newer than source '%s', skipping rewriting (--update)" tgt src)
            exitSuccess
      _ -> pure ()
    validateBreakpoint :: Maybe String -> [Y.Rule] -> IO ()
    validateBreakpoint Nothing _ = pure ()
    validateBreakpoint (Just rule) rules =
      let names = map (.name) rules
       in unless
            (rule `elem` names)
            (invalidCLIArguments (printf "The rule '%s' provided in '--breakpoint' option is absent across given rewriting rules: %s" rule (intercalate ", " names)))
    output :: Maybe FilePath -> String -> IO ()
    output target expr = case (_inPlace, target, _inputFile) of
      (True, _, Just file) -> do
        logDebug (printf "The option '--in-place' is specified, writing back to '%s'..." file)
        writeFile file expr
        logDebug (printf "The file '%s' was modified in-place" file)
      (True, _, Nothing) ->
        error "The option --in-place requires an input file"
      (False, Just file, _) -> do
        logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
        writeFile file expr
        logDebug (printf "The command result was saved in '%s'" file)
      (False, Nothing, _) -> do
        logDebug "The option '--target' is not specified, printing to console..."
        putStrLn expr
    context :: Expression -> PrintContext -> RewriteContext
    context loc ctx = RewriteContext loc _maxDepth _maxCycles _depthSensitive buildTerm _must _breakpoint (saveStepFunc _stepsDir ctx)
    toPrintCtx :: XmirContext -> Expression -> PrintContext
    toPrintCtx xmirCtx focus =
      PrintCtx
        _sugarType
        _flat
        _margin
        xmirCtx
        _nonumber
        _compress
        _canonize
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
  expr <- parseInput input _inputFormat
  seedTaus expr
  let printCtx = toPrintCtx foc
      exclude = (`F.exclude` excluded)
      include = (`F.include` included)
  (bytes, chain) <- dataize expr (context loc printCtx)
  when _sequence (printRewrittens printCtx (exclude $ include chain, False) >>= putStrLn)
  unless _quiet (putStrLn (P.printBytes bytes))
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
    context :: Expression -> PrintContext -> DataizeContext
    context loc ctx = DataizeContext loc _maxDepth _maxCycles _depthSensitive _shuffle buildTerm (saveStepFunc _stepsDir ctx)
    toPrintCtx :: Expression -> PrintContext
    toPrintCtx focus =
      PrintCtx
        _sugarType
        _flat
        _margin
        defaultXmirContext
        _nonumber
        _compress
        _canonize
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
  explained >>= printOut _targetFile
  where
    explained :: IO String
    explained
      | _morph = pure (explainMorphRules Y.morphingRules)
      | _dataize = pure (explainDataizeRules Y.dataizationRules)
      | _contextualize = pure (explainContextualizeRules Y.contextualizationRules)
      | otherwise = explainRules <$> getRules _normalize _shuffle _rules
    validateOpts :: IO ()
    validateOpts = do
      let selected = length (filter id [not (null _rules), _normalize, _morph, _dataize, _contextualize])
      when (selected == 0) (invalidCLIArguments "Either --rule, --normalize, --morph, --dataize or --contextualize must be specified")
      when (selected > 1) (invalidCLIArguments "Only one of --rule, --normalize, --morph, --dataize or --contextualize can be specified")

runMerge :: OptsMerge -> IO ()
runMerge OptsMerge{..} = do
  validateOpts
  inputs' <- traverse (readInput . Just) _inputs
  exprs <- traverse (`parseInput` _inputFormat) inputs'
  expr <- merge exprs
  let listing = const (P.printExpression' expr (_sugarType, UNICODE, _flat, _margin))
      xmirCtx = XmirContext _omitListing _omitComments listing
      printCtx = toPrintCtx xmirCtx
  expr' <- printInFormat printCtx expr
  printOut _targetFile expr'
  where
    validateOpts :: IO ()
    validateOpts = do
      when (null _inputs) (throwIO (InvalidCLIArguments "At least one input file must be specified for 'merge' command"))
      validateXmirOptions _outputFormat [(_omitListing, "omit-listing"), (_omitComments, "omit-comments")] "Q"
    toPrintCtx :: XmirContext -> PrintContext
    toPrintCtx xmirCtx =
      PrintCtx
        _sugarType
        _flat
        _margin
        xmirCtx
        False
        False
        False
        False
        (justMeetPopularity Nothing)
        (justMeetLength Nothing)
        ExRoot
        Nothing
        Nothing
        Nothing
        _outputFormat

runMatch :: OptsMatch -> IO ()
runMatch OptsMatch{..} = do
  input <- readInput _inputFile
  expr <- parseInput input PHI
  seedTaus expr
  if isNothing _pattern
    then logDebug "The --pattern is not provided, no substitutions are built"
    else do
      ptn <- parseExpressionThrows (fromJust _pattern)
      condition <- traverse parseConditionThrows _when
      substs <- matchExpressionWithRule expr (rule ptn condition) (RuleContext buildTerm)
      if null substs
        then throwIO EmptySubstsOnMatch
        else putStrLn (P.printSubsts' substs (_sugarType, UNICODE, _flat, defaultMargin))
  where
    rule :: Expression -> Maybe Y.Condition -> Y.Rule
    rule ptn cnd = Y.Rule "custom" Nothing Nothing ptn ExRoot cnd Nothing Nothing
