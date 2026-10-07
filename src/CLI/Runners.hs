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
import Control.Concurrent (rtsSupportsBoundThreads, setNumCapabilities)
import Control.Exception
import Control.Monad (unless, when)
import Data.Foldable (traverse_)
import Data.IORef (newIORef)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T
import Dataize
import Deps (Judgment (..), dontSaveMade)
import Emit (emitted)
import Encoding
import Engine (Engine (..), building, current, stepOf)
import Evaluate (evaluation, fired)
import Files (overwrite)
import qualified Filter as F
import LaTeX (explainContextualizeRules, explainDataizeRules, explainMorphRules, explainRules)
import Logger
import Margin (defaultMargin)
import Merge (merge)
import Morph
import Parser (parseExpressionThrows)
import qualified Printer as P
import qualified Random as R
import Rewriter
import Rule (RuleContext (..), matchExpressionWithRule)
import Slots (anonymous)
import System.Directory (doesFileExist, getModificationTime)
import System.Exit (exitSuccess)
import System.Random (mkStdGen, setStdGen)
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
  validateNoOverlap "show" included "hide" excluded
  setStdGen (mkStdGen _seed)
  rules <- getRules _normalize _shuffle _rules
  linked <- engine
  validateBreakpoint _breakpoint rules
  input <- readInput _inputFile
  (expr, atoms) <- parseInputWithAtoms input _inputFormat
  validateXmirTopLevel _outputFormat expr
  seedTaus expr
  logDebug (printf "Amount of rewriting cycles across all the rules: %d, per rule: %d" _maxCycles _maxDepth)
  let listing = case (rules, _inputFormat) of
        ([], PHI) -> (\_ -> escapeXMLText input)
        (_, _) -> (\rewritten -> escapeXMLText (P.printExpression' rewritten (_sugarType, UNICODE, _flat, _margin)))
      xmirCtx = XmirContext _omitListing _omitComments _hideRho listing atoms
      printCtx = toPrintCtx xmirCtx foc
      exclude = (`F.exclude` excluded)
      include = (`F.include` included)
  save <- saveStepFunc _stepsDir printCtx included excluded
  let steps = map (stepOf linked) rules
  (rewrittens, exceeded) <- rewrite expr steps (RewriteContext loc _maxDepth _maxCycles _depthSensitive Nothing (building linked) linked._normal (every steps) _must _breakpoint save dontSaveMade)
  rewrittens' <- include (if _sequence then NE.toList rewrittens else [NE.last rewrittens]) >>= exclude
  logDebug (printf "Printing rewritten 𝜑-expression as %s" (show _outputFormat))
  exprs <- printRewrittens printCtx (rewrittens', exceeded)
  output _targetFile exprs
  where
    validateOpts :: IO ()
    validateOpts = do
      when (_inPlace && isNothing _inputFile) (invalidCLIArguments "The option --in-place requires an input file")
      when (_inPlace && isJust _targetFile) (invalidCLIArguments "The options --in-place and --target cannot be used together")
      when (_inPlace && _outputFormat /= PHI) (invalidCLIArguments "The option --in-place can only be used together with --output=phi")
      when (_inPlace && _sequence) (invalidCLIArguments "The options --in-place and --sequence cannot be used together, since the file must keep one program")
      when (_inPlace && _focus /= "Q") (invalidCLIArguments "The options --in-place and --focus cannot be used together, since the file must keep the whole program")
      when (_inPlace && not (null _show)) (invalidCLIArguments "The options --in-place and --show cannot be used together, since the file must keep the whole program")
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
        overwrite file expr
        logDebug (printf "The file '%s' was modified in-place" file)
      (True, _, Nothing) ->
        error "The option --in-place requires an input file"
      (False, Just file, _) -> do
        logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
        overwrite file expr
        logDebug (printf "The command result was saved in '%s'" file)
      (False, Nothing, _) -> do
        logDebug "The option '--target' is not specified, printing to console..."
        putStrLn expr
    toPrintCtx :: XmirContext -> Expression -> PrintContext
    toPrintCtx xmirCtx focus =
      PrintCtx
        _sugarType
        _hideRho
        Nothing
        False
        _flat
        _margin
        xmirCtx
        _nonumber
        _compress
        _canonize
        _sequence
        _headers
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
  deadline <- timed _maxSeconds
  lambdas <- lambdasOf _symbolic
  excluded <- validatedDispatches "hide" _hide
  included <- validatedDispatches "show" _show
  [loc] <- validatedDispatches "locator" [_locator]
  [foc] <- validatedDispatches "focus" [_focus]
  validateNoOverlap "show" included "hide" excluded
  input <- readInput _inputFile
  (expr, atoms) <- parseInputWithAtoms input _inputFormat
  setStdGen (mkStdGen _seed)
  seedTaus expr
  let printCtx = toPrintCtx atoms foc
      exclude = (`F.exclude` excluded)
      include = (`F.include` included)
  save <- saveStepFunc _stepsDir printCtx included excluded
  tally <- tallied _maxFirings
  minted <- newIORef 0
  memo <- memoized _acyclic
  linked <- engine
  (outcome, chain, _) <-
    withEvalFunc
      _protocol
      printCtx
      ( \record -> do
          let ctx = ReduceContext loc loc Nothing _maxDepth _maxCycles (Steps _maxSteps 0) tally minted deadline memo 1 (Just (1, loc)) _depthSensitive _shuffle _partial False 1 _acyclic Dataization [] Map.empty lambdas (building linked) reduction evaluation fired save record linked
          (universe, aiming) <- aimed printCtx Dataization _inside expr ctx
          started universe aiming
          dataize universe emptyState aiming
      )
  when _sequence (include chain >>= exclude >>= \shown -> printRewrittens printCtx (shown, False) >>= putStrLn)
  unless _quiet (printOutcome printCtx (\residue -> F.include' residue included >>= (`F.exclude'` excluded)) outcome >>= putStrLn)
  where
    printOutcome :: PrintContext -> (Expression -> IO Expression) -> Outcome -> IO String
    printOutcome _ _ (Dataized bytes) = pure (P.printBytes bytes)
    printOutcome ctx narrowed (Residual residue) = do
      logDebug "Dataization got stuck on a λ function that cannot fire, printing the residual program (--partial)"
      answer <- narrowed residue
      validateXmirTopLevel _outputFormat answer
      printAnswer ctx answer
    validateOpts :: IO ()
    validateOpts = do
      validateLatexOptions
        _outputFormat
        [(_nonumber, "nonumber"), (_compress, "compress")]
        [(_expression, "expression"), (_label, "label"), (_meetPrefix, "meet-prefix")]
        [(_meetPopularity, "meet-popularity"), (_meetLength, "meet-length")]
      validateXmirOptions _outputFormat [(_omitListing, "omit-listing"), (_omitComments, "omit-comments")] _focus
      when (length _show > 1) (invalidCLIArguments "The option --show can be used only once")
      when (isJust _abridged && isNothing _protocol) (invalidCLIArguments "The option --abridged requires --protocol, since only the protocol is abridged")
      when (_abridgedData && isNothing _abridged) (invalidCLIArguments "The option --abridged-data requires --abridged, since only an abridged protocol cuts its data")
      when
        (isJust _inside && _locator /= "Q")
        (invalidCLIArguments "The options --inside and --locator cannot be used together, since --inside aims the run at the binding it mints")
    toPrintCtx :: Atoms -> Expression -> PrintContext
    toPrintCtx atoms focus =
      PrintCtx
        _sugarType
        _hideRho
        _abridged
        _abridgedData
        _flat
        _margin
        (XmirContext _omitListing _omitComments _hideRho listing atoms)
        _nonumber
        _compress
        _canonize
        _sequence
        _headers
        (justMeetPopularity _meetPopularity)
        (justMeetLength _meetLength)
        focus
        _expression
        _label
        _meetPrefix
        _outputFormat
    listing :: Expression -> String
    listing e = escapeXMLText (P.printExpression' e (_sugarType, UNICODE, _flat, _margin))

runMorph :: OptsMorph -> IO ()
runMorph OptsMorph{..} = do
  validateOpts
  deadline <- timed _maxSeconds
  when rtsSupportsBoundThreads (setNumCapabilities _jobs)
  lambdas <- lambdasOf _symbolic
  excluded <- validatedDispatches "hide" _hide
  included <- validatedDispatches "show" _show
  [loc] <- validatedDispatches "locator" [_locator]
  [foc] <- validatedDispatches "focus" [_focus]
  validateNoOverlap "show" included "hide" excluded
  input <- readInput _inputFile
  (expr, atoms) <- parseInputWithAtoms input _inputFormat
  setStdGen (mkStdGen _seed)
  seedTaus expr
  let printCtx = toPrintCtx atoms foc
      exclude = (`F.exclude` excluded)
      include = (`F.include` included)
  save <- saveStepFunc _stepsDir printCtx included excluded
  tally <- tallied _maxFirings
  minted <- newIORef 0
  memo <- memoized _acyclic
  linked <- engine
  (morphed, chain, _) <-
    withEvalFunc
      _protocol
      printCtx
      ( \record -> do
          let ctx = ReduceContext loc loc Nothing _maxDepth _maxCycles (Steps _maxSteps 0) tally minted deadline memo 1 Nothing _depthSensitive _shuffle _partial _deep _jobs _acyclic Morphing [] Map.empty lambdas (building linked) reduction evaluation fired save record linked
          (universe, aiming) <- aimed printCtx Morphing _inside expr ctx
          started universe aiming
          morph universe emptyState aiming
      )
  printed <-
    if _quiet
      then pure Nothing
      else do
        answer <- F.include' (if foc == ExRoot then morphed else maybe morphed fst (lastMaybe chain)) included >>= (`F.exclude'` excluded)
        validateXmirTopLevel _outputFormat answer
        Just <$> printAnswer printCtx answer
  when _sequence (include chain >>= exclude >>= \shown -> printRewrittens printCtx (shown, False) >>= putStrLn)
  mapM_ putStrLn printed
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe items = Just (last items)
    validateOpts :: IO ()
    validateOpts = do
      validateLatexOptions
        _outputFormat
        [(_nonumber, "nonumber"), (_compress, "compress")]
        [(_expression, "expression"), (_label, "label"), (_meetPrefix, "meet-prefix")]
        [(_meetPopularity, "meet-popularity"), (_meetLength, "meet-length")]
      validateXmirOptions _outputFormat [(_omitListing, "omit-listing"), (_omitComments, "omit-comments")] _focus
      when (length _show > 1) (invalidCLIArguments "The option --show can be used only once")
      when (isJust _abridged && isNothing _protocol) (invalidCLIArguments "The option --abridged requires --protocol, since only the protocol is abridged")
      when (_abridgedData && isNothing _abridged) (invalidCLIArguments "The option --abridged-data requires --abridged, since only an abridged protocol cuts its data")
      when (_jobs > 1 && not _deep) (invalidCLIArguments "The option --jobs requires --deep, since only the deep walk runs on several workers")
      when
        (isJust _inside && _locator /= "Q")
        (invalidCLIArguments "The options --inside and --locator cannot be used together, since --inside aims the run at the binding it mints")
    toPrintCtx :: Atoms -> Expression -> PrintContext
    toPrintCtx atoms focus =
      PrintCtx
        _sugarType
        _hideRho
        _abridged
        _abridgedData
        _flat
        _margin
        (XmirContext _omitListing _omitComments _hideRho listing atoms)
        _nonumber
        _compress
        _canonize
        _sequence
        _headers
        (justMeetPopularity _meetPopularity)
        (justMeetLength _meetLength)
        focus
        _expression
        _label
        _meetPrefix
        _outputFormat
    listing :: Expression -> String
    listing e = escapeXMLText (P.printExpression' e (_sugarType, UNICODE, _flat, _margin))

runExplain :: OptsExplain -> IO ()
runExplain OptsExplain{..} = do
  setStdGen (mkStdGen _seed)
  validateOpts
  explained >>= printOut _targetFile
  where
    explained :: IO String
    explained
      | _morph = explainMorphRules <$> shuffled Y.morphingRules
      | _dataize = explainDataizeRules <$> shuffled Y.dataizationRules
      | _contextualize = explainContextualizeRules <$> shuffled Y.contextualizationRules
      | otherwise = explainRules <$> getRules _normalize _shuffle _rules
    shuffled :: [a] -> IO [a]
    shuffled xs
      | _shuffle = R.shuffle xs
      | otherwise = pure xs
    validateOpts :: IO ()
    validateOpts = do
      let selected = length (filter id [_morph, _dataize, _contextualize])
      when (selected == 0 && null _rules && not _normalize) (invalidCLIArguments "Either --rule, --normalize, --morph, --dataize or --contextualize must be specified")
      when (selected > 1) (invalidCLIArguments "Only one of --morph, --dataize or --contextualize can be specified")
      when (selected == 1 && not (null _rules)) (invalidCLIArguments "The --rule option cannot be used together with --morph, --dataize or --contextualize")
      when (selected == 1 && _normalize) (invalidCLIArguments "The --normalize option cannot be used together with --morph, --dataize or --contextualize")

runMerge :: OptsMerge -> IO ()
runMerge OptsMerge{..} = do
  validateOpts
  inputs' <- traverse (readInput . Just) _inputs
  setStdGen (mkStdGen _seed)
  (exprs, atoms) <- unzip <$> traverse (`parseInputWithAtoms` _inputFormat) inputs'
  expr <- merge exprs
  validateXmirTopLevel _outputFormat expr
  let listing = const (escapeXMLText (P.printExpression' expr (_sugarType, UNICODE, _flat, _margin)))
      xmirCtx = XmirContext _omitListing _omitComments False listing (Map.unions atoms)
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
        False
        Nothing
        False
        _flat
        _margin
        xmirCtx
        False
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
  when (isJust _when && isNothing _pattern) (invalidCLIArguments "The option --when requires --pattern, since there is nothing to check it against")
  setStdGen (mkStdGen _seed)
  input <- readInput _inputFile
  expr <- parseInput input PHI
  if isNothing _pattern
    then logDebug "The --pattern is not provided, no substitutions are built"
    else do
      ptn <- parseExpressionThrows (fromJust _pattern)
      condition <- traverse parseConditionThrows _when
      traverse_ (throwIO . AnonymousMetaInCondition . T.unpack) (anonymous condition)
      linked <- engine
      substs <- matchExpressionWithRule expr (rule ptn condition) (RuleContext (building linked) Nothing linked._normal)
      if null substs
        then throwIO EmptySubstsOnMatch
        else putStrLn (P.printSubsts' substs (_sugarType, UNICODE, _flat, defaultMargin))
  where
    rule :: Expression -> Maybe Y.Condition -> Y.Rule
    rule ptn cnd = Y.Rule "custom" Nothing Nothing ptn ExRoot cnd Nothing Nothing

runCompile :: OptsCompile -> IO ()
runCompile OptsCompile{..} = do
  custom <- getRules False False _rules
  source <- either (throwIO . CouldNotCompile) pure (emitted Y.normalizationRules custom Y.contextualizationRules Y.morphingRules Y.dataizationRules current)
  overwrite _targetFile source
  logInfo (printf "The rules were compiled into '%s'" _targetFile)
  exists <- doesFileExist "cabal.project.local"
  if exists
    then putStrLn "The file 'cabal.project.local' exists, so add these lines to it to link the compiled rules in:\npackage phino\n  flags: +compiled"
    else do
      overwrite "cabal.project.local" "package phino\n  flags: +compiled\n"
      logInfo "The file 'cabal.project.local' was written, so the next build links the compiled rules in"
