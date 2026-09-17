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
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.IORef
import Data.List (intercalate, nub)
import Data.Maybe
import qualified Data.Text as T
import Deps (Evaluation (EvRun), Judgment, SaveEvalFunc, SaveStepFunc, State (..), dontSaveEval, emptyNesting, emptyProtocol, endEvalXml, saveEval, saveEvalXml, saveStep)
import Encoding
import Files (ensuredFile, overwrite)
import Functions (execFunctions)
import LaTeX (LatexContext (LatexContext), defaultMeetLength, defaultMeetPopularity, expressionToLaTeX, rewrittensToLatex)
import Lambdas (Lambdas, emptyLambdas, readLambdas, taken)
import Lining (LineFormat (SINGLELINE))
import Locator (locatedExpression)
import Logger
import Morph (ReduceContext, emptyState, insideUniverse)
import Parser (parseExpressionThrows)
import qualified Printer as P
import qualified Random as R
import Rewriter (Rewritten, Rewrittens', stepHeaders)
import Sugar (SugarType (SALTY))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, takeExtension)
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

-- Run the action with a function writing the protocol of the run, holding the
-- file open for the whole of it. Opening it for writing truncates it, so that
-- it always holds the firings of exactly one run: a caller reading it back
-- never picks up lines left over from the previous run, even when this run
-- fires nothing at all. The handle is closed on the way out, failure included,
-- so the last lines reach the disk even when the run gives up. What the
-- protocol has counted so far rides in an 'IORef' next to the handle, since it
-- is the cursor of the file and not a property of the reduction (see
-- 'Protocol'). Every term is flattened into a single line, whatever '--flat'
-- says about the main output, since the file is a tree of one-line records. The
-- encoding is pinned to UTF-8 rather than taken from the locale, since the file
-- is read back by other programs.
withEvalFunc :: forall a. Maybe FilePath -> PrintContext -> (SaveEvalFunc -> IO a) -> IO a
withEvalFunc Nothing _ action = action dontSaveEval
withEvalFunc (Just file) ctx action = do
  createDirectoryIfMissing True (takeDirectory file)
  logDebug (printf "The option '--protocol' is specified, every firing will be recorded in '%s' as %s" file (if markup then "XML" else "text"))
  if markup then markedUp else plain
  where
    -- Which of the two formats the file holds is decided by the name it was
    -- given and by nothing else: '.xml' asks for the markup one, every other
    -- name for the indented text the option has always written (#1245). There
    -- is no flag for it, since a caller naming a file '.xml' and getting text
    -- back has been told nothing useful.
    markup :: Bool
    markup = map toLower (takeExtension file) == ".xml"
    -- The markup format closes on the way out what the run left open, so the
    -- document is well-formed however the run ended. The closing runs before
    -- the handle does, and the handle closes whether or not it succeeded.
    markedUp :: IO a
    markedUp = do
      cursor <- newIORef emptyNesting
      bracket opened (\protocol -> endEvalXml protocol cursor `finally` hClose protocol) $ \protocol ->
        action (saveEvalXml protocol cursor (flattened ctx))
    plain :: IO a
    plain = do
      cursor <- newIORef emptyProtocol
      bracket opened hClose $ \protocol ->
        action (saveEval protocol cursor (flattened ctx) (salted ctx))
    -- 'withFile' would do the same, except that it annotates whatever the action
    -- throws with the name of the file, and a dataization failure has to reach
    -- the user as it is
    opened :: IO Handle
    opened = do
      protocol <- openFile file WriteMode
      hSetEncoding protocol utf8
      pure protocol

-- The λ functions this run may fire. phino implements none of them, so without
-- '--symbolic' there are none at all and every λ function a program names gets
-- stuck — which is exactly what '--partial' parks on. The file is read here,
-- before anything is parsed or reduced, so a key that is no regular expression
-- or an answer the calculus cannot read fails the run up front rather than
-- half-way through a derivation.
lambdasOf :: Maybe FilePath -> IO Lambdas
lambdasOf Nothing = do
  logDebug "The option '--symbolic' is not specified, no λ function can be fired"
  pure emptyLambdas
lambdasOf (Just file) = do
  logDebug (printf "The option '--symbolic' is specified, reading the λ functions from '%s'" file)
  ensuredFile file >>= readLambdas

-- The state a run starts from: nothing manufactured yet and every symbol the
-- program already carries counted as minted, so a fresh 𝜎 is never spelled like
-- one the input was written with (see 'taken').
started :: Expression -> State
started expr = emptyState{_minted = taken expr}

-- Open the protocol with the run itself — the judgment it runs and the term it
-- is aimed at — which is the line every firing of it stands under.
heading :: SaveEvalFunc -> PrintContext -> Judgment -> Expression -> IO ()
heading record ctx judgment locator =
  record . EvRun judgment . T.pack =<< flattened ctx locator

-- How every term of the protocol is rendered: as 𝜑 on a single line, in the
-- sugar and the margin the run prints its own answer with. The protocol is a
-- tree of one-line 𝜑 records whatever '--output' the run was given, so a
-- program reading it back never has to know what the run printed.
flattened :: PrintContext -> Expression -> IO String
flattened ctx = pure . printPhi ctx{_line = SINGLELINE}

-- The same, in canonical 𝜑 rather than in the sugar the run prints with. The
-- operand a protocol line names is the term an entry of the '--symbolic' file
-- wrote, and the sweet syntax writes 'ξ.x' as a bare 'x', which reads as a name
-- and not as the term it is — so the comment that names an operand spells it
-- salty and the value beside it stays as the run spells it (#1265).
salted :: PrintContext -> Expression -> IO String
salted ctx = flattened ctx{_sugar = SALTY}

-- Aim the run at the '--inside' expression instead of at '--locator': the
-- expression is bound to a synthetic attribute prepended to the input
-- expression, which the run takes as the universe, and the locator becomes that
-- attribute (see 'insideUniverse'). Without the option nothing moves and the
-- context is handed back as it came.
aimed :: Maybe String -> Expression -> ReduceContext -> IO (Expression, ReduceContext)
aimed Nothing expr ctx = pure (expr, ctx)
aimed (Just src) expr@(ExFormation _) ctx = do
  target <- parseExpressionThrows src
  logDebug (printf "The option '--inside' is specified, reducing '%s' inside the given universe" (P.printExpression target))
  insideUniverse target expr ctx
aimed (Just _) expr _ =
  invalidCLIArguments
    (printf "The option --inside requires the input expression to be a formation, but given: %s" (P.printExpression expr))

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
          yamls <- mapM ensuredFile (nub rules)
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
    overwrite file content
    logDebug (printf "The command result was saved in '%s'" file)
