-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module CLI.Helpers where

import AST
import Abridge (abridged)
import CLI.Types
import CLI.Validators (invalidCLIArguments)
import CST (EXPRESSION)
import Canonizer (canonize, canonizeExpr)
import Compiled (compiled)
import Control.Exception
import Control.Monad ((>=>))
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.IORef
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Deps (Evaluation (EvRun), Judgment, SaveEvalFunc, SaveStepFunc, State (..), dontSaveEval, emptyNesting, emptyProgress, emptyProtocol, endEval, endEvalXml, progressed, saveEval, saveEvalXml, saveStep)
import Encoding
import Engine (Engine, fresh, yaml)
import Files (ensuredFile, overwrite)
import Functions (buildFunctions, execFunctions)
import GHC.Clock (getMonotonicTime)
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
import Sugar (SugarType (SALTY), withoutRho)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, takeExtension)
import System.IO (Handle, IOMode (WriteMode), getContents', hClose, hSetEncoding, openFile, utf8)
import Text.Printf (printf)
import XMIR (Atoms, expressionToXMIR, parseXMIRThrows, printXMIR, xmirAtoms, xmirToPhi)
import Yaml (normalizationRules)
import qualified Yaml as Y

justMeetPopularity :: Maybe Int -> Int
justMeetPopularity = fromMaybe defaultMeetPopularity

justMeetLength :: Maybe Int -> Int
justMeetLength = fromMaybe defaultMeetLength

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

withEvalFunc :: forall a. Maybe FilePath -> PrintContext -> (SaveEvalFunc -> IO a) -> IO a
withEvalFunc target ctx action = withEvalFunc' target ctx (tracked >=> action)
  where
    tracked :: SaveEvalFunc -> IO SaveEvalFunc
    tracked record = do
      enabled <- logging INFO
      if enabled
        then do
          cursor <- newIORef . emptyProgress =<< getMonotonicTime
          pure (progressed cursor 5 (flattened ctx) record)
        else pure record

withEvalFunc' :: forall a. Maybe FilePath -> PrintContext -> (SaveEvalFunc -> IO a) -> IO a
withEvalFunc' Nothing _ action = action dontSaveEval
withEvalFunc' (Just file) ctx action = do
  createDirectoryIfMissing True (takeDirectory file)
  logDebug (printf "The option '--protocol' is specified, every firing will be recorded in '%s' as %s" file (if markup then "XML" else "text"))
  if markup then markedUp else plain
  where
    markup :: Bool
    markup = map toLower (takeExtension file) == ".xml"
    markedUp :: IO a
    markedUp = do
      cursor <- newIORef emptyNesting
      began <- getMonotonicTime
      bracket opened (\protocol -> endEvalXml protocol cursor began `finally` hClose protocol) $ \protocol ->
        action (saveEvalXml protocol cursor (flattened ctx))
    plain :: IO a
    plain = do
      cursor <- newIORef emptyProtocol
      began <- getMonotonicTime
      bracket opened (\protocol -> endEval protocol cursor began `finally` hClose protocol) $ \protocol ->
        action (saveEval protocol cursor (flattened ctx) (salted ctx))
    opened :: IO Handle
    opened = do
      protocol <- openFile file WriteMode
      hSetEncoding protocol utf8
      pure protocol

lambdasOf :: Maybe FilePath -> IO Lambdas
lambdasOf Nothing = do
  logDebug "The option '--symbolic' is not specified, no λ function can be fired"
  pure emptyLambdas
lambdasOf (Just file) = do
  logDebug (printf "The option '--symbolic' is specified, reading the λ functions from '%s'" file)
  ensuredFile file >>= readLambdas

started :: Expression -> State
started expr = emptyState{_minted = taken expr}

heading :: SaveEvalFunc -> PrintContext -> Judgment -> Expression -> IO ()
heading record ctx judgment locator =
  record . EvRun judgment . T.pack =<< flattened ctx locator

flattened :: PrintContext -> Expression -> IO String
flattened ctx@PrintCtx{..} expr =
  pure (P.printExpressionWith shaped expr (_sugar, UNICODE, SINGLELINE, _margin))
  where
    shaped :: SugarType -> EXPRESSION -> EXPRESSION
    shaped sugar = maybe id abridged _abridged . hidden ctx sugar

salted :: PrintContext -> Expression -> IO String
salted ctx = flattened ctx{_sugar = SALTY}

aimed :: Maybe String -> Expression -> ReduceContext -> IO (Expression, ReduceContext)
aimed Nothing expr ctx = pure (expr, ctx)
aimed (Just src) expr@(ExFormation _) ctx = do
  target <- parseExpressionThrows src
  logDebug (printf "The option '--inside' is specified, reducing '%s' inside the given universe" (P.printExpression target))
  insideUniverse target expr ctx
aimed (Just _) expr _ =
  invalidCLIArguments
    (printf "The option --inside requires the input expression to be a formation, but given: %s" (P.printExpression expr))

readInput :: Maybe FilePath -> IO String
readInput inputFile' = case inputFile' of
  Just pth -> do
    logDebug (printf "Reading from file: '%s'" pth)
    readFile =<< ensuredFile pth
  Nothing -> do
    logDebug "Reading from stdin"
    getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))

parseInput :: String -> IOFormat -> IO Expression
parseInput phi PHI = parseExpressionThrows phi
parseInput xmir XMIR = parseXMIRThrows xmir >>= xmirToPhi
parseInput _ LATEX = invalidCLIArguments "LaTeX cannot be used as input format"

parseInputWithAtoms :: String -> IOFormat -> IO (Expression, Atoms)
parseInputWithAtoms xmir XMIR = do
  doc <- parseXMIRThrows xmir
  (,) <$> xmirToPhi doc <*> xmirAtoms doc
parseInputWithAtoms input format = (,M.empty) <$> parseInput input format

printRewrittens :: PrintContext -> Rewrittens' -> IO String
printRewrittens ctx@PrintCtx{..} rewrittens@(chain, _)
  | _outputFormat == LATEX && _sequence = rewrittensToLatex rewrittens (printCtxToLatexCtx ctx)
  | otherwise = withHeaders <$> mapM (printFocused ctx . fst) (canonized chain)
  where
    canonized :: [Rewritten] -> [Rewritten]
    canonized = if _canonize then canonize else id
    withHeaders :: [String] -> String
    withHeaders rendered
      | _headers && _sequence = intercalate "\n" (zipWith prefixed (stepHeaders chain) rendered)
      | otherwise = intercalate "\n" rendered
      where
        prefixed :: String -> String -> String
        prefixed = printf "\n%s\n%s"

printAnswer :: PrintContext -> Expression -> IO String
printAnswer ctx@PrintCtx{..} expr = printFocused ctx (if _canonize then canonizeExpr expr else expr)

printFocused :: PrintContext -> Expression -> IO String
printFocused ctx@PrintCtx{..} expr
  | _focus == ExRoot = printInFormat ctx expr
  | otherwise = locatedExpression _focus expr >>= printExpression ctx

printExpression :: PrintContext -> Expression -> IO String
printExpression ctx@PrintCtx{..} ex = case _outputFormat of
  PHI -> pure (printPhi ctx ex)
  XMIR -> throwIO CouldNotPrintExpressionInXMIR
  LATEX -> pure (expressionToLaTeX ex (printCtxToLatexCtx ctx))

printInFormat :: PrintContext -> Expression -> IO String
printInFormat ctx@PrintCtx{..} expr = case _outputFormat of
  PHI -> pure (printPhi ctx expr)
  XMIR -> expressionToXMIR expr _xmirCtx <&> printXMIR
  LATEX -> pure (expressionToLaTeX expr (printCtxToLatexCtx ctx))

printPhi :: PrintContext -> Expression -> String
printPhi ctx@PrintCtx{..} expr = P.printExpressionWith (hidden ctx) expr (_sugar, UNICODE, _line, _margin)

hidden :: PrintContext -> SugarType -> EXPRESSION -> EXPRESSION
hidden PrintCtx{..} sugar
  | _hideRho = withoutRho sugar
  | otherwise = id

printCtxToLatexCtx :: PrintContext -> LatexContext
printCtxToLatexCtx PrintCtx{..} =
  LatexContext _sugar _line _margin _nonumber _compress _canonize _meetPopularity _meetLength _focus _expression _label _meetPrefix _headers

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

validateRewriteRule :: Y.Rule -> IO Y.Rule
validateRewriteRule rule =
  let used = maybe [] (map Y.function) rule.where_
   in case filter (\fn -> fn `notElem` (buildFunctions ++ execFunctions)) used of
        (fn : _) -> invalidCLIArguments (printf "Function '%s' in rule '%s' is not supported" fn rule.name)
        [] -> case filter (`elem` execFunctions) used of
          [] -> pure rule
          (fn : _) ->
            invalidCLIArguments
              (printf "Function '%s' in rule '%s' is available only for dataization and morphing, not for rewriting" fn rule.name)

printOut :: Maybe FilePath -> String -> IO ()
printOut target content = case target of
  Nothing -> do
    logDebug "The option '--target' is not specified, printing to console..."
    putStrLn content
  Just file -> do
    logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
    overwrite file content
    logDebug (printf "The command result was saved in '%s'" file)

engine :: IO Engine
engine = case compiled of
  Nothing -> pure yaml
  Just linked
    | fresh linked -> logDebug "The built-in rules run compiled, as 'phino compile' wrote them" >> pure linked
    | otherwise -> throwIO StaleEngine
