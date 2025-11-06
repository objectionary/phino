{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import AST (Program (Program))
import Control.Exception (Exception (displayException), SomeException, handle, throw, throwIO)
import Control.Exception.Base
import Control.Monad (when)
import Data.Char (toLower, toUpper)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Data.Version (showVersion)
import Dataize (DataizeContext (DataizeContext), dataize)
import Deps (saveStep)
import Encoding (Encoding (ASCII, UNICODE))
import Functions (buildTerm)
import qualified Functions
import LaTeX (LatexContext (LatexContext), explainRules, programToLaTeX, rewrittensToLatex)
import Lining (LineFormat (MULTILINE, SINGLELINE))
import Logger
import Misc (ensuredFile)
import qualified Misc
import Must (Must (..))
import Options.Applicative
import Parser (parseProgramThrows)
import Paths_phino (version)
import qualified Printer as P
import Rewriter (RewriteContext (RewriteContext), Rewritten (..), rewrite')
import Sugar
import System.Exit (ExitCode (..), exitFailure)
import System.IO (getContents')
import Text.Printf (printf)
import XMIR (XmirContext (XmirContext), defaultXmirContext, parseXMIRThrows, printXMIR, programToXMIR, xmirToPhi)
import Yaml (normalizationRules)
import qualified Yaml as Y

data CmdException
  = InvalidRewriteArguments {message :: String}
  | CouldNotReadFromStdin {message :: String}
  | CouldNotDataize
  deriving (Exception)

instance Show CmdException where
  show InvalidRewriteArguments {..} = printf "Invalid set of arguments for 'rewrite' command: %s" message
  show CouldNotReadFromStdin {..} = printf "Could not read input from stdin\nReason: %s" message
  show CouldNotDataize = "Could not dataize given program"

data Command
  = CmdRewrite OptsRewrite
  | CmdDataize OptsDataize
  | CmdExplain OptsExplain
  | CmdMerge OptsMerge

data IOFormat = XMIR | PHI | LATEX
  deriving (Eq)

instance Show IOFormat where
  show XMIR = "xmir"
  show PHI = "phi"
  show LATEX = "latex"

data OptsDataize = OptsDataize
  { logLevel :: LogLevel,
    inputFormat :: IOFormat,
    sugarType :: SugarType,
    flat :: LineFormat,
    maxDepth :: Integer,
    maxCycles :: Integer,
    depthSensitive :: Bool,
    stepsDir :: Maybe FilePath,
    inputFile :: Maybe FilePath
  }

data OptsExplain = OptsExplain
  { logLevel :: LogLevel,
    rules :: [FilePath],
    normalize :: Bool,
    shuffle :: Bool,
    targetFile :: Maybe FilePath
  }

data OptsRewrite = OptsRewrite
  { logLevel :: LogLevel,
    rules :: [FilePath],
    inputFormat :: IOFormat,
    outputFormat :: IOFormat,
    sugarType :: SugarType,
    flat :: LineFormat,
    normalize :: Bool,
    shuffle :: Bool,
    omitListing :: Bool,
    omitComments :: Bool,
    depthSensitive :: Bool,
    must :: Must,
    maxDepth :: Integer,
    maxCycles :: Integer,
    inPlace :: Bool,
    sequence :: Bool,
    targetFile :: Maybe FilePath,
    stepsDir :: Maybe FilePath,
    inputFile :: Maybe FilePath
  }

data OptsMerge = OptsMerge
  { logLevel :: LogLevel,
    inputFormat :: IOFormat,
    outputFormat :: IOFormat,
    sugarType :: SugarType,
    flat :: LineFormat,
    omitListing :: Bool,
    omitComments :: Bool,
    targetFile :: Maybe FilePath,
    inputs :: [FilePath]
  }

optLogLevel :: Parser LogLevel
optLogLevel =
  option
    parseLogLevel
    ( long "log-level"
        <> metavar "LEVEL"
        <> help ("Log level (" <> intercalate ", " (map show [DEBUG, INFO, WARNING, ERROR, NONE]) <> ")")
        <> value INFO
        <> showDefault
    )
  where
    parseLogLevel :: ReadM LogLevel
    parseLogLevel = eitherReader $ \lvl -> case map toUpper lvl of
      "DEBUG" -> Right DEBUG
      "INFO" -> Right INFO
      "WARNING" -> Right WARNING
      "WARN" -> Right WARNING
      "ERROR" -> Right ERROR
      "ERR" -> Right ERROR
      "NONE" -> Right NONE
      _ -> Left $ "unknown log-level: " <> lvl

parseIOFormat :: String -> ReadM IOFormat
parseIOFormat type' = eitherReader $ \format -> case (map toLower format, type') of
  ("xmir", _) -> Right XMIR
  ("phi", _) -> Right PHI
  ("latex", "output") -> Right LATEX
  _ -> Left (printf "The value '%s' can't be used for '--%s' option, use --help to check possible values" format type')

argInputFile :: Parser (Maybe FilePath)
argInputFile = optional (argument str (metavar "FILE" <> help "Path to input file"))

optMaxDepth :: Parser Integer
optMaxDepth = option auto (long "max-depth" <> metavar "DEPTH" <> help "Maximum number of rewriting iterations per rule" <> value 25 <> showDefault)

optMaxCycles :: Parser Integer
optMaxCycles = option auto (long "max-cycles" <> metavar "CYCLES" <> help "Maximum number of rewriting cycles across all rules" <> value 25 <> showDefault)

optInputFormat :: Parser IOFormat
optInputFormat = option (parseIOFormat "input") (long "input" <> metavar "FORMAT" <> help "Program input format (phi, xmir)" <> value PHI <> showDefault)

optOutputFormat :: Parser IOFormat
optOutputFormat = option (parseIOFormat "output") (long "output" <> metavar "FORMAT" <> help "Program output format (phi, xmir, latex)" <> value PHI <> showDefault)

optDepthSensitive :: Parser Bool
optDepthSensitive = switch (long "depth-sensitive" <> help "Fail if rewriting is not finished after reaching max attempts (see --max-cycles or --max-depth)")

optRule :: Parser [FilePath]
optRule = many (strOption (long "rule" <> metavar "[FILE]" <> help "Path to custom rule"))

optNormalize :: Parser Bool
optNormalize = switch (long "normalize" <> help "Use built-in normalization rules")

optTarget :: Parser (Maybe FilePath)
optTarget = optional (strOption (long "target" <> short 't' <> metavar "FILE" <> help "File to save output to"))

optStepsDir :: Parser (Maybe FilePath)
optStepsDir = optional (strOption (long "steps-dir" <> metavar "FILE" <> help "Directory to save intermediate steps during rewriting/dataizing"))

optShuffle :: Parser Bool
optShuffle = switch (long "shuffle" <> help "Shuffle rules before applying")

optSugar :: [String] -> Parser SugarType
optSugar opts = flag SALTY SWEET (long "sweet" <> help (printf "Print result and intermediate (see %s option(s)) 𝜑-programs using syntax sugar" (intercalate ", " opts)))

optSugar' :: Parser SugarType
optSugar' = flag SALTY SWEET (long "sweet" <> help "Print result 𝜑-program using syntax sugar")

optLineFormat :: [String] -> Parser LineFormat
optLineFormat opts = flag MULTILINE SINGLELINE (long "flat" <> help (printf "Print result and intermediate (see %s option(s)) 𝜑-programs in one line" (intercalate ", " opts)))

optLineFormat' :: Parser LineFormat
optLineFormat' = flag MULTILINE SINGLELINE (long "flat" <> help "Print result 𝜑-program in one line")

optOmitListing :: Parser Bool
optOmitListing = switch (long "omit-listing" <> help "Omit full program listing in XMIR output")

optOmitComments :: Parser Bool
optOmitComments = switch (long "omit-comments" <> help "Omit comments in XMIR output")

explainParser :: Parser Command
explainParser =
  CmdExplain
    <$> ( OptsExplain
            <$> optLogLevel
            <*> optRule
            <*> optNormalize
            <*> optShuffle
            <*> optTarget
        )

dataizeParser :: Parser Command
dataizeParser =
  let steps = ["--steps-dir"]
   in CmdDataize
        <$> ( OptsDataize
                <$> optLogLevel
                <*> optInputFormat
                <*> optSugar steps
                <*> optLineFormat steps
                <*> optMaxDepth
                <*> optMaxCycles
                <*> optDepthSensitive
                <*> optStepsDir
                <*> argInputFile
            )

rewriteParser :: Parser Command
rewriteParser =
  let opts = ["--sequence", "--steps-dir"]
   in CmdRewrite
        <$> ( OptsRewrite
                <$> optLogLevel
                <*> optRule
                <*> optInputFormat
                <*> optOutputFormat
                <*> optSugar opts
                <*> optLineFormat opts
                <*> optNormalize
                <*> optShuffle
                <*> optOmitListing
                <*> optOmitComments
                <*> optDepthSensitive
                <*> option
                  auto
                  ( long "must"
                      <> metavar "RANGE"
                      <> help "Must-rewrite range (e.g., '3', '..5', '3..', '3..5'). Stops execution if number of rules applied is not in range. Use 0 to disable."
                      <> value MtDisabled
                      <> showDefaultWith show
                  )
                <*> optMaxDepth
                <*> optMaxCycles
                <*> switch (long "in-place" <> help "Edit file in-place instead of printing to output")
                <*> switch (long "sequence" <> help "Result output contains all intermediate 𝜑-programs concatenated with EOL")
                <*> optTarget
                <*> optStepsDir
                <*> argInputFile
            )

mergeParser :: Parser Command
mergeParser =
  CmdMerge
    <$> ( OptsMerge
            <$> optLogLevel
            <*> optInputFormat
            <*> optOutputFormat
            <*> optSugar'
            <*> optLineFormat'
            <*> optOmitListing
            <*> optOmitComments
            <*> optTarget
            <*> many (argument str (metavar "[FILE]" <> help "Paths to input files"))
        )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "rewrite" (info rewriteParser (progDesc "Rewrite the 𝜑-program"))
        <> command "dataize" (info dataizeParser (progDesc "Dataize the 𝜑-program"))
        <> command "explain" (info explainParser (progDesc "Explain rules in LaTeX format"))
        <> command "merge" (info mergeParser (progDesc "Merge 𝜑-programs into single one by merging their top level formations"))
    )

parserInfo :: ParserInfo Command
parserInfo =
  info
    (commandParser <**> helper <**> simpleVersioner (showVersion version))
    (fullDesc <> header "Phino - CLI Manipulator of 𝜑-Calculus Expressions")

handler :: SomeException -> IO ()
handler e = case fromException e of
  Just ExitSuccess -> pure () -- prevent printing error on --version etc.
  _ -> do
    logError (displayException e)
    exitFailure

setLogLevel' :: Command -> IO ()
setLogLevel' cmd =
  let level = case cmd of
        CmdRewrite OptsRewrite {logLevel} -> logLevel
        CmdDataize OptsDataize {logLevel} -> logLevel
        CmdExplain OptsExplain {logLevel} -> logLevel
        CmdMerge OptsMerge {logLevel} -> logLevel
   in setLogLevel level

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  cmd <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  setLogLevel' cmd
  case cmd of
    CmdRewrite opts@OptsRewrite {..} -> do
      validateOpts
      logDebug (printf "Amount of rewriting cycles across all the rules: %d, per rule: %d" maxCycles maxDepth)
      input <- readInput inputFile
      let xmirCtx = Just (XmirContext omitListing omitComments input)
      rules' <- getRules normalize shuffle rules
      program <- parseProgram input inputFormat
      rewrittens <- rewrite' program rules' (context program xmirCtx)
      logDebug (printf "Printing rewritten 𝜑-program as %s" (show outputFormat))
      progs <- printRewrittens xmirCtx rewrittens
      output targetFile progs
      where
        validateOpts :: IO ()
        validateOpts = do
          when
            (sugarType == SWEET && outputFormat == XMIR)
            (throwIO (InvalidRewriteArguments "The --sweet and --output=xmir can't stay together"))
          when
            (inPlace && isNothing inputFile)
            (throwIO (InvalidRewriteArguments "--in-place requires an input file"))
          when
            (inPlace && isJust targetFile)
            (throwIO (InvalidRewriteArguments "--in-place and --target cannot be used together"))
          validateMaxDepth maxDepth
          validateMaxCycles maxCycles
          validateMust must
        output :: Maybe FilePath -> String -> IO ()
        output target prog = case (inPlace, target, inputFile) of
          (True, _, Just file) -> do
            logDebug (printf "The option '--in-place' is specified, writing back to '%s'..." file)
            writeFile file prog
            logInfo (printf "The file '%s' was modified in-place" file)
          (False, Just file, _) -> do
            logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
            writeFile file prog
            logInfo (printf "The command result was saved in '%s'" file)
          (False, Nothing, _) -> do
            logDebug "The option '--target' is not specified, printing to console..."
            putStrLn prog
        context :: Program -> Maybe XmirContext -> RewriteContext
        context program ctx =
          RewriteContext
            program
            maxDepth
            maxCycles
            depthSensitive
            sequence
            buildTerm
            must
            (saveStep stepsDir (ioToExtension outputFormat) (printProgram outputFormat (sugarType, flat) ctx))
        printRewrittens :: Maybe XmirContext -> [Rewritten] -> IO String
        printRewrittens ctx rewrittens
          | outputFormat == LATEX = pure (rewrittensToLatex rewrittens (LatexContext sugarType flat))
          | otherwise = mapM (printProgram outputFormat (sugarType, flat) ctx . program) rewrittens <&> intercalate "\n"
    CmdDataize opts@OptsDataize {..} -> do
      validateOpts
      input <- readInput inputFile
      prog <- parseProgram input inputFormat
      dataized <- dataize prog (context prog)
      maybe (throwIO CouldNotDataize) (putStrLn . P.printBytes) dataized
      where
        validateOpts :: IO ()
        validateOpts = do
          validateMaxDepth maxDepth
          validateMaxCycles maxCycles
        context :: Program -> DataizeContext
        context program =
          DataizeContext
            program
            maxDepth
            maxCycles
            depthSensitive
            buildTerm
            (saveStep stepsDir (ioToExtension PHI) (printProgram PHI (SWEET, MULTILINE) Nothing))
    CmdExplain opts@OptsExplain {..} -> do
      validateOpts
      rules' <- getRules normalize shuffle rules
      let latex = explainRules rules'
      output targetFile (explainRules rules')
      where
        validateOpts :: IO ()
        validateOpts =
          when
            (null rules && not normalize)
            (throwIO (InvalidRewriteArguments "Either --rule or --normalize must be specified"))
    CmdMerge opts@OptsMerge{..} -> do
      inputs' <- traverse (readInput . Just) inputs
      putStrLn ""
  where
    validateIntArgument :: Integer -> (Integer -> Bool) -> String -> IO ()
    validateIntArgument num cmp msg =
      when
        (cmp num)
        (throwIO (InvalidRewriteArguments msg))
    validateMaxDepth :: Integer -> IO ()
    validateMaxDepth depth = validateIntArgument depth (<= 0) "--max-depth must be positive"
    validateMaxCycles :: Integer -> IO ()
    validateMaxCycles cycles = validateIntArgument cycles (<= 0) "--max-cycles must be positive"
    validateMust :: Must -> IO ()
    validateMust MtDisabled = pure ()
    validateMust (MtExact n) = validateIntArgument n (<= 0) "--must exact value must be positive"
    validateMust (MtRange minVal maxVal) = do
      maybe (pure ()) (\n -> validateIntArgument n (< 0) "--must minimum must be non-negative") minVal
      maybe (pure ()) (\n -> validateIntArgument n (< 0) "--must maximum must be non-negative") maxVal
      case (minVal, maxVal) of
        (Just min, Just max)
          | min > max ->
              throwIO
                ( InvalidRewriteArguments
                    (printf "--must range invalid: minimum (%d) is greater than maximum (%d)" min max)
                )
        _ -> pure ()
    readInput :: Maybe FilePath -> IO String
    readInput inputFile' = case inputFile' of
      Just pth -> do
        logDebug (printf "Reading from file: '%s'" pth)
        readFile =<< ensuredFile pth
      Nothing -> do
        logDebug "Reading from stdin"
        getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))
    parseProgram :: String -> IOFormat -> IO Program
    parseProgram phi PHI = parseProgramThrows phi
    parseProgram xmir XMIR = do
      doc <- parseXMIRThrows xmir
      xmirToPhi doc
    printProgram :: IOFormat -> (SugarType, LineFormat) -> Maybe XmirContext -> Program -> IO String
    printProgram PHI (sugar, line) _ prog = pure (P.printProgram' prog (sugar, UNICODE, line))
    printProgram XMIR cfg Nothing prog = printProgram XMIR cfg (Just defaultXmirContext) prog
    printProgram XMIR _ (Just ctx) prog = programToXMIR prog ctx <&> printXMIR
    printProgram LATEX (sugar, line) _ prog = pure (programToLaTeX prog (LatexContext sugar line))
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
                mapM Y.yamlRule yamls
      if shuffle
        then do
          logDebug "The --shuffle option is provided, rules are used in random order"
          Misc.shuffle ordered
        else pure ordered
    output :: Maybe FilePath -> String -> IO ()
    output target content = case target of
      Nothing -> do
        logDebug "The option '--target' is not specified, printing to console..."
        putStrLn content
      Just file -> do
        logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
        writeFile file content
        logInfo (printf "The command result was saved in '%s'" file)
    ioToExtension :: IOFormat -> String
    ioToExtension LATEX = "tex"
    ioToExtension format = show format
