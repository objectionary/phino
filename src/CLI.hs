{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import AST
import qualified Canonizer as C
import Condition (parseConditionThrows)
import Control.Exception (Exception (displayException), SomeException, handle, throw, throwIO)
import Control.Exception.Base
import Control.Monad (unless, when, (>=>))
import Data.Char (toLower, toUpper)
import Data.Foldable (for_)
import qualified Data.Foldable
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Version (showVersion)
import Dataize (DataizeContext (DataizeContext), dataize)
import Deps (SaveStepFunc, saveStep)
import Encoding (Encoding (ASCII, UNICODE))
import qualified Filter as F
import Functions (buildTerm)
import qualified Functions
import LaTeX (LatexContext (LatexContext), explainRules, programToLaTeX, rewrittensToLatex)
import Lining (LineFormat (MULTILINE, SINGLELINE))
import Logger
import Merge (merge)
import Misc (ensuredFile)
import qualified Misc
import Must (Must (..), validateMust)
import Options.Applicative
import Parser (parseExpressionThrows, parseProgramThrows)
import Paths_phino (version)
import qualified Printer as P
import Rewriter (RewriteContext (RewriteContext), Rewritten (..), rewrite')
import Rule (RuleContext (RuleContext), matchProgramWithRule)
import Sugar
import System.Exit (ExitCode (..), exitFailure)
import System.IO (getContents')
import Text.Printf (printf)
import XMIR (XmirContext (XmirContext), defaultXmirContext, parseXMIRThrows, printXMIR, programToXMIR, xmirToPhi)
import Yaml (normalizationRules)
import qualified Yaml as Y

data PrintProgramContext = PrintProgCtx
  { sugar :: SugarType
  , line :: LineFormat
  , xmirCtx :: XmirContext
  , nonumber :: Bool
  , expression :: Maybe String
  , label :: Maybe String
  , outputFormat :: IOFormat
  }

data CmdException
  = InvalidCLIArguments {message :: String}
  | CouldNotReadFromStdin {message :: String}
  | CouldNotDataize
  deriving (Exception)

instance Show CmdException where
  show InvalidCLIArguments{..} = printf "Invalid set of arguments: %s" message
  show CouldNotReadFromStdin{..} = printf "Could not read input from stdin\nReason: %s" message
  show CouldNotDataize = "Could not dataize given program"

data Command
  = CmdRewrite OptsRewrite
  | CmdDataize OptsDataize
  | CmdExplain OptsExplain
  | CmdMerge OptsMerge
  | CmdMatch OptsMatch

data IOFormat = XMIR | PHI | LATEX
  deriving (Eq)

instance Show IOFormat where
  show XMIR = "xmir"
  show PHI = "phi"
  show LATEX = "latex"

data OptsDataize = OptsDataize
  { logLevel :: LogLevel
  , logLines :: Integer
  , inputFormat :: IOFormat
  , outputFormat :: IOFormat
  , sugarType :: SugarType
  , flat :: LineFormat
  , omitListing :: Bool
  , omitComments :: Bool
  , nonumber :: Bool
  , sequence :: Bool
  , depthSensitive :: Bool
  , quiet :: Bool
  , maxDepth :: Integer
  , maxCycles :: Integer
  , hide :: [String]
  , show' :: [String]
  , expression :: Maybe String
  , label :: Maybe String
  , stepsDir :: Maybe FilePath
  , inputFile :: Maybe FilePath
  }

data OptsExplain = OptsExplain
  { logLevel :: LogLevel
  , logLines :: Integer
  , rules :: [FilePath]
  , normalize :: Bool
  , shuffle :: Bool
  , targetFile :: Maybe FilePath
  }

data OptsRewrite = OptsRewrite
  { logLevel :: LogLevel
  , logLines :: Integer
  , inputFormat :: IOFormat
  , outputFormat :: IOFormat
  , sugarType :: SugarType
  , flat :: LineFormat
  , must :: Must
  , normalize :: Bool
  , shuffle :: Bool
  , omitListing :: Bool
  , omitComments :: Bool
  , depthSensitive :: Bool
  , nonumber :: Bool
  , inPlace :: Bool
  , sequence :: Bool
  , canonize :: Bool
  , maxDepth :: Integer
  , maxCycles :: Integer
  , rules :: [FilePath]
  , hide :: [String]
  , show' :: [String]
  , expression :: Maybe String
  , label :: Maybe String
  , targetFile :: Maybe FilePath
  , stepsDir :: Maybe FilePath
  , inputFile :: Maybe FilePath
  }

data OptsMerge = OptsMerge
  { logLevel :: LogLevel
  , logLines :: Integer
  , inputFormat :: IOFormat
  , outputFormat :: IOFormat
  , sugarType :: SugarType
  , flat :: LineFormat
  , omitListing :: Bool
  , omitComments :: Bool
  , targetFile :: Maybe FilePath
  , inputs :: [FilePath]
  }

data OptsMatch = OptsMatch
  { logLevel :: LogLevel
  , logLines :: Integer
  , sugarType :: SugarType
  , flat :: LineFormat
  , pattern :: Maybe String
  , when' :: Maybe String
  , inputFile :: Maybe FilePath
  }

validateIntegerOption :: (Integer -> Bool) -> String -> Integer -> ReadM Integer
validateIntegerOption cmp msg num
  | cmp num = return num
  | otherwise = readerError msg

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

optLogLines :: Parser Integer
optLogLines =
  option
    (auto >>= validateIntegerOption (>= -1) "--log-lines must be >= -1")
    (long "log-lines" <> metavar "LINES" <> help "Amount of lines printed to console per each log operation (0 - print nothing, -1 - no limits)" <> value 25 <> showDefault)

optRule :: Parser [FilePath]
optRule = many (strOption (long "rule" <> metavar "[FILE]" <> help "Path to custom rule"))

optInputFormat :: Parser IOFormat
optInputFormat = option (parseIOFormat "input") (long "input" <> metavar "FORMAT" <> help "Program input format (phi, xmir)" <> value PHI <> showDefault)

parseIOFormat :: String -> ReadM IOFormat
parseIOFormat type' = eitherReader $ \format -> case (map toLower format, type') of
  ("xmir", _) -> Right XMIR
  ("phi", _) -> Right PHI
  ("latex", "output") -> Right LATEX
  _ -> Left (printf "The value '%s' can't be used for '--%s' option, use --help to check possible values" format type')

optOutputFormat :: Parser IOFormat
optOutputFormat =
  option
    (parseIOFormat "output")
    (long "output" <> metavar "FORMAT" <> help (printf "Result and intermediate (see %s option(s)) programs output format (phi, xmir, latex)" _intermediateOptions) <> value PHI <> showDefault)

argInputFile :: Parser (Maybe FilePath)
argInputFile = optional (argument str (metavar "FILE" <> help "Path to input file"))

optMaxDepth :: Parser Integer
optMaxDepth =
  option
    (auto >>= validateIntegerOption (> 0) "--max-depth must be positive")
    (long "max-depth" <> metavar "DEPTH" <> help "Maximum number of rewriting iterations per rule" <> value 25 <> showDefault)

optMaxCycles :: Parser Integer
optMaxCycles =
  option
    (auto >>= validateIntegerOption (> 0) "--max-cycles must be positive")
    (long "max-cycles" <> metavar "CYCLES" <> help "Maximum number of rewriting cycles across all rules" <> value 25 <> showDefault)

optDepthSensitive :: Parser Bool
optDepthSensitive = switch (long "depth-sensitive" <> help "Fail if rewriting is not finished after reaching max attempts (see --max-cycles or --max-depth)")

optNonumber :: Parser Bool
optNonumber = switch (long "nonumber" <> help "Turn off equation auto numbering in LaTeX rendering (see --output option)")

optSequence :: Parser Bool
optSequence = switch (long "sequence" <> help "Result output contains all intermediate 𝜑-programs concatenated with EOL")

optExpression :: Parser (Maybe String)
optExpression = optional (strOption (long "expression" <> metavar "NAME" <> help "Name for 'phiExpression' element when rendering to LaTeX (see --output option)"))

optLabel :: Parser (Maybe String)
optLabel = optional (strOption (long "label" <> metavar "NAME" <> help "Name for 'label' element when rendering to LaTeX (see --output option)"))

optHide :: Parser [String]
optHide = many (strOption (long "hide" <> metavar "FQN" <> help "Location of object to exclude from result and intermediate programs after rewriting. Must be a valid dispatch expression; e.g. Q.org.eolang"))

optShow :: Parser [String]
optShow =
  many
    ( strOption
        ( long "show"
            <> metavar "FQN"
            <> help
              "Location of object to include to result and intermediate programs after rewriting. \
              \Must be a valid dispatch expression; e.g. Q.org.eolang. Unlike --hide, can be used only once"
        )
    )

optNormalize :: Parser Bool
optNormalize = switch (long "normalize" <> help "Use built-in normalization rules")

optTarget :: Parser (Maybe FilePath)
optTarget = optional (strOption (long "target" <> short 't' <> metavar "FILE" <> help "File to save output to"))

optStepsDir :: Parser (Maybe FilePath)
optStepsDir = optional (strOption (long "steps-dir" <> metavar "FILE" <> help "Directory to save intermediate steps during rewriting/dataizing"))

optShuffle :: Parser Bool
optShuffle = switch (long "shuffle" <> help "Shuffle rules before applying")

optSugar :: Parser SugarType
optSugar = flag SALTY SWEET (long "sweet" <> help (printf "Print result and intermediate (see %s option(s)) 𝜑-programs using syntax sugar" _intermediateOptions))

optSugar' :: Parser SugarType
optSugar' = flag SALTY SWEET (long "sweet" <> help "Print result 𝜑-program using syntax sugar")

optLineFormat :: Parser LineFormat
optLineFormat = flag MULTILINE SINGLELINE (long "flat" <> help (printf "Print result and intermediate (see %s option(s)) 𝜑-programs in one line" _intermediateOptions))

optLineFormat' :: Parser LineFormat
optLineFormat' = flag MULTILINE SINGLELINE (long "flat" <> help "Print result 𝜑-program in one line")

optOmitListing :: Parser Bool
optOmitListing = switch (long "omit-listing" <> help "Omit full program listing in XMIR output")

optOmitComments :: Parser Bool
optOmitComments = switch (long "omit-comments" <> help "Omit comments in XMIR output")

_intermediateOptions :: String
_intermediateOptions = intercalate ", " ["--sequence", "--steps-dir"]

explainParser :: Parser Command
explainParser =
  CmdExplain
    <$> ( OptsExplain
            <$> optLogLevel
            <*> optLogLines
            <*> optRule
            <*> optNormalize
            <*> optShuffle
            <*> optTarget
        )

dataizeParser :: Parser Command
dataizeParser =
  CmdDataize
    <$> ( OptsDataize
            <$> optLogLevel
            <*> optLogLines
            <*> optInputFormat
            <*> optOutputFormat
            <*> optSugar
            <*> optLineFormat
            <*> optOmitListing
            <*> optOmitComments
            <*> optNonumber
            <*> optSequence
            <*> optDepthSensitive
            <*> switch (long "quiet" <> help "Don't print the result of dataization")
            <*> optMaxDepth
            <*> optMaxCycles
            <*> optHide
            <*> optShow
            <*> optExpression
            <*> optLabel
            <*> optStepsDir
            <*> argInputFile
        )

rewriteParser :: Parser Command
rewriteParser =
  CmdRewrite
    <$> ( OptsRewrite
            <$> optLogLevel
            <*> optLogLines
            <*> optInputFormat
            <*> optOutputFormat
            <*> optSugar
            <*> optLineFormat
            <*> option
              auto
              ( long "must"
                  <> metavar "RANGE"
                  <> help "Must-rewrite range (e.g., '3', '..5', '3..', '3..5'). Stops execution if number of rules applied is not in range. Use 0 to disable."
                  <> value MtDisabled
                  <> showDefaultWith show
              )
            <*> optNormalize
            <*> optShuffle
            <*> optOmitListing
            <*> optOmitComments
            <*> optDepthSensitive
            <*> optNonumber
            <*> switch (long "in-place" <> help "Edit file in-place instead of printing to output")
            <*> optSequence
            <*> switch (long "canonize" <> help "Rename all functions attached to λ binding with F1, F2, etc.")
            <*> optMaxDepth
            <*> optMaxCycles
            <*> optRule
            <*> optHide
            <*> optShow
            <*> optExpression
            <*> optLabel
            <*> optTarget
            <*> optStepsDir
            <*> argInputFile
        )

mergeParser :: Parser Command
mergeParser =
  CmdMerge
    <$> ( OptsMerge
            <$> optLogLevel
            <*> optLogLines
            <*> optInputFormat
            <*> option (parseIOFormat "output") (long "output" <> metavar "FORMAT" <> help (printf "Result program output format (phi, xmir, latex)") <> value PHI <> showDefault)
            <*> optSugar'
            <*> optLineFormat'
            <*> optOmitListing
            <*> optOmitComments
            <*> optTarget
            <*> many (argument str (metavar "[FILE]" <> help "Paths to input files"))
        )

matchParser :: Parser Command
matchParser =
  CmdMatch
    <$> ( OptsMatch
            <$> optLogLevel
            <*> optLogLines
            <*> optSugar
            <*> optLineFormat
            <*> optional (strOption (long "pattern" <> metavar "EXPRESSION" <> help "Pattern expression to match against"))
            <*> optional (strOption (long "when" <> metavar "CONDITION" <> help "Predicate for matched substitutions"))
            <*> argInputFile
        )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "rewrite" (info rewriteParser (progDesc "Rewrite the 𝜑-program"))
        <> command "dataize" (info dataizeParser (progDesc "Dataize the 𝜑-program"))
        <> command "explain" (info explainParser (progDesc "Explain rules in LaTeX format"))
        <> command "merge" (info mergeParser (progDesc "Merge 𝜑-programs into single one by merging their top level formations"))
        <> command "match" (info matchParser (progDesc "Match 𝜑-program against provided pattern and build matched substitutions"))
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

setLogger :: Command -> IO ()
setLogger cmd =
  let (level, lines) = case cmd of
        CmdRewrite OptsRewrite{logLevel, logLines} -> (logLevel, logLines)
        CmdDataize OptsDataize{logLevel, logLines} -> (logLevel, logLines)
        CmdExplain OptsExplain{logLevel, logLines} -> (logLevel, logLines)
        CmdMerge OptsMerge{logLevel, logLines} -> (logLevel, logLines)
        CmdMatch OptsMatch{logLevel, logLines} -> (logLevel, logLines)
   in setLogConfig level lines

invalidCLIArguments :: String -> IO a
invalidCLIArguments msg = throwIO (InvalidCLIArguments msg)

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  cmd <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  setLogger cmd
  case cmd of
    CmdRewrite OptsRewrite{..} -> do
      validateOpts
      excluded <- expressionsToFilter "hide" hide
      included <- expressionsToFilter "show" show'
      logDebug (printf "Amount of rewriting cycles across all the rules: %d, per rule: %d" maxCycles maxDepth)
      input <- readInput inputFile
      rules' <- getRules normalize shuffle rules
      program <- parseProgram input inputFormat
      let listing = if null rules' then const input else (\prog -> P.printProgram' prog (sugarType, UNICODE, flat))
          xmirCtx = XmirContext omitListing omitComments listing
          printCtx = PrintProgCtx sugarType flat xmirCtx nonumber expression label outputFormat
          _canonize = if canonize then C.canonize else id
          _hide = (`F.exclude` excluded)
          _show = (`F.include` included)
      rewrittens <- rewrite' program rules' (context printCtx) <&> _canonize . _hide . _show
      let rewrittens' = if sequence then rewrittens else [last rewrittens]
      logDebug (printf "Printing rewritten 𝜑-program as %s" (show outputFormat))
      progs <- printRewrittens printCtx rewrittens'
      output targetFile progs
      where
        validateOpts :: IO ()
        validateOpts = do
          when
            (inPlace && isNothing inputFile)
            (invalidCLIArguments "The option --in-place requires an input file")
          when
            (inPlace && isJust targetFile)
            (invalidCLIArguments "The options --in-place and --target cannot be used together")
          when (length show' > 1) (invalidCLIArguments "The option --show can be used only once")
          validateLatexOptions outputFormat nonumber expression label
          validateMust' must
          validateXmirOptions outputFormat omitListing omitComments
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
        context :: PrintProgramContext -> RewriteContext
        context ctx =
          RewriteContext
            maxDepth
            maxCycles
            depthSensitive
            buildTerm
            must
            (saveStepFunc stepsDir ctx)
    CmdDataize OptsDataize{..} -> do
      validateOpts
      excluded <- expressionsToFilter "hide" hide
      included <- expressionsToFilter "show" show'
      input <- readInput inputFile
      prog <- parseProgram input inputFormat
      let printCtx = PrintProgCtx sugarType flat defaultXmirContext nonumber expression label outputFormat
          _hide = (`F.exclude` excluded)
          _show = (`F.include` included)
      (maybeBytes, seq) <- dataize prog (context prog printCtx)
      when sequence (printRewrittens printCtx (_hide (_show seq)) >>= putStrLn)
      unless quiet (putStrLn (maybe (P.printExpression ExTermination) P.printBytes maybeBytes))
      where
        validateOpts :: IO ()
        validateOpts = do
          validateLatexOptions outputFormat nonumber expression label
          validateXmirOptions outputFormat omitListing omitComments
          when (length show' > 1) (invalidCLIArguments "The option --show can be used only once")
        context :: Program -> PrintProgramContext -> DataizeContext
        context program ctx =
          DataizeContext
            program
            maxDepth
            maxCycles
            depthSensitive
            buildTerm
            (saveStepFunc stepsDir ctx)
    CmdExplain OptsExplain{..} -> do
      validateOpts
      rules' <- getRules normalize shuffle rules
      let latex = explainRules rules'
      output targetFile (explainRules rules')
      where
        validateOpts :: IO ()
        validateOpts =
          when
            (null rules && not normalize)
            (throwIO (InvalidCLIArguments "Either --rule or --normalize must be specified"))
    CmdMerge OptsMerge{..} -> do
      validateOpts
      inputs' <- traverse (readInput . Just) inputs
      progs <- traverse (`parseProgram` inputFormat) inputs'
      prog <- merge progs
      let listing = const (P.printProgram' prog (sugarType, UNICODE, flat))
          xmirCtx = XmirContext omitListing omitComments listing
          printProgCtx = PrintProgCtx sugarType flat xmirCtx False Nothing Nothing outputFormat
      prog' <- printProgram printProgCtx prog
      output targetFile prog'
      where
        validateOpts :: IO ()
        validateOpts = do
          when
            (null inputs)
            (throwIO (InvalidCLIArguments "At least one input file must be specified for 'merge' command"))
          validateXmirOptions outputFormat omitListing omitComments
    CmdMatch OptsMatch{..} -> do
      input <- readInput inputFile
      prog <- parseProgram input PHI
      if isNothing pattern
        then logInfo "The --pattern is not provided, no substitutions are built"
        else do
          ptn <- parseExpressionThrows (fromJust pattern)
          condition <- traverse parseConditionThrows when'
          substs <- matchProgramWithRule prog (rule ptn condition) (RuleContext buildTerm)
          if null substs
            then logInfo "Provided pattern was not matched, no substitutions are built"
            else putStrLn (P.printSubsts' substs (sugarType, UNICODE, flat))
      where
        rule :: Expression -> Maybe Y.Condition -> Y.Rule
        rule ptn cnd = Y.Rule Nothing Nothing ptn ExGlobal cnd Nothing Nothing

-- Prepare saveStepFunc
saveStepFunc :: Maybe FilePath -> PrintProgramContext -> SaveStepFunc
saveStepFunc stepsDir ctx@PrintProgCtx{..} = saveStep stepsDir ioToExt (printProgram ctx)
  where
    ioToExt :: String
    ioToExt
      | outputFormat == LATEX = "tex"
      | otherwise = show outputFormat

-- Get list of expressions which must be hidden in printed programs
expressionsToFilter :: String -> [String] -> IO [Expression]
expressionsToFilter opt = traverse (parseExpressionThrows >=> asFilter)
  where
    asFilter :: Expression -> IO Expression
    asFilter expr = asFilter' expr
      where
        asFilter' :: Expression -> IO Expression
        asFilter' exp@(ExDispatch ExGlobal _) = pure exp
        asFilter' exp@(ExDispatch expr attr) = asFilter' expr >> pure exp
        asFilter' _ =
          invalidCLIArguments
            ( printf
                "Only dispatch expression started with Φ (or Q) can be used in --%s, but given: %s"
                opt
                (P.printExpression' expr P.logPrintConfig)
            )

-- Validate LaTeX options
validateLatexOptions :: IOFormat -> Bool -> Maybe String -> Maybe String -> IO ()
validateLatexOptions outputFormat nonumber expression label = do
  when
    (nonumber && outputFormat /= LATEX)
    (invalidCLIArguments "The --nonumber option can stay together with --output=latex only")
  when
    (isJust expression && outputFormat /= LATEX)
    (invalidCLIArguments "The --expression option can stay together with --output=latex only")
  when
    (isJust label && outputFormat /= LATEX)
    (invalidCLIArguments "The --label option can stay together with --output=latex only")

-- Validate 'must' option
validateMust' :: Must -> IO ()
validateMust' must = for_ (validateMust must) invalidCLIArguments

-- Validate options for output to XMIR
validateXmirOptions :: IOFormat -> Bool -> Bool -> IO ()
validateXmirOptions outputFormat omitListing omitComments = do
  when
    (outputFormat /= XMIR && omitListing)
    (invalidCLIArguments "--omit-listing can be used only with --output=xmir")
  when
    (outputFormat /= XMIR && omitComments)
    (invalidCLIArguments "--omit-comments can be used only with --output=xmir")

-- Read input from file or stdin
readInput :: Maybe FilePath -> IO String
readInput inputFile' = case inputFile' of
  Just pth -> do
    logDebug (printf "Reading from file: '%s'" pth)
    readFile =<< ensuredFile pth
  Nothing -> do
    logDebug "Reading from stdin"
    getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))

-- Parse program from String input depending on input IO format
parseProgram :: String -> IOFormat -> IO Program
parseProgram phi PHI = parseProgramThrows phi
parseProgram xmir XMIR = do
  doc <- parseXMIRThrows xmir
  xmirToPhi doc

printRewrittens :: PrintProgramContext -> [Rewritten] -> IO String
printRewrittens ctx@PrintProgCtx{..} rewrittens
  | outputFormat == LATEX = pure (rewrittensToLatex rewrittens (LatexContext sugar line nonumber expression label))
  | otherwise = mapM (printProgram ctx . fst) rewrittens <&> intercalate "\n"

-- Convert program to corresponding String format
printProgram :: PrintProgramContext -> Program -> IO String
printProgram PrintProgCtx{..} prog = case outputFormat of
  PHI -> pure (P.printProgram' prog (sugar, UNICODE, line))
  XMIR -> programToXMIR prog xmirCtx <&> printXMIR
  LATEX -> pure (programToLaTeX prog (LatexContext sugar line nonumber expression label))

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
            mapM Y.yamlRule yamls
  if shuffle
    then do
      logDebug "The --shuffle option is provided, rules are used in random order"
      Misc.shuffle ordered
    else pure ordered

-- Output content
output :: Maybe FilePath -> String -> IO ()
output target content = case target of
  Nothing -> do
    logDebug "The option '--target' is not specified, printing to console..."
    putStrLn content
  Just file -> do
    logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
    writeFile file content
    logInfo (printf "The command result was saved in '%s'" file)
