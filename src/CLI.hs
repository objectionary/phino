{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import AST
import qualified Canonizer as C
import Condition (parseConditionThrows)
import Control.Exception.Base (Exception (displayException), SomeException, catch, fromException, handle, throwIO)
import Control.Monad (forM_, unless, when, (>=>))
import Data.Char (toLower, toUpper)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Version (showVersion)
import Dataize (DataizeContext (DataizeContext), dataize)
import Deps (SaveStepFunc, saveStep)
import Encoding (Encoding (UNICODE))
import qualified Filter as F
import Functions (buildTerm)
import LaTeX (LatexContext (LatexContext), explainRules, expressionToLaTeX, programToLaTeX, rewrittensToLatex)
import Lining (LineFormat (MULTILINE, SINGLELINE))
import Locator (locatedExpression)
import Logger
import Merge (merge)
import Misc (ensuredFile)
import qualified Misc
import Must (Must (..), validateMust)
import Options.Applicative
import Parser (parseExpressionThrows, parseProgramThrows)
import Paths_phino (version)
import qualified Printer as P
import Rewriter (RewriteContext (RewriteContext), Rewritten, rewrite)
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
  , compress :: Bool
  , meetPopularity :: Int
  , meetLength :: Int
  , focus :: Expression
  , expression :: Maybe String
  , label :: Maybe String
  , meetPrefix :: Maybe String
  , outputFormat :: IOFormat
  }

data CmdException
  = InvalidCLIArguments String
  | CouldNotReadFromStdin String
  | CouldNotDataize
  deriving (Exception)

instance Show CmdException where
  show (InvalidCLIArguments msg) = printf "Invalid set of arguments: %s" msg
  show (CouldNotReadFromStdin msg) = printf "Could not read input from stdin\nReason: %s" msg
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
  , logLines :: Int
  , inputFormat :: IOFormat
  , outputFormat :: IOFormat
  , sugarType :: SugarType
  , flat :: LineFormat
  , omitListing :: Bool
  , omitComments :: Bool
  , nonumber :: Bool
  , sequence :: Bool
  , canonize :: Bool
  , depthSensitive :: Bool
  , quiet :: Bool
  , compress :: Bool
  , maxDepth :: Int
  , maxCycles :: Int
  , meetPopularity :: Maybe Int
  , meetLength :: Maybe Int
  , hide :: [String]
  , show' :: [String]
  , locator :: String
  , focus :: String
  , expression :: Maybe String
  , label :: Maybe String
  , meetPrefix :: Maybe String
  , stepsDir :: Maybe FilePath
  , inputFile :: Maybe FilePath
  }

data OptsExplain = OptsExplain
  { logLevel :: LogLevel
  , logLines :: Int
  , rules :: [FilePath]
  , normalize :: Bool
  , shuffle :: Bool
  , targetFile :: Maybe FilePath
  }

data OptsRewrite = OptsRewrite
  { logLevel :: LogLevel
  , logLines :: Int
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
  , compress :: Bool
  , maxDepth :: Int
  , maxCycles :: Int
  , meetPopularity :: Maybe Int
  , meetLength :: Maybe Int
  , rules :: [FilePath]
  , hide :: [String]
  , show' :: [String]
  , locator :: String
  , focus :: String
  , expression :: Maybe String
  , label :: Maybe String
  , meetPrefix :: Maybe String
  , targetFile :: Maybe FilePath
  , stepsDir :: Maybe FilePath
  , inputFile :: Maybe FilePath
  }

data OptsMerge = OptsMerge
  { logLevel :: LogLevel
  , logLines :: Int
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
  , logLines :: Int
  , sugarType :: SugarType
  , flat :: LineFormat
  , pattern :: Maybe String
  , when' :: Maybe String
  , inputFile :: Maybe FilePath
  }

validateIntOption :: (Int -> Bool) -> String -> Int -> ReadM Int
validateIntOption cmp msg num
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

optLogLines :: Parser Int
optLogLines =
  option
    (auto >>= validateIntOption (>= -1) "--log-lines must be >= -1")
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

optMaxDepth :: Parser Int
optMaxDepth =
  option
    (auto >>= validateIntOption (> 0) "--max-depth must be positive")
    (long "max-depth" <> metavar "DEPTH" <> help "Maximum number of rewriting iterations per rule" <> value 25 <> showDefault)

optMaxCycles :: Parser Int
optMaxCycles =
  option
    (auto >>= validateIntOption (> 0) "--max-cycles must be positive")
    (long "max-cycles" <> metavar "CYCLES" <> help "Maximum number of rewriting cycles across all rules" <> value 25 <> showDefault)

optMeetPopularity :: Parser (Maybe Int)
optMeetPopularity =
  optional
    ( option
        ( auto
            >>= validateIntOption (> 0) "--meet-popularity must be positive"
            >>= validateIntOption (< 100) "--meet-popularity must be <= 100"
        )
        (long "meet-popularity" <> metavar "PERCENTAGE" <> help "The minimum popularity of an expression in order to be suitable for \\phiMeet{}, in percentage (default: 50)")
    )

optMeetLength :: Parser (Maybe Int)
optMeetLength =
  optional
    ( option
        (auto >>= validateIntOption (> 0) "--meet-length must be positive")
        (long "meet-length" <> metavar "NODES" <> help "The minimum length of an expression that fits into \\phiMeet{}, in AST nodes (default: 8)")
    )

optDepthSensitive :: Parser Bool
optDepthSensitive = switch (long "depth-sensitive" <> help "Fail if rewriting is not finished after reaching max attempts (see --max-cycles or --max-depth)")

optNonumber :: Parser Bool
optNonumber = switch (long "nonumber" <> help "Turn off equation auto numbering in LaTeX rendering (see --output option)")

optSequence :: Parser Bool
optSequence = switch (long "sequence" <> help "Result output contains all intermediate 𝜑-programs concatenated with EOL")

optCanonize :: Parser Bool
optCanonize = switch (long "canonize" <> help "Rename all functions attached to λ binding with F1, F2, etc.")

optExpression :: Parser (Maybe String)
optExpression = optional (strOption (long "expression" <> metavar "NAME" <> help "Name for 'phiExpression' element when rendering to LaTeX (see --output option)"))

optLabel :: Parser (Maybe String)
optLabel = optional (strOption (long "label" <> metavar "NAME" <> help "Name for 'label' element when rendering to LaTeX (see --output option)"))

optMeetPrefix :: Parser (Maybe String)
optMeetPrefix = optional (strOption (long "meet-prefix" <> metavar "PREFIX" <> help "Prefix to be inserted before index in \\phiMeet{} and \\phiAgain{} LaTeX functions, e.g. \\phiMeet{foo:1}"))

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

optLocator :: Parser String
optLocator = strOption (long "locator" <> metavar "FQN" <> help "Location of object to dataize. Must be a valid dispatch expression; e.g. Q.foo.bar" <> value "Q" <> showDefault)

optFocus :: Parser String
optFocus =
  strOption
    ( long "focus"
        <> metavar "FQN"
        <> help "Location of only object to be printed in entire program. Must be a valid dispatch expression; e.g. Q.foo.bar"
        <> value "Q"
        <> showDefault
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

optCompress :: Parser Bool
optCompress = switch (long "compress" <> help "Compress expressions in LaTeX output using \\phiMeet{} and \\phiAgain{} functions")

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
            <*> optCanonize
            <*> optDepthSensitive
            <*> switch (long "quiet" <> help "Don't print the result of dataization")
            <*> optCompress
            <*> optMaxDepth
            <*> optMaxCycles
            <*> optMeetPopularity
            <*> optMeetLength
            <*> optHide
            <*> optShow
            <*> optLocator
            <*> optFocus
            <*> optExpression
            <*> optLabel
            <*> optMeetPrefix
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
            <*> optCanonize
            <*> optCompress
            <*> optMaxDepth
            <*> optMaxCycles
            <*> optMeetPopularity
            <*> optMeetLength
            <*> optRule
            <*> optHide
            <*> optShow
            <*> optLocator
            <*> optFocus
            <*> optExpression
            <*> optLabel
            <*> optMeetPrefix
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
      excluded <- validatedDispatches "hide" hide
      included <- validatedDispatches "show" show'
      [loc] <- validatedDispatches "locator" [locator]
      [foc] <- validatedDispatches "focus" [focus]
      logDebug (printf "Amount of rewriting cycles across all the rules: %d, per rule: %d" maxCycles maxDepth)
      input <- readInput inputFile
      rules' <- getRules normalize shuffle rules
      program <- parseProgram input inputFormat
      let listing = if null rules' then const input else (\prog -> P.printProgram' prog (sugarType, UNICODE, flat))
          xmirCtx = XmirContext omitListing omitComments listing
          printCtx = printProgCtx xmirCtx foc
          _canonize = if canonize then C.canonize else id
          _hide = (`F.exclude` excluded)
          _show = (`F.include` included)
      rewrittens <- rewrite program rules' (context loc printCtx) <&> _canonize . _hide . _show
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
          validateLatexOptions
            outputFormat
            [(nonumber, "nonumber"), (compress, "compress")]
            [(expression, "expression"), (label, "label"), (meetPrefix, "meet-prefix")]
            [(meetPopularity, "meet-popularity"), (meetLength, "meet-length")]
          validateMust' must
          validateXmirOptions outputFormat [(omitListing, "omit-listing"), (omitComments, "omit-comments")] focus
        output :: Maybe FilePath -> String -> IO ()
        output target prog = case (inPlace, target, inputFile) of
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
        context loc ctx =
          RewriteContext
            loc
            maxDepth
            maxCycles
            depthSensitive
            buildTerm
            must
            (saveStepFunc stepsDir ctx)
        printProgCtx :: XmirContext -> Expression -> PrintProgramContext
        printProgCtx xmirCtx focus =
          PrintProgCtx
            sugarType
            flat
            xmirCtx
            nonumber
            compress
            (justMeetPopularity meetPopularity)
            (justMeetLength meetLength)
            focus
            expression
            label
            meetPrefix
            outputFormat
    CmdDataize OptsDataize{..} -> do
      validateOpts
      excluded <- validatedDispatches "hide" hide
      included <- validatedDispatches "show" show'
      [loc] <- validatedDispatches "locator" [locator]
      [foc] <- validatedDispatches "focus" [focus]
      input <- readInput inputFile
      prog <- parseProgram input inputFormat
      let printCtx = printProgCtx foc
          _canonize = if canonize then C.canonize else id
          _hide = (`F.exclude` excluded)
          _show = (`F.include` included)
      (maybeBytes, seq) <- dataize (context loc prog printCtx)
      when sequence (printRewrittens printCtx (_canonize $ _hide $ _show seq) >>= putStrLn)
      unless quiet (putStrLn (maybe (P.printExpression ExTermination) P.printBytes maybeBytes))
      where
        validateOpts :: IO ()
        validateOpts = do
          validateLatexOptions
            outputFormat
            [(nonumber, "nonumber"), (compress, "compress")]
            [(expression, "expression"), (label, "label"), (meetPrefix, "meet-prefix")]
            [(meetPopularity, "meet-popularity"), (meetLength, "meet-length")]
          validateXmirOptions outputFormat [(omitListing, "omit-listing"), (omitComments, "omit-comments")] focus
          when (length show' > 1) (invalidCLIArguments "The option --show can be used only once")
        context :: Expression -> Program -> PrintProgramContext -> DataizeContext
        context loc prog ctx =
          DataizeContext
            loc
            prog
            maxDepth
            maxCycles
            depthSensitive
            buildTerm
            (saveStepFunc stepsDir ctx)
        printProgCtx :: Expression -> PrintProgramContext
        printProgCtx focus =
          PrintProgCtx
            sugarType
            flat
            defaultXmirContext
            nonumber
            compress
            (justMeetPopularity meetPopularity)
            (justMeetLength meetLength)
            focus
            expression
            label
            meetPrefix
            outputFormat
    CmdExplain OptsExplain{..} -> do
      validateOpts
      rules' <- getRules normalize shuffle rules
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
          printCtx = printProgCtx xmirCtx
      prog' <- printProgram printCtx prog
      output targetFile prog'
      where
        validateOpts :: IO ()
        validateOpts = do
          when
            (null inputs)
            (throwIO (InvalidCLIArguments "At least one input file must be specified for 'merge' command"))
          validateXmirOptions outputFormat [(omitListing, "omit-listing"), (omitComments, "omit-comments")] "Q"
        printProgCtx :: XmirContext -> PrintProgramContext
        printProgCtx xmirCtx =
          PrintProgCtx
            sugarType
            flat
            xmirCtx
            False
            False
            (justMeetPopularity Nothing)
            (justMeetLength Nothing)
            ExGlobal
            Nothing
            Nothing
            Nothing
            outputFormat
    CmdMatch OptsMatch{..} -> do
      input <- readInput inputFile
      prog <- parseProgram input PHI
      if isNothing pattern
        then logDebug "The --pattern is not provided, no substitutions are built"
        else do
          ptn <- parseExpressionThrows (fromJust pattern)
          condition <- traverse parseConditionThrows when'
          substs <- matchProgramWithRule prog (rule ptn condition) (RuleContext buildTerm)
          if null substs
            then logDebug "Provided pattern was not matched, no substitutions are built"
            else putStrLn (P.printSubsts' substs (sugarType, UNICODE, flat))
      where
        rule :: Expression -> Maybe Y.Condition -> Y.Rule
        rule ptn cnd = Y.Rule Nothing Nothing ptn ExGlobal cnd Nothing Nothing

justMeetPopularity :: Maybe Int -> Int
justMeetPopularity = fromMaybe 50

justMeetLength :: Maybe Int -> Int
justMeetLength = fromMaybe 8

-- Prepare saveStepFunc
saveStepFunc :: Maybe FilePath -> PrintProgramContext -> SaveStepFunc
saveStepFunc stepsDir ctx@PrintProgCtx{..} = saveStep stepsDir ioToExt (printProgram ctx)
  where
    ioToExt :: String
    ioToExt
      | outputFormat == LATEX = "tex"
      | otherwise = show outputFormat

-- Validate given expressions as valid dispatches
validatedDispatches :: String -> [String] -> IO [Expression]
validatedDispatches opt = traverse (parseExpressionThrows >=> asDispatch)
  where
    asDispatch :: Expression -> IO Expression
    asDispatch expr = asDispatch' expr
      where
        asDispatch' :: Expression -> IO Expression
        asDispatch' exp@ExGlobal = pure exp
        asDispatch' exp@(ExDispatch ex _) = asDispatch' ex >> pure exp
        asDispatch' _ =
          invalidCLIArguments
            ( printf
                "Only dispatch expression started with Φ (or Q) can be used in --%s, but given: %s"
                opt
                (P.printExpression' expr P.logPrintConfig)
            )

-- Validate LaTeX options
validateLatexOptions :: IOFormat -> [(Bool, String)] -> [(Maybe String, String)] -> [(Maybe Int, String)] -> IO ()
validateLatexOptions LATEX _ _ _ = pure ()
validateLatexOptions _ bools strings ints = do
  let (bools', opts) = unzip bools
      msg = "The --%s option can stay together with --output=latex only"
      callback (maybe', opt) = when (isJust maybe') (invalidCLIArguments (printf msg opt))
  validateBoolOpts (zip bools' (map (printf msg) opts))
  forM_ strings callback
  forM_ ints callback

-- Validate 'must' option
validateMust' :: Must -> IO ()
validateMust' must = for_ (validateMust must) invalidCLIArguments

-- Validate options for output to XMIR
validateXmirOptions :: IOFormat -> [(Bool, String)] -> String -> IO ()
validateXmirOptions XMIR _ focus = when (focus /= "Q") (invalidCLIArguments "Only --focus=Q is allowed to be used with --output=xmir")
validateXmirOptions _ bools _ =
  let (bools', opts) = unzip bools
   in validateBoolOpts (zip bools' (map (printf "The --%s can be used only with --output=xmir") opts))

validateBoolOpts :: [(Bool, String)] -> IO ()
validateBoolOpts bools = forM_ bools (\(bool, msg) -> when bool (invalidCLIArguments msg))

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
parseProgram _ LATEX = invalidCLIArguments "LaTeX cannot be used as input format"

printRewrittens :: PrintProgramContext -> [Rewritten] -> IO String
printRewrittens ctx@PrintProgCtx{..} rewrittens
  | outputFormat == LATEX = rewrittensToLatex rewrittens (LatexContext sugar line nonumber compress meetPopularity meetLength focus expression label meetPrefix)
  | focus == ExGlobal = mapM (printProgram ctx . fst) rewrittens <&> intercalate "\n"
  | otherwise = mapM (\(prog, _) -> locatedExpression focus prog >>= printExpression ctx) rewrittens <&> intercalate "\n"

printExpression :: PrintProgramContext -> Expression -> IO String
printExpression PrintProgCtx{..} ex = case outputFormat of
  PHI -> pure (P.printExpression' ex (sugar, UNICODE, line))
  XMIR -> programToXMIR (Program ex) xmirCtx <&> printXMIR
  LATEX -> pure (expressionToLaTeX ex (LatexContext sugar line nonumber compress meetPopularity meetLength focus expression label meetPrefix))

-- Convert program to corresponding String format
printProgram :: PrintProgramContext -> Program -> IO String
printProgram PrintProgCtx{..} prog = case outputFormat of
  PHI -> pure (P.printProgram' prog (sugar, UNICODE, line))
  XMIR -> programToXMIR prog xmirCtx <&> printXMIR
  LATEX -> pure (programToLaTeX prog (LatexContext sugar line nonumber compress meetPopularity meetLength focus expression label meetPrefix))

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
    logDebug (printf "The command result was saved in '%s'" file)
