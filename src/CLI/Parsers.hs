-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI.Parsers where

import CLI.Types
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Version (showVersion)
import LaTeX (defaultMeetLength, defaultMeetPopularity)
import Lining (LineFormat (..))
import Logger
import Margin (defaultMargin)
import Must
import Options.Applicative
import Paths_phino (version)
import Sugar (SugarType (..))
import Text.Printf (printf)

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
        <> help ("Log level (" <> intercalate ", " (map show [DEBUG, ERROR, NONE]) <> ")")
        <> value ERROR
        <> showDefault
    )
  where
    parseLogLevel :: ReadM LogLevel
    parseLogLevel = eitherReader $ \lvl -> case map toUpper lvl of
      "DEBUG" -> Right DEBUG
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

optMargin :: Parser Int
optMargin =
  option
    (auto >>= validateIntOption (> 0) "--margin must be positive")
    (long "margin" <> help "The maximum right margin for the printed 𝜑-programs or 𝜑-expressions" <> value defaultMargin <> showDefault)

optMeetPopularity :: Parser (Maybe Int)
optMeetPopularity =
  optional
    ( option
        ( auto
            >>= validateIntOption (> 0) "--meet-popularity must be positive"
            >>= validateIntOption (< 100) "--meet-popularity must be <= 100"
        )
        ( long "meet-popularity"
            <> metavar "PERCENTAGE"
            <> help (printf "The minimum popularity of an expression in order to be suitable for \\phiMeet{}, in percentage (default: %d)" defaultMeetPopularity)
        )
    )

optMeetLength :: Parser (Maybe Int)
optMeetLength =
  optional
    ( option
        (auto >>= validateIntOption (> 0) "--meet-length must be positive")
        ( long "meet-length"
            <> metavar "NODES"
            <> help (printf "The minimum length of an expression that fits into \\phiMeet{}, in AST nodes (default: %d)" defaultMeetLength)
        )
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
optHide =
  many
    ( strOption
        ( long "hide"
            <> metavar "FQN"
            <> help "Location of object to exclude from result and intermediate programs after rewriting. Must be a valid dispatch expression; e.g. Q.org.eolang"
        )
    )

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
            <*> optMargin
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
            <*> optMargin
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
            <*> optMargin
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
