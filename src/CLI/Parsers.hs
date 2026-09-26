-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI.Parsers where

import CLI.Types
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Version (showVersion)
import Deps (Acyclic, certainty)
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
        <> help ("Log level (" <> intercalate ", " (map show [DEBUG, INFO, ERROR, NONE]) <> ")")
        <> value ERROR
        <> showDefault
    )
  where
    parseLogLevel :: ReadM LogLevel
    parseLogLevel = eitherReader $ \lvl -> case map toUpper lvl of
      "DEBUG" -> Right DEBUG
      "INFO" -> Right INFO
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
optInputFormat = option (parseIOFormat "input") (long "input" <> metavar "FORMAT" <> help "Expression input format (phi, xmir)" <> value PHI <> showDefault)

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
    (long "output" <> metavar "FORMAT" <> help (printf "Result and intermediate (see %s option(s)) expressions output format (phi, xmir, latex)" _intermediateOptions) <> value PHI <> showDefault)

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

optMaxSteps :: Parser Int
optMaxSteps =
  option
    (auto >>= validateIntOption (> 0) "--max-steps must be positive")
    (long "max-steps" <> metavar "STEPS" <> help "Maximum number of nested morphing and dataization steps" <> value 1000 <> showDefault)

optMaxFirings :: Parser (Maybe Int)
optMaxFirings =
  optional
    ( option
        (auto >>= validateIntOption (> 0) "--max-firings must be positive")
        (long "max-firings" <> metavar "FIRINGS" <> help "Maximum number of λ functions the whole run may fire, unlimited unless given")
    )

-- Every use of a binding copies the term bound to it, so a program reading one
-- binding in five places meets one formation five times, and fires it five
-- times over, minting five symbols for one answer. This keeps what a firing
-- answered, for the next firing of the same formation (see 'Memo').
optMemo :: Parser Bool
optMemo = switch (long "memo" <> help "Fire every formation once: what 𝔼 answered a formation is kept for the whole run, and a later firing of the same formation, ρ included, takes the answer, with the very symbol the first firing minted, instead of firing it again")

optMargin :: Parser Int
optMargin =
  option
    (auto >>= validateIntOption (> 0) "--margin must be positive")
    (long "margin" <> help "The maximum right margin for the printed 𝜑-expressions" <> value defaultMargin <> showDefault)

optMeetPopularity :: Parser (Maybe Int)
optMeetPopularity =
  optional
    ( option
        ( auto
            >>= validateIntOption (> 0) "--meet-popularity must be positive"
            >>= validateIntOption (<= 100) "--meet-popularity must be <= 100"
        )
        ( long "meet-popularity"
            <> metavar "PERCENTAGE"
            <> help (printf "The minimum popularity of an expression in order to be suitable for \\phinoMeet{}, in percentage (default: %d)" defaultMeetPopularity)
        )
    )

optMeetLength :: Parser (Maybe Int)
optMeetLength =
  optional
    ( option
        (auto >>= validateIntOption (> 0) "--meet-length must be positive")
        ( long "meet-length"
            <> metavar "NODES"
            <> help (printf "The minimum length of an expression that fits into \\phinoMeet{}, in AST nodes (default: %d)" defaultMeetLength)
        )
    )

optDepthSensitive :: Parser Bool
optDepthSensitive = switch (long "depth-sensitive" <> help "Fail if rewriting is not finished after reaching max attempts (see --max-cycles or --max-depth)")

optNonumber :: Parser Bool
optNonumber = switch (long "nonumber" <> help "Turn off equation auto numbering in LaTeX rendering (see --output option)")

optSequence :: Parser Bool
optSequence = switch (long "sequence" <> help "Result output contains all intermediate 𝜑-expressions concatenated with EOL")

optHeaders :: Parser Bool
optHeaders =
  switch
    ( long "headers"
        <> help "Prefix every intermediate step (see --sequence) with a header line: step number, the rule that produced it, and AST node count before and after that rule"
    )

optCanonize :: Parser Bool
optCanonize = switch (long "canonize" <> help "Rename all functions attached to λ binding with Fn1, Fn2, etc.")

optExpression :: Parser (Maybe String)
optExpression = optional (strOption (long "expression" <> metavar "NAME" <> help "Name for 'phiExpression' element when rendering to LaTeX (see --output option)"))

optLabel :: Parser (Maybe String)
optLabel = optional (strOption (long "label" <> metavar "NAME" <> help "Name for 'label' element when rendering to LaTeX (see --output option)"))

optMeetPrefix :: Parser (Maybe String)
optMeetPrefix = optional (strOption (long "meet-prefix" <> metavar "PREFIX" <> help "Prefix to be inserted before index in \\phinoMeet{} and \\phinoAgain{} LaTeX functions, e.g. \\phinoMeet{foo:1}"))

optBreakpoint :: Parser (Maybe FilePath)
optBreakpoint = optional (strOption (long "breakpoint" <> metavar "FILE" <> help "The name of the first unmatched rule which leads to stopping entire rewriting process and returning original expression"))

optHide :: Parser [String]
optHide =
  many
    ( strOption
        ( long "hide"
            <> metavar "FQN"
            <> help "Location of object to exclude from result and intermediate expressions after rewriting. Must be a valid dispatch expression; e.g. Q.org.eolang"
        )
    )

optShow :: Parser [String]
optShow =
  many
    ( strOption
        ( long "show"
            <> metavar "FQN"
            <> help
              "Location of object to include to result and intermediate expressions after rewriting. \
              \Must be a valid dispatch expression; e.g. Q.org.eolang. Unlike --hide, can be used only once"
        )
    )

optLocator :: Parser String
optLocator = strOption (long "locator" <> metavar "FQN" <> help "Location of object to rewrite, dataize or morph. Must be a valid dispatch expression; e.g. Q.foo.bar" <> value "Q" <> showDefault)

optFocus :: Parser String
optFocus =
  strOption
    ( long "focus"
        <> metavar "FQN"
        <> help "Location of only object to be printed in entire expression. Must be a valid dispatch expression; e.g. Q.foo.bar"
        <> value "Q"
        <> showDefault
    )

optNormalize :: Parser Bool
optNormalize = switch (long "normalize" <> help "Use built-in normalization rules")

optMorph :: Parser Bool
optMorph = switch (long "morph" <> help "Explain built-in morphing rules")

optDataize :: Parser Bool
optDataize = switch (long "dataize" <> help "Explain built-in dataization rules")

optContextualize :: Parser Bool
optContextualize = switch (long "contextualize" <> help "Explain built-in contextualization rules")

optTarget :: Parser (Maybe FilePath)
optTarget = optional (strOption (long "target" <> short 't' <> metavar "FILE" <> help "File to save output to"))

optStepsDir :: Parser (Maybe FilePath)
optStepsDir = optional (strOption (long "steps-dir" <> metavar "FILE" <> help "Directory to save intermediate steps during rewriting/dataizing"))

optPartial :: Parser Bool
optPartial = switch (long "partial" <> help "Partial evaluation: compute what the known inputs decide and, instead of failing on a λ function that cannot fire (no entry of the --symbolic file answers it), leave it in place and print the residual 𝜑-program")

-- 𝕄 stops at the first formation it reaches and hands its bindings back as
-- they were written, so what a program holds but nothing demands is never
-- reduced. This walks into them (see 'deepened').
optDeep :: Parser Bool
optDeep = switch (long "deep" <> help "Don't stop at the first formation: enter its bindings too, recursively, firing every λ function the --symbolic file answers and standing its answer in the place of what it computed, while everything else stays as it was written")

-- The step budget is otherwise the only thing that ends the 𝕄 and 𝔻 recursion,
-- so a λ function answering with a firing of itself, or an object dataized
-- through a body that comes back to itself, runs to the limit before it fails.
-- This stops it the moment it enters a formation it is already inside, by the
-- mode the option names, since no mode is right for every run (see 'entering').
optAcyclic :: Parser (Maybe Acyclic)
optAcyclic = optional (option parseAcyclic (long "acyclic" <> metavar "MODE" <> help "Stop reducing as soon as the reduction enters a formation it is already inside (fires its λ function or dataizes its φ body again) instead of going round until --max-steps runs out, and leave the term in place the way --partial leaves a λ function that cannot fire; 'proven' takes it for the same one up to a renaming of symbols, 'plausible' also when it holds the earlier one under wrappers it gained, such as a growing accumulator"))
  where
    parseAcyclic :: ReadM Acyclic
    parseAcyclic = eitherReader $ \mode -> case filter ((== map toLower mode) . certainty) [minBound .. maxBound] of
      found : _ -> Right found
      [] -> Left (printf "The value '%s' can't be used for '--acyclic' option, use --help to check possible values" mode)

-- Which λ functions this run may fire. phino implements none of them itself
-- (see 'Lambdas'), so without this option every λ function a program names gets
-- stuck.
optSymbolic :: Parser (Maybe FilePath)
optSymbolic =
  optional
    ( strOption
        ( long "symbolic"
            <> metavar "FILE"
            <> help
              "Path to the YAML file of λ functions this run may fire, each entry keyed by a regular expression \
              \over λ names under \"λ\", naming the operands it brings down to data under \"dataize\", the ones \
              \it reduces to a normal form under \"morph\" and the terms of those it stands the data of into \
              \unknowns under \"symbolize\", and answering with the term under \"𝑛\""
        )
    )

-- The external face of the trick phino plays internally to reduce a
-- sub-expression against a universe: prepend a synthetic binding holding it to
-- that universe and aim the locator at the binding. It is the same trick a λ
-- function's operands are reduced with (see 'insideUniverse' in 'Morph'), made
-- available to whoever asks phino to reduce a term that is not part of the
-- program.
optInside :: Parser (Maybe String)
optInside =
  optional
    ( strOption
        ( long "inside"
            <> metavar "EXPRESSION"
            <> help
              "The 𝜑-expression to dataize or morph inside the input expression, which is taken as the universe \
              \Φ: a synthetic binding holding it is prepended to the universe and the locator is aimed at that \
              \binding. Cannot be used together with --locator"
        )
    )

optProtocol :: Parser (Maybe FilePath)
optProtocol =
  optional
    ( strOption
        ( long "protocol"
            <> metavar "FILE"
            <> help
              "File to record every λ function fired during the run: the run at the top, one block per firing \
              \under it, and inside each block the operands it bound and the term it answered with, with a \
              \firing nested in the reduction of an operand one level deeper again. The name of the file \
              \decides the format: '.xml' writes XML, every other name writes the same tree as indented text"
        )
    )

optAbridged :: Parser Bool
optAbridged =
  switch
    ( long "abridged"
        <> help
          "Shorten every 𝜑-expression written to the --protocol file: a formation longer than sixty characters \
          \keeps its φ, Δ and λ bindings and folds the rest into a count, as '+34 attrs', and a byte string \
          \longer than eight bytes keeps its first four bytes and its length, as '00-00-00-00-...(45b)'"
    )

optShuffle :: Parser Bool
optShuffle = switch (long "shuffle" <> help "Shuffle rules before applying")

optSeed :: Parser Int
optSeed =
  option
    auto
    ( long "seed"
        <> metavar "SEED"
        <> help "Seed for the random generator that mints fresh synthetic names, making output reproducible across runs"
        <> value 0
        <> showDefault
    )

optSugar :: Parser SugarType
optSugar = flag SALTY SWEET (long "sweet" <> help (printf "Print result and intermediate (see %s option(s)) 𝜑-expressions using syntax sugar" _intermediateOptions))

optHideRho :: Parser Bool
optHideRho = switch (long "hide-rho" <> help "Remove every ρ binding from result and intermediate 𝜑-expressions for cleaner output")

optSugar' :: Parser SugarType
optSugar' = flag SALTY SWEET (long "sweet" <> help "Print result 𝜑-expression using syntax sugar")

optLineFormat :: Parser LineFormat
optLineFormat = flag MULTILINE SINGLELINE (long "flat" <> help (printf "Print result and intermediate (see %s option(s)) 𝜑-expressions in one line" _intermediateOptions))

optLineFormat' :: Parser LineFormat
optLineFormat' = flag MULTILINE SINGLELINE (long "flat" <> help "Print result 𝜑-expression in one line")

optMust :: Parser Must
optMust =
  option
    auto
    ( long "must"
        <> metavar "RANGE"
        <> help "Must-rewrite range (e.g., '3', '..5', '3..', '3..5'). Stops execution if number of rules applied is not in range. Use 0 to disable."
        <> value MtDisabled
        <> showDefaultWith show
    )

optOmitListing :: Parser Bool
optOmitListing = switch (long "omit-listing" <> help "Omit full expression listing in XMIR output")

optOmitComments :: Parser Bool
optOmitComments = switch (long "omit-comments" <> help "Omit comments in XMIR output")

optCompress :: Parser Bool
optCompress = switch (long "compress" <> help "Compress expressions in LaTeX output using \\phinoMeet{} and \\phinoAgain{} functions")

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
            <*> optMorph
            <*> optDataize
            <*> optContextualize
            <*> optShuffle
            <*> optSeed
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
            <*> optHideRho
            <*> optLineFormat
            <*> optOmitListing
            <*> optOmitComments
            <*> optNonumber
            <*> optSequence
            <*> optHeaders
            <*> optCanonize
            <*> optDepthSensitive
            <*> optShuffle
            <*> optSeed
            <*> switch (long "quiet" <> help "Don't print the result of dataization")
            <*> optPartial
            <*> optAcyclic
            <*> optCompress
            <*> optMaxDepth
            <*> optMaxCycles
            <*> optMaxSteps
            <*> optMaxFirings
            <*> optMemo
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
            <*> optInside
            <*> optStepsDir
            <*> optProtocol
            <*> optAbridged
            <*> optSymbolic
            <*> argInputFile
        )

morphParser :: Parser Command
morphParser =
  CmdMorph
    <$> ( OptsMorph
            <$> optLogLevel
            <*> optLogLines
            <*> optInputFormat
            <*> optOutputFormat
            <*> optSugar
            <*> optHideRho
            <*> optLineFormat
            <*> optOmitListing
            <*> optOmitComments
            <*> optNonumber
            <*> optSequence
            <*> optHeaders
            <*> optCanonize
            <*> optDepthSensitive
            <*> optShuffle
            <*> optSeed
            <*> switch (long "quiet" <> help "Don't print the result of morphing")
            <*> optPartial
            <*> optDeep
            <*> optAcyclic
            <*> optCompress
            <*> optMaxDepth
            <*> optMaxCycles
            <*> optMaxSteps
            <*> optMaxFirings
            <*> optMemo
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
            <*> optInside
            <*> optStepsDir
            <*> optProtocol
            <*> optAbridged
            <*> optSymbolic
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
            <*> optHideRho
            <*> optLineFormat
            <*> optMust
            <*> optNormalize
            <*> optShuffle
            <*> optSeed
            <*> optOmitListing
            <*> optOmitComments
            <*> optDepthSensitive
            <*> optNonumber
            <*> switch (long "in-place" <> help "Edit file in-place instead of printing to output")
            <*> switch (long "update" <> help "Skip rewriting if --target file is newer than the input file")
            <*> optSequence
            <*> optHeaders
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
            <*> optBreakpoint
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
            <*> option (parseIOFormat "output") (long "output" <> metavar "FORMAT" <> help (printf "Result expression output format (phi, xmir, latex)") <> value PHI <> showDefault)
            <*> optSugar'
            <*> optLineFormat'
            <*> optOmitListing
            <*> optOmitComments
            <*> optMargin
            <*> optTarget
            <*> many (argument str (metavar "[FILE]" <> help "Paths to input files"))
            <*> optSeed
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
            <*> optSeed
        )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "rewrite" (info rewriteParser (progDesc "Rewrite the 𝜑-expression"))
        <> command "dataize" (info dataizeParser (progDesc "Dataize the 𝜑-expression"))
        <> command "morph" (info morphParser (progDesc "Morph the 𝜑-expression"))
        <> command "explain" (info explainParser (progDesc "Explain rules in LaTeX format"))
        <> command "merge" (info mergeParser (progDesc "Merge 𝜑-expressions into single one by merging their top level formations"))
        <> command "match" (info matchParser (progDesc "Match 𝜑-expression against provided pattern and build matched substitutions"))
    )

optPin :: Parser (Maybe Pin)
optPin = optional (PinVersion <$> literal <|> PinFile <$> file)
  where
    literal :: Parser String
    literal =
      strOption
        ( long "pin"
            <> metavar "VERSION"
            <> help "Fail if this version doesn't match the version of phino"
        )
    file :: Parser FilePath
    file =
      strOption
        ( long "pin-file"
            <> metavar "FILE"
            <> help "Fail if the version written in this file doesn't match the version of phino"
        )

cliArgsParser :: Parser CliArgs
cliArgsParser = CliArgs <$> optPin <*> commandParser

parserInfo :: ParserInfo CliArgs
parserInfo =
  info
    (cliArgsParser <**> helper <**> simpleVersioner (showVersion version))
    (fullDesc <> header "Phino - CLI Manipulator of 𝜑-Calculus Expressions")
