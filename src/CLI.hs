{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI (runCLI) where

import Ast (Program (Program), Expression(..), Binding(..))
import Control.Exception (Exception (displayException), SomeException, handle, throw, throwIO)
import Control.Exception.Base
import Control.Monad (when)
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Version (showVersion)
import Dataize (DataizeContext (DataizeContext), dataize)
import Functions (buildTerm)
import qualified Functions
import Logger
import Matcher (matchProgram, matchExpressionDeep, matchBinding, defaultScope, Subst(..), MetaValue(..))
import Misc (ensuredFile)
import qualified Misc
import Options.Applicative
import Parser (parseProgramThrows)
import Paths_phino (version)
import Pretty (PrintMode (SALTY, SWEET), prettyBytes, prettyProgram')
import Rewriter (RewriteContext (RewriteContext), rewrite')
import System.Exit (ExitCode (..), exitFailure)
import System.IO (getContents')
import Text.Printf (printf)
import XMIR (XmirContext (XmirContext), parseXMIRThrows, printXMIR, programToXMIR, xmirToPhi)
import Yaml (normalizationRules, parseConditionString, Condition)
import qualified Yaml as Y
import Parser (parseExpression, parseBinding)
import Pretty (prettyExpression, prettyExpression', prettyBinding)
import Rule (RuleContext(..), meetCondition)
import Builder (buildExpression, buildBinding)
import qualified Data.Map.Strict as Map

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
  | CmdMatch OptsMatch

data IOFormat = XMIR | PHI
  deriving (Eq)

instance Show IOFormat where
  show XMIR = "xmir"
  show PHI = "phi"

data OptsDataize = OptsDataize
  { logLevel :: LogLevel,
    inputFormat :: IOFormat,
    maxDepth :: Integer,
    maxCycles :: Integer,
    depthSensitive :: Bool,
    inputFile :: Maybe FilePath
  }

data OptsMatch = OptsMatch
  { logLevel :: LogLevel,
    pattern :: String,
    whenCondition :: Maybe String,
    inputFormat :: IOFormat,
    inputFile :: Maybe FilePath
  }

data OptsRewrite = OptsRewrite
  { logLevel :: LogLevel,
    rules :: [FilePath],
    inputFormat :: IOFormat,
    outputFormat :: IOFormat,
    printMode :: PrintMode,
    normalize :: Bool,
    nothing :: Bool,
    shuffle :: Bool,
    omitListing :: Bool,
    omitComments :: Bool,
    depthSensitive :: Bool,
    must :: Integer,
    maxDepth :: Integer,
    maxCycles :: Integer,
    targetFile :: Maybe FilePath,
    inputFile :: Maybe FilePath
  }

parseIOFormat :: String -> ReadM IOFormat
parseIOFormat type' = eitherReader $ \format -> case map toLower format of
  "xmir" -> Right XMIR
  "phi" -> Right PHI
  _ -> Left (printf "invalid %s format: expected '%s' or '%s'" type' (show PHI) (show XMIR))

argInputFile :: Parser (Maybe FilePath)
argInputFile = optional (argument str (metavar "FILE" <> help "Path to input file"))

optMaxDepth :: Parser Integer
optMaxDepth = option auto (long "max-depth" <> metavar "DEPTH" <> help "Maximum number of rewriting iterations per rule" <> value 25 <> showDefault)

optMaxCycles :: Parser Integer
optMaxCycles = option auto (long "max-cycles" <> metavar "CYCLES" <> help "Maximum number of rewriting cycles across all rules" <> value 25 <> showDefault)

optInputFormat :: Parser IOFormat
optInputFormat = option (parseIOFormat "input") (long "input" <> metavar "FORMAT" <> help "Program input format (phi, xmir)" <> value PHI <> showDefault)

optDepthSensitive :: Parser Bool
optDepthSensitive = switch (long "depth-sensitive" <> help "Fail if rewriting is not finished after reaching max attempts (see --max-cycles or --max-depth)")

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

dataizeParser :: Parser Command
dataizeParser =
  CmdDataize
    <$> ( OptsDataize
            <$> optLogLevel
            <*> optInputFormat
            <*> optMaxDepth
            <*> optMaxCycles
            <*> optDepthSensitive
            <*> argInputFile
        )

matchParser :: Parser Command
matchParser =
  CmdMatch
    <$> ( OptsMatch
            <$> optLogLevel
            <*> strOption (long "pattern" <> metavar "PATTERN" <> help "Pattern to match (supports meta-variables like !a, !e)")
            <*> optional (strOption (long "when" <> metavar "CONDITION" <> help "Optional condition to filter matches"))
            <*> optInputFormat
            <*> argInputFile
        )

rewriteParser :: Parser Command
rewriteParser =
  CmdRewrite
    <$> ( OptsRewrite
            <$> optLogLevel
            <*> many (strOption (long "rule" <> metavar "FILE" <> help "Path to custom rule"))
            <*> optInputFormat
            <*> option (parseIOFormat "output") (long "output" <> metavar "FORMAT" <> help "Program output format (phi, xmir)" <> value PHI <> showDefault)
            <*> flag SALTY SWEET (long "sweet" <> help "Print 𝜑-program using syntax sugar")
            <*> switch (long "normalize" <> help "Use built-in normalization rules")
            <*> switch (long "nothing" <> help "Just desugar provided 𝜑-program")
            <*> switch (long "shuffle" <> help "Shuffle rules before applying")
            <*> switch (long "omit-listing" <> help "Omit full program listing in XMIR output")
            <*> switch (long "omit-comments" <> help "Omit comments in XMIR output")
            <*> optDepthSensitive
            <*> ( flag' 1 (long "must" <> help "Enable must-rewrite with default value 1")
                    <|> option auto (long "must" <> metavar "N" <> help "Must-rewrite, stops execution if not exactly N rules applied (default 1 when specified without value, if 0 - flag is disabled)" <> value 0)
                )
            <*> optMaxDepth
            <*> optMaxCycles
            <*> optional (strOption (long "target" <> short 't' <> metavar "FILE" <> help "File to save output to"))
            <*> argInputFile
        )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "rewrite" (info rewriteParser (progDesc "Rewrite the program"))
        <> command "dataize" (info dataizeParser (progDesc "Dataize the program"))
        <> command "match" (info matchParser (progDesc "Find patterns in the program"))
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
        CmdMatch OptsMatch {logLevel} -> logLevel
   in setLogLevel level

runCLI :: [String] -> IO ()
runCLI args = handle handler $ do
  cmd <- handleParseResult (execParserPure defaultPrefs parserInfo args)
  setLogLevel' cmd
  case cmd of
    CmdRewrite opts@OptsRewrite {..} -> do
      validateRewriteArguments opts
      logDebug (printf "Amount of rewriting cycles across all the rules: %d, per rule: %d" maxCycles maxDepth)
      input <- readInput inputFile
      rules' <- getRules
      program <- parseProgram input inputFormat
      rewritten <- rewrite' program rules' (RewriteContext program maxDepth maxCycles depthSensitive buildTerm must)
      logDebug (printf "Printing rewritten 𝜑-program as %s" (show outputFormat))
      prog <- printProgram rewritten outputFormat printMode input
      output prog
      where
        getRules :: IO [Y.Rule]
        getRules = do
          ordered <-
            if nothing
              then do
                logDebug "The --nothing option is provided, no rules are used"
                pure []
              else
                if normalize
                  then do
                    let rules' = normalizationRules
                    logDebug (printf "The --normalize option is provided, %d built-it normalization rules are used" (length rules'))
                    pure rules'
                  else
                    if null rules
                      then throwIO (InvalidRewriteArguments "no --rule, no --normalize, no --nothing are provided")
                      else do
                        logDebug (printf "Using rules from files: [%s]" (intercalate ", " rules))
                        yamls <- mapM ensuredFile rules
                        mapM Y.yamlRule yamls
          if shuffle
            then do
              logDebug "The --shuffle option is provided, rules are used in random order"
              Misc.shuffle ordered
            else pure ordered
        printProgram :: Program -> IOFormat -> PrintMode -> String -> IO String
        printProgram prog PHI mode _ = pure (prettyProgram' prog mode)
        printProgram prog XMIR _ listing = do
          xmir <- programToXMIR prog (XmirContext omitListing omitComments listing)
          pure (printXMIR xmir)
        output :: String -> IO ()
        output prog = case targetFile of
          Nothing -> do
            logDebug "The option '--target' is not specified, printing to console..."
            putStrLn prog
          Just file -> do
            logDebug (printf "The option '--target' is specified, printing to '%s'..." file)
            writeFile file prog
            logInfo (printf "The result program was saved in '%s'" file)
    CmdDataize opts@OptsDataize {..} -> do
      validateDataizeArguments opts
      input <- readInput inputFile
      prog <- parseProgram input inputFormat
      dataized <- dataize prog (DataizeContext prog maxDepth maxCycles depthSensitive buildTerm)
      maybe (throwIO CouldNotDataize) (putStrLn . prettyBytes) dataized
    CmdMatch OptsMatch {..} -> do
      input <- readInput inputFile
      prog <- parseProgram input inputFormat
      cond <- case whenCondition of
        Nothing -> pure Nothing
        Just condStr -> Just <$> parseConditionString condStr
      result <- performMatch' pattern cond prog SALTY
      putStr result
  where
    validateRewriteArguments :: OptsRewrite -> IO ()
    validateRewriteArguments OptsRewrite{..} = do
      when
        (printMode == SWEET && outputFormat == XMIR)
        (throwIO (InvalidRewriteArguments "The --sweet and --output=xmir can't stay together"))
      validateMaxDepth maxDepth
      validateMaxCycles maxCycles
      validateIntArgument must (< 0) "--must must be positive"
    validateDataizeArguments :: OptsDataize -> IO ()
    validateDataizeArguments OptsDataize{..} = do
      validateMaxDepth maxDepth
      validateMaxCycles maxCycles
    validateIntArgument :: Integer -> (Integer -> Bool) -> String -> IO ()
    validateIntArgument num cmp msg =
      when
        (cmp num)
        (throwIO (InvalidRewriteArguments msg))
    validateMaxDepth :: Integer -> IO ()
    validateMaxDepth depth = validateIntArgument depth (<= 0) "--max-depth must be positive"
    validateMaxCycles :: Integer -> IO ()
    validateMaxCycles cycles = validateIntArgument cycles (<= 0) "--max-cycles must be positive"
    readInput :: Maybe FilePath -> IO String
    readInput inputFile' = case inputFile' of
      Just pth -> do
        logDebug (printf "Reading from file: '%s'" pth)
        readFile =<< ensuredFile pth
      Nothing -> do
        logDebug "Reading from stdin"
        getContents' `catch` (\(e :: SomeException) -> throwIO (CouldNotReadFromStdin (show e)))
    performMatch' :: String -> Maybe Condition -> Program -> PrintMode -> IO String
    performMatch' patternStr maybeCond prog@(Program rootExpr) mode = do
      logDebug (printf "Searching for pattern: %s" patternStr)
      case parseExpression patternStr of
        Right expr -> do
          let substs = matchProgram expr prog
          logDebug (printf "Found %d matches for expression pattern" (length substs))
          filtered <- filterByCondition' maybeCond substs prog
          logDebug (printf "After filtering: %d matches" (length filtered))
          let exprs = extractMatchedExpressions expr filtered
          formatMatches exprs mode
        Left _ -> case parseBinding patternStr of
          Right binding -> do
            let substs = findBindingMatches binding rootExpr
            logDebug (printf "Found %d matches for binding pattern" (length substs))
            filtered <- filterByCondition' maybeCond (map snd substs) prog
            logDebug (printf "After filtering: %d matches" (length filtered))
            let matchedBindings = [bd | (bd, s) <- substs, s `elem` filtered]
            formatBindingMatches matchedBindings mode
          Left err -> throwIO $ userError $ "Invalid pattern: " ++ err
    findBindingMatches :: Binding -> Expression -> [(Binding, Subst)]
    findBindingMatches pattern expr = case expr of
      ExFormation bds ->
        let direct = [(bd, s) | bd <- bds, s <- matchBinding pattern bd defaultScope]
            nested = concatMap (searchInBinding pattern) bds
        in direct ++ nested
      ExDispatch e _ -> findBindingMatches pattern e
      ExApplication e bd -> 
        findBindingMatches pattern e ++ searchInBinding pattern bd
      _ -> []
      where
        searchInBinding :: Binding -> Binding -> [(Binding, Subst)]
        searchInBinding ptn (BiTau _ e) = findBindingMatches ptn e
        searchInBinding _ _ = []
    extractMatchedExpressions :: Expression -> [Subst] -> [Expression]
    extractMatchedExpressions pattern substs = 
      [expr | Subst m <- substs,
              MvExpression expr _ <- Map.elems m]
    filterByCondition' :: Maybe Condition -> [Subst] -> Program -> IO [Subst]
    filterByCondition' Nothing substs _ = pure substs
    filterByCondition' (Just cond) substs prog = do
      logDebug (printf "Filtering %d matches with condition" (length substs))
      let ctx = RuleContext prog buildTerm
      meetCondition cond substs ctx
    formatMatches :: [Expression] -> PrintMode -> IO String
    formatMatches [] _ = pure ""
    formatMatches exprs mode = do
      let formatted = map (\e -> case mode of
                                  SWEET -> prettyExpression' e
                                  SALTY -> prettyExpression e) exprs
      pure $ unlines formatted
    formatBindingMatches :: [Binding] -> PrintMode -> IO String
    formatBindingMatches [] _ = pure ""
    formatBindingMatches bds mode = do
      let formatted = map prettyBinding bds
      pure $ unlines formatted
    parseProgram :: String -> IOFormat -> IO Program
    parseProgram phi PHI = parseProgramThrows phi
    parseProgram xmir XMIR = do
      doc <- parseXMIRThrows xmir
      xmirToPhi doc
