{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLITypesSpec (spec) where

import AST (Expression (ExRoot))
import CLI.Types
import Control.Monad (forM_)
import Lining (LineFormat (MULTILINE, SINGLELINE))
import Logger (LogLevel (DEBUG))
import Must (Must (MtExact))
import Sugar (SugarType (SALTY, SWEET))
import Test.Hspec
import XMIR (XmirContext (XmirContext))

spec :: Spec
spec = do
  -- This codebase always destructures these records via RecordWildCards
  -- (e.g. 'OptsRewrite{..}'), never by calling a field's named accessor
  -- directly. HPC instruments every derived accessor as its own top-level
  -- declaration, so without a direct call by name each accessor is reported
  -- as uncovered by the line-based coverage metric regardless of how many
  -- tests exercise the record through pattern matching. The tests below call
  -- every accessor by name (via record-dot syntax, since several of these
  -- records share field names and a bare call like '_hideRho record' stays
  -- ambiguous even with DuplicateRecordFields) to close that gap.
  describe "PrintContext field accessors" $
    it "exposes every PrintContext field via its accessor" $ do
      let xmirCtx = XmirContext True False (const "listing")
          printCtx =
            PrintCtx
              { _sugar = SWEET
              , _hideRho = True
              , _line = SINGLELINE
              , _margin = 80
              , _xmirCtx = xmirCtx
              , _nonumber = True
              , _compress = True
              , _canonize = True
              , _sequence = True
              , _headers = True
              , _meetPopularity = 5
              , _meetLength = 10
              , _focus = ExRoot
              , _expression = Just "expr"
              , _label = Just "label"
              , _meetPrefix = Just "prefix"
              , _outputFormat = PHI
              }
      printCtx._sugar `shouldBe` SWEET
      printCtx._hideRho `shouldBe` True
      printCtx._line `shouldBe` SINGLELINE
      printCtx._margin `shouldBe` 80
      case printCtx._xmirCtx of
        XmirContext{} -> pure ()
      printCtx._nonumber `shouldBe` True
      printCtx._compress `shouldBe` True
      printCtx._canonize `shouldBe` True
      printCtx._sequence `shouldBe` True
      printCtx._headers `shouldBe` True
      printCtx._meetPopularity `shouldBe` 5
      printCtx._meetLength `shouldBe` 10
      printCtx._focus `shouldBe` ExRoot
      printCtx._expression `shouldBe` Just "expr"
      printCtx._label `shouldBe` Just "label"
      printCtx._meetPrefix `shouldBe` Just "prefix"
      printCtx._outputFormat `shouldBe` PHI

  describe "OptsDataize field accessors" $
    it "exposes every OptsDataize field via its accessor" $ do
      let opts =
            OptsDataize
              { _logLevel = DEBUG
              , _logLines = 25
              , _inputFormat = PHI
              , _outputFormat = XMIR
              , _sugarType = SWEET
              , _hideRho = True
              , _flat = MULTILINE
              , _omitListing = True
              , _omitComments = True
              , _nonumber = True
              , _sequence = True
              , _headers = True
              , _canonize = True
              , _depthSensitive = True
              , _shuffle = True
              , _seed = 42
              , _quiet = True
              , _compress = True
              , _maxDepth = 100
              , _maxCycles = 50
              , _maxSteps = 200
              , _margin = 80
              , _meetPopularity = Just 3
              , _meetLength = Just 4
              , _hide = ["x"]
              , _show = ["y"]
              , _locator = "Q"
              , _focus = "Q"
              , _expression = Just "expr"
              , _label = Just "label"
              , _meetPrefix = Just "prefix"
              , _stepsDir = Just "steps"
              , _evaluations = Just "evaluations.jsonl"
              , _inputFile = Just "input.phi"
              }
      opts._logLevel `shouldBe` DEBUG
      opts._logLines `shouldBe` 25
      opts._inputFormat `shouldBe` PHI
      opts._outputFormat `shouldBe` XMIR
      opts._sugarType `shouldBe` SWEET
      opts._hideRho `shouldBe` True
      opts._flat `shouldBe` MULTILINE
      opts._omitListing `shouldBe` True
      opts._omitComments `shouldBe` True
      opts._nonumber `shouldBe` True
      opts._sequence `shouldBe` True
      opts._headers `shouldBe` True
      opts._canonize `shouldBe` True
      opts._depthSensitive `shouldBe` True
      opts._shuffle `shouldBe` True
      opts._seed `shouldBe` 42
      opts._quiet `shouldBe` True
      opts._compress `shouldBe` True
      opts._maxDepth `shouldBe` 100
      opts._maxCycles `shouldBe` 50
      opts._maxSteps `shouldBe` 200
      opts._margin `shouldBe` 80
      opts._meetPopularity `shouldBe` Just 3
      opts._meetLength `shouldBe` Just 4
      opts._hide `shouldBe` ["x"]
      opts._show `shouldBe` ["y"]
      opts._locator `shouldBe` "Q"
      opts._focus `shouldBe` "Q"
      opts._expression `shouldBe` Just "expr"
      opts._label `shouldBe` Just "label"
      opts._meetPrefix `shouldBe` Just "prefix"
      opts._stepsDir `shouldBe` Just "steps"
      opts._evaluations `shouldBe` Just "evaluations.jsonl"
      opts._inputFile `shouldBe` Just "input.phi"

  describe "OptsExplain field accessors" $
    it "exposes every OptsExplain field via its accessor" $ do
      let opts =
            OptsExplain
              { _logLevel = DEBUG
              , _logLines = 25
              , _rules = ["rule.yaml"]
              , _normalize = True
              , _morph = True
              , _dataize = True
              , _contextualize = True
              , _shuffle = True
              , _targetFile = Just "target.tex"
              }
      opts._logLevel `shouldBe` DEBUG
      opts._logLines `shouldBe` 25
      opts._rules `shouldBe` ["rule.yaml"]
      opts._normalize `shouldBe` True
      opts._morph `shouldBe` True
      opts._dataize `shouldBe` True
      opts._contextualize `shouldBe` True
      opts._shuffle `shouldBe` True
      opts._targetFile `shouldBe` Just "target.tex"

  describe "OptsRewrite field accessors" $
    it "exposes every OptsRewrite field via its accessor" $ do
      let opts =
            OptsRewrite
              { _logLevel = DEBUG
              , _logLines = 25
              , _inputFormat = PHI
              , _outputFormat = XMIR
              , _sugarType = SALTY
              , _hideRho = True
              , _flat = MULTILINE
              , _must = MtExact 3
              , _normalize = True
              , _shuffle = True
              , _seed = 42
              , _omitListing = True
              , _omitComments = True
              , _depthSensitive = True
              , _nonumber = True
              , _inPlace = True
              , _update = True
              , _sequence = True
              , _headers = True
              , _canonize = True
              , _compress = True
              , _maxDepth = 100
              , _maxCycles = 50
              , _margin = 80
              , _meetPopularity = Just 3
              , _meetLength = Just 4
              , _rules = ["rule.yaml"]
              , _hide = ["x"]
              , _show = ["y"]
              , _locator = "Q"
              , _focus = "Q"
              , _expression = Just "expr"
              , _label = Just "label"
              , _meetPrefix = Just "prefix"
              , _breakpoint = Just "bp"
              , _targetFile = Just "target.phi"
              , _stepsDir = Just "steps"
              , _inputFile = Just "input.phi"
              }
      opts._logLevel `shouldBe` DEBUG
      opts._logLines `shouldBe` 25
      opts._inputFormat `shouldBe` PHI
      opts._outputFormat `shouldBe` XMIR
      opts._sugarType `shouldBe` SALTY
      opts._hideRho `shouldBe` True
      opts._flat `shouldBe` MULTILINE
      opts._must `shouldBe` MtExact 3
      opts._normalize `shouldBe` True
      opts._shuffle `shouldBe` True
      opts._seed `shouldBe` 42
      opts._omitListing `shouldBe` True
      opts._omitComments `shouldBe` True
      opts._depthSensitive `shouldBe` True
      opts._nonumber `shouldBe` True
      opts._inPlace `shouldBe` True
      opts._update `shouldBe` True
      opts._sequence `shouldBe` True
      opts._headers `shouldBe` True
      opts._canonize `shouldBe` True
      opts._compress `shouldBe` True
      opts._maxDepth `shouldBe` 100
      opts._maxCycles `shouldBe` 50
      opts._margin `shouldBe` 80
      opts._meetPopularity `shouldBe` Just 3
      opts._meetLength `shouldBe` Just 4
      opts._rules `shouldBe` ["rule.yaml"]
      opts._hide `shouldBe` ["x"]
      opts._show `shouldBe` ["y"]
      opts._locator `shouldBe` "Q"
      opts._focus `shouldBe` "Q"
      opts._expression `shouldBe` Just "expr"
      opts._label `shouldBe` Just "label"
      opts._meetPrefix `shouldBe` Just "prefix"
      opts._breakpoint `shouldBe` Just "bp"
      opts._targetFile `shouldBe` Just "target.phi"
      opts._stepsDir `shouldBe` Just "steps"
      opts._inputFile `shouldBe` Just "input.phi"

  describe "OptsMerge field accessors" $
    it "exposes every OptsMerge field via its accessor" $ do
      let opts =
            OptsMerge
              { _logLevel = DEBUG
              , _logLines = 25
              , _inputFormat = PHI
              , _outputFormat = XMIR
              , _sugarType = SWEET
              , _flat = MULTILINE
              , _omitListing = True
              , _omitComments = True
              , _margin = 80
              , _targetFile = Just "target.phi"
              , _inputs = ["a.phi", "b.phi"]
              }
      opts._logLevel `shouldBe` DEBUG
      opts._logLines `shouldBe` 25
      opts._inputFormat `shouldBe` PHI
      opts._outputFormat `shouldBe` XMIR
      opts._sugarType `shouldBe` SWEET
      opts._flat `shouldBe` MULTILINE
      opts._omitListing `shouldBe` True
      opts._omitComments `shouldBe` True
      opts._margin `shouldBe` 80
      opts._targetFile `shouldBe` Just "target.phi"
      opts._inputs `shouldBe` ["a.phi", "b.phi"]

  describe "OptsMatch field accessors" $
    it "exposes every OptsMatch field via its accessor" $ do
      let opts =
            OptsMatch
              { _logLevel = DEBUG
              , _logLines = 25
              , _sugarType = SWEET
              , _flat = MULTILINE
              , _seed = 42
              , _pattern = Just "pattern"
              , _when = Just "condition"
              , _inputFile = Just "input.phi"
              }
      opts._logLevel `shouldBe` DEBUG
      opts._logLines `shouldBe` 25
      opts._sugarType `shouldBe` SWEET
      opts._flat `shouldBe` MULTILINE
      opts._seed `shouldBe` 42
      opts._pattern `shouldBe` Just "pattern"
      opts._when `shouldBe` Just "condition"
      opts._inputFile `shouldBe` Just "input.phi"

  describe "CliArgs field accessors" $
    it "exposes every CliArgs field via its accessor" $ do
      let matchOpts =
            OptsMatch
              { _logLevel = DEBUG
              , _logLines = 25
              , _sugarType = SWEET
              , _flat = MULTILINE
              , _seed = 42
              , _pattern = Just "pattern"
              , _when = Just "condition"
              , _inputFile = Just "input.phi"
              }
          cliArgs = CliArgs{_pin = Just "1.0.0", _command = CmdMatch matchOpts}
      cliArgs._pin `shouldBe` Just "1.0.0"
      case cliArgs._command of
        CmdMatch _ -> pure ()
        _ -> expectationFailure "expected CmdMatch"

  describe "Show CmdException" $
    forM_
      [ ("InvalidCLIArguments", show (InvalidCLIArguments "bad args"), "Invalid set of arguments: bad args")
      ,
        ( "CouldNotReadFromStdin"
        , show (CouldNotReadFromStdin "reason")
        , "Could not read input from stdin\nReason: reason"
        )
      , ("CouldNotDataize", show CouldNotDataize, "Could not dataize given expression")
      ,
        ( "CouldNotPrintExpressionInXMIR"
        , show CouldNotPrintExpressionInXMIR
        , "Could not print expression with --output=xmir, only expression printing is allowed"
        )
      ,
        ( "EmptySubstsOnMatch"
        , show EmptySubstsOnMatch
        , "Provided pattern was not matched, no substitutions are built"
        )
      ,
        ( "VersionMismatch"
        , show (VersionMismatch "1.0.0" "2.0.0")
        , "Version mismatch: --pin requires '1.0.0', but this is phino 2.0.0"
        )
      ]
      (\(name, actual, expected) -> it ("shows " ++ name) (actual `shouldBe` expected))

  describe "Show IOFormat" $
    forM_
      [ (XMIR, "xmir")
      , (PHI, "phi")
      , (LATEX, "latex")
      ]
      (\(format, expected) -> it ("shows " ++ show format) (show format `shouldBe` expected))
