{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import Control.Exception
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import Data.Version (showVersion)
import GHC.IO.Handle
import Paths_phino (version)
import System.Directory (removeFile)
import System.Exit (ExitCode (ExitFailure))
import System.IO
import Test.Hspec
import Text.Printf (printf)

withStdin :: String -> IO a -> IO a
withStdin input action =
  bracket (openTempFile "." "stdinXXXXXX.tmp") cleanup $ \(filePath, h) -> do
    hSetEncoding h utf8
    hPutStr h input
    hFlush h
    hClose h
    withFile filePath ReadMode $ \hIn -> do
      hSetEncoding hIn utf8
      bracket (hDuplicate stdin) restoreStdin $ \_ -> do
        hDuplicateTo hIn stdin
        hSetEncoding stdin utf8
        action
  where
    restoreStdin orig = hDuplicateTo orig stdin >> hClose orig
    cleanup (fp, _) = removeFile fp

withStdout :: IO a -> IO (String, a)
withStdout action =
  bracket
    (openTempFile "." "stdoutXXXXXX.tmp")
    cleanup
    ( \(path, hTmp) -> do
        hSetEncoding hTmp utf8
        oldOut <- hDuplicate stdout
        oldErr <- hDuplicate stderr
        hDuplicateTo hTmp stdout
        hDuplicateTo hTmp stderr

        result <-
          action `finally` do
            hFlush stdout
            hFlush stderr
            hDuplicateTo oldOut stdout >> hClose oldOut
            hDuplicateTo oldErr stderr >> hClose oldErr
            hClose hTmp

        captured <- readFile path
        _ <- evaluate (length captured)
        return (captured, result)
    )
  where
    cleanup (fp, _) = removeFile fp

testCLI :: [String] -> [String] -> Expectation
testCLI args outputs = do
  (out, _) <- withStdout (try (runCLI args) :: IO (Either ExitCode ()))
  forM_
    outputs
    ( \output ->
        unless (output `isInfixOf` out) $
          expectationFailure
            ("Expected that output contains:\n" ++ output ++ "\nbut got:\n" ++ out)
    )

testCLIFailed :: [String] -> String -> Expectation
testCLIFailed args output = do
  (out, result) <- withStdout (try (runCLI args) :: IO (Either ExitCode ()))
  out `shouldContain` output
  result `shouldBe` Left (ExitFailure 1)

spec :: Spec
spec = do
  it "prints version" $
    testCLI ["--version"] [showVersion version]

  it "prints help" $
    testCLI
      ["--help"]
      ["Phino - CLI Manipulator of 𝜑-Calculus Expressions", "Usage:"]

  it "prints debug info with --log-level=DEBUG" $
    withStdin "Q -> [[]]" $
      testCLI ["rewrite", "--nothing", "--log-level=DEBUG"] ["[DEBUG]:"]

  describe "rewrites" $ do
    it "desugares with --nothing flag from file" $
      testCLI
        ["rewrite", "--nothing", "test-resources/cli/desugar.phi"]
        ["Φ ↦ ⟦\n  foo ↦ Φ.org.eolang,\n  ρ ↦ ∅\n⟧"]

    it "desugares with --nothing flag from stdin" $
      withStdin "{[[foo ↦ QQ]]}" $
        testCLI ["rewrite", "--nothing"] ["Φ ↦ ⟦\n  foo ↦ Φ.org.eolang,\n  ρ ↦ ∅\n⟧"]

    it "rewrites with single rule" $
      withStdin "{T(x -> Q.y)}" $
        testCLI ["rewrite", "--rule=resources/dc.yaml"] ["Φ ↦ ⊥"]

    it "normalizes with --normalize flag" $
      testCLI
        ["rewrite", "--normalize", "test-resources/cli/normalize.phi"]
        [ unlines
            [ "Φ ↦ ⟦",
              "  x ↦ ⟦",
              "    ρ ↦ ⟦",
              "      y ↦ ⟦ ρ ↦ ∅ ⟧,",
              "      ρ ↦ ∅",
              "    ⟧",
              "  ⟧,",
              "  ρ ↦ ∅",
              "⟧"
            ]
        ]

    it "fails with negative --max-depth" $
      withStdin "" $
        testCLIFailed ["rewrite", "--max-depth=-1"] "--max-depth must be positive"

    it "fails with no rewriting options provided" $
      withStdin "" $
        testCLIFailed ["rewrite"] "no --rule, no --normalize, no --nothing are provided"

    it "normalizes from stdin" $
      withStdin "Φ ↦ ⟦ a ↦ ⟦ b ↦ ∅ ⟧ (b ↦ [[ ]]) ⟧" $
        testCLI
          ["rewrite", "--normalize"]
          [ unlines
              [ "Φ ↦ ⟦",
                "  a ↦ ⟦",
                "    b ↦ ⟦ ρ ↦ ∅ ⟧,",
                "    ρ ↦ ∅",
                "  ⟧,",
                "  ρ ↦ ∅",
                "⟧"
              ]
          ]

    it "rewrites with --sweet flag" $
      withStdin "Q -> [[ x -> 5]]" $
        testCLI
          ["rewrite", "--nothing", "--sweet"]
          ["{⟦\n  x ↦ 5\n⟧}"]

    it "rewrites as XMIR" $
      withStdin "Q -> [[ x -> Q.y ]]" $
        testCLI
          ["rewrite", "--nothing", "--output=xmir"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "  <o base=\"Q.y\" name=\"x\"/>"]

    it "rewrites with XMIR as input" $
      withStdin "<object><o name=\"app\"><o name=\"x\" base=\"Q.number\"/></o></object>" $
        testCLI
          ["rewrite", "--nothing", "--input=xmir", "--sweet"]
          [ unlines
              [ "{⟦",
                "  app ↦ ⟦",
                "    x ↦ Φ.number",
                "  ⟧",
                "⟧}"
              ]
          ]

    it "rewrites as XMIR with omit-listing flag" $
      withStdin "Q -> [[ x -> Q.y ]]" $
        testCLI
          ["rewrite", "--nothing", "--output=xmir", "--omit-listing"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "<listing>1 line(s)</listing>", "  <o base=\"Q.y\" name=\"x\"/>"]

    it "does not fail on exactly 1 rewriting" $
      withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
        testCLI
          ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=1", "--sweet"]
          ["x ↦ \"bar\""]

    it "fails with --nothing and --must=1" $
      withStdin "Q -> [[ ]]" $
        testCLIFailed ["rewrite", "--nothing", "--must=1"] "it's expected rewriting cycles to be in range 1, but rewriting stopped after 0"

    it "fails with --normalize and --must=1" $
      withStdin "Q -> [[ x -> [[ y -> 5 ]].y ]].x" $
        testCLIFailed ["rewrite", "--max-depth=2", "--normalize", "--must=1"] "it's expected rewriting cycles to be in range 1, but rewriting has already reached 2"

    describe "must range tests" $ do
      it "accepts range ..5 (0 to 5 cycles)" $
        withStdin "Q -> [[ ]]" $
          testCLI ["rewrite", "--nothing", "--must=..5", "--sweet"] ["{⟦⟧}"]

      it "accepts range 0..0 (exactly 0 cycles)" $
        withStdin "Q -> [[ ]]" $
          testCLI ["rewrite", "--nothing", "--must=0..0", "--sweet"] ["{⟦⟧}"]

      it "accepts range 1..1 (exactly 1 cycle)" $
        withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
          testCLI
            ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=1..1", "--sweet"]
            ["x ↦ \"bar\""]

      it "accepts range 1..3 when 1 cycle happens" $
        withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
          testCLI
            ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=1..3", "--sweet"]
            ["x ↦ \"bar\""]

      it "accepts range 0.. (0 or more)" $
        withStdin "Q -> [[ ]]" $
          testCLI ["rewrite", "--nothing", "--must=0..", "--sweet"] ["{⟦⟧}"]

      it "fails when cycles exceed range ..1" $
        withStdin "Q -> [[ x -> [[ y -> 5 ]].y ]].x" $
          testCLIFailed 
            ["rewrite", "--max-depth=2", "--normalize", "--must=..1"] 
            "it's expected rewriting cycles to be in range ..1, but rewriting has already reached 2"

      it "fails when cycles below range 2.." $
        withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
          testCLIFailed
            ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=2.."]
            "it's expected rewriting cycles to be in range 2.., but rewriting stopped after 1"

      it "fails with invalid range 5..3" $
        withStdin "Q -> [[ ]]" $
          testCLIFailed
            ["rewrite", "--nothing", "--must=5..3"]
            "cannot parse value `5..3'"

      it "fails with negative in range -1..5" $
        withStdin "Q -> [[ ]]" $
          testCLIFailed
            ["rewrite", "--nothing", "--must=-1..5"]
            "cannot parse value `-1..5'"

      it "fails with malformed range syntax" $
        withStdin "Q -> [[ ]]" $
          testCLIFailed
            ["rewrite", "--nothing", "--must=3...5"]
            "cannot parse value `3...5'"

    it "prints to target file" $
      withStdin "Q -> [[ ]]" $
        bracket
          (openTempFile "." "targetXXXXXX.tmp")
          (\(path, _) -> removeFile path)
          ( \(path, h) -> do
              hClose h
              testCLI
                ["rewrite", "--nothing", "--sweet", printf "--target=%s" path]
                [printf "was saved in '%s'" path]
              content <- readFile path
              content `shouldBe` "{⟦⟧}"
          )

    it "rewrites with cycles" $
      withStdin "Q -> [[ x -> \"x\" ]]" $
        testCLI
          ["rewrite", "--sweet", "--rule=test-resources/cli/infinite.yaml", "--max-depth=1", "--max-cycles=2"]
          [ unlines
              [ "{⟦",
                "  x ↦ \"x_hi_hi\"",
                "⟧}"
              ]
          ]
    
    it "fails with --depth-sensitive" $
      withStdin "Q -> [[ x -> \"x\"]]" $
        testCLIFailed
          ["rewrite", "--depth-sensitive", "--max-depth=1", "--max-cycles=1", "--rule=test-resources/cli/infinite.yaml"]
          "[ERROR]: With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --max-depth=1"

    it "fails on --sweet and --output=xmir together" $
      withStdin "Q -> [[ ]]" $
        testCLIFailed
          ["rewrite", "--sweet", "--output=xmir", "--nothing"]
          "The --sweet and --output=xmir can't stay together"

  describe "dataize" $ do
    it "dataizes simple program" $
      withStdin "Q -> [[ D> 01- ]]" $
        testCLI ["dataize"] ["01-"]

    it "fails to dataize" $
      withStdin "Q -> [[ ]]" $
        testCLIFailed ["dataize"] "[ERROR]: Could not dataize given program"

  describe "explain" $ do
    it "explains single rule" $
      testCLI
        ["explain", "--rule=resources/copy.yaml"]
        ["\\documentclass{article}", "\\usepackage{amsmath}", "\\begin{document}", "\\rule{COPY}", "\\end{document}"]

    it "explains multiple rules" $
      testCLI
        ["explain", "--rule=resources/copy.yaml", "--rule=resources/alpha.yaml"]
        ["\\documentclass{article}", "\\rule{COPY}", "\\rule{ALPHA}"]

    it "explains normalization rules" $
      testCLI
        ["explain", "--normalize"]
        ["\\documentclass{article}", "\\begin{document}", "\\end{document}"]

    it "fails with no rules specified" $
      testCLIFailed
        ["explain"]
        "Either --rule or --normalize must be specified"

    it "writes to target file" $
      bracket
        (openTempFile "." "explainXXXXXX.tex")
        (\(path, _) -> removeFile path)
        ( \(path, h) -> do
            hClose h
            testCLI
              ["explain", "--normalize", printf "--target=%s" path]
              [printf "was saved in '%s'" path]
            content <- readFile path
            content `shouldContain` "\\documentclass{article}"
            content `shouldContain` "\\begin{document}"
        )
