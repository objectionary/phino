{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import Control.Exception
import Control.Monad (forM_, unless, when)
import Data.List (isInfixOf)
import Data.Version (showVersion)
import GHC.IO.Handle
import Paths_phino (version)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
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

withTempFile :: String -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile pattern =
  bracket
    (openTempFile "." pattern)
    (\(path, _) -> removeFile path)

testCLI' :: [String] -> [String] -> Either ExitCode () -> Expectation
testCLI' args outputs exit = do
  (out, result) <- withStdout (try (runCLI args) :: IO (Either ExitCode ()))
  forM_
    outputs
    ( \output ->
        unless (output `isInfixOf` out) $
          expectationFailure
            ("Expected that output contains:\n" ++ output ++ "\nbut got:\n" ++ out)
    )
  result `shouldBe` exit

testCLISucceeded :: [String] -> [String] -> Expectation
testCLISucceeded args outputs = testCLI' args outputs (Right ())

testCLIFailed :: [String] -> [String] -> Expectation
testCLIFailed args outputs = testCLI' args outputs (Left (ExitFailure 1))

spec :: Spec
spec = do
  it "prints version" $
    testCLISucceeded ["--version"] [showVersion version]

  it "prints help" $
    testCLISucceeded
      ["--help"]
      ["Phino - CLI Manipulator of 𝜑-Calculus Expressions", "Usage:"]

  it "prints debug info with --log-level=DEBUG" $
    withStdin "Q -> [[]]" $
      testCLISucceeded ["rewrite", "--log-level=DEBUG"] ["[DEBUG]:"]

  describe "rewriting" $ do
    describe "fails" $ do
      it "with --input=latex" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--input=latex"]
            ["The value 'latex' can't be used for '--input' option"]

      it "with negative --max-depth" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--max-depth=-1"]
            ["--max-depth must be positive"]

      it "with --normalize and --must=1" $
        withStdin "Q -> [[ x -> [[ y -> 5 ]].y ]].x" $
          testCLIFailed
            ["rewrite", "--max-depth=2", "--normalize", "--must=1"]
            ["it's expected rewriting cycles to be in range [1], but rewriting has already reached 2"]

      it "when --in-place is used without input file" $
        withStdin "Q -> [[ ]]" $
          testCLIFailed
            ["rewrite", "--in-place"]
            ["--in-place requires an input file"]

      it "when --in-place is used with --target" $
        withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
          hPutStr h "Q -> [[ ]]"
          hClose h
          testCLIFailed
            ["rewrite", "--in-place", "--target=output.phi", path]
            ["--in-place and --target cannot be used together"]

      it "with --depth-sensitive" $
        withStdin "Q -> [[ x -> \"x\"]]" $
          testCLIFailed
            ["rewrite", "--depth-sensitive", "--max-depth=1", "--max-cycles=1", "--rule=test-resources/cli/infinite.yaml"]
            ["[ERROR]: With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --max-depth=1"]

      it "on --sweet and --output=xmir together" $
        withStdin "Q -> [[ ]]" $
          testCLIFailed
            ["rewrite", "--sweet", "--output=xmir"]
            ["The --sweet and --output=xmir can't stay together"]

      it "with looping rules" $
        withStdin "Q -> [[ x -> \"0\" ]]" $
          testCLIFailed
            ["rewrite", "--rule=test-resources/cli/first.yaml", "--rule=test-resources/cli/second.yaml", "--max-depth=1", "--max-cycles=3"]
            ["it seems rewriting is looping"]

      it "with wrong attribute and valid error message" $
        testCLIFailed
          ["rewrite", "test-resources/cli/with-$this-attribute.phi"]
          [ "[ERROR]: Couldn't parse given phi program, cause: program:10:13:",
            "10 |             $this ↦ ⟦⟧",
            "   |             ^^",
            "unexpected \"$t\""
          ]

    it "saves steps to dir with --steps-dir" $ do
      let dir = "test-steps-temp"
      dirExists <- doesDirectoryExist dir
      when dirExists (removeDirectoryRecursive dir)
      withStdin "Q -> [[ x -> \"hello\"]]" $ do
        testCLISucceeded
          ["rewrite", "--rule=test-resources/cli/infinite.yaml", "--max-cycles=2", "--max-depth=2", "--steps-dir=" ++ dir, "--sweet"]
          ["hello_hi_hi"]
        (`shouldBe` True) <$> doesDirectoryExist dir
        files <- listDirectory dir
        length files `shouldBe` 4
        (`shouldBe` True) <$> doesFileExist (dir ++ "/00001.phi")
        (`shouldBe` True) <$> doesFileExist (dir ++ "/00003.phi")
        removeDirectoryRecursive dir

    it "desugares without any rules flag from file" $
      testCLISucceeded
        ["rewrite", "test-resources/cli/desugar.phi"]
        ["Φ ↦ ⟦\n  foo ↦ Φ.org.eolang,\n  ρ ↦ ∅\n⟧"]

    it "desugares with without any rules flag from stdin" $
      withStdin "{[[foo ↦ QQ]]}" $
        testCLISucceeded ["rewrite"] ["Φ ↦ ⟦\n  foo ↦ Φ.org.eolang,\n  ρ ↦ ∅\n⟧"]

    it "rewrites with single rule" $
      withStdin "{T(x -> Q.y)}" $
        testCLISucceeded ["rewrite", "--rule=resources/dc.yaml"] ["Φ ↦ ⊥"]

    it "normalizes with --normalize flag" $
      testCLISucceeded
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

    it "normalizes from stdin" $
      withStdin "Φ ↦ ⟦ a ↦ ⟦ b ↦ ∅ ⟧ (b ↦ [[ ]]) ⟧" $
        testCLISucceeded
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
        testCLISucceeded
          ["rewrite", "--sweet"]
          ["{⟦\n  x ↦ 5\n⟧}"]

    it "rewrites as XMIR" $
      withStdin "Q -> [[ x -> Q.y ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "  <o base=\"Φ.y\" name=\"x\"/>"]

    it "rewrites as LaTeX" $
      withStdin "Q -> [[ x -> QQ.z(y -> 5), q -> T, w -> $, ^ -> Q, @ -> 1, y -> \"H$@^M\"]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet"]
          [ "\\begin{phiquation}",
            "{[[",
            "  x -> QQ.z(",
            "    y -> 5",
            "  ),",
            "  q -> T,",
            "  w -> \\char36{},",
            "  \\char94{} -> Q,",
            "  \\char64{} -> 1,",
            "  y -> \"H\\char36{}\\char64{}\\char94{}M\"",
            "]]}",
            "\\end{phiquation}"
          ]

    it "rewrites with XMIR as input" $
      withStdin "<object><o name=\"app\"><o name=\"x\" base=\"Φ.number\"/></o></object>" $
        testCLISucceeded
          ["rewrite", "--input=xmir", "--sweet"]
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
        testCLISucceeded
          ["rewrite", "--output=xmir", "--omit-listing"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "<listing>1 line(s)</listing>", "  <o base=\"Φ.y\" name=\"x\"/>"]

    it "does not fail on exactly 1 rewriting" $
      withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
        testCLISucceeded
          ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=1", "--sweet"]
          ["x ↦ \"bar\""]

    it "prints many programs with --sequence" $
      withStdin "{[[ x -> \"foo\" ]]}" $
        testCLISucceeded
          [ "rewrite",
            "--rule=test-resources/cli/first.yaml",
            "--rule=test-resources/cli/second.yaml",
            "--max-depth=1",
            "--max-cycles=2",
            "--sequence",
            "--sweet",
            "--flat"
          ]
          [ unlines
              [ "{⟦ x ↦ \"foo\" ⟧}",
                "{Φ.x( y ↦ \"foo\" )}",
                "{⟦ x ↦ \"foo\" ⟧}"
              ]
          ]

    it "prints only one latex preamble with --sequence" $
      withStdin "{[[ x -> \"foo\" ]]}" $
        testCLISucceeded
          [ "rewrite",
            "--rule=test-resources/cli/first.yaml",
            "--rule=test-resources/cli/second.yaml",
            "--max-depth=1",
            "--max-cycles=2",
            "--sequence",
            "--sweet",
            "--flat",
            "--output=latex"
          ]
          [ unlines
              [ "\\begin{phiquation}",
                "{[[ x -> \"foo\" ]]}",
                "{Q.x( y -> \"foo\" )}",
                "{[[ x -> \"foo\" ]]}",
                "\\end{phiquation}"
              ]
          ]

    describe "must range tests" $ do
      describe "fails" $ do
        it "when cycles exceed range ..1" $
          withStdin "Q -> [[ x -> [[ y -> 5 ]].y ]].x" $
            testCLIFailed
              ["rewrite", "--max-depth=2", "--normalize", "--must=..1"]
              ["it's expected rewriting cycles to be in range [..1], but rewriting has already reached 2"]

        it "when cycles below range 2.." $
          withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
            testCLIFailed
              ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=2.."]
              ["it's expected rewriting cycles to be in range [2..], but rewriting stopped after 1"]

        it "with invalid range 5..3" $
          withStdin "Q -> [[ ]]" $
            testCLIFailed
              ["rewrite", "--must=5..3"]
              ["cannot parse value `5..3'"]

        it "with negative in range -1..5" $
          withStdin "Q -> [[ ]]" $
            testCLIFailed
              ["rewrite", "--must=-1..5"]
              ["cannot parse value `-1..5'"]

        it "with malformed range syntax" $
          withStdin "Q -> [[ ]]" $
            testCLIFailed
              ["rewrite", "--must=3...5"]
              ["cannot parse value `3...5'"]

      it "accepts range ..5 (0 to 5 cycles)" $
        withStdin "Q -> [[ ]]" $
          testCLISucceeded ["rewrite", "--must=..5", "--sweet"] ["{⟦⟧}"]

      it "accepts range 0..0 (exactly 0 cycles)" $
        withStdin "Q -> [[ ]]" $
          testCLISucceeded ["rewrite", "--must=0..0", "--sweet"] ["{⟦⟧}"]

      it "accepts range 1..1 (exactly 1 cycle)" $
        withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
          testCLISucceeded
            ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=1..1", "--sweet"]
            ["x ↦ \"bar\""]

      it "accepts range 1..3 when 1 cycle happens" $
        withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
          testCLISucceeded
            ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=1..3", "--sweet"]
            ["x ↦ \"bar\""]

      it "accepts range 0.. (0 or more)" $
        withStdin "Q -> [[ ]]" $
          testCLISucceeded ["rewrite", "--must=0..", "--sweet"] ["{⟦⟧}"]

    it "prints to target file" $
      withStdin "Q -> [[ ]]" $
        withTempFile "targetXXXXXX.tmp" $ \(path, h) -> do
          hClose h
          testCLISucceeded
            ["rewrite", "--sweet", printf "--target=%s" path]
            [printf "The command result was saved in '%s'" path]
          content <- readFile path
          content `shouldBe` "{⟦⟧}"

    it "modifies file in-place" $
      withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
        hPutStr h "Q -> [[ x -> \"foo\" ]]"
        hClose h
        testCLISucceeded
          ["rewrite", "--rule=test-resources/cli/simple.yaml", "--in-place", "--sweet", path]
          [printf "The file '%s' was modified in-place" path]
        content <- readFile path
        content `shouldBe` "{⟦\n  x ↦ \"bar\"\n⟧}"

    it "rewrites with cycles" $
      withStdin "Q -> [[ x -> \"x\" ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--rule=test-resources/cli/infinite.yaml", "--max-depth=1", "--max-cycles=2"]
          [ unlines
              [ "{⟦",
                "  x ↦ \"x_hi_hi\"",
                "⟧}"
              ]
          ]

    it "prints in line with --flat" $
      withStdin "Q -> [[ x -> 5, y -> \"hey\", z -> [[ w -> [[ ]] ]] ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat"]
          ["{⟦ x ↦ 5, y ↦ \"hey\", z ↦ ⟦ w ↦ ⟦⟧ ⟧ ⟧}"]

  describe "dataize" $ do
    it "dataizes simple program" $
      withStdin "Q -> [[ D> 01- ]]" $
        testCLISucceeded ["dataize"] ["01-"]

    it "fails to dataize" $
      withStdin "Q -> [[ ]]" $
        testCLIFailed ["dataize"] ["[ERROR]: Could not dataize given program"]

  describe "explain" $ do
    it "explains single rule" $
      testCLISucceeded
        ["explain", "--rule=resources/copy.yaml"]
        ["\\documentclass{article}", "\\usepackage{amsmath}", "\\begin{document}", "\\rule{COPY}", "\\end{document}"]

    it "explains multiple rules" $
      testCLISucceeded
        ["explain", "--rule=resources/copy.yaml", "--rule=resources/alpha.yaml"]
        ["\\documentclass{article}", "\\rule{COPY}", "\\rule{ALPHA}"]

    it "explains normalization rules" $
      testCLISucceeded
        ["explain", "--normalize"]
        ["\\documentclass{article}", "\\begin{document}", "\\end{document}"]

    it "fails with no rules specified" $
      testCLIFailed
        ["explain"]
        ["Either --rule or --normalize must be specified"]

    it "writes to target file" $
      bracket
        (openTempFile "." "explainXXXXXX.tex")
        (\(path, _) -> removeFile path)
        ( \(path, h) -> do
            hClose h
            testCLISucceeded
              ["explain", "--normalize", printf "--target=%s" path]
              [printf "was saved in '%s'" path]
            content <- readFile path
            content `shouldContain` "\\documentclass{article}"
            content `shouldContain` "\\begin{document}"
        )
