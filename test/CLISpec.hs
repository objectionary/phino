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
import System.IO.Silently (capture_)
import Test.Hspec

withStdin :: String -> IO a -> IO a
withStdin input action = do
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
  out <- capture_ (runCLI args)
  forM_
    outputs
    ( \output ->
        unless (output `isInfixOf` out) $
          expectationFailure
            ("Expected that output contains:\n" ++ output ++ "\nbut got:\n" ++ out)
    )

testCLIFailed :: [String] -> String -> Expectation
testCLIFailed args output = withStdin "" $ do
  (out, result) <- withStdout (try (runCLI args) :: IO (Either ExitCode ()))
  out `shouldContain` output
  result `shouldBe` Left (ExitFailure 1)

spec :: Spec
spec = do
  it "prints version" $ do
    testCLI ["--version"] [showVersion version]

  it "prints help" $ do
    output <- capture_ (runCLI ["--help"])
    output `shouldContain` "Phino - CLI Manipulator of 𝜑-Calculus Expressions"
    output `shouldContain` "Usage:"

  it "prints debug info with --log-level=DEBUG" $ do
    withStdin "Q -> [[]]" $ testCLI ["rewrite", "--nothing", "--log-level=DEBUG"] ["[DEBUG]:"]

  describe "rewrites" $ do
    it "desugares with --nothing flag from file" $
      testCLI
        ["rewrite", "--nothing", "--phi-input=test-resources/cli/desugar.phi"]
        ["Φ ↦ ⟦\n  foo ↦ Φ.org.eolang,\n  ρ ↦ ∅\n⟧"]

    it "desugares with --nothing flag from stdin" $
      withStdin "{[[foo ↦ QQ]]}" $
        testCLI ["rewrite", "--nothing"] ["Φ ↦ ⟦\n  foo ↦ Φ.org.eolang,\n  ρ ↦ ∅\n⟧"]

    it "rewrites with single rule" $
      withStdin "{T(x -> Q.y)}" $
        testCLI ["rewrite", "--rule=resources/dc.yaml"] ["Φ ↦ ⊥"]

    it "normalizes with --normalize flag" $
      testCLI
        ["rewrite", "--normalize", "--phi-input=test-resources/cli/normalize.phi"]
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
      testCLIFailed
        ["rewrite", "--max-depth=-1"]
        "--max-depth must be positive"

    it "fails with no rewriting options provided" $
      testCLIFailed
        ["rewrite"]
        "no --rule, no --normalize, no --nothing are provided"

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

    it "rewrites as XMIR" $
      withStdin "Q -> [[ x -> Q.y ]]" $
        testCLI
          ["rewrite", "--nothing", "--output=xmir"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "  <o base=\"Q.y\" name=\"x\"/>"]
