{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import System.IO.Silently (capture_)
import System.Directory (removeFile)
import Test.Hspec
import System.IO
import Control.Exception
import GHC.IO.Handle
import Paths_phino (version)
import Data.Version (showVersion)
import Control.Monad (unless)
import Data.List (isInfixOf)

withRedirectedStdin :: String -> IO a -> IO a
withRedirectedStdin input action = do
  bracket (openTempFile "." "stdin.tmp") cleanup $ \(filePath, h) -> do
    hSetEncoding h utf8
    hPutStr h input
    hFlush h
    hSeek h AbsoluteSeek 0
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

testCLI :: [String] -> String -> Expectation
testCLI args output = do
  out <- capture_ (runCLI args)
  unless (output `isInfixOf` out) $
    expectationFailure
      ( "Expected that output contains:\n" ++ output ++ "\nbut got:\n" ++ out)

spec :: Spec
spec = do
  it "prints version" $ do
    testCLI ["--version"] (showVersion version)

  it "prints help" $ do
    output <- capture_ (runCLI ["--help"])
    output `shouldContain` "Phino - CLI Manipulator of 𝜑-Calculus Expressions"
    output `shouldContain` "Usage:"

  describe "rewrites" $ do
    it "desugares with --nothing flag from file" $ do
      testCLI
        ["rewrite", "--nothing", "--phi-input=test-resources/cli/desugar.phi"]
        "Φ ↦ ⟦\n  foo ↦ Φ.org.eolang\n⟧"

    it "desugares with --nothing flag from stdin" $ do
      withRedirectedStdin "{[[foo ↦ QQ]]}" $ do
        testCLI ["rewrite", "--nothing"] "Φ ↦ ⟦\n  foo ↦ Φ.org.eolang\n⟧"

    it "rewrites with single rule" $ do
      withRedirectedStdin "{T(x -> Q.y)}" $ do
        testCLI ["rewrite", "--rule=resources/dc.yaml"] "Φ ↦ ⊥"

    it "normalizes with --normalize flag" $ do
      testCLI
        ["rewrite", "--normalize", "--phi-input=test-resources/cli/normalize.phi"]
        "Φ ↦ ⟦\n  x ↦ ⊥\n⟧"
