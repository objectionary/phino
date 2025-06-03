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

spec :: Spec
spec = do
  describe "rewrites" $ do
    it "desugares with --nothing flag from file" $ do
      let args = ["rewrite", "--nothing", "--phi-input=test-resources/cli/desugar.phi"]
      output <- capture_ (runCLI args)
      output `shouldContain` "Φ ↦ ⟦\n  foo ↦ Φ.org.eolang\n⟧"

    it "desugares with --nothing flag from stdin" $ do
      withRedirectedStdin "{[[foo ↦ QQ]]}" $ do
        let args = ["rewrite", "--nothing"]
        output <- capture_ (runCLI args)
        output `shouldContain` "Φ ↦ ⟦\n  foo ↦ Φ.org.eolang\n⟧"
    
    it "rewrites with single rule" $ do
      withRedirectedStdin "{T(x -> Q.y)}" $ do
        let args = ["rewrite", "--rule=resources/dc.yaml"]
        output <- capture_ (runCLI args)
        output `shouldContain` "Φ ↦ ⊥"
