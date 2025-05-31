{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import System.IO.Silently (capture_)
import Test.Hspec

spec :: Spec
spec = do
  it "desugares with --nothing flag from file" $ do
    let args = ["rewrite", "--nothing", "test/resources/cli/desugar.phi"]
    output <- capture_ (runCLI args)
    output `shouldContain` "Φ ↦ ⟦\n  foo ↦ Φ.org.eolang\n⟧"

  -- @todo:
  it "desugares with --nothing flag from stdin" $ do
    pendingWith "Capturing from stdin is not supported yet"
    let args = ["rewrite", "--nothing", "{[[foo ↦ QQ]]}"]
    output <- capture_ (runCLI args)
    output `shouldContain` "Φ ↦ ⟦\n  foo ↦ Φ.org.eolang\n⟧"
