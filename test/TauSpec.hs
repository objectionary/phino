{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module TauSpec where

import AST
import Control.Monad (replicateM)
import Tau (freshTau, seedTaus)
import Test.Hspec (Spec, describe, it, shouldBe)

emptyProgram :: Program
emptyProgram = Program (ExFormation [])

programWith :: [Attribute] -> Program
programWith attrs = Program (ExFormation [BiTau attr ExGlobal | attr <- attrs])

spec :: Spec
spec = describe "Tau" $ do
  it "mints sequential names after seeding from an empty document" $ do
    seedTaus emptyProgram
    names <- replicateM 3 freshTau
    names `shouldBe` ["a🌵0", "a🌵1", "a🌵2"]
  it "resets the cursor on every seeding so output is deterministic" $ do
    seedTaus emptyProgram
    first <- freshTau
    seedTaus emptyProgram
    second <- freshTau
    (first, second) `shouldBe` ("a🌵0", "a🌵0")
  it "skips names already taken in the document" $ do
    seedTaus (programWith [AtLabel "a🌵0", AtLabel "a🌵2"])
    names <- replicateM 2 freshTau
    names `shouldBe` ["a🌵1", "a🌵3"]
