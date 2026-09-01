{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module TauSpec where

import AST
import Control.Monad (replicateM)
import Tau (freshTau, seedTaus)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Tau" $ do
  it "mints sequential names after seeding from an empty document" $ do
    seedTaus (ExFormation [])
    names <- replicateM 3 freshTau
    names `shouldBe` ["a🌵0", "a🌵1", "a🌵2"]
  it "resets the cursor on every seeding so output is deterministic" $ do
    seedTaus (ExFormation [])
    first <- freshTau
    seedTaus (ExFormation [])
    second <- freshTau
    (first, second) `shouldBe` ("a🌵0", "a🌵0")
  it "skips names already taken in the document" $ do
    seedTaus
      ( ExFormation
          [ BiTau (AtLabel "a🌵0") ExRoot
          , BiTau (AtLabel "a🌵2") ExRoot
          ]
      )
    names <- replicateM 2 freshTau
    names `shouldBe` ["a🌵1", "a🌵3"]
  it "scans labels through an ExPhiMeet wrapper" $ do
    seedTaus (ExPhiMeet Nothing 1 (ExFormation [BiTau (AtLabel "a🌵0") ExRoot]))
    name <- freshTau
    name `shouldBe` "a🌵1"
  it "scans labels through an ExPhiAgain wrapper" $ do
    seedTaus (ExPhiAgain Nothing 1 (ExFormation [BiTau (AtLabel "a🌵0") ExRoot]))
    name <- freshTau
    name `shouldBe` "a🌵1"
