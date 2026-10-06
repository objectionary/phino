{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module TauSpec where

import AST
import Control.Monad (forM_, join, replicateM)
import Tau (freshTau, seedTaus, tausOf)
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
  forM_
    [ ("scans labels through an ExPhiMeet wrapper", ExPhiMeet Nothing 1)
    , ("scans labels through an ExPhiAgain wrapper", ExPhiAgain Nothing 1)
    ]
    ( \(desc, wrap) -> it desc $ do
        seedTaus (wrap (ExFormation [BiTau (AtLabel "a🌵0") ExRoot]))
        name <- freshTau
        name `shouldBe` "a🌵1"
    )
  it "mints names carrying the entry they were minted for" $ do
    seedTaus (ExFormation [])
    mint <- tausOf 7
    names <- replicateM 3 mint
    names `shouldBe` ["a🌵7-0", "a🌵7-1", "a🌵7-2"]
  it "skips the names of an entry the document already took" $ do
    seedTaus (ExFormation [BiTau (AtLabel "a🌵4-0") ExRoot, BiTau (AtLabel "a🌵4-1") ExXi])
    name <- join (tausOf 4)
    name `shouldBe` "a🌵4-2"
  it "does not move the cursor the run mints its own names with" $ do
    seedTaus (ExFormation [])
    _ <- tausOf 2 >>= \mint -> replicateM 5 mint
    name <- freshTau
    name `shouldBe` "a🌵0"
