{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ForksSpec (spec) where

import AST
import Forks (joined)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "joined" $ do
    let application :: [Int] -> Expression
        application idxs = ExApplication (ExDispatch ExRoot (AtLabel "f")) (ArTau (AtLabel "x") (ExFormation (map (\idx -> BiTau (AtLabel "k") (ExFormation [BiLambda (FnSymbol idx)])) idxs)))
    it "joins two terms that differ in one symbol under a fresh symbol past the count" $
      joined (application [3]) (application [8]) 11 `shouldBe` Just (application [12], [(12, (3, 8))], 12)
    it "names a pair of symbols met twice by one fresh symbol" $
      joined (application [4, 4]) (application [9, 9]) 17 `shouldBe` Just (application [18, 18], [(18, (4, 9))], 18)
    it "names two pairs sharing a symbol by two fresh symbols" $
      joined (application [5, 5]) (application [6, 7]) 20 `shouldBe` Just (application [21, 22], [(21, (5, 6)), (22, (5, 7))], 22)
    it "mints nothing for two terms that are the same" $
      joined (application [2, 6]) (application [2, 6]) 13 `shouldBe` Just (application [2, 6], [], 13)
    it "does not join two terms that differ in a datum" $
      joined (ExFormation [BiDelta (BtOne "01"), BiLambda (FnSymbol 4)]) (ExFormation [BiDelta (BtOne "02"), BiLambda (FnSymbol 9)]) 10 `shouldBe` Nothing
    it "does not join two terms that differ in a λ name" $
      joined (ExFormation [BiLambda (Function "L_one")]) (ExFormation [BiLambda (Function "L_two")]) 7 `shouldBe` Nothing
    it "does not join a symbol with a λ name" $
      joined (ExFormation [BiLambda (FnSymbol 3)]) (ExFormation [BiLambda (Function "L_three")]) 5 `shouldBe` Nothing
    it "does not join two terms that differ in an attribute" $
      joined (ExDispatch (application [3]) (AtLabel "a")) (ExDispatch (application [8]) (AtLabel "b")) 9 `shouldBe` Nothing
    it "does not join two formations of different sizes" $
      joined (application [3]) (application [3, 4]) 6 `shouldBe` Nothing
