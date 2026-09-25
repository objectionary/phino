{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module NormalsSpec where

import AST
import Normals
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "normality" $ do
    it "knows a normal form it has learned" $
      normality
        (learned (ExFormation [BiVoid (AtLabel "kq"), BiTau (AtLabel "zu") ExRoot]) noNormals)
        (ExFormation [BiVoid (AtLabel "kq"), BiTau (AtLabel "zu") ExRoot])
        `shouldBe` Normal
    it "knows a formation standing deep inside a normal form it has learned" $
      normality
        (learned (ExFormation [BiTau (AtLabel "wo") (ExFormation [BiDelta (BtOne "3F"), BiVoid AtRho])]) noNormals)
        (ExFormation [BiDelta (BtOne "3F"), BiVoid AtRho])
        `shouldBe` Normal
    it "knows nothing about a term when it has learned nothing" $
      normality noNormals (ExFormation [BiVoid (AtLabel "kq")]) `shouldBe` Unknown
    it "does not know a formation differing in one body from a learned one" $
      normality
        (learned (ExFormation [BiTau (AtLabel "zu") ExRoot]) noNormals)
        (ExFormation [BiTau (AtLabel "zu") ExXi])
        `shouldBe` Unknown
    it "points at a known formation standing under a dispatch" $
      normality
        (learned (ExFormation [BiVoid (AtLabel "yx")]) noNormals)
        (ExDispatch (ExFormation [BiVoid (AtLabel "yx")]) (AtLabel "yx"))
        `shouldBe` Parts [Normal]
    it "points at a known formation standing in the argument of an application" $
      normality
        (learned (ExFormation [BiVoid (AtLabel "pe")]) noNormals)
        (ExApplication (ExDispatch ExRoot (AtLabel "ga")) (ArTau (AtLabel "pe") (ExFormation [BiVoid (AtLabel "pe")])))
        `shouldBe` Parts [Unknown, Normal]
    it "points at a known formation among the bodies of an unknown one" $
      normality
        (learned (ExFormation [BiVoid (AtLabel "hu")]) noNormals)
        (ExFormation [BiVoid (AtLabel "ob"), BiTau (AtLabel "ri") ExXi, BiTau (AtLabel "ez") (ExFormation [BiVoid (AtLabel "hu")])])
        `shouldBe` Parts [Unknown, Normal]
