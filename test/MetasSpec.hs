{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Metas module that collects the meta-variables a term was
written with and drops the index from the ones that stand alone in their kind.
-}
module MetasSpec where

import AST
  ( Attribute (AtAny, AtMeta)
  , Binding (BiDelta, BiMeta, BiTau, BiVoid)
  , Bytes (BtMeta)
  , Expression (ExAny, ExDispatch, ExFormation, ExMeta)
  , Slot (Slot)
  )
import Metas (Metas (metas), lonely)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "metas" $ do
    it "names every meta-variable a formation was written with" $
      metas (ExFormation [BiMeta "B1", BiTau (AtMeta "t1") (ExMeta "n1"), BiDelta (BtMeta "d1")])
        `shouldBe` ["B1", "t1", "n1", "d1"]

    it "names an anonymous meta-variable by the sigil of its kind alone" $
      metas (ExDispatch (ExAny (Slot "n" 3)) (AtAny (Slot "t" 7))) `shouldBe` ["n", "t"]

  describe "lonely" $ do
    it "drops the index from every kind the term names just once" $
      lonely (ExDispatch (ExMeta "n1") (AtMeta "t1")) `shouldBe` ExDispatch (ExMeta "n") (AtMeta "t")

    it "dont drop the index from a kind the term names twice" $
      lonely (ExFormation [BiMeta "B1", BiMeta "B2"]) `shouldBe` ExFormation [BiMeta "B1", BiMeta "B2"]

    it "dont drop the index of a kind an anonymous meta-variable already stands for" $
      lonely (ExFormation [BiVoid (AtMeta "t1"), BiVoid (AtAny (Slot "t" 9))])
        `shouldBe` ExFormation [BiVoid (AtMeta "t1"), BiVoid (AtAny (Slot "t" 9))]

    it "dont drop a suffix that counts nothing" $
      lonely (ExMeta "nfoo") `shouldBe` ExMeta "nfoo"
