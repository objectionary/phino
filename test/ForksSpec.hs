{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ForksSpec (spec) where

import AST
import Data.Text (Text)
import Forks (forked, joined)
import Parser (parseExpressionThrows)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
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

  describe "forked" $ do
    let fork :: Text -> Bytes -> String -> String -> IO (Maybe Expression)
        fork func condition left right = forked (Function func) condition <$> parseExpressionThrows left <*> parseExpressionThrows right
    it "asks both sides for an attribute the λ it is given picks between" $ do
      expected <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ L_qw, on ↦ ⟦ Δ ⤍ 1C- ⟧, left ↦ ⟦ φ ↦ Φ.hx ⟧.φ, right ↦ ⟦ φ ↦ Φ.zu( ke ↦ Φ.ty ) ⟧.φ ⟧ ⟧"
      fork "L_qw" (BtOne "1C") "⟦ φ ↦ Φ.hx ⟧" "⟦ φ ↦ Φ.zu( ke ↦ Φ.ty ) ⟧" >>= (`shouldBe` Just expected)
    it "asks both sides for an attribute only one of them binds" $ do
      expected <- parseExpressionThrows "⟦ gv ↦ ⟦ λ ⤍ L_ny, on ↦ ⟦ Δ ⤍ 2B-31-70-0F ⟧, left ↦ ⟦ gv ↦ Φ.ob ⟧.gv, right ↦ ⟦ mu ↦ Φ.ra ⟧.gv ⟧, mu ↦ ⟦ λ ⤍ L_ny, on ↦ ⟦ Δ ⤍ 2B-31-70-0F ⟧, left ↦ ⟦ gv ↦ Φ.ob ⟧.mu, right ↦ ⟦ mu ↦ Φ.ra ⟧.mu ⟧ ⟧"
      fork "L_ny" (BtMany ["2B", "31", "70", "0F"]) "⟦ gv ↦ Φ.ob ⟧" "⟦ mu ↦ Φ.ra ⟧" >>= (`shouldBe` Just expected)
    it "skips the ρ of both sides" $ do
      expected <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ L_ef, on ↦ ⟦ Δ ⤍ 0A- ⟧, left ↦ ⟦ ρ ↦ Φ.ji, φ ↦ Φ.wo ⟧.φ, right ↦ ⟦ ρ ↦ Φ.sa, φ ↦ Φ.pl ⟧.φ ⟧ ⟧"
      fork "L_ef" (BtOne "0A") "⟦ ρ ↦ Φ.ji, φ ↦ Φ.wo ⟧" "⟦ ρ ↦ Φ.sa, φ ↦ Φ.pl ⟧" >>= (`shouldBe` Just expected)
    it "keeps void an attribute both sides leave void" $ do
      expected <- parseExpressionThrows "⟦ dk ↦ ∅, φ ↦ ⟦ λ ⤍ L_vb, on ↦ ⟦ Δ ⤍ 3E- ⟧, left ↦ ⟦ dk ↦ ∅, φ ↦ Φ.ic ⟧.φ, right ↦ ⟦ φ ↦ Φ.yo, dk ↦ ∅ ⟧.φ ⟧ ⟧"
      fork "L_vb" (BtOne "3E") "⟦ dk ↦ ∅, φ ↦ Φ.ic ⟧" "⟦ φ ↦ Φ.yo, dk ↦ ∅ ⟧" >>= (`shouldBe` Just expected)
    it "asks both sides for an attribute one of them leaves void" $ do
      expected <- parseExpressionThrows "⟦ ws ↦ ⟦ λ ⤍ L_oh, on ↦ ⟦ Δ ⤍ 52- ⟧, left ↦ ⟦ ws ↦ ∅ ⟧.ws, right ↦ ⟦ ws ↦ Φ.ug ⟧.ws ⟧ ⟧"
      fork "L_oh" (BtOne "52") "⟦ ws ↦ ∅ ⟧" "⟦ ws ↦ Φ.ug ⟧" >>= (`shouldBe` Just expected)
    it "copies the λ of neither side" $ do
      expected <- parseExpressionThrows "⟦ φ ↦ ⟦ λ ⤍ L_cz, on ↦ ⟦ Δ ⤍ 07- ⟧, left ↦ ⟦ λ ⤍ L_kp, φ ↦ Φ.ev ⟧.φ, right ↦ ⟦ φ ↦ Φ.ni ⟧.φ ⟧ ⟧"
      fork "L_cz" (BtOne "07") "⟦ λ ⤍ L_kp, φ ↦ Φ.ev ⟧" "⟦ φ ↦ Φ.ni ⟧" >>= (`shouldBe` Just expected)
    it "fabricates nothing when a side holds a datum" $
      fork "L_ax" (BtOne "11") "⟦ Δ ⤍ 4F- ⟧" "⟦ φ ↦ Φ.bu ⟧" >>= (`shouldBe` Nothing)
    it "fabricates nothing when a side is no formation" $
      fork "L_rt" (BtOne "2D") "⟦ φ ↦ Φ.mo ⟧" "Φ.gl.fy" >>= (`shouldBe` Nothing)
