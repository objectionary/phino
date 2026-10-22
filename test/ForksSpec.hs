{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ForksSpec (spec) where

import AST
import Data.Text (Text)
import Forks (Refusal (..), Side (..), forked, joined)
import Parser (parseExpressionThrows)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "joined" $ do
    let application :: [Int] -> Expression
        application idxs = ExApplication (ExDispatch ExRoot (AtLabel "f")) (ArTau (AtLabel "x") (ExFormation (map (\idx -> BiTau (AtLabel "k") (ExFormation [BiLambda (FnSymbol idx)])) idxs)))
    it "joins two terms that differ in one symbol under a fresh symbol past the count" $
      joined (application [3]) (application [8]) 11 `shouldBe` Right (application [12], [(12, (3, 8))], 12)
    it "names a pair of symbols met twice by one fresh symbol" $
      joined (application [4, 4]) (application [9, 9]) 17 `shouldBe` Right (application [18, 18], [(18, (4, 9))], 18)
    it "names two pairs sharing a symbol by two fresh symbols" $
      joined (application [5, 5]) (application [6, 7]) 20 `shouldBe` Right (application [21, 22], [(21, (5, 6)), (22, (5, 7))], 22)
    it "mints nothing for two terms that are the same" $
      joined (application [2, 6]) (application [2, 6]) 13 `shouldBe` Right (application [2, 6], [], 13)
    it "tells the datum two formations differ in" $
      joined (ExFormation [BiDelta (BtOne "01"), BiLambda (FnSymbol 4)]) (ExFormation [BiDelta (BtOne "02"), BiLambda (FnSymbol 9)]) 10 `shouldBe` Left [AtDelta]
    it "tells the λ of two formations naming two functions" $
      joined (ExFormation [BiLambda (Function "L_one")]) (ExFormation [BiLambda (Function "L_two")]) 7 `shouldBe` Left [AtLambda]
    it "tells the λ of two formations naming a symbol and a function" $
      joined (ExFormation [BiLambda (FnSymbol 3)]) (ExFormation [BiLambda (Function "L_three")]) 5 `shouldBe` Left [AtLambda]
    it "tells no attribute of two dispatches of two attributes" $
      joined (ExDispatch (application [3]) (AtLabel "a")) (ExDispatch (application [8]) (AtLabel "b")) 9 `shouldBe` Left []
    it "tells no attribute inside an application whose arguments differ" $
      joined (application [3]) (application [3, 4]) 6 `shouldBe` Left []
    let apart :: String -> String -> IO (Either [Attribute] (Expression, [(Int, (Int, Int))], Int))
        apart left right = (\one two -> joined one two 41) <$> parseExpressionThrows left <*> parseExpressionThrows right
    it "tells every attribute down to the datum two formations differ in" $
      apart "⟦ φ ↦ ⟦ ko ↦ ⟦ Δ ⤍ 01- ⟧ ⟧ ⟧" "⟦ φ ↦ ⟦ ko ↦ ⟦ Δ ⤍ 02- ⟧ ⟧ ⟧" >>= (`shouldBe` Left [AtPhi, AtLabel "ko", AtDelta])
    it "tells the attribute only the right formation binds among the others" $
      apart "⟦ ub ↦ Φ.ka, xe ↦ Φ.mo ⟧" "⟦ ub ↦ Φ.ka, ρ ↦ Φ.zi, xe ↦ Φ.mo ⟧" >>= (`shouldBe` Left [AtRho])
    it "tells the attribute only the left formation binds among the others" $
      apart "⟦ ub ↦ Φ.ka, ρ ↦ Φ.zi, xe ↦ Φ.mo ⟧" "⟦ ub ↦ Φ.ka, xe ↦ Φ.mo ⟧" >>= (`shouldBe` Left [AtRho])
    it "tells the attribute the left formation binds past the end of the right one" $
      apart "⟦ ub ↦ Φ.ka, ρ ↦ Φ ⟧" "⟦ ub ↦ Φ.ka ⟧" >>= (`shouldBe` Left [AtRho])
    it "tells the first attribute of the left formation when both bind the same ones in other orders" $
      apart "⟦ ub ↦ Φ.ka, xe ↦ Φ.mo ⟧" "⟦ xe ↦ Φ.mo, ub ↦ Φ.ka ⟧" >>= (`shouldBe` Left [AtLabel "ub"])
    it "tells the attribute one formation leaves void and the other binds" $
      apart "⟦ ws ↦ ∅ ⟧" "⟦ ws ↦ Φ.ug ⟧" >>= (`shouldBe` Left [AtLabel "ws"])
    it "tells the attribute bound to an application whose arguments differ and nothing inside it" $
      apart "⟦ φ ↦ Φ.fe( ix ↦ ⟦ ke ↦ Φ.ab ⟧ ) ⟧" "⟦ φ ↦ Φ.fe( ix ↦ ⟦ ke ↦ Φ.yo ⟧ ) ⟧" >>= (`shouldBe` Left [AtPhi])
    it "tells no attribute of a dispatch and a formation" $
      apart "Φ.ab.cd" "⟦ φ ↦ Φ.ab ⟧" >>= (`shouldBe` Left [])

  describe "forked" $ do
    let fork :: Text -> Either Int Bytes -> String -> String -> IO (Either Refusal Expression)
        fork func condition left right = forked (Function func) condition <$> parseExpressionThrows left <*> parseExpressionThrows right
    it "asks the copies of both sides for an attribute the λ it is given picks between" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ φ ↦ Φ.hx ⟧, right🌵 ↦ ⟦ φ ↦ Φ.zu( ke ↦ Φ.ty ) ⟧, φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_qw, on🌵 ↦ ⟦ Δ ⤍ 1C- ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_qw" (Right (BtOne "1C")) "⟦ φ ↦ Φ.hx ⟧" "⟦ φ ↦ Φ.zu( ke ↦ Φ.ty ) ⟧" >>= (`shouldBe` Right expected)
    it "asks the copies of both sides for an attribute only one of them binds" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ gv ↦ Φ.ob ⟧, right🌵 ↦ ⟦ mu ↦ Φ.ra ⟧, gv ↦ ⟦ ρ ↦ ∅, λ ⤍ L_ny, on🌵 ↦ ⟦ Δ ⤍ 2B-31-70-0F ⟧, left🌵 ↦ ξ.ρ.left🌵.gv, right🌵 ↦ ξ.ρ.right🌵.gv ⟧, mu ↦ ⟦ ρ ↦ ∅, λ ⤍ L_ny, on🌵 ↦ ⟦ Δ ⤍ 2B-31-70-0F ⟧, left🌵 ↦ ξ.ρ.left🌵.mu, right🌵 ↦ ξ.ρ.right🌵.mu ⟧ ⟧"
      fork "L_ny" (Right (BtMany ["2B", "31", "70", "0F"])) "⟦ gv ↦ Φ.ob ⟧" "⟦ mu ↦ Φ.ra ⟧" >>= (`shouldBe` Right expected)
    it "keeps the ρ of both sides in their copies and asks neither for it" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ρ ↦ Φ.ji, φ ↦ Φ.wo ⟧, right🌵 ↦ ⟦ ρ ↦ Φ.sa, φ ↦ Φ.pl ⟧, φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_ef, on🌵 ↦ ⟦ Δ ⤍ 0A- ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_ef" (Right (BtOne "0A")) "⟦ ρ ↦ Φ.ji, φ ↦ Φ.wo ⟧" "⟦ ρ ↦ Φ.sa, φ ↦ Φ.pl ⟧" >>= (`shouldBe` Right expected)
    it "takes every void of both sides as a void of its own and applies each side to its voids" $ do
      expected <- parseExpressionThrows "⟦ tu ↦ ∅, os ↦ ∅, left🌵 ↦ ⟦ φ ↦ Φ.km( ξ.tu ), tu ↦ ∅ ⟧( tu ↦ ξ.tu ), right🌵 ↦ ⟦ φ ↦ Φ.wb( ξ.os ), os ↦ ∅ ⟧( os ↦ ξ.os ), φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_gi, on🌵 ↦ ⟦ Δ ⤍ 6D- ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_gi" (Right (BtOne "6D")) "⟦ φ ↦ Φ.km( ξ.tu ), tu ↦ ∅ ⟧" "⟦ φ ↦ Φ.wb( ξ.os ), os ↦ ∅ ⟧" >>= (`shouldBe` Right expected)
    it "applies a side binding its ρ to its voids and keeps that ρ" $ do
      expected <- parseExpressionThrows "⟦ pe ↦ ∅, left🌵 ↦ ⟦ ρ ↦ Φ.ov, φ ↦ Φ.ha( ξ.pe ), pe ↦ ∅ ⟧( pe ↦ ξ.pe ), right🌵 ↦ ⟦ φ ↦ Φ.ix ⟧, φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_dz, on🌵 ↦ ⟦ Δ ⤍ 44- ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_dz" (Right (BtOne "44")) "⟦ ρ ↦ Φ.ov, φ ↦ Φ.ha( ξ.pe ), pe ↦ ∅ ⟧" "⟦ φ ↦ Φ.ix ⟧" >>= (`shouldBe` Right expected)
    it "keeps one void for an attribute both sides leave void" $ do
      expected <- parseExpressionThrows "⟦ dk ↦ ∅, left🌵 ↦ ⟦ dk ↦ ∅, φ ↦ Φ.ic ⟧( dk ↦ ξ.dk ), right🌵 ↦ ⟦ φ ↦ Φ.yo, dk ↦ ∅ ⟧( dk ↦ ξ.dk ), φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_vb, on🌵 ↦ ⟦ Δ ⤍ 3E- ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_vb" (Right (BtOne "3E")) "⟦ dk ↦ ∅, φ ↦ Φ.ic ⟧" "⟦ φ ↦ Φ.yo, dk ↦ ∅ ⟧" >>= (`shouldBe` Right expected)
    it "takes as its own void an attribute one side leaves void and the other binds" $ do
      expected <- parseExpressionThrows "⟦ ws ↦ ∅, left🌵 ↦ ⟦ ws ↦ ∅ ⟧( ws ↦ ξ.ws ), right🌵 ↦ ⟦ ws ↦ Φ.ug ⟧ ⟧"
      fork "L_oh" (Right (BtOne "52")) "⟦ ws ↦ ∅ ⟧" "⟦ ws ↦ Φ.ug ⟧" >>= (`shouldBe` Right expected)
    it "skips an attribute of a side whose name ends with the 🌵 mark" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ left🌵 ↦ Φ.ub, rq🌵 ↦ ∅, φ ↦ Φ.ar ⟧, right🌵 ↦ ⟦ φ ↦ Φ.se ⟧, φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_hn, on🌵 ↦ ⟦ Δ ⤍ 19- ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_hn" (Right (BtOne "19")) "⟦ left🌵 ↦ Φ.ub, rq🌵 ↦ ∅, φ ↦ Φ.ar ⟧" "⟦ φ ↦ Φ.se ⟧" >>= (`shouldBe` Right expected)
    it "binds the condition to the symbol it came down to, not to the bytes of that symbol" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ φ ↦ Φ.re ⟧, right🌵 ↦ ⟦ φ ↦ Φ.dy( Φ.ka ) ⟧, φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_uo, on🌵 ↦ ⟦ λ ⤍ 𝜎37 ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_uo" (Left 37) "⟦ φ ↦ Φ.re ⟧" "⟦ φ ↦ Φ.dy( Φ.ka ) ⟧" >>= (`shouldBe` Right expected)
    it "takes as voids of a copy of the function the voids of the formations both sides bind its attribute to" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ke ↦ ⟦ ix ↦ ∅, φ ↦ Φ.qa ⟧ ⟧, right🌵 ↦ ⟦ ke ↦ ⟦ φ ↦ Φ.vu, ow ↦ ∅ ⟧ ⟧, ke ↦ ⟦ ρ ↦ ∅, λ ⤍ L_bj, ix ↦ ∅, ow ↦ ∅, on🌵 ↦ ⟦ Δ ⤍ 5E- ⟧, left🌵 ↦ ξ.ρ.left🌵.ke( ix ↦ ξ.ix ), right🌵 ↦ ξ.ρ.right🌵.ke( ow ↦ ξ.ow ) ⟧ ⟧"
      fork "L_bj" (Right (BtOne "5E")) "⟦ ke ↦ ⟦ ix ↦ ∅, φ ↦ Φ.qa ⟧ ⟧" "⟦ ke ↦ ⟦ φ ↦ Φ.vu, ow ↦ ∅ ⟧ ⟧" >>= (`shouldBe` Right expected)
    it "keeps one void of a copy of the function for a void both sides leave in its attribute" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ke ↦ ⟦ ix ↦ ∅ ⟧, φ ↦ Φ.ne ⟧, right🌵 ↦ ⟦ ke ↦ ⟦ ix ↦ ∅, φ ↦ Φ.ag ⟧ ⟧, ke ↦ ⟦ ρ ↦ ∅, λ ⤍ L_wy, ix ↦ ∅, on🌵 ↦ ⟦ Δ ⤍ 2F- ⟧, left🌵 ↦ ξ.ρ.left🌵.ke( ix ↦ ξ.ix ), right🌵 ↦ ξ.ρ.right🌵.ke( ix ↦ ξ.ix ) ⟧, φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_wy, on🌵 ↦ ⟦ Δ ⤍ 2F- ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_wy" (Right (BtOne "2F")) "⟦ ke ↦ ⟦ ix ↦ ∅ ⟧, φ ↦ Φ.ne ⟧" "⟦ ke ↦ ⟦ ix ↦ ∅, φ ↦ Φ.ag ⟧ ⟧" >>= (`shouldBe` Right expected)
    it "orders the voids of a copy of the function by the left side first" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ke ↦ ⟦ zo ↦ ∅, ab ↦ ∅ ⟧ ⟧, right🌵 ↦ ⟦ ke ↦ ⟦ mi ↦ ∅, ab ↦ ∅ ⟧ ⟧, ke ↦ ⟦ ρ ↦ ∅, λ ⤍ L_rc, zo ↦ ∅, ab ↦ ∅, mi ↦ ∅, on🌵 ↦ ⟦ λ ⤍ 𝜎61 ⟧, left🌵 ↦ ξ.ρ.left🌵.ke( zo ↦ ξ.zo, ab ↦ ξ.ab ), right🌵 ↦ ξ.ρ.right🌵.ke( mi ↦ ξ.mi, ab ↦ ξ.ab ) ⟧ ⟧"
      fork "L_rc" (Left 61) "⟦ ke ↦ ⟦ zo ↦ ∅, ab ↦ ∅ ⟧ ⟧" "⟦ ke ↦ ⟦ mi ↦ ∅, ab ↦ ∅ ⟧ ⟧" >>= (`shouldBe` Right expected)
    it "takes no ρ of the formation a side binds an attribute to as a void of a copy of the function" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ke ↦ ⟦ ρ ↦ ∅, ux ↦ ∅ ⟧ ⟧, right🌵 ↦ ⟦ ke ↦ ⟦ ρ ↦ ∅, ux ↦ ∅, φ ↦ Φ.sl ⟧ ⟧, ke ↦ ⟦ ρ ↦ ∅, λ ⤍ L_tp, ux ↦ ∅, on🌵 ↦ ⟦ Δ ⤍ 6B- ⟧, left🌵 ↦ ξ.ρ.left🌵.ke( ux ↦ ξ.ux ), right🌵 ↦ ξ.ρ.right🌵.ke( ux ↦ ξ.ux ) ⟧ ⟧"
      fork "L_tp" (Right (BtOne "6B")) "⟦ ke ↦ ⟦ ρ ↦ ∅, ux ↦ ∅ ⟧ ⟧" "⟦ ke ↦ ⟦ ρ ↦ ∅, ux ↦ ∅, φ ↦ Φ.sl ⟧ ⟧" >>= (`shouldBe` Right expected)
    it "hands the voids of a copy of the function to a side binding its attribute to no formation" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ke ↦ Φ.ob ⟧, right🌵 ↦ ⟦ ke ↦ ⟦ ux ↦ ∅, φ ↦ ξ.ux ⟧ ⟧, ke ↦ ⟦ ρ ↦ ∅, λ ⤍ L_ga, ux ↦ ∅, on🌵 ↦ ⟦ Δ ⤍ 0C- ⟧, left🌵 ↦ ξ.ρ.left🌵.ke( ux ↦ ξ.ux ), right🌵 ↦ ξ.ρ.right🌵.ke( ux ↦ ξ.ux ) ⟧ ⟧"
      fork "L_ga" (Right (BtOne "0C")) "⟦ ke ↦ Φ.ob ⟧" "⟦ ke ↦ ⟦ ux ↦ ∅, φ ↦ ξ.ux ⟧ ⟧" >>= (`shouldBe` Right expected)
    it "takes no void of an attribute of a side whose name ends with the 🌵 mark" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ke ↦ ⟦ left🌵 ↦ ∅, ux ↦ ∅ ⟧ ⟧, right🌵 ↦ ⟦ ke ↦ Φ.ob ⟧, ke ↦ ⟦ ρ ↦ ∅, λ ⤍ L_md, ux ↦ ∅, on🌵 ↦ ⟦ Δ ⤍ 71- ⟧, left🌵 ↦ ξ.ρ.left🌵.ke( ux ↦ ξ.ux ), right🌵 ↦ ξ.ρ.right🌵.ke( ux ↦ ξ.ux ) ⟧ ⟧"
      fork "L_md" (Right (BtOne "71")) "⟦ ke ↦ ⟦ left🌵 ↦ ∅, ux ↦ ∅ ⟧ ⟧" "⟦ ke ↦ Φ.ob ⟧" >>= (`shouldBe` Right expected)
    it "applies nothing to a side binding the attribute of a copy of the function to a formation with no void" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ke ↦ ⟦ φ ↦ Φ.lo ⟧ ⟧, right🌵 ↦ ⟦ ke ↦ ⟦ ev ↦ ∅ ⟧ ⟧, ke ↦ ⟦ ρ ↦ ∅, λ ⤍ L_fz, ev ↦ ∅, on🌵 ↦ ⟦ Δ ⤍ 3A- ⟧, left🌵 ↦ ξ.ρ.left🌵.ke, right🌵 ↦ ξ.ρ.right🌵.ke( ev ↦ ξ.ev ) ⟧ ⟧"
      fork "L_fz" (Right (BtOne "3A")) "⟦ ke ↦ ⟦ φ ↦ Φ.lo ⟧ ⟧" "⟦ ke ↦ ⟦ ev ↦ ∅ ⟧ ⟧" >>= (`shouldBe` Right expected)
    it "hands the voids of a copy of the function to a side that does not bind its attribute" $ do
      expected <- parseExpressionThrows "⟦ left🌵 ↦ ⟦ ke ↦ ⟦ ev ↦ ∅, jo ↦ ∅ ⟧ ⟧, right🌵 ↦ ⟦ φ ↦ Φ.ty ⟧, ke ↦ ⟦ ρ ↦ ∅, λ ⤍ L_pu, ev ↦ ∅, jo ↦ ∅, on🌵 ↦ ⟦ Δ ⤍ 5C- ⟧, left🌵 ↦ ξ.ρ.left🌵.ke( ev ↦ ξ.ev, jo ↦ ξ.jo ), right🌵 ↦ ξ.ρ.right🌵.ke( ev ↦ ξ.ev, jo ↦ ξ.jo ) ⟧, φ ↦ ⟦ ρ ↦ ∅, λ ⤍ L_pu, on🌵 ↦ ⟦ Δ ⤍ 5C- ⟧, left🌵 ↦ ξ.ρ.left🌵.φ, right🌵 ↦ ξ.ρ.right🌵.φ ⟧ ⟧"
      fork "L_pu" (Right (BtOne "5C")) "⟦ ke ↦ ⟦ ev ↦ ∅, jo ↦ ∅ ⟧ ⟧" "⟦ φ ↦ Φ.ty ⟧" >>= (`shouldBe` Right expected)
    it "tells that the left side has a datum" $
      fork "L_ax" (Right (BtOne "11")) "⟦ Δ ⤍ 4F- ⟧" "⟦ φ ↦ Φ.bu ⟧" >>= (`shouldBe` Left (Holding Lefthand AtDelta))
    it "tells that the left side has a λ function" $
      fork "L_cz" (Right (BtOne "07")) "⟦ λ ⤍ L_kp, φ ↦ Φ.ev ⟧" "⟦ φ ↦ Φ.ni ⟧" >>= (`shouldBe` Left (Holding Lefthand AtLambda))
    it "tells that the right side has a λ when it is a bare symbol" $
      fork "L_wq" (Left 83) "⟦ φ ↦ Φ.ju( ξ.zi ), zi ↦ Φ.ko ⟧" "⟦ λ ⤍ 𝜎329 ⟧" >>= (`shouldBe` Left (Holding Righthand AtLambda))
    it "tells that the right side is no formation" $
      fork "L_rt" (Right (BtOne "2D")) "⟦ φ ↦ Φ.mo ⟧" "Φ.gl.fy" >>= (`shouldBe` Left (Unformed Righthand))
    it "tells about the left side before the right one" $
      fork "L_ib" (Left 5) "Φ.vy" "⟦ Δ ⤍ 1A- ⟧" >>= (`shouldBe` Left (Unformed Lefthand))
