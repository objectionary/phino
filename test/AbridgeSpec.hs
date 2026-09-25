{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Abridge module, which shortens how a long formation and a
long byte string are spelled in a protocol written under '--abridged'.
-}
module AbridgeSpec where

import AST
import Abridge (abridged)
import Data.Text qualified as T
import Encoding (Encoding (..))
import Lining (LineFormat (..))
import Margin (defaultMargin)
import Printer (printExpressionWith)
import Sugar (SugarType (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "abridged" $ do
    it "leaves a formation no longer than sixty characters as it is" $
      printExpressionWith
        (const abridged)
        (ExFormation [BiTau (AtLabel "kübel") (ExDispatch ExXi (AtLabel "wand")), BiVoid (AtLabel "zaun")])
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ kübel ↦ wand, zaun ↦ ∅ ⟧"
    it "keeps the data and the λ of a long formation and folds the rest into a count" $
      printExpressionWith
        (const abridged)
        ( ExFormation
            ( BiDelta (BtMany ["00", "77", "66"])
                : BiLambda (Function "L_xxx")
                : map (\idx -> BiVoid (AtLabel (T.pack ("atributo-" <> show idx)))) [1 .. 34 :: Int]
            )
        )
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ Δ ⤍ 00-77-66, λ ⤍ L_xxx, +34 attrs ⟧"
    it "keeps the φ of a long formation and folds the formation it holds" $
      printExpressionWith
        (const abridged)
        ( ExFormation
            [ BiTau (AtLabel "hund") ExRoot
            , BiTau AtPhi (ExFormation (map (\idx -> BiTau (AtLabel (T.pack ("pfote-" <> show idx))) ExRoot) [1 .. 9 :: Int]))
            , BiTau (AtLabel "katze") ExXi
            ]
        )
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ φ ↦ ⟦ +9 attrs ⟧, +2 attrs ⟧"
    it "cuts a long byte string to its first bytes and its length" $
      printExpressionWith
        (const abridged)
        (ExFormation [BiDelta (BtMany (replicate 45 "A7"))])
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "A7-A7-A7-A7-...(45b):Δ"
    it "cuts a long byte string inside a formation too short to fold" $
      printExpressionWith
        (const abridged)
        (ExFormation [BiTau (AtLabel "k") (ExFormation [BiDelta (BtMany ["01", "02", "03", "04", "05", "06", "07", "08", "09", "0A"])])])
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "01-02-03-04-...(10b):Δ:k"
    it "keeps a byte string of eight bytes whole" $
      printExpressionWith
        (const abridged)
        (ExFormation (BiDelta (BtMany ["40", "60", "E0", "00", "00", "00", "00", "01"]) : map (\idx -> BiVoid (AtLabel (T.pack ("ränder-" <> show idx)))) [1 .. 5 :: Int]))
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ Δ ⤍ 40-60-E0-00-00-00-00-01, +5 attrs ⟧"
    it "spells a folded formation the same way in ASCII" $
      printExpressionWith
        (const abridged)
        (ExFormation (BiLambda (Function "F") : map (\idx -> BiVoid (AtLabel (T.pack ("sehr-langes-" <> show idx)))) [1 .. 5 :: Int]))
        (SWEET, ASCII, SINGLELINE, defaultMargin)
        `shouldBe` "[[ L> F, +5 attrs ]]"
