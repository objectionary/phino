{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

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
import Text.Printf (printf)

spec :: Spec
spec =
  describe "abridged" $ do
    it "leaves a formation no longer than the width as it is" $
      printExpressionWith
        (const (abridged 64))
        (ExFormation [BiTau (AtLabel "kübel") (ExDispatch ExXi (AtLabel "wand")), BiVoid (AtLabel "zaun")])
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ kübel ↦ wand, zaun ↦ ∅ ⟧"
    it "keeps a formation exactly as wide as the width" $
      printExpressionWith
        (const (abridged 68))
        (ExFormation (map (\idx -> BiTau (AtLabel (T.pack ("ort-" <> show idx))) ExRoot) [1 .. 6 :: Int]))
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ ort-1 ↦ Φ, ort-2 ↦ Φ, ort-3 ↦ Φ, ort-4 ↦ Φ, ort-5 ↦ Φ, ort-6 ↦ Φ ⟧"
    it "folds a formation one character wider than the width" $
      printExpressionWith
        (const (abridged 67))
        (ExFormation (map (\idx -> BiTau (AtLabel (T.pack ("ort-" <> show idx))) ExRoot) [1 .. 6 :: Int]))
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ +6 ⟧"
    it "keeps the data and the λ of a long formation and folds the rest into a count" $
      printExpressionWith
        (const (abridged 64))
        ( ExFormation
            ( BiDelta (BtMany ["00", "77", "66"])
                : BiLambda (Function "L_xxx")
                : map (\idx -> BiVoid (AtLabel (T.pack ("atributo-" <> show idx)))) [1 .. 34 :: Int]
            )
        )
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ Δ ⤍ 00-77-66, λ ⤍ L_xxx, +34 ⟧"
    it "keeps the φ of a long formation and folds the formation it holds" $
      printExpressionWith
        (const (abridged 64))
        ( ExFormation
            [ BiTau (AtLabel "hund") ExRoot
            , BiTau AtPhi (ExFormation (map (\idx -> BiTau (AtLabel (T.pack ("pfote-" <> show idx))) ExRoot) [1 .. 9 :: Int]))
            , BiTau (AtLabel "katze") ExXi
            ]
        )
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ φ ↦ ⟦ +9 ⟧, +2 ⟧"
    it "cuts a long byte string to its first bytes, its last bytes and the count of the bytes between them" $
      printExpressionWith
        (const (abridged 64))
        (ExFormation [BiDelta (BtMany (map (printf "%02X") [7 .. 51 :: Int]))])
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "07-08-..(41b)..-32-33:Δ"
    it "cuts a long byte string inside a formation too short to fold" $
      printExpressionWith
        (const (abridged 64))
        (ExFormation [BiTau (AtLabel "k") (ExFormation [BiDelta (BtMany ["01", "02", "03", "04", "05", "06", "07", "08", "09", "0A"])])])
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "01-02-..(6b)..-09-0A:Δ:k"
    it "keeps a byte string of eight bytes whole" $
      printExpressionWith
        (const (abridged 64))
        (ExFormation (BiDelta (BtMany ["40", "60", "E0", "00", "00", "00", "00", "01"]) : map (\idx -> BiVoid (AtLabel (T.pack ("ränder-" <> show idx)))) [1 .. 5 :: Int]))
        (SWEET, UNICODE, SINGLELINE, defaultMargin)
        `shouldBe` "⟦ Δ ⤍ 40-60-E0-00-00-00-00-01, +5 ⟧"
    it "spells a folded formation the same way in ASCII" $
      printExpressionWith
        (const (abridged 64))
        (ExFormation (BiLambda (Function "F") : map (\idx -> BiVoid (AtLabel (T.pack ("sehr-langes-" <> show idx)))) [1 .. 5 :: Int]))
        (SWEET, ASCII, SINGLELINE, defaultMargin)
        `shouldBe` "[[ L> F, +5 ]]"
