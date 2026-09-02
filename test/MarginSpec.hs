{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MarginSpec where

import AST
import Bytes (numToBts, strToBts)
import CST
import Control.Monad (forM_)
import Lining (toSingleLine)
import Margin (withMargin)
import Render (render)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

bigLabel :: Attribute
bigLabel = AtLabel "aVeryLongAttributeNameThatWontFitOnOneLine"

bigFormation :: Expression
bigFormation = ExFormation [BiTau bigLabel ExRoot]

nestedFormation :: Expression
nestedFormation =
  ExFormation
    [ BiTau (AtLabel "x") ExRoot
    , BiTau (AtLabel "y") (ExFormation [BiTau bigLabel ExRoot, BiTau AtRho ExRoot])
    ]

longCalleeShortArg :: Expression
longCalleeShortArg = ExApplication bigFormation (ArTau (AtLabel "y") (ExDispatch ExXi AtRho))

shortCalleeLongArg :: Expression
shortCalleeLongArg = ExApplication (ExDispatch ExRoot (AtLabel "x")) (ArTau (AtLabel "y") bigFormation)

spec :: Spec
spec = do
  describe "withMargin on EX_FORMATION" $ do
    it "leaves an empty formation untouched at any margin" $
      withMargin 0 (expressionToCST (ExFormation [])) `shouldBe` expressionToCST (ExFormation [])

    forM_
      [
        ( "keeps a formation on one line when it fits the margin"
        , 100
        , nestedFormation
        , "⟦ x ↦ Φ, y ↦ ⟦ aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ, ρ ↦ Φ ⟧ ⟧"
        )
      ,
        ( "wraps a formation across lines when it does not fit the margin"
        , 1
        , nestedFormation
        , "⟦\n  x ↦ Φ,\n  y ↦ ⟦\n    aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ,\n    ρ ↦ Φ\n  ⟧\n⟧"
        )
      ]
      (\(desc, margin, expression, expected) -> it desc (render (withMargin margin (expressionToCST expression)) `shouldBe` expected))

  describe "withMargin leaves data primitives untouched" $ do
    it "a number literal is never wrapped" $
      let cst = expressionToCST (DataNumber (numToBts 42))
       in withMargin 0 cst `shouldBe` cst
    it "a string literal is never wrapped" $
      let cst = expressionToCST (DataString (strToBts "hello"))
       in withMargin 0 cst `shouldBe` cst

  describe "withMargin on EX_APPLICATION" $
    forM_
      [
        ( "keeps the whole application on one line when it all fits"
        , 100
        , longCalleeShortArg
        , "⟦ aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ ⟧( y ↦ ρ )"
        )
      ,
        ( "wraps only the callee formation when the callee alone still fits alongside the argument"
        , 10
        , longCalleeShortArg
        , "⟦\n  aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ\n⟧( y ↦ ρ )"
        )
      ,
        ( "wraps both the callee and the argument when neither fits alongside the other"
        , 1
        , longCalleeShortArg
        , "⟦\n  aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ\n⟧(\n  y ↦ ρ\n)"
        )
      ,
        ( "keeps a short callee on one line and wraps only the argument"
        , 60
        , shortCalleeLongArg
        , "Φ.x(\n  y ↦ ⟦ aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ ⟧\n)"
        )
      ,
        ( "wraps the argument formation itself when it does not fit even on its own line"
        , 1
        , shortCalleeLongArg
        , "Φ.x(\n  y ↦ ⟦\n    aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ\n  ⟧\n)"
        )
      ]
      (\(desc, margin, expression, expected) -> it desc (render (withMargin margin (expressionToCST expression)) `shouldBe` expected))

  describe "withMargin on positional (AA_EXPRS) application arguments" $
    forM_
      [ (100, "Φ.x( ⟦ aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ, ρ ↦ Φ ⟧ )")
      ,
        ( 1
        , "Φ.x(\n  ⟦\n    aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ,\n    ρ ↦ Φ\n  ⟧\n)"
        )
      ]
      ( \(margin, expected) ->
          it ("margin " ++ show margin) $
            let ex = ExApplication (ExDispatch ExRoot (AtLabel "x")) (ArAlpha (Alpha 0) (ExFormation [BiTau bigLabel ExRoot, BiTau AtRho ExRoot]))
             in render (withMargin margin (expressionToCST ex)) `shouldBe` expected
      )

  describe "withMargin on mixed tau/alpha (AA_TAUS with PA_ALPHA) application arguments" $
    forM_
      [ (100, "Φ.x( a ↦ Φ, α5 ↦ ⟦ aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ ⟧ )")
      ,
        ( 1
        , "Φ.x(\n  a ↦ Φ,\n  α5 ↦ ⟦\n    aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ\n  ⟧\n)"
        )
      ]
      ( \(margin, expected) ->
          it ("margin " ++ show margin) $
            let ex = ExApplication (ExApplication (ExDispatch ExRoot (AtLabel "x")) (ArTau (AtLabel "a") ExRoot)) (ArAlpha (Alpha 5) bigFormation)
             in render (withMargin margin (expressionToCST ex)) `shouldBe` expected
      )

  describe "withMargin on a manually built AA_TAU / APP_BINDING argument" $ do
    let manual =
          EX_APPLICATION
            (EX_DISPATCH (EX_GLOBAL Φ) NO_SPACE (AT_LABEL "x"))
            NO_SPACE
            EOL
            (TAB 1)
            (AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "aVeryLongAttributeNameThatWontFitOnOneLine") ARROW (EX_GLOBAL Φ))))
            EOL
            (TAB 0)
            1
    it "keeps it on one line when it fits" $
      render (withMargin 100 manual) `shouldBe` "Φ.x( aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ )"
    it "wraps it when it does not fit" $
      render (withMargin 1 manual) `shouldBe` "Φ.x(\n  aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ\n)"

  describe "withMargin on EX_DISPATCH" $
    forM_
      [
        ( "recurses into the dispatched-upon expression"
        , 1
        , ExDispatch bigFormation (AtLabel "z")
        , "⟦\n  aVeryLongAttributeNameThatWontFitOnOneLine ↦ Φ\n⟧.z"
        )
      ]
      (\(desc, margin, expression, expected) -> it desc (render (withMargin margin (expressionToCST expression)) `shouldBe` expected))

  describe "withMargin on EX_PHI_MEET" $ do
    it "produces the same result whether the margin is 1 or 1000" $
      let meet = expressionToCST (ExPhiMeet Nothing 3 bigFormation)
       in withMargin 1 meet `shouldBe` withMargin 1000 meet

    it "collapses its body via toSingleLine regardless of the margin" $
      let meet = expressionToCST (ExPhiMeet Nothing 3 bigFormation)
       in withMargin 1 meet `shouldBe` EX_PHI_MEET Nothing 3 (toSingleLine (expressionToCST bigFormation))

  describe "withMargin on EX_PHI_AGAIN" $
    it "threads the margin into its body" $ do
      let again = expressionToCST (ExPhiAgain Nothing 3 bigFormation)
      withMargin 1 again `shouldNotBe` withMargin 1000 again
