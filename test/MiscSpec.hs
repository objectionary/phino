{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MiscSpec where

import AST
import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Misc
  ( attributeFromBinding
  , attributesFromBindings
  , attributesFromBindings'
  , fqnToAttrs
  , orThrow
  , recoverFormations
  , toDouble
  , uniqueBindings
  , withVoidRho
  )
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe, shouldContain, shouldReturn, shouldSatisfy)

testWithVoidRho :: [(String, [Binding], [Binding])] -> SpecWith (Arg Expectation)
testWithVoidRho useCases =
  forM_ useCases $ \(desc, before, after) ->
    it desc $ withVoidRho before `shouldBe` after

spec :: Spec
spec = do
  describe "with void rho binding" $
    testWithVoidRho
      [
        ( "[[x -> ?]] => [[x -> ?, ^ -> ?]]"
        , [BiVoid (AtLabel "x")]
        , [BiVoid (AtLabel "x"), BiVoid AtRho]
        )
      ,
        ( "[[^ -> ?, x -> ?]] => [[^ -> ?, x -> ?]]"
        , [BiVoid AtRho, BiVoid (AtLabel "x")]
        , [BiVoid AtRho, BiVoid (AtLabel "x")]
        )
      ,
        ( "[[^ -> Q.x, x -> $.y]] => [[^ -> Q.x, x -> $.y]]"
        , [BiTau AtRho (ExDispatch ExRoot (AtLabel "x")), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        , [BiTau AtRho (ExDispatch ExRoot (AtLabel "x")), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        )
      , ("[[!B]] => [[!B]]", [BiMeta "B"], [BiMeta "B"])
      , ("[[x -> ?, !B]] => [[x -> ?, !B]]", [BiVoid (AtLabel "x"), BiMeta "B"], [BiVoid (AtLabel "x"), BiMeta "B"])
      ,
        ( "[[x -> ?, !B, y -> ?]] => [[x -> ?, !B, y -> ?]]"
        , [BiVoid (AtLabel "x"), BiMeta "B", BiVoid (AtLabel "y")]
        , [BiVoid (AtLabel "x"), BiMeta "B", BiVoid (AtLabel "y")]
        )
      ,
        ( "[[^ -> ?, !B, y -> ?]] => [[^ -> ?, !B, y -> ?]]"
        , [BiVoid AtRho, BiMeta "B", BiVoid (AtLabel "y")]
        , [BiVoid AtRho, BiMeta "B", BiVoid (AtLabel "y")]
        )
      ,
        ( "[[!t -> ?, x -> $.y]] => [[!t -> Q.x, x -> $.y]]"
        , [BiVoid (AtMeta "t"), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        , [BiVoid (AtMeta "t"), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        )
      ,
        ( "[[!t -> Q.x, x -> $.y]] => [[!t -> Q.x, x -> $.y]]"
        , [BiTau (AtMeta "t") (ExDispatch ExRoot (AtLabel "x")), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        , [BiTau (AtMeta "t") (ExDispatch ExRoot (AtLabel "x")), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        )
      ]

  describe "unique bindings" $ do
    it "fails with duplicate attribute" $
      uniqueBindings [BiVoid AtRho, BiVoid AtRho] `shouldSatisfy` isLeft
    it "does not fail on different attributes" $
      uniqueBindings [BiVoid AtPhi, BiVoid AtRho] `shouldSatisfy` isRight

  describe "orThrow" $ do
    it "returns the value on Right" $
      orThrow userError (Right (5 :: Int)) `shouldReturn` 5
    it "throws the built exception on Left" $ do
      result <- try (orThrow userError (Left "boom")) :: IO (Either IOException Int)
      case result of
        Left err -> show err `shouldContain` "boom"
        Right _ -> fail "expected orThrow to throw"

  describe "attributeFromBinding" $ do
    it "BiTau yields its attribute" $
      attributeFromBinding (BiTau AtRho ExRoot) `shouldBe` Just AtRho
    it "BiVoid yields its attribute" $
      attributeFromBinding (BiVoid AtPhi) `shouldBe` Just AtPhi
    it "BiDelta yields AtDelta" $
      attributeFromBinding (BiDelta BtEmpty) `shouldBe` Just AtDelta
    it "BiLambda yields AtLambda" $
      attributeFromBinding (BiLambda (Function "F")) `shouldBe` Just AtLambda
    it "BiMeta yields Nothing" $
      attributeFromBinding (BiMeta "B") `shouldBe` Nothing

  describe "attributesFromBindings" $ do
    it "is empty for an empty list" $
      attributesFromBindings [] `shouldBe` []
    it "drops BiMeta entries, which carry no attribute" $
      attributesFromBindings [BiVoid AtRho, BiMeta "B", BiVoid AtPhi] `shouldBe` [AtRho, AtPhi]

  describe "attributesFromBindings'" $
    it "keeps a Nothing placeholder for each BiMeta entry" $
      attributesFromBindings' [BiVoid AtRho, BiMeta "B"] `shouldBe` [Just AtRho, Nothing]

  describe "recoverFormations" $ do
    it "adds a missing void rho to an empty formation" $
      recoverFormations (ExFormation []) `shouldBe` ExFormation [BiVoid AtRho]
    it "recovers a nested formation reached through a BiTau binding" $
      recoverFormations (ExFormation [BiTau (AtLabel "x") (ExFormation [])])
        `shouldBe` ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho]), BiVoid AtRho]
    it "recurses through ExDispatch" $
      recoverFormations (ExDispatch (ExFormation []) (AtLabel "y"))
        `shouldBe` ExDispatch (ExFormation [BiVoid AtRho]) (AtLabel "y")
    it "recurses through ExApplication's tau argument" $
      recoverFormations (ExApplication ExRoot (ArTau (AtLabel "a") (ExFormation [])))
        `shouldBe` ExApplication ExRoot (ArTau (AtLabel "a") (ExFormation [BiVoid AtRho]))
    it "recurses through ExApplication's alpha argument" $
      recoverFormations (ExApplication ExRoot (ArAlpha (Alpha 0) (ExFormation [])))
        `shouldBe` ExApplication ExRoot (ArAlpha (Alpha 0) (ExFormation [BiVoid AtRho]))
    it "leaves every other expression untouched" $
      recoverFormations ExXi `shouldBe` ExXi

  describe "fqnToAttrs" $ do
    it "converts a dispatch chain into an attribute list, root first" $
      fqnToAttrs (ExDispatch (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number"))
        `shouldBe` Just [AtLabel "org", AtLabel "eolang", AtLabel "number"]
    it "is Just [] for the bare root" $
      fqnToAttrs ExRoot `shouldBe` Just []
    it "is Nothing for an expression that is not a dispatch chain" $
      fqnToAttrs (ExFormation []) `shouldBe` Nothing

  describe "toDouble" $
    it "converts an Int to the equal Double" $
      toDouble 5 `shouldBe` 5.0
