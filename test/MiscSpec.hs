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

  describe "attributeFromBinding" $
    forM_
      [ ("BiTau yields its attribute", BiTau AtRho ExRoot, Just AtRho)
      , ("BiVoid yields its attribute", BiVoid AtPhi, Just AtPhi)
      , ("BiDelta yields AtDelta", BiDelta BtEmpty, Just AtDelta)
      , ("BiLambda yields AtLambda", BiLambda (Function "F"), Just AtLambda)
      , ("BiMeta yields Nothing", BiMeta "B", Nothing)
      ]
      (\(desc, binding, expected) -> it desc (attributeFromBinding binding `shouldBe` expected))

  describe "attributesFromBindings" $
    forM_
      [ ("is empty for an empty list", [], [])
      ,
        ( "drops BiMeta entries, which carry no attribute"
        , [BiVoid AtRho, BiMeta "B", BiVoid AtPhi]
        , [AtRho, AtPhi]
        )
      ]
      (\(desc, bindings, expected) -> it desc (attributesFromBindings bindings `shouldBe` expected))

  describe "attributesFromBindings'" $
    it "keeps a Nothing placeholder for each BiMeta entry" $
      attributesFromBindings' [BiVoid AtRho, BiMeta "B"] `shouldBe` [Just AtRho, Nothing]

  describe "recoverFormations" $
    forM_
      [ ("adds a missing void rho to an empty formation", ExFormation [], ExFormation [BiVoid AtRho])
      ,
        ( "recovers a nested formation reached through a BiTau binding"
        , ExFormation [BiTau (AtLabel "x") (ExFormation [])]
        , ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho]), BiVoid AtRho]
        )
      ,
        ( "recurses through ExDispatch"
        , ExDispatch (ExFormation []) (AtLabel "y")
        , ExDispatch (ExFormation [BiVoid AtRho]) (AtLabel "y")
        )
      ,
        ( "recurses through ExApplication's tau argument"
        , ExApplication ExRoot (ArTau (AtLabel "a") (ExFormation []))
        , ExApplication ExRoot (ArTau (AtLabel "a") (ExFormation [BiVoid AtRho]))
        )
      ,
        ( "recurses through ExApplication's alpha argument"
        , ExApplication ExRoot (ArAlpha (Alpha 0) (ExFormation []))
        , ExApplication ExRoot (ArAlpha (Alpha 0) (ExFormation [BiVoid AtRho]))
        )
      , ("leaves every other expression untouched", ExXi, ExXi)
      ]
      (\(desc, expr, expected) -> it desc (recoverFormations expr `shouldBe` expected))

  describe "fqnToAttrs" $
    forM_
      [
        ( "converts a dispatch chain into an attribute list, root first"
        , ExDispatch (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number")
        , Just [AtLabel "org", AtLabel "eolang", AtLabel "number"]
        )
      , ("is Just [] for the bare root", ExRoot, Just [])
      , ("is Nothing for an expression that is not a dispatch chain", ExFormation [], Nothing)
      ]
      (\(desc, expr, expected) -> it desc (fqnToAttrs expr `shouldBe` expected))

  describe "toDouble" $
    it "converts an Int to the equal Double" $
      toDouble 5 `shouldBe` 5.0
