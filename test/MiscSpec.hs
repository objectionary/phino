{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MiscSpec where

import AST
import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Misc
  ( attributesFromBindings
  , attributesFromBindings'
  , fqnToAttrs
  , orThrow
  , toDouble
  , uniqueBindings
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldReturn, shouldSatisfy)

spec :: Spec
spec = do
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
