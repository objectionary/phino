-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MiscSpec where

import AST
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Misc (uniqueBindings, withVoidRho)
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe, shouldSatisfy)

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
        , [BiTau AtRho (ExDispatch ExGlobal (AtLabel "x")), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        , [BiTau AtRho (ExDispatch ExGlobal (AtLabel "x")), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
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
        ( "[[!a -> ?, x -> $.y]] => [[!a -> Q.x, x -> $.y]]"
        , [BiVoid (AtMeta "a"), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        , [BiVoid (AtMeta "a"), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        )
      ,
        ( "[[!a -> Q.x, x -> $.y]] => [[!a -> Q.x, x -> $.y]]"
        , [BiTau (AtMeta "a") (ExDispatch ExGlobal (AtLabel "x")), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        , [BiTau (AtMeta "a") (ExDispatch ExGlobal (AtLabel "x")), BiTau AtRho (ExDispatch ExTermination (AtLabel "y"))]
        )
      ]

  describe "unique bindings" $ do
    it "fails with duplicate attribute" $
      uniqueBindings [BiVoid AtRho, BiVoid AtRho] `shouldSatisfy` isLeft
    it "does not fail on different attributes" $
      uniqueBindings [BiVoid AtPhi, BiVoid AtRho] `shouldSatisfy` isRight
