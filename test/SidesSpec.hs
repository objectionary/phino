{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module SidesSpec where

import AST
import Control.Monad (foldM)
import Sides (gauge, minted, named, resolver, sides, spelled)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "gauge" $ do
    it "keeps the depth of every name it hands out" $ do
      store <- sides
      top <- foldM (\prev _ -> named store (ExApplication (ExApplication (ExDispatch ExRoot (AtLabel "f")) (ArTau (AtLabel "a") prev)) (ArTau (AtLabel "b") prev))) ExXi [1 .. 30 :: Int]
      depth <- gauge store
      look <- resolver store
      (deepness look depth top, deepness look (const Nothing) (ExDispatch ExRoot (AtLabel "q"))) `shouldBe` (61, 1)
    it "measures a name as the term it stands for" $ do
      store <- sides
      inner <- named store (ExApplication (ExDispatch ExRoot (AtLabel "g")) (ArTau (AtLabel "x") ExXi))
      outer <- named store (ExDispatch inner (AtLabel "q"))
      depth <- gauge store
      look <- resolver store
      deepness look depth outer `shouldBe` deepness (const Nothing) (const Nothing) (ExDispatch (ExApplication (ExDispatch ExRoot (AtLabel "g")) (ArTau (AtLabel "x") ExXi)) (AtLabel "q"))
  describe "spelled" $
    it "walks every distinct term once in a term holding the level below it twice" $ do
      let doubled :: Expression -> Expression
          doubled inner = ExFormation [BiTau AtRho inner, BiTau (AtLabel "x") inner]
          deep :: Expression -> Expression
          deep leaf = iterate doubled leaf !! 60
          arg :: Expression
          arg = ExApplication (ExDispatch ExRoot (AtLabel "f")) (ArTau (AtLabel "a") (ExDispatch ExRoot (AtLabel "g")))
      store <- sides
      (name, _) <- minted store arg
      found <- timeout 10000000 (hashExpression <$> spelled store (deep name))
      found `shouldBe` Just (hashExpression (deep arg))
