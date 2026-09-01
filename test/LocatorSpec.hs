{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LocatorSpec where

import AST (Attribute (AtLabel), Binding (BiTau, BiVoid), Expression (ExFormation, ExRoot, ExXi))
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.List (intercalate)
import Locator (locatedExpression, withLocatedExpression)
import Parser (parseExpressionThrows)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

isLeft :: Either e a -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

spec :: Spec
spec = do
  describe "located expression" $ do
    forM_
      [ ("[[ x -> [[ y -> [[ z -> ? ]] ]] ]]", "Q.x.y", "[[ z -> ? ]]")
      , ("[[ x -> ?, y -> [[ z -> ?, w -> [[ a -> $.x ]] ]], z -> ? ]]", "Q.y.w.a", "$.x")
      , ("[[ x -> ?, y -> ? ]]", "Q", "[[ x -> ?, y -> ? ]]")
      ]
      ( \(expr, locator, res) -> it (intercalate " => " [expr, locator, res]) $ do
          expr' <- parseExpressionThrows expr
          locator' <- parseExpressionThrows locator
          res' <- parseExpressionThrows res
          located <- locatedExpression locator' expr'
          located `shouldBe` res'
      )

    it "short-circuits on ExRoot regardless of the given expression" $ do
      expr' <- parseExpressionThrows "[[ x -> ? ]]"
      located <- locatedExpression ExRoot expr'
      located `shouldBe` expr'

    it "fails with CanNotFindObjectByLocator on a missing single-level attribute" $ do
      expr' <- parseExpressionThrows "[[ x -> ? ]]"
      locator' <- parseExpressionThrows "Q.y"
      result <- try (locatedExpression locator' expr') :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

    it "fails with CanNotFindObjectByLocator on a missing multi-level attribute" $ do
      expr' <- parseExpressionThrows "[[ x -> [[ y -> ? ]] ]]"
      locator' <- parseExpressionThrows "Q.x.z"
      result <- try (locatedExpression locator' expr') :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

    it "fails with CanNotFindObjectByLocator when the chain runs into a non-formation" $ do
      expr' <- parseExpressionThrows "[[ x -> $ ]]"
      locator' <- parseExpressionThrows "Q.x.y"
      result <- try (locatedExpression locator' expr') :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

    it "fails with InvalidLocatorProvided on a non-Q-dispatch locator" $ do
      expr' <- parseExpressionThrows "[[ x -> ? ]]"
      locator' <- parseExpressionThrows "$.x"
      result <- try (locatedExpression locator' expr') :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

  describe "with located expression" $ do
    forM_
      [ ("[[ x -> $ ]]", "Q.x", "[[ y -> ? ]]", "[[ x -> [[ y -> ? ]] ]]")
      , ("[[ x -> ?, y -> [[ x -> ?, y -> [[ ]] ]] ]]", "Q.y.y", "Q.x.y", "[[ x -> ?, y -> [[ x -> ?, y -> Q.x.y ]] ]]")
      , ("[[ x -> [[ y -> [[ z -> [[ w -> ? ]] ]] ]] ]]", "Q.x.y", "$.a(x -> [[]])", "[[ x -> [[ y -> $.a(x -> [[]]) ]] ]]")
      , ("[[ b -> 1, x -> [[ y -> $ ]] ]]", "Q.x.y", "5", "[[ b -> 1, x -> [[ y -> 5 ]] ]]")
      , ("[[ a -> ?, x -> [[ y -> $ ]] ]]", "Q.x.y", "5", "[[ a -> ?, x -> [[ y -> 5 ]] ]]")
      ]
      ( \(input, locator, expr, res) -> it (intercalate " => " [input, locator, expr, res]) $ do
          input' <- parseExpressionThrows input
          locator' <- parseExpressionThrows locator
          expr' <- parseExpressionThrows expr
          res' <- parseExpressionThrows res
          loc <- withLocatedExpression locator' expr' input'
          loc `shouldBe` res'
      )

    it "short-circuits on ExRoot, ignoring the input expression" $ do
      target <- parseExpressionThrows "[[ y -> ? ]]"
      input' <- parseExpressionThrows "[[ x -> ? ]]"
      loc <- withLocatedExpression ExRoot target input'
      loc `shouldBe` target

    -- locatedInBindings only ever matches a BiTau binding, so a lone `x -> ?`
    -- (BiVoid) target can never satisfy the "Just" check that gates the
    -- replace in withLocatedExpression', even though withReplacedExpression
    -- itself does have a case for replacing a BiVoid. The only way to reach
    -- that case is a duplicate attribute name where a later BiTau binding
    -- with the same attribute makes locatedInBindings succeed, while
    -- withReplacedExpression still replaces the earlier (BiVoid) occurrence
    -- first.
    it "replaces the first (BiVoid) occurrence of a duplicated attribute" $ do
      -- Built directly rather than parsed: the parser itself rejects a
      -- duplicated attribute name, but Locator operates on Expression
      -- values regardless of how they were constructed.
      let input' = ExFormation [BiVoid (AtLabel "x"), BiTau (AtLabel "x") ExXi]
      locator' <- parseExpressionThrows "Q.x"
      expr' <- parseExpressionThrows "5"
      let res' = ExFormation [BiTau (AtLabel "x") expr', BiTau (AtLabel "x") ExXi]
      loc <- withLocatedExpression locator' expr' input'
      loc `shouldBe` res'

    it "passes non-matching bindings through before replacing the single-attr match" $ do
      input' <- parseExpressionThrows "[[ x -> ?, y -> $ ]]"
      locator' <- parseExpressionThrows "Q.y"
      expr' <- parseExpressionThrows "5"
      res' <- parseExpressionThrows "[[ x -> ?, y -> 5 ]]"
      loc <- withLocatedExpression locator' expr' input'
      loc `shouldBe` res'

    it "fails with CanNotFindObjectByLocator on a missing single-level attribute" $ do
      input' <- parseExpressionThrows "[[ x -> ? ]]"
      locator' <- parseExpressionThrows "Q.y"
      expr' <- parseExpressionThrows "5"
      result <- try (withLocatedExpression locator' expr' input') :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

    it "fails with CanNotFindObjectByLocator when the multi-attr chain is exhausted" $ do
      input' <- parseExpressionThrows "[[ x -> [[ z -> ? ]] ]]"
      locator' <- parseExpressionThrows "Q.x.y.w"
      expr' <- parseExpressionThrows "5"
      result <- try (withLocatedExpression locator' expr' input') :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

    it "fails with CanNotFindObjectByLocator when the multi-attr chain runs into a non-formation" $ do
      input' <- parseExpressionThrows "[[ x -> $ ]]"
      locator' <- parseExpressionThrows "Q.x.y"
      expr' <- parseExpressionThrows "5"
      result <- try (withLocatedExpression locator' expr' input') :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

    it "fails with InvalidLocatorProvided on a non-Q-dispatch locator" $ do
      input' <- parseExpressionThrows "[[ x -> ? ]]"
      locator' <- parseExpressionThrows "$.x"
      expr' <- parseExpressionThrows "5"
      result <- try (withLocatedExpression locator' expr' input') :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft
