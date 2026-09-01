{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LocatorSpec where

import AST (Attribute (AtLabel), Binding (BiTau, BiVoid), Expression (ExFormation, ExRoot, ExXi))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_)
import Data.List (intercalate)
import Locator (locatedExpression, withLocatedExpression)
import Parser (parseExpressionThrows)
import Printer (printExpression)
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, shouldBe, shouldSatisfy, shouldThrow)
import Text.Printf (printf)

isLeft :: Either e a -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

invalidLocatorMessage :: Expression -> String
invalidLocatorMessage locator =
  printf
    "Invalid locator is provided. 'Q' or dispatch started with 'Q' expected, but got: '%s'"
    (printExpression locator)

canNotFindObjectMessage :: Expression -> String
canNotFindObjectMessage locator = printf "Can't find object by locator: '%s'" (printExpression locator)

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

    it "fails with CanNotFindObjectByLocator when an intermediate (non-final) attribute is missing" $ do
      expr' <- parseExpressionThrows "[[ w -> ? ]]"
      locator' <- parseExpressionThrows "Q.x.y.z"
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

    it "throws InvalidLocatorProvided with the exact message for a non-dispatch-chain locator expression" $ do
      expr' <- parseExpressionThrows "[[ x -> ? ]]"
      locatedExpression ExXi expr' `shouldThrow` anyException
      result <- try (locatedExpression ExXi expr') :: IO (Either SomeException Expression)
      case result of
        Left err -> do
          show err `shouldBe` invalidLocatorMessage ExXi
          displayException err `shouldBe` invalidLocatorMessage ExXi
        Right _ -> expectationFailure "expected locatedExpression to throw"

    it "throws InvalidLocatorProvided when the locator is a bare formation, not a dispatch chain" $ do
      expr' <- parseExpressionThrows "[[ x -> ? ]]"
      let locator' = ExFormation []
      locatedExpression locator' expr' `shouldThrow` anyException
      result <- try (locatedExpression locator' expr') :: IO (Either SomeException Expression)
      case result of
        Left err -> do
          show err `shouldBe` invalidLocatorMessage locator'
          displayException err `shouldBe` invalidLocatorMessage locator'
        Right _ -> expectationFailure "expected locatedExpression to throw"

    it "throws CanNotFindObjectByLocator with the exact message for a missing attribute" $ do
      expr' <- parseExpressionThrows "[[ x -> ? ]]"
      locator' <- parseExpressionThrows "Q.y"
      result <- try (locatedExpression locator' expr') :: IO (Either SomeException Expression)
      case result of
        Left err -> do
          show err `shouldBe` canNotFindObjectMessage locator'
          displayException err `shouldBe` canNotFindObjectMessage locator'
        Right _ -> expectationFailure "expected locatedExpression to throw"

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

    it "passes a non-tau, non-void binding through unchanged before replacing the match" $ do
      input' <- parseExpressionThrows "[[ L> Func, y -> $ ]]"
      locator' <- parseExpressionThrows "Q.y"
      expr' <- parseExpressionThrows "5"
      res' <- parseExpressionThrows "[[ L> Func, y -> 5 ]]"
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

    it "throws InvalidLocatorProvided with the exact message for a non-dispatch-chain locator expression" $ do
      input' <- parseExpressionThrows "[[ x -> ? ]]"
      expr' <- parseExpressionThrows "5"
      withLocatedExpression ExXi expr' input' `shouldThrow` anyException
      result <- try (withLocatedExpression ExXi expr' input') :: IO (Either SomeException Expression)
      case result of
        Left err -> do
          show err `shouldBe` invalidLocatorMessage ExXi
          displayException err `shouldBe` invalidLocatorMessage ExXi
        Right _ -> expectationFailure "expected withLocatedExpression to throw"

    it "throws InvalidLocatorProvided when the locator is a bare formation, not a dispatch chain" $ do
      input' <- parseExpressionThrows "[[ x -> ? ]]"
      expr' <- parseExpressionThrows "5"
      let locator' = ExFormation []
      withLocatedExpression locator' expr' input' `shouldThrow` anyException
      result <- try (withLocatedExpression locator' expr' input') :: IO (Either SomeException Expression)
      case result of
        Left err -> do
          show err `shouldBe` invalidLocatorMessage locator'
          displayException err `shouldBe` invalidLocatorMessage locator'
        Right _ -> expectationFailure "expected withLocatedExpression to throw"

    it "throws CanNotFindObjectByLocator with the exact message for a missing attribute" $ do
      input' <- parseExpressionThrows "[[ x -> ? ]]"
      locator' <- parseExpressionThrows "Q.y"
      expr' <- parseExpressionThrows "5"
      result <- try (withLocatedExpression locator' expr' input') :: IO (Either SomeException Expression)
      case result of
        Left err -> do
          show err `shouldBe` canNotFindObjectMessage locator'
          displayException err `shouldBe` canNotFindObjectMessage locator'
        Right _ -> expectationFailure "expected withLocatedExpression to throw"
