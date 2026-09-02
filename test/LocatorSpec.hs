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
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, shouldBe, shouldThrow)
import Text.Printf (printf)

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

    forM_
      [ ("fails with CanNotFindObjectByLocator on a missing multi-level attribute", "[[ x -> [[ y -> ? ]] ]]", parseExpressionThrows "Q.x.z", canNotFindObjectMessage)
      , ("fails with CanNotFindObjectByLocator when an intermediate (non-final) attribute is missing", "[[ w -> ? ]]", parseExpressionThrows "Q.x.y.z", canNotFindObjectMessage)
      , ("fails with CanNotFindObjectByLocator when the chain runs into a non-formation", "[[ x -> $ ]]", parseExpressionThrows "Q.x.y", canNotFindObjectMessage)
      , ("fails with InvalidLocatorProvided on a non-Q-dispatch locator", "[[ x -> ? ]]", parseExpressionThrows "$.x", invalidLocatorMessage)
      , ("throws InvalidLocatorProvided with the exact message for a non-dispatch-chain locator expression", "[[ x -> ? ]]", pure ExXi, invalidLocatorMessage)
      , ("throws InvalidLocatorProvided when the locator is a bare formation, not a dispatch chain", "[[ x -> ? ]]", pure (ExFormation []), invalidLocatorMessage)
      , ("throws CanNotFindObjectByLocator with the exact message for a missing attribute", "[[ x -> ? ]]", parseExpressionThrows "Q.y", canNotFindObjectMessage)
      ]
      ( \(desc, exprText, locatorAction, messageOf) -> it desc $ do
          expr' <- parseExpressionThrows exprText
          locator' <- locatorAction
          locatedExpression locator' expr' `shouldThrow` anyException
          result <- try (locatedExpression locator' expr') :: IO (Either SomeException Expression)
          case result of
            Left err -> do
              show err `shouldBe` messageOf locator'
              displayException err `shouldBe` messageOf locator'
            Right _ -> expectationFailure "expected locatedExpression to throw"
      )

  describe "with located expression" $ do
    forM_
      [ ("[[ x -> $ ]]", "Q.x", "[[ y -> ? ]]", "[[ x -> [[ y -> ? ]] ]]")
      , ("[[ x -> ?, y -> [[ x -> ?, y -> [[ ]] ]] ]]", "Q.y.y", "Q.x.y", "[[ x -> ?, y -> [[ x -> ?, y -> Q.x.y ]] ]]")
      , ("[[ x -> [[ y -> [[ z -> [[ w -> ? ]] ]] ]] ]]", "Q.x.y", "$.a(x -> [[]])", "[[ x -> [[ y -> $.a(x -> [[]]) ]] ]]")
      , ("[[ b -> 1, x -> [[ y -> $ ]] ]]", "Q.x.y", "5", "[[ b -> 1, x -> [[ y -> 5 ]] ]]")
      , ("[[ a -> ?, x -> [[ y -> $ ]] ]]", "Q.x.y", "5", "[[ a -> ?, x -> [[ y -> 5 ]] ]]")
      , ("[[ L> Func, y -> $ ]]", "Q.y", "5", "[[ L> Func, y -> 5 ]]")
      , ("[[ x -> ?, y -> $ ]]", "Q.y", "5", "[[ x -> ?, y -> 5 ]]")
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

    forM_
      [ ("fails with CanNotFindObjectByLocator when the multi-attr chain is exhausted", "[[ x -> [[ z -> ? ]] ]]", parseExpressionThrows "Q.x.y.w", canNotFindObjectMessage)
      , ("fails with CanNotFindObjectByLocator when the multi-attr chain runs into a non-formation", "[[ x -> $ ]]", parseExpressionThrows "Q.x.y", canNotFindObjectMessage)
      , ("fails with InvalidLocatorProvided on a non-Q-dispatch locator", "[[ x -> ? ]]", parseExpressionThrows "$.x", invalidLocatorMessage)
      , ("throws InvalidLocatorProvided with the exact message for a non-dispatch-chain locator expression", "[[ x -> ? ]]", pure ExXi, invalidLocatorMessage)
      , ("throws InvalidLocatorProvided when the locator is a bare formation, not a dispatch chain", "[[ x -> ? ]]", pure (ExFormation []), invalidLocatorMessage)
      , ("throws CanNotFindObjectByLocator with the exact message for a missing attribute", "[[ x -> ? ]]", parseExpressionThrows "Q.y", canNotFindObjectMessage)
      ]
      ( \(desc, inputText, locatorAction, messageOf) -> it desc $ do
          input' <- parseExpressionThrows inputText
          locator' <- locatorAction
          expr' <- parseExpressionThrows "5"
          withLocatedExpression locator' expr' input' `shouldThrow` anyException
          result <- try (withLocatedExpression locator' expr' input') :: IO (Either SomeException Expression)
          case result of
            Left err -> do
              show err `shouldBe` messageOf locator'
              displayException err `shouldBe` messageOf locator'
            Right _ -> expectationFailure "expected withLocatedExpression to throw"
      )
