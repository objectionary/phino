-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LocatorSpec where

import Control.Monad (forM_)
import Data.List (intercalate)
import Locator (locatedExpression, withLocatedExpression)
import Parser (parseExpressionThrows)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "located expression" $
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

  describe "with located expression" $
    forM_
      [ ("[[ x -> $ ]]", "Q.x", "[[ y -> ? ]]", "[[ x -> [[ y -> ? ]] ]]")
      , ("[[ x -> ?, y -> [[ x -> ?, y -> [[ ]] ]] ]]", "Q.y.y", "Q.x.y", "[[ x -> ?, y -> [[ x -> ?, y -> Q.x.y ]] ]]")
      , ("[[ x -> [[ y -> [[ z -> [[ w -> ? ]] ]] ]] ]]", "Q.x.y", "$.a(x -> [[]])", "[[ x -> [[ y -> $.a(x -> [[]]) ]] ]]")
      ]
      ( \(input, locator, expr, res) -> it (intercalate " => " [input, locator, expr, res]) $ do
          input' <- parseExpressionThrows input
          locator' <- parseExpressionThrows locator
          expr' <- parseExpressionThrows expr
          res' <- parseExpressionThrows res
          loc <- withLocatedExpression locator' expr' input'
          loc `shouldBe` res'
      )
