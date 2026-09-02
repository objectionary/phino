-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MergeSpec where

import AST (Expression)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.List (intercalate)
import Merge (merge)
import Parser (parseExpressionThrows)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldContain, shouldThrow)

spec :: Spec
spec = do
  describe "merge expressions" $
    forM_
      [
        ( ["[[ x -> 1 ]]", "[[ y -> 2 ]]"]
        , "[[ x -> 1, y -> 2 ]]"
        )
      ,
        ( ["[[ x -> [[ y -> 1 ]] ]]", "[[ x -> [[ z -> 2 ]] ]]"]
        , "[[ x -> [[ y -> 1, z -> 2 ]] ]]"
        )
      ,
        ( ["[[ x -> 1 ]]", "[[ x -> 1]]"]
        , "[[ x -> 1]]"
        )
      ,
        ( ["[[ org -> [[ eolang -> [[ number -> [[ ]] ]] ]] ]]", "[[ org -> [[ eolang -> [[ bytes -> [[ ]] ]] ]] ]]"]
        , "[[ org -> [[ eolang -> [[ number -> [[ ]], bytes -> [[ ]] ]] ]] ]]"
        )
      ,
        ( ["[[ x -> 1 ]]", "[[ y -> 2 ]]", "[[ z -> 3 ]]"]
        , "[[ x -> 1, y -> 2, z -> 3 ]]"
        )
      ,
        ( ["[[ x -> ? ]]", "[[ x -> ? ]]"]
        , "[[ x -> ? ]]"
        )
      ,
        ( ["[[ D> 42-, x -> [[ ]] ]]", "[[ D> 42-, y -> [[ ]] ]]"]
        , "[[ x -> [[ ]], y -> [[ ]], D> 42- ]]"
        )
      ]
      ( \(exprs, res) -> it res $ do
          parsed <- mapM parseExpressionThrows exprs
          merged <- merge parsed
          res' <- parseExpressionThrows res
          merged `shouldBe` res'
      )

  describe "fails to merge" $
    forM_
      [ ["Q", "$"]
      , ["[[ x -> 1]]", "[[ x -> 2 ]]"]
      , ["[[ x -> [[ y -> Q ]] ]]", "[[ x -> [[ y -> $ ]] ]]"]
      ]
      ( \exprs -> it (intercalate " and " exprs) $ do
          parsed <- mapM parseExpressionThrows exprs
          merge parsed `shouldThrow` anyException
      )

  describe "merge exception messages" $
    forM_
      [
        ( "EmptyExpressionList explains there is nothing to merge"
        , []
        , "Nothing to merge: provide at least one expression"
        )
      ,
        ( "WrongExpressionFormat renders the offending non-formation expression"
        , ["Q"]
        , "Invalid expression format"
        )
      ,
        ( "CanNotMergeBinding renders both conflicting bindings"
        , ["[[ x -> 1 ]]", "[[ x -> 2 ]]"]
        , "Can't merge two bindings, conflict found"
        )
      ]
      ( \(desc, exprs, message) -> it desc $ do
          parsed <- mapM parseExpressionThrows exprs
          result <- try (merge parsed) :: IO (Either SomeException Expression)
          case result of
            Left err -> show err `shouldContain` message
            Right _ -> fail ("expected merge to throw for: " ++ desc)
      )

  describe "merge of a single expression" $
    it "returns that expression unchanged" $ do
      parsed <- parseExpressionThrows "[[ x -> 1 ]]"
      merged <- merge [parsed]
      merged `shouldBe` parsed
