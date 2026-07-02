-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MergeSpec where

import Control.Monad (forM_)
import Data.List (intercalate)
import Merge (merge)
import Parser (parseExpressionThrows)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldThrow)

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
