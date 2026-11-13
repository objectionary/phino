{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MergeSpec where

import AST (Expression)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
import Data.Yaml qualified as Yaml
import Files (allPathsIn)
import GHC.Generics (Generic)
import Merge (merge)
import Parser (Form (MONOFORM), parseExpressionThrows, parseExpressionThrows')
import Printer (printExpression)
import System.FilePath
import Test.Hspec
import Text.Printf (printf)

data YamlPack = YamlPack
  { left :: String
  , right :: String
  , result :: Maybe String
  , fails :: Maybe String
  }
  deriving (Generic, Show, FromJSON)

yamlPack :: FilePath -> IO YamlPack
yamlPack = Yaml.decodeFileThrow

named :: [Expression] -> [(FilePath, Expression)]
named = zip ["left.phi", "right.phi"]

spec :: Spec
spec = do
  describe "merge packs" $ do
    let resources = "test-resources/merge-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          YamlPack{..} <- yamlPack pth
          parsed <- mapM (parseExpressionThrows' MONOFORM) [left, right]
          case (result, fails) of
            (Just expected, Nothing) -> do
              merged <- merge (named parsed)
              expected' <- parseExpressionThrows' MONOFORM expected
              merged `shouldBe` expected'
            (Nothing, Just message) -> do
              thrown <- try (merge (named parsed)) :: IO (Either SomeException Expression)
              case thrown of
                Left err -> show err `shouldContain` message
                Right merged -> expectationFailure (printf "Merge united the sides into %s, while the pack expects it to fail" (printExpression merged))
            _ -> expectationFailure "The pack holds neither a single 'result' nor a single 'fails'"
      )

  describe "merges a list of any length" $ do
    it "unites three formations into one" $ do
      parsed <- mapM parseExpressionThrows ["[[ x -> 1 ]]", "[[ y -> 2 ]]", "[[ z -> 3 ]]"]
      merged <- merge (zip ["a.phi", "b.phi", "c.phi"] parsed)
      expected <- parseExpressionThrows "[[ x -> 1, y -> 2, z -> 3 ]]"
      merged `shouldBe` expected

    it "returns a lonely formation untouched" $ do
      parsed <- parseExpressionThrows "[[ x -> 1 ]]"
      merged <- merge [("a.phi", parsed)]
      merged `shouldBe` parsed

    it "dont accept an empty list of expressions" $ do
      thrown <- try (merge []) :: IO (Either SomeException Expression)
      case thrown of
        Left err -> show err `shouldContain` "Nothing to merge: provide at least one expression"
        Right merged -> expectationFailure (printf "Merge answered %s, while an empty list has nothing to unite" (printExpression merged))

  describe "names the place and the files of a clash" $ do
    it "names the locator of a deep conflict and both files" $ do
      parsed <- mapM parseExpressionThrows ["[[ x -> [[ y -> [[ z -> Q ]]]]]]", "[[ x -> [[ y -> [[ z -> ξ ]]]]]]"]
      thrown <- try (merge (named parsed)) :: IO (Either SomeException Expression)
      case thrown of
        Left err -> do
          show err `shouldContain` "at Φ.x.y"
          show err `shouldContain` "left.phi"
          show err `shouldContain` "right.phi"
        Right merged -> expectationFailure (printf "Merge united the sides into %s, while they clash deep inside" (printExpression merged))

    it "names the whole program when the top-level bindings clash" $ do
      parsed <- mapM parseExpressionThrows ["[[ x -> 1 ]]", "[[ x -> 2 ]]"]
      thrown <- try (merge (named parsed)) :: IO (Either SomeException Expression)
      case thrown of
        Left err -> do
          show err `shouldContain` "at Φ,"
          show err `shouldContain` "left.phi"
          show err `shouldContain` "right.phi"
          show err `shouldNotContain` "at Φ."
        Right merged -> expectationFailure (printf "Merge united the sides into %s, while the top-level bindings clash" (printExpression merged))

    it "names the two files of a clashing binding among three" $ do
      parsed <-
        mapM
          parseExpressionThrows
          [ "⟦ org ↦ ⟦ eolang ↦ ⟦ number ↦ ⟦ plus ↦ ⟦ λ ⤍ L_plus ⟧ ⟧ ⟧ ⟧ ⟧"
          , "⟦ org ↦ ⟦ eolang ↦ ⟦ bytes ↦ ⟦⟧ ⟧ ⟧ ⟧"
          , "⟦ org ↦ ⟦ eolang ↦ ⟦ number ↦ ⟦ plus ↦ ⟦ λ ⤍ L_add ⟧ ⟧ ⟧ ⟧ ⟧"
          ]
      thrown <- try (merge (zip ["one.phi", "two.phi", "three.phi"] parsed)) :: IO (Either SomeException Expression)
      case thrown of
        Left err -> do
          show err `shouldContain` "at Φ.org.eolang.number.plus"
          show err `shouldContain` "one.phi"
          show err `shouldContain` "three.phi"
          show err `shouldNotContain` "two.phi"
        Right merged -> expectationFailure (printf "Merge united the sides into %s, while the third file clashes with the first" (printExpression merged))

    it "names the file that holds the clashing binding, not the file of the union" $ do
      left <- parseExpressionThrows "[[ x -> [[ p -> 1 ]] ]]"
      one <- parseExpressionThrows "[[ x -> [[ q -> 2 ]] ]]"
      two <- parseExpressionThrows "[[ x -> [[ q -> 3 ]] ]]"
      let orders =
            [ zip ["a.phi", "b.phi", "c.phi"] [left, one, two]
            , zip ["a.phi", "c.phi", "b.phi"] [left, two, one]
            ]
      forM_ orders $ \ordered -> do
        thrown <- try (merge ordered) :: IO (Either SomeException Expression)
        case thrown of
          Left err -> do
            show err `shouldContain` "at Φ.x"
            show err `shouldContain` "b.phi"
            show err `shouldNotContain` "a.phi"
          Right merged -> expectationFailure (printf "Merge united the sides into %s, while the last two sides clash inside x" (printExpression merged))
