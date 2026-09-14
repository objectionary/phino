{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Merge module that unites a few top level formations
into a single one.
-}
module MergeSpec where

import AST (Expression)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
import Data.Yaml qualified as Yaml
import Files (allPathsIn)
import GHC.Generics (Generic)
import Merge (merge)
import Parser (parseExpressionThrows)
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

spec :: Spec
spec = do
  describe "merge packs" $ do
    let resources = "test-resources/merge-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          YamlPack{..} <- yamlPack pth
          parsed <- mapM parseExpressionThrows [left, right]
          case (result, fails) of
            (Just expected, Nothing) -> do
              merged <- merge parsed
              expected' <- parseExpressionThrows expected
              merged `shouldBe` expected'
            (Nothing, Just message) -> do
              thrown <- try (merge parsed) :: IO (Either SomeException Expression)
              case thrown of
                Left err -> show err `shouldContain` message
                Right merged -> expectationFailure (printf "Merge united the sides into %s, while the pack expects it to fail" (printExpression merged))
            _ -> expectationFailure "The pack holds neither a single 'result' nor a single 'fails'"
      )

  describe "merges a list of any length" $ do
    it "unites three formations into one" $ do
      parsed <- mapM parseExpressionThrows ["[[ x -> 1 ]]", "[[ y -> 2 ]]", "[[ z -> 3 ]]"]
      merged <- merge parsed
      expected <- parseExpressionThrows "[[ x -> 1, y -> 2, z -> 3 ]]"
      merged `shouldBe` expected

    it "returns a lonely formation untouched" $ do
      parsed <- parseExpressionThrows "[[ x -> 1 ]]"
      merged <- merge [parsed]
      merged `shouldBe` parsed

    it "dont accept an empty list of expressions" $ do
      thrown <- try (merge []) :: IO (Either SomeException Expression)
      case thrown of
        Left err -> show err `shouldContain` "Nothing to merge: provide at least one expression"
        Right merged -> expectationFailure (printf "Merge answered %s, while an empty list has nothing to unite" (printExpression merged))
