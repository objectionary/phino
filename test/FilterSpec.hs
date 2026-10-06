{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FilterSpec where

import AST (Expression (ExRoot))
import Control.Monad (forM_)
import Data.Aeson
import Data.Yaml qualified as Yaml
import Deps (Judgment (..))
import Files (allPathsIn)
import Filter qualified as F
import GHC.Generics (Generic)
import Parser (parseExpressionThrows)
import System.FilePath
import Test.Hspec

data YamlPack = YamlPack
  { expression :: String
  , shown :: [String]
  , hidden :: [String]
  , result :: String
  }
  deriving (Generic, Show, FromJSON)

yamlPack :: FilePath -> IO YamlPack
yamlPack = Yaml.decodeFileThrow

spec :: Spec
spec = do
  describe "filter packs" $ do
    let resources = "test-resources/filter-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          YamlPack{..} <- yamlPack pth
          expr <- parseExpressionThrows expression
          included <- traverse parseExpressionThrows shown
          excluded <- traverse parseExpressionThrows hidden
          res <- parseExpressionThrows result
          [(expr', _)] <- F.include [(expr, Nothing)] included >>= (`F.exclude` excluded)
          expr' `shouldBe` res
      )

  describe "direct unit tests" $ do
    describe "exclude" $ do
      forM_
        [ ("fails when the fqn is not a Q-dispatch chain", "[[ x -> ?, y -> ? ]]", "$.x")
        , ("fails when the fqn is the whole program", "[[ x -> ?, y -> ? ]]", "Q")
        , ("fails when nothing matches the fqn", "[[ x -> ? ]]", "Q.nope")
        , ("fails when a nested fqn stops short of its last attribute", "[[ x -> [[ y -> ? ]] ]]", "Q.x.nope")
        , ("fails for a non-formation expression", "Q.x", "Q.y")
        , ("fails when the fqn walks into something that is not a formation", "[[ org -> [[ eolang -> Q.x ]] ]]", "Q.org.eolang.number")
        ]
        ( \(desc, phi, locator) ->
            it desc $ do
              expr <- parseExpressionThrows phi
              fqn <- parseExpressionThrows locator
              F.exclude [(expr, Nothing)] [fqn] `shouldThrow` anyException
        )

      it "recurses over a multi-element rewrite list, preserving each rule label" $ do
        first' <- parseExpressionThrows "[[ x -> ?, y -> ? ]]"
        second' <- parseExpressionThrows "[[ x -> ?, y -> ? ]]"
        fqn <- parseExpressionThrows "Q.x"
        expected <- parseExpressionThrows "[[ y -> ? ]]"
        excluded <- F.exclude [(first', Just (Normalization, "rule-a")), (second', Just (Evaluation, "rule-b"))] [fqn]
        map fst excluded `shouldBe` [expected, expected]
        map snd excluded `shouldBe` [Just (Normalization, "rule-a"), Just (Evaluation, "rule-b")]

    describe "include" $ do
      forM_
        [ ("fails when the fqn is not a Q-dispatch chain", "[[ x -> ? ]]", "$.x")
        , ("fails when nothing matches the fqn", "[[ x -> ? ]]", "Q.absent")
        , ("fails when a nested fqn stops short of its last attribute", "[[ a -> [[ b -> ? ]] ]]", "Q.a.zzz")
        , ("fails for a non-formation expression", "Q.x", "Q.y")
        ]
        ( \(desc, exprText, fqnText) -> it desc $ do
            expr <- parseExpressionThrows exprText
            fqn <- parseExpressionThrows fqnText
            F.include [(expr, Nothing)] [fqn] `shouldThrow` anyException
        )

      it "keeps the whole program when the fqn is Q" $ do
        expr <- parseExpressionThrows "[[ a -> [[ b -> ?, c -> ? ]], d -> ? ]]"
        [(expr', _)] <- F.include [(expr, Nothing)] [ExRoot]
        expr' `shouldBe` expr

      it "fails when one of several fqns matches nothing" $ do
        expr <- parseExpressionThrows "[[ x -> ?, y -> ? ]]"
        found <- parseExpressionThrows "Q.x"
        absent <- parseExpressionThrows "Q.zzz"
        F.include [(expr, Nothing)] [found, absent] `shouldThrow` anyException

      it "recurses over a multi-element rewrite list, pinning every element to the fqns" $ do
        first' <- parseExpressionThrows "[[ x -> ?, y -> ? ]]"
        second' <- parseExpressionThrows "[[ x -> ?, y -> ? ]]"
        fqn <- parseExpressionThrows "Q.x"
        expected <- parseExpressionThrows "[[ x -> ? ]]"
        included <- F.include [(first', Just (Normalization, "rule-a")), (second', Just (Evaluation, "rule-b"))] [fqn]
        map fst included `shouldBe` [expected, expected]
        map snd included `shouldBe` [Just (Normalization, "rule-a"), Just (Evaluation, "rule-b")]

      it "keeps every matching fqn, not only the first one" $ do
        expr <- parseExpressionThrows "[[ x -> ?, y -> ? ]]"
        firstFqn <- parseExpressionThrows "Q.x"
        secondFqn <- parseExpressionThrows "Q.y"
        expected <- parseExpressionThrows "[[ x -> ?, y -> ? ]]"
        [(expr', _)] <- F.include [(expr, Nothing)] [firstFqn, secondFqn]
        expr' `shouldBe` expected
