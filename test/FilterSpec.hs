{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Filter module that provides include and exclude
functions for filtering phi-calculus programs by FQN expressions.
-}
module FilterSpec where

import AST
import Control.Monad (forM_, when)
import Data.Aeson
import Data.Yaml qualified as Yaml
import Encoding (Encoding (UNICODE))
import Filter qualified as F
import GHC.Generics (Generic)
import Lining (LineFormat (MULTILINE))
import Misc
import Parser (parseExpressionThrows, parseProgramThrows)
import Printer (printProgram')
import Sugar (SugarType (SALTY))
import System.FilePath
import Test.Hspec

data YamlPack = YamlPack
  { program :: String
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
          prog <- parseProgramThrows program
          included <- traverse parseExpressionThrows shown
          excluded <- traverse parseExpressionThrows hidden
          res <- parseProgramThrows result
          let [(prog', _)] = F.exclude (F.include [(prog, Nothing)] included) excluded
          prog' `shouldBe` res
          when
            (prog' /= res)
            ( expectationFailure
                ( "Expected:\n"
                    ++ printProgram' res (SALTY, UNICODE, MULTILINE)
                    ++ "\nbut got:\n"
                    ++ printProgram' prog' (SALTY, UNICODE, MULTILINE)
                )
            )
      )

  describe "exclude with empty expression list" $
    it "returns programs unchanged" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      let result = F.exclude [(prog, Nothing)] []
      result `shouldBe` [(prog, Nothing)]

  describe "exclude with empty program list" $
    it "returns empty list" $ do
      fqn <- parseExpressionThrows "Q.x"
      let result = F.exclude [] [fqn]
      result `shouldBe` []

  describe "exclude with non-formation program" $
    it "returns program unchanged" $ do
      let prog = Program ExGlobal
          fqn = ExDispatch ExGlobal (AtLabel "x")
          result = F.exclude [(prog, Nothing)] [fqn]
      result `shouldBe` [(prog, Nothing)]

  describe "exclude with termination program" $
    it "returns program unchanged" $ do
      let prog = Program ExTermination
          fqn = ExDispatch ExGlobal (AtLabel "x")
          result = F.exclude [(prog, Nothing)] [fqn]
      result `shouldBe` [(prog, Nothing)]

  describe "exclude with non-dispatch FQN" $
    it "handles non-dispatch expression gracefully" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      let fqn = ExGlobal
          [(prog', _)] = F.exclude [(prog, Nothing)] [fqn]
      prog' `shouldBe` prog

  describe "exclude with xi FQN" $
    it "handles xi expression gracefully" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      let fqn = ExThis
          [(prog', _)] = F.exclude [(prog, Nothing)] [fqn]
      prog' `shouldBe` prog

  describe "exclude multiple programs" $
    it "processes all programs in list" $ do
      prog1 <- parseProgramThrows "{[[ x -> Q, y -> $ ]]}"
      prog2 <- parseProgramThrows "{[[ a -> Q, b -> $ ]]}"
      fqn <- parseExpressionThrows "Q.x"
      let result = F.exclude [(prog1, Nothing), (prog2, Nothing)] [fqn]
      length result `shouldBe` 2

  describe "include with empty expression list" $
    it "returns programs unchanged" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      let result = F.include [(prog, Nothing)] []
      result `shouldBe` [(prog, Nothing)]

  describe "include with empty program list" $
    it "returns empty list" $ do
      fqn <- parseExpressionThrows "Q.x"
      let result = F.include [] [fqn]
      result `shouldBe` []

  describe "include with non-formation program" $
    it "returns empty formation" $ do
      let prog = Program ExGlobal
          fqn = ExDispatch ExGlobal (AtLabel "x")
          [(prog', _)] = F.include [(prog, Nothing)] [fqn]
      prog' `shouldBe` Program (ExFormation [BiVoid AtRho])

  describe "include with termination program" $
    it "returns empty formation" $ do
      let prog = Program ExTermination
          fqn = ExDispatch ExGlobal (AtLabel "x")
          [(prog', _)] = F.include [(prog, Nothing)] [fqn]
      prog' `shouldBe` Program (ExFormation [BiVoid AtRho])

  describe "include with non-existent attribute" $
    it "returns empty formation" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      fqn <- parseExpressionThrows "Q.nonexistent"
      let [(prog', _)] = F.include [(prog, Nothing)] [fqn]
      prog' `shouldBe` Program (ExFormation [BiVoid AtRho])

  describe "include with non-matching nested attribute" $
    it "returns empty formation when path doesnt exist" $ do
      prog <- parseProgramThrows "{[[ x -> [[ y -> Q ]] ]]}"
      fqn <- parseExpressionThrows "Q.x.nonexistent"
      let [(prog', _)] = F.include [(prog, Nothing)] [fqn]
      prog' `shouldBe` Program (ExFormation [BiVoid AtRho])

  describe "include with non-dispatch FQN" $
    it "handles non-dispatch expression gracefully" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      let fqn = ExGlobal
          [(prog', _)] = F.include [(prog, Nothing)] [fqn]
      prog' `shouldBe` Program (ExFormation [BiVoid AtRho])

  describe "include with xi FQN" $
    it "handles xi expression gracefully" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      let fqn = ExThis
          [(prog', _)] = F.include [(prog, Nothing)] [fqn]
      prog' `shouldBe` Program (ExFormation [BiVoid AtRho])

  describe "include multiple programs" $
    it "processes all programs in list" $ do
      prog1 <- parseProgramThrows "{[[ x -> Q, y -> $ ]]}"
      prog2 <- parseProgramThrows "{[[ a -> Q, b -> $ ]]}"
      fqn <- parseExpressionThrows "Q.x"
      let result = F.include [(prog1, Nothing), (prog2, Nothing)] [fqn]
      length result `shouldBe` 2

  describe "include with nested formation not matching" $
    it "skips non-matching bindings to find target" $ do
      prog <- parseProgramThrows "{[[ a -> Q, x -> [[ y -> Q ]] ]]}"
      fqn <- parseExpressionThrows "Q.x.y"
      let [(prog', _)] = F.include [(prog, Nothing)] [fqn]
          isFormation (Program (ExFormation _)) = True
          isFormation _ = False
      prog' `shouldSatisfy` isFormation

  describe "include with deep nested path" $
    it "follows multi-level FQN" $ do
      prog <- parseProgramThrows "{[[ a -> [[ b -> [[ c -> Q ]] ]] ]]}"
      fqn <- parseExpressionThrows "Q.a.b.c"
      let [(prog', _)] = F.include [(prog, Nothing)] [fqn]
          isFormation (Program (ExFormation _)) = True
          isFormation _ = False
      prog' `shouldSatisfy` isFormation

  describe "exclude with deep nested path" $
    it "removes from multi-level FQN" $ do
      prog <- parseProgramThrows "{[[ a -> [[ b -> [[ c -> Q, d -> $ ]] ]] ]]}"
      fqn <- parseExpressionThrows "Q.a.b.c"
      let [(prog', _)] = F.exclude [(prog, Nothing)] [fqn]
          isFormation (Program (ExFormation _)) = True
          isFormation _ = False
      prog' `shouldSatisfy` isFormation

  describe "exclude preserves rule metadata" $
    it "keeps Nothing rule through exclude" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      fqn <- parseExpressionThrows "Q.y"
      let [(_, rule)] = F.exclude [(prog, Nothing)] [fqn]
      rule `shouldBe` Nothing

  describe "include preserves rule metadata" $
    it "keeps Nothing rule through include" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      fqn <- parseExpressionThrows "Q.x"
      let [(_, rule)] = F.include [(prog, Nothing)] [fqn]
      rule `shouldBe` Nothing

  describe "exclude with non-formation nested binding" $
    it "handles tau with non-formation value" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      fqn <- parseExpressionThrows "Q.x.y"
      let [(prog', _)] = F.exclude [(prog, Nothing)] [fqn]
      prog' `shouldBe` prog

  describe "include with formation lacking target binding" $
    it "returns empty formation when binding not found" $ do
      prog <- parseProgramThrows "{[[ x -> [[ a -> Q ]] ]]}"
      fqn <- parseExpressionThrows "Q.y.z"
      let [(prog', _)] = F.include [(prog, Nothing)] [fqn]
      prog' `shouldBe` Program (ExFormation [BiVoid AtRho])

  describe "exclude with void binding" $
    it "handles void bindings correctly" $ do
      prog <- parseProgramThrows "{[[ x -> ?, y -> Q ]]}"
      fqn <- parseExpressionThrows "Q.x"
      let [(prog', _)] = F.exclude [(prog, Nothing)] [fqn]
          Program (ExFormation bds) = prog'
      length bds `shouldBe` 2

  describe "exclude with meta binding" $
    it "handles meta bindings correctly" $ do
      let prog = Program (ExFormation [BiMeta "B", BiTau (AtLabel "x") ExGlobal])
          fqn = ExDispatch ExGlobal (AtLabel "x")
          [(prog', _)] = F.exclude [(prog, Nothing)] [fqn]
          Program (ExFormation bds) = prog'
      length bds `shouldBe` 1

  describe "include uses only first FQN" $
    it "ignores additional FQNs in list" $ do
      prog <- parseProgramThrows "{[[ x -> Q, y -> $ ]]}"
      fqn1 <- parseExpressionThrows "Q.x"
      fqn2 <- parseExpressionThrows "Q.y"
      let [(prog', _)] = F.include [(prog, Nothing)] [fqn1, fqn2]
          Program (ExFormation bds) = prog'
          names = [attr | BiTau attr _ <- bds]
      AtLabel "x" `elem` names `shouldBe` True
