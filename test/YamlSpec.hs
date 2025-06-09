{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module YamlSpec where

import Control.Monad
import Misc
import System.FilePath
import Test.Hspec (Spec, describe, it, runIO, shouldReturn, shouldNotBe, shouldBe)
import Yaml (yamlRule, Condition)
import Ast (Expression, Program (Program))
import qualified Data.Yaml as Y
import Matcher (matchProgram)
import Rewriter (meets)
import Data.Aeson
import GHC.Generics

data ConditionPack = ConditionPack
  {
    expression :: Expression,
    pattern :: Expression,
    condition :: Condition
  }
  deriving (Generic, FromJSON, Show)

conditionPack :: FilePath -> IO ConditionPack
conditionPack = Y.decodeFileThrow

spec :: Spec
spec = do
  describe "parses yaml rule" $ do
    let resources = "test-resources/yaml-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          _ <- yamlRule pth
          pure () `shouldReturn` ()
      )
  
  describe "check conditions" $ do
    let resources = "test-resources/condition-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- conditionPack pth
          let matched = matchProgram (pattern pack) (Program (expression pack))
          matched `shouldNotBe` []
          meets (condition pack) matched `shouldBe` matched
      )

