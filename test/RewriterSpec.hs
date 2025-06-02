{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import Control.Monad (forM_)
import Data.Aeson
import Data.Yaml qualified as Yaml
import GHC.Generics
import Misc (allPathsIn)
import Rewriter (rewrite)
import Test.Hspec (Spec, describe, runIO, shouldBe, it)
import qualified Yaml
import System.FilePath (makeRelative)

data YamlPack = YamlPack
  { input :: String,
    output :: String,
    ruleSet :: Maybe Yaml.RuleSet
  }
  deriving (Generic, FromJSON, Show)

yamlPack :: FilePath -> IO YamlPack
yamlPack = Yaml.decodeFileThrow

spec :: Spec
spec = do
  describe "rewrite packs" $ do
    let resources = "test-resources/rewriter-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> do
          pack <- runIO $ yamlPack pth
          let output' = output pack
              input' = input pack
              ruleSet' = ruleSet pack
          rewritten <- runIO $ rewrite input' ruleSet'
          it (makeRelative resources pth) (rewritten `shouldBe` output')
      )
