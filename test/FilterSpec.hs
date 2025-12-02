{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FilterSpec where

import Control.Monad
import Data.Aeson
import Data.Yaml qualified as Yaml
import Filter qualified as F
import GHC.Generics (Generic)
import Misc
import Parser (parseExpressionThrows, parseProgramThrows)
import System.FilePath
import Test.Hspec

data YamlPack = YamlPack
  { program :: String,
    shown :: [String],
    hidden :: [String],
    result :: String
  }
  deriving (Generic, Show, FromJSON)

yamlPack :: FilePath -> IO YamlPack
yamlPack = Yaml.decodeFileThrow

spec :: Spec
spec =
  describe "filter packs" $ do
    let resources = "test-resources/filter-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          YamlPack {..} <- yamlPack pth
          prog <- parseProgramThrows program
          include <- traverse parseExpressionThrows shown
          exclude <- traverse parseExpressionThrows hidden
          res <- parseProgramThrows result
          let [(prog', _)] = F.hide (F.show [(prog, Nothing)] include) exclude
          prog' `shouldBe` res
      )
