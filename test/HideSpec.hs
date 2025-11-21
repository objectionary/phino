{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module HideSpec where

import Control.Monad
import Data.Aeson
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Hide (hide)
import Misc
import Parser (parseExpressionThrows, parseProgramThrows)
import System.FilePath
import Test.Hspec

data YamlPack = YamlPack
  { program :: String,
    hidden :: String,
    result :: String
  }
  deriving (Generic, Show, FromJSON)

yamlPack :: FilePath -> IO YamlPack
yamlPack = Yaml.decodeFileThrow

spec :: Spec
spec =
  describe "hide packs" $ do
    let resources = "test-resources/hide-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          YamlPack {..} <- yamlPack pth
          prog <- parseProgramThrows program
          expr <- parseExpressionThrows hidden
          res <- parseProgramThrows result
          let [(prog', _)] = hide [(prog, Nothing)] [expr]
          prog' `shouldBe` res
      )
