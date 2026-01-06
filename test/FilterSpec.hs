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

import Control.Monad (forM_, when)
import Data.Aeson
import Data.Yaml qualified as Yaml
import Encoding (Encoding (UNICODE))
import Filter qualified as F
import GHC.Generics (Generic)
import Lining (LineFormat (MULTILINE))
import Margin (defaultMargin)
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
spec = describe "filter packs" $ do
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
            cfg = (SALTY, UNICODE, MULTILINE, defaultMargin)
        prog' `shouldBe` res
        when
          (prog' /= res)
          ( expectationFailure
              ( "Expected:\n"
                  ++ printProgram' res cfg
                  ++ "\nbut got:\n"
                  ++ printProgram' prog' cfg
              )
          )
    )
