{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module YamlSpec where

import Ast (Expression)
import Control.Monad
import Data.Aeson
import GHC.Generics
import Misc
import System.FilePath
import Test.Hspec (Spec, describe, it, runIO, shouldReturn)
import Yaml (yamlRule)
import qualified Yaml as Y

data ConditionPack = ConditionPack
  { expression :: Expression,
    pattern :: Expression,
    condition :: Y.Condition
  }
  deriving (Generic, FromJSON, Show)

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
