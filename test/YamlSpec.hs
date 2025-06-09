{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module YamlSpec where

import Ast (Expression, Program (Program))
import Control.Monad
import Data.Aeson
import Data.Yaml qualified as Y
import GHC.Generics
import Matcher (matchProgram)
import Misc
import Printer (printSubstitutions)
import Rewriter (meets)
import System.FilePath
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldReturn)
import Yaml (Condition, yamlRule)

data ConditionPack = ConditionPack
  { expression :: Expression,
    pattern :: Expression,
    condition :: Condition
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

  describe "check conditions" $ do
    let resources = "test-resources/condition-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- Y.decodeFileThrow pth :: IO ConditionPack
          let matched = matchProgram (pattern pack) (Program (expression pack))
          unless (matched /= []) (expectationFailure "List of matched substitutions is empty which is not expected")
          let met = meets (condition pack) matched
          unless
            (met == matched)
            ( expectationFailure $
                "Condition must not decrease the list of substitutions\nExpected:\n"
                  ++ printSubstitutions matched
                  ++ "\nGot:\n" 
                  ++ printSubstitutions met
            )
      )
