{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ConditionSpec where

import Ast (Expression, Program (Program))
import Condition (meetCondition)
import Control.Monad
import Data.Aeson
import Data.Yaml qualified as Y
import GHC.Generics
import Matcher (matchProgram)
import Misc
import Printer (printSubstitutions)
import System.FilePath
import Test.Hspec (Spec, describe, expectationFailure, it, runIO)
import Yaml qualified as Y

data ConditionPack = ConditionPack
  { expression :: Expression,
    pattern :: Expression,
    condition :: Y.Condition
  }
  deriving (Generic, FromJSON, Show)

spec :: Spec
spec = do
  describe "check conditions" $ do
    let resources = "test-resources/condition-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- Y.decodeFileThrow pth :: IO ConditionPack
          let matched = matchProgram (pattern pack) (Program (expression pack))
          unless (matched /= []) (expectationFailure "List of matched substitutions is empty which is not expected")
          let met = meetCondition (condition pack) matched
          when
            (null met)
            ( expectationFailure $
                "List of substitution after condition check must be not empty\nOriginal substitutions:\n"
                  ++ printSubstitutions matched
            )
      )
