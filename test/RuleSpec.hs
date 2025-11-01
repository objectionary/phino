{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RuleSpec where

import AST (Expression, Program (Program))
import Control.Monad
import Data.Aeson
import Data.Yaml qualified as Y
import Functions (buildTerm)
import GHC.Generics
import Matcher
import Misc
import Rule (RuleContext (RuleContext), meetCondition)
import System.FilePath
import Test.Hspec (Spec, describe, expectationFailure, it, runIO)
import Yaml qualified
import Printer (printSubsts)

data ConditionPack = ConditionPack
  { failure :: Maybe Bool,
    expression :: Expression,
    pattern :: Expression,
    condition :: Yaml.Condition
  }
  deriving (Generic, FromJSON, Show)

spec :: Spec
spec = describe "check conditions" $ do
  let resources = "test-resources/condition-packs"
  packs <- runIO (allPathsIn resources)
  forM_
    packs
    ( \pth -> it (makeRelative resources pth) $ do
        pack <- Y.decodeFileThrow pth :: IO ConditionPack
        let prog = Program (expression pack)
        let matched = matchProgram (pattern pack) prog
        unless (matched /= []) (expectationFailure "List of matched substitutions is empty which is not expected")
        met <- meetCondition (condition pack) matched (RuleContext prog buildTerm)
        case failure pack of
          Just True ->
            unless
              (null met)
              ( expectationFailure $
                  "List of substitutions after condition check must be empty, but got:\n"
                    ++ printSubsts matched
              )
          _ ->
            when
              (null met)
              ( expectationFailure $
                  "List of substitution after condition check must be not empty\nOriginal substitutions:\n"
                    ++ printSubsts matched
              )
    )
