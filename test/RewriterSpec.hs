{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Char (isSpace)
import Data.Foldable (foldlM)
import Data.Yaml qualified as Yaml
import GHC.Generics
import Misc (allPathsIn, ensuredFile)
import Parser (parseProgramThrows)
import Printer (printProgram)
import Rewriter (rewrite)
import System.FilePath (makeRelative, replaceExtension, (</>))
import Test.Hspec (Spec, describe, expectationFailure, it, pending, runIO)
import Yaml (normalizationRules)
import Yaml qualified as Y

data Rules = Rules
  { basic :: Maybe [String],
    custom :: Maybe [Y.Rule]
  }
  deriving (Generic, FromJSON, Show)

data YamlPack = YamlPack
  { input :: String,
    output :: String,
    rules :: Maybe Rules,
    skip :: Maybe Bool,
    repeat_ :: Maybe Integer,
    normalize :: Maybe Bool
  }
  deriving (Generic, Show)

instance FromJSON YamlPack where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "repeat_" -> "repeat"
            other -> other
        }

yamlPack :: FilePath -> IO YamlPack
yamlPack = Yaml.decodeFileThrow

noSpaces :: String -> String
noSpaces = filter (not . isSpace)

spec :: Spec
spec = do
  describe "rewrite packs" $ do
    let resources = "test-resources/rewriter-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- yamlPack pth
          let normalize' = case normalize pack of
                Just _ -> True
                _ -> False
              repeat' =
                if normalize'
                  then 50
                  else case repeat_ pack of
                    Just num -> num
                    _ -> 1
          case skip pack of
            Just True -> pending
            _ -> do
              program <- parseProgramThrows (input pack)
              rules' <- case rules pack of
                Just _rules -> case custom _rules of
                  Just custom' -> pure custom'
                  _ -> case basic _rules of
                    Just basic' ->
                      mapM
                        ( \name -> do
                            yaml <- ensuredFile ("resources" </> replaceExtension name ".yaml")
                            Y.yamlRule yaml
                        )
                        basic'
                    _ -> pure []
                Nothing ->
                  if normalize'
                    then pure normalizationRules
                    else pure []
              rewritten <- foldlM (\prog _ -> rewrite prog rules') program [1 .. repeat']
              result' <- parseProgramThrows (output pack)
              unless (rewritten == result') $
                expectationFailure
                  ( "Wrong rewritten program. Expected:\n"
                      ++ printProgram result'
                      ++ "\nGot:\n"
                      ++ printProgram rewritten
                  )
      )
