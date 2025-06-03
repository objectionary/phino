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
import Misc (allPathsIn, ensuredFile)
import Rewriter (rewrite)
import System.FilePath (makeRelative, replaceExtension, (</>))
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Yaml qualified as Y

data Rules = Rules
  { basic :: Maybe [String],
    custom :: Maybe [Y.Rule]
  }
  deriving (Generic, FromJSON, Show)

data YamlPack = YamlPack
  { input :: String,
    output :: String,
    rules :: Maybe Rules
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
          rules' <- case rules pack of
            Just _rules -> case custom _rules of
              Just custom' -> pure custom'
              _ -> case basic _rules of
                Just basic' ->
                  runIO $
                    mapM
                      ( \name -> do
                          yaml <- ensuredFile ("resources" </> replaceExtension name ".yaml")
                          Y.yamlRule yaml
                      )
                      basic'
                _ -> pure []
            Nothing -> pure []
          rewritten <- runIO $ rewrite input' rules'
          it (makeRelative resources pth) (rewritten `shouldBe` output')
      )
