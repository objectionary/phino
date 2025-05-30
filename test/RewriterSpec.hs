{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import Ast
import Control.Monad (forM_)
import Data.Aeson
import Data.Text (unpack)
import Data.Yaml qualified as Yaml
import GHC.Generics
import Misc (allPathsIn)
import Parser (parseProgram)
import Rewriter (OptsRewrite (..), defaultOptsRewrite, rewrite)
import System.FilePath (replaceExtension, takeBaseName, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

instance FromJSON Program where
  parseJSON =
    withText
      "Program"
      ( \txt -> case parseProgram (unpack txt) of
          Left err -> fail err
          Right expr -> pure expr
      )

data YamlPack = YamlPack
  { input :: String,
    output :: String,
    desugar :: Maybe Bool
  }
  deriving (Generic, FromJSON, Show)

yamlPack :: FilePath -> IO YamlPack
yamlPack = Yaml.decodeFileThrow

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "temp"

spec :: Spec
spec = do
  describe "rewrite from packs" $ do
    packs <- runIO (allPathsIn "test/resources/rewriter-packs")
    forM_
      packs
      ( \pth -> do
          pack <- runIO $ yamlPack pth
          let output' = output pack
          rewritten <-
            runIO
              ( withTempDir $ \temp -> do
                  let input' = temp </> replaceExtension (takeFileName pth) ".phi"
                  writeFile input' (input pack)
                  let opts =
                        defaultOptsRewrite
                          { phi = input',
                            nothing = case desugar pack of
                              Just desugar' -> desugar'
                              _ -> False
                          }
                  rewrite opts
              )
          it (takeBaseName pth) (rewritten `shouldBe` output')
      )
