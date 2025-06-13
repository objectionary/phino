{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Char (isSpace)
import Data.Yaml qualified as Yaml
import GHC.Generics
import Misc (allPathsIn, ensuredFile)
import Parser (parseProgramThrows)
import Printer (printProgram)
import System.FilePath (makeRelative, replaceExtension, (</>))
import Test.Hspec (Spec, describe, expectationFailure, it, pending, runIO)
import Yaml qualified as Y
import Rewriter (rewrite)
import Data.Foldable (foldlM)

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
    repeat_ :: Maybe Integer
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
                Nothing -> pure []
              rewritten <- case repeat_ pack of
                Just num -> foldlM (\prog _ -> rewrite prog rules') program [1..num]
                _ -> rewrite program rules'
              result' <- parseProgramThrows (output pack)
              unless (rewritten == result') $
                expectationFailure
                  ( "Wrong rewritten program. Expected:\n"
                      ++ printProgram result'
                      ++ "\nGot:\n"
                      ++ printProgram rewritten
                  )
      )
