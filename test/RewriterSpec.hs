{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import AST (Expression (ExRoot))
import Control.Exception (SomeException)
import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.List.NonEmpty qualified as NE
import Data.Yaml qualified as Yaml
import Deps (dontSaveStep)
import Files (allPathsIn, ensuredFile)
import Functions (buildTerm)
import GHC.Generics
import Must (Must (..))
import Parser (parseExpressionThrows)
import Printer (printExpression)
import Rewriter (RewriteContext (RewriteContext), rewrite)
import System.FilePath (makeRelative, replaceExtension, (</>))
import Tau (seedTaus)
import Test.Hspec (Spec, describe, expectationFailure, it, pending, runIO, shouldBe, shouldThrow)
import Yaml (normalizationRules)
import Yaml qualified as Y

data Rules = Rules
  { basic :: Maybe [String]
  , custom :: Maybe [Y.Rule]
  }
  deriving (Generic, FromJSON, Show)

data YamlPack = YamlPack
  { input :: String
  , output :: String
  , rules :: Maybe Rules
  , skip :: Maybe Bool
  , repeat_ :: Maybe Int
  , must :: Maybe Int
  , normalize :: Maybe Bool
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
  describe "--max-cycles and --max-depth limits" $ do
    it "throws with --depth-sensitive once --max-cycles is reached" $
      rewrite ExRoot [] (RewriteContext ExRoot 5 0 True buildTerm MtDisabled Nothing dontSaveStep)
        `shouldThrow` (\exc -> "--max-cycles=0" `isInfixOf` show (exc :: SomeException))
    it "stops silently without --depth-sensitive once --max-cycles is reached" $ do
      (_, exceeded) <- rewrite ExRoot [] (RewriteContext ExRoot 5 0 False buildTerm MtDisabled Nothing dontSaveStep)
      exceeded `shouldBe` True
    it "throws with --depth-sensitive once --max-depth is reached for a rule" $
      rewrite ExRoot normalizationRules (RewriteContext ExRoot 0 5 True buildTerm MtDisabled Nothing dontSaveStep)
        `shouldThrow` (\exc -> "--max-depth=0" `isInfixOf` show (exc :: SomeException))
    it "does not throw without --depth-sensitive once --max-depth is reached for a rule" $ do
      (rewrittens, _) <- rewrite ExRoot normalizationRules (RewriteContext ExRoot 0 5 False buildTerm MtDisabled Nothing dontSaveStep)
      fst (NE.last rewrittens) `shouldBe` ExRoot

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
              must' = case must pack of
                Just num -> MtExact num
                _ -> MtDisabled
          case skip pack of
            Just True -> pending
            _ -> do
              expr <- parseExpressionThrows (input pack)
              seedTaus expr
              rules' <- case rules pack of
                Just _rules -> case custom _rules of
                  Just custom' -> pure custom'
                  _ -> case basic _rules of
                    Just basic' ->
                      mapM
                        ( \name -> do
                            yaml <- ensuredFile ("resources/normalize" </> replaceExtension name ".yaml")
                            Y.yamlRule yaml
                        )
                        basic'
                    _ -> pure []
                Nothing ->
                  if normalize'
                    then pure normalizationRules
                    else pure []
              (rewrittens, _) <-
                rewrite
                  expr
                  rules'
                  ( RewriteContext
                      ExRoot
                      repeat'
                      repeat'
                      False
                      buildTerm
                      must'
                      Nothing
                      dontSaveStep
                  )
              let (rewritten, _) = NE.last rewrittens
              result' <- parseExpressionThrows (output pack)
              unless (rewritten == result') $
                expectationFailure
                  ( "Wrong rewritten expression. Expected:\n"
                      ++ printExpression result'
                      ++ "\nGot:\n"
                      ++ printExpression rewritten
                  )
      )
