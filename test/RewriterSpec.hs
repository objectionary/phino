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
import Test.Hspec (Spec, describe, expectationFailure, it, pending, runIO, shouldSatisfy, shouldThrow)
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
  describe "--max-cycles and --max-depth limits" $
    forM_
      [
        ( "throws with --depth-sensitive once --max-cycles is reached"
        , []
        , (5, 0, True)
        , Left "--max-cycles=0"
        )
      ,
        ( "stops silently without --depth-sensitive once --max-cycles is reached"
        , []
        , (5, 0, False)
        , Right snd
        )
      ,
        ( "throws with --depth-sensitive once --max-depth is reached for a rule"
        , normalizationRules
        , (0, 5, True)
        , Left "--max-depth=0"
        )
      ,
        ( "does not throw without --depth-sensitive once --max-depth is reached for a rule"
        , normalizationRules
        , (0, 5, False)
        , Right (\(rewrittens, _) -> fst (NE.last rewrittens) == ExRoot)
        )
      ]
      ( \(desc, rewriteRules, (maxDepth, maxCycles, depthSensitive), expected) -> it desc $ do
          let action = rewrite ExRoot rewriteRules (RewriteContext ExRoot maxDepth maxCycles depthSensitive buildTerm MtDisabled Nothing dontSaveStep)
          case expected of
            Left fragment -> action `shouldThrow` (\exc -> fragment `isInfixOf` show (exc :: SomeException))
            Right predicate -> do
              result <- action
              result `shouldSatisfy` predicate
      )

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
