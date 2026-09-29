{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import AST (Attribute (AtLabel), Binding (BiTau), Expression (ExDispatch, ExFormation, ExRoot, ExTermination))
import Control.Exception (SomeException)
import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Char (isSpace)
import Data.List (isInfixOf, nub)
import Data.List.NonEmpty qualified as NE
import Data.Yaml qualified as Yaml
import Deps (Judgment (..), dontSaveStep)
import Files (allPathsIn, ensuredFile)
import Functions (buildTerm)
import GHC.Generics
import Must (Must (..))
import Parser (parseExpressionThrows)
import Printer (printExpression)
import Rewriter (RewriteContext (RewriteContext), rewrite)
import System.FilePath (makeRelative, replaceExtension, (</>))
import Tau (seedTaus)
import Test.Hspec (Spec, describe, expectationFailure, it, pending, runIO, shouldBe, shouldSatisfy, shouldThrow)
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
        , "⟦ t ↦ ⊥.a ⟧"
        , (5, 0, True)
        , Left "--max-cycles=0"
        )
      ,
        ( "stops silently without --depth-sensitive once --max-cycles is reached"
        , "⟦ t ↦ ⊥.a ⟧"
        , (5, 0, False)
        , Right snd
        )
      ,
        ( "throws with --depth-sensitive once --max-depth is reached for a rule"
        , "⟦ t ↦ ⊥.a ⟧"
        , (0, 5, True)
        , Left "--max-depth=0"
        )
      ,
        ( "does not throw without --depth-sensitive once --max-depth is reached for a rule"
        , "⟦ t ↦ ⊥.a ⟧"
        , (0, 5, False)
        , Right (\(rewrittens, _) -> fst (NE.last rewrittens) == ExFormation [BiTau (AtLabel "t") (ExDispatch ExTermination (AtLabel "a"))])
        )
      ,
        ( "throws with --depth-sensitive when a rule still applies after --max-depth steps"
        , "⟦ t ↦ ⊥.a.b ⟧"
        , (1, 5, True)
        , Left "--max-depth=1"
        )
      ,
        ( "does not throw with --depth-sensitive when a rule finishes in exactly --max-depth steps"
        , "⟦ t ↦ ⊥.a ⟧"
        , (1, 5, True)
        , Right (\(rewrittens, _) -> fst (NE.last rewrittens) == ExFormation [BiTau (AtLabel "t") ExTermination])
        )
      ,
        ( "does not throw with --depth-sensitive when rewriting finishes in exactly --max-cycles cycles"
        , "⟦ t ↦ ⊥.a ⟧"
        , (5, 1, True)
        , Right (\(rewrittens, _) -> fst (NE.last rewrittens) == ExFormation [BiTau (AtLabel "t") ExTermination])
        )
      ]
      ( \(desc, input', (maxDepth, maxCycles, depthSensitive), expected) -> it desc $ do
          expr <- parseExpressionThrows input'
          let action = rewrite expr normalizationRules (RewriteContext ExRoot maxDepth maxCycles depthSensitive Nothing buildTerm MtDisabled Nothing dontSaveStep)
          case expected of
            Left fragment -> action `shouldThrow` (\exc -> fragment `isInfixOf` show (exc :: SomeException))
            Right predicate -> do
              result <- action
              result `shouldSatisfy` predicate
      )

  describe "--must once --max-cycles stops the run" $
    forM_
      [
        ( "throws when --must demands more cycles than --max-cycles allowed"
        , MtExact 3
        , Left "--must=3"
        )
      ,
        ( "throws when the lower bound of --must lies above --max-cycles"
        , MtRange (Just 2) Nothing
        , Left "--must=2.."
        )
      ,
        ( "does not throw when --max-cycles stops the run inside the range of --must"
        , MtRange (Just 1) (Just 4)
        , Right snd
        )
      ]
      ( \(desc, must', expected) -> it desc $ do
          expr <- parseExpressionThrows "⟦ t ↦ ⊥.a.b.c ⟧"
          let action = rewrite expr normalizationRules (RewriteContext ExRoot 1 1 False Nothing buildTerm must' Nothing dontSaveStep)
          case expected of
            Left fragment -> action `shouldThrow` (\exc -> fragment `isInfixOf` show (exc :: SomeException))
            Right predicate -> do
              result <- action
              result `shouldSatisfy` predicate
      )

  describe "judges the steps it takes" $
    it "takes every step by normalization" $ do
      expr <- parseExpressionThrows "⟦ k ↦ ⟦ w ↦ ⟦ Δ ⤍ 1F- ⟧ ⟧.w ⟧"
      (rewrittens, _) <- rewrite expr normalizationRules (RewriteContext ExRoot 25 25 False Nothing buildTerm MtDisabled Nothing dontSaveStep)
      nub [judgment | (_, Just (judgment, _)) <- NE.toList rewrittens] `shouldBe` [Normalization]

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
                      Nothing
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
