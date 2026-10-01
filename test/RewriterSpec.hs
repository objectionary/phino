{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import AST (Argument (ArTau), Attribute (AtLabel), Binding (BiMeta, BiTau, BiVoid), Expression (ExApplication, ExDispatch, ExFormation, ExRoot, ExTermination, ExXi))
import Control.Exception (SomeException)
import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Char (isSpace)
import Data.List (isInfixOf, nub)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Yaml qualified as Yaml
import Deps (Judgment (..), dontSaveStep)
import Engine (Engine (_matching, _normal), building, stepOf)
import Files (allPathsIn, ensuredFile)
import Fixtures (linked)
import Functions (buildTerm)
import GHC.Generics
import Must (Must (..))
import Parser (parseExpressionThrows)
import Printer (printExpression)
import Rewriter (RewriteContext (RewriteContext), direct, every, fast, rewrite)
import Rule (RuleContext (RuleContext), Step (Step, _applied))
import System.FilePath (makeRelative, replaceExtension, (</>))
import Tau (seedTaus)
import Test.Hspec (Spec, describe, expectationFailure, it, pending, runIO, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
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
          let action = rewrite expr (map (stepOf linked) normalizationRules) (RewriteContext ExRoot maxDepth maxCycles depthSensitive Nothing (building linked) (_normal linked) (_matching linked) MtDisabled Nothing dontSaveStep)
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
          let action = rewrite expr (map (stepOf linked) normalizationRules) (RewriteContext ExRoot 1 1 False Nothing (building linked) (_normal linked) (_matching linked) must' Nothing dontSaveStep)
          case expected of
            Left fragment -> action `shouldThrow` (\exc -> fragment `isInfixOf` show (exc :: SomeException))
            Right predicate -> do
              result <- action
              result `shouldSatisfy` predicate
      )

  describe "judges the steps it takes" $
    it "takes every step by normalization" $ do
      expr <- parseExpressionThrows "⟦ k ↦ ⟦ w ↦ ⟦ Δ ⤍ 1F- ⟧ ⟧.w ⟧"
      (rewrittens, _) <- rewrite expr (map (stepOf linked) normalizationRules) (RewriteContext ExRoot 25 25 False Nothing (building linked) (_normal linked) (_matching linked) MtDisabled Nothing dontSaveStep)
      nub [judgment | (_, Just (judgment, _)) <- NE.toList rewrittens] `shouldBe` [Normalization]

  describe "rewrites by a locator" $ do
    it "rewrites the located part step after step" $ do
      expr <- parseExpressionThrows "⟦ t ↦ ⊥.a.b, u ↦ ⊥.c ⟧"
      (rewrittens, _) <- rewrite expr (map (stepOf linked) normalizationRules) (RewriteContext (ExDispatch ExRoot (AtLabel "t")) 25 25 False Nothing (building linked) (_normal linked) (_matching linked) MtDisabled Nothing dontSaveStep)
      fst (NE.last rewrittens) `shouldBe` ExFormation [BiTau (AtLabel "t") ExTermination, BiTau (AtLabel "u") (ExDispatch ExTermination (AtLabel "c"))]
    it "fails on a locator that points nowhere even when no rule runs" $ do
      expr <- parseExpressionThrows "⟦ t ↦ ⊥.a ⟧"
      rewrite expr [] (RewriteContext (ExDispatch ExRoot (AtLabel "w")) 25 25 False Nothing (building linked) (_normal linked) (_matching linked) MtDisabled Nothing dontSaveStep)
        `shouldThrow` (\exc -> "Can't find object by locator" `isInfixOf` show (exc :: SomeException))

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
              let steps = map (stepOf linked) rules'
              (rewrittens, _) <-
                rewrite
                  expr
                  steps
                  ( RewriteContext
                      ExRoot
                      repeat'
                      repeat'
                      False
                      Nothing
                      (building linked)
                      (_normal linked)
                      (every steps)
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
  describe "asks which steps match" $ do
    it "does not try a step the matching does not name" $
      (fst . NE.last . fst <$> rewrite ExXi [direct "qv" False (\_ expr -> [ExRoot | ExXi <- [expr]])] (RewriteContext ExRoot 25 25 False Nothing (building linked) (_normal linked) (\_ _ -> Set.empty) MtDisabled Nothing dontSaveStep))
        `shouldReturn` ExXi
    it "asks the matching again once a step changed the term" $
      (fst . NE.last . fst <$> rewrite ExXi [direct "xr" False (\_ expr -> [ExRoot | ExXi <- [expr]]), direct "rt" False (\_ expr -> [ExTermination | ExRoot <- [expr]])] (RewriteContext ExRoot 25 1 False Nothing (building linked) (_normal linked) (\_ expr -> Set.fromList [idx | (idx, ptn) <- [(0, ExXi), (1, ExRoot)], ptn == expr]) MtDisabled Nothing dontSaveStep))
        `shouldReturn` ExTermination
  describe "every" $
    it "names each of the steps it is handed" $
      every [Step "wd" (\_ _ -> pure Nothing), Step "ok" (\_ _ -> pure Nothing), Step "wd" (\_ _ -> pure Nothing)] Nothing (ExDispatch ExXi (AtLabel "pz"))
        `shouldBe` Set.fromList [0, 1, 2]
  describe "direct" $ do
    it "rewrites every place the function matches at" $
      _applied (direct "tx" False (\_ expr -> [ExRoot | ExXi <- [expr]])) (RuleContext buildTerm Nothing (const True)) (ExDispatch (ExApplication ExXi (ArTau (AtLabel "o") ExXi)) (AtLabel "m"))
        `shouldReturn` Just (ExDispatch (ExApplication ExRoot (ArTau (AtLabel "o") ExRoot)) (AtLabel "m"))
    it "tells it matched nowhere" $
      _applied (direct "tx" False (\_ expr -> [ExRoot | ExXi <- [expr]])) (RuleContext buildTerm Nothing (const True)) (ExDispatch ExTermination (AtLabel "m"))
        `shouldReturn` Nothing
    it "hands the world to the function" $
      _applied (direct "tw" False (\universe expr -> [world | ExXi <- [expr], Just world <- [universe]])) (RuleContext buildTerm (Just ExTermination) (const True)) ExXi
        `shouldReturn` Just ExTermination
  describe "fast" $ do
    it "holds for a formation rewritten between the same two meta bindings" $
      fast (ExFormation [BiMeta "B1", BiVoid (AtLabel "j"), BiMeta "B2"]) (ExFormation [BiMeta "B1", BiVoid (AtLabel "k"), BiMeta "B2"])
        `shouldBe` True
    it "fails for a formation rewritten into a dispatch" $
      fast (ExFormation [BiMeta "B1", BiVoid (AtLabel "j"), BiMeta "B2"]) (ExDispatch ExXi (AtLabel "k"))
        `shouldBe` False
