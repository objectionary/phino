{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module EvaluateSpec (spec) where

import AST
import CLI.Helpers (started)
import Control.Exception (SomeException)
import Control.Monad
import Data.Aeson (FromJSON (parseJSON), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.List (find, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Yaml qualified as Decode
import Deps (Acyclic, Evaluation (EvRun), Judgment (Morphing), Term (TeExpression), certainty)
import Encoding (Encoding (UNICODE))
import Files (allPathsIn)
import Fixtures (defaultReduceContext, fixtureLambdas, recorded, recorded', withLambdas, withLambdasOf)
import Forks (explored)
import GHC.Generics (Generic)
import Lambdas (readLambdas)
import Lining (LineFormat (SINGLELINE))
import Margin (defaultMargin)
import Matcher (substEmpty)
import Morph (ReduceContext (..), Steps (..), emptyState, execBuildTerm, memoized, morph, tallied)
import Parser (Form (MONOFORM), parseExpressionThrows')
import Printer (printExpression, printExpression', printExpressionHidingRho')
import Sugar (SugarType (SWEET))
import System.FilePath (makeRelative)
import Tau (seedTaus)
import Test.Hspec
import Yaml (ExtraArgument (..))

data SymbolPack = SymbolPack
  { symbolic :: String
  , location :: Maybe String
  , input :: String
  , partial :: Maybe Bool
  , acyclic :: Maybe String
  , steps :: Maybe Int
  , firings :: Maybe Int
  , protocol :: String
  , result :: Maybe String
  , fails :: Maybe String
  , hideRho :: Maybe Bool
  , paths :: Maybe Int
  }
  deriving (Generic, Show)

instance FromJSON SymbolPack where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '-'}

testSymbols :: FilePath -> Expectation
testSymbols pth = do
  SymbolPack{..} <- Decode.decodeFileThrow pth
  expr <- parseExpressionThrows' MONOFORM input
  seedTaus expr
  loc <- parseExpressionThrows' MONOFORM (fromMaybe "Q" location)
  let hidden = hideRho /= Just False
  withLambdasOf (T.pack symbolic) $ \file -> do
    known <- readLambdas file
    (_, written) <- recorded' hidden $ \record rewind -> do
      let mode = named <$> acyclic
      cells <- memoized mode
      tally <- tallied firings
      base <- defaultReduceContext loc
      record (EvRun Morphing (T.pack (printExpression loc)))
      let walked =
            explored (Just (fromMaybe 1 paths)) rewind $ \forks -> do
              let ctx =
                    base
                      { _partial = partial == Just True
                      , _acyclic = mode
                      , _memo = cells
                      , _steps = Steps (fromMaybe 250 steps) 0
                      , _tally = tally
                      , _symbolic = known
                      , _saveEval = record
                      , _opened = Just (1, Morphing, loc)
                      , _forks = forks
                      }
              started expr ctx
              morph expr emptyState ctx
      case fails of
        Just message ->
          walked `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))
        Nothing -> do
          morphed <- (\(term, _, _) -> term) . last <$> walked
          forM_ result $ \res -> do
            expected <- parseExpressionThrows' MONOFORM res
            spelled hidden morphed `shouldBe` spelled False expected
    written `shouldBe` protocol
  where
    named :: String -> Acyclic
    named mode = fromMaybe (error ("The pack names an unknown mode of acyclic: " ++ mode)) (find ((== mode) . certainty) [minBound .. maxBound])
    spelled :: Bool -> Expression -> String
    spelled hidden term =
      (if hidden then printExpressionHidingRho' else printExpression') term (SWEET, UNICODE, SINGLELINE, defaultMargin)

spec :: Spec
spec = do
  known <- runIO fixtureLambdas

  describe "evaluate with the λ functions of '--symbolic'" $ do
    let resources = "test-resources/evaluate-packs"
    packs <- runIO (allPathsIn resources)
    forM_ packs (\pth -> it (makeRelative resources pth) (testSymbols pth))

  describe "execBuildTerm 'evaluate'" $ do
    let univ = ExFormation []
        runEvaluate args = do
          ctx <- withLambdas known <$> defaultReduceContext ExRoot
          execBuildTerm univ ctx "evaluate" args substEmpty
    forM_
      [
        ( "the first argument is not a formation"
        , [ArgExpression ExRoot, ArgExpression univ]
        , "Function evaluate() expects a formation"
        )
      ,
        ( "not given exactly two expression arguments"
        , [ArgExpression univ]
        , "requires exactly 2 expression arguments"
        )
      ]
      ( \(desc, args, message) ->
          it ("throws when " ++ desc) $
            runEvaluate args `shouldThrow` (\e -> message `isInfixOf` show (e :: SomeException))
      )

    it "gets stuck on a λ naming a symbol, instead of refusing the formation" $ do
      (_, written) <- recorded $ \record -> do
        ctx <- withLambdas known <$> defaultReduceContext ExRoot
        let stuck = ctx{_saveEval = record}
            fire = execBuildTerm univ stuck "evaluate" [ArgExpression (ExFormation [BiLambda (FnSymbol 1)]), ArgExpression univ] substEmpty
        fire `shouldThrow` (\e -> "No entry of --symbolic answers the λ function '𝜎1'" `isInfixOf` show (e :: SomeException))
      written `shouldBe` "  unanswered(𝜎1)  # 𝕄(𝜎1:λ)\n"

    it "throws when the formation carries more than one λ binding" $
      runEvaluate [ArgExpression (ExFormation [BiLambda (Function "L_one"), BiLambda (Function "L_two")]), ArgExpression univ]
        `shouldThrow` (\e -> "Duplicated attribute 'λ'" `isInfixOf` show (e :: SomeException))

    forM_
      [ ("carries no binding at all", ExFormation [])
      , ("carries bindings but none of them a λ", ExFormation [BiVoid AtRho])
      , ("decorates another object instead of firing a λ", ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "bool"))])
      ]
      ( \(desc, form) ->
          it ("answers unchanged a formation that " ++ desc) $ do
            answered <- runEvaluate [ArgExpression form, ArgExpression univ]
            case answered of
              TeExpression expr -> expr `shouldBe` form
              _ -> expectationFailure "expected TeExpression"
      )
    it "evaluates a λ-bearing formation to the answer of its entry, normalized" $ do
      let form = ExFormation [BiLambda (Function "L_answer"), BiTau AtRho (ExFormation [BiDelta (BtOne "00")])]
      answered <- withLambdasOf "- λ: L_answer\n  𝑛: ⟦ Δ ⤍ FF- ⟧\n" readLambdas
      ctx <- withLambdas answered <$> defaultReduceContext ExRoot
      result <- execBuildTerm univ ctx "evaluate" [ArgExpression form, ArgExpression univ] substEmpty
      case result of
        TeExpression expr -> expr `shouldBe` ExFormation [BiDelta (BtOne "FF")]
        _ -> expectationFailure "expected TeExpression"
