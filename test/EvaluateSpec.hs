{-# LANGUAGE DeriveAnyClass #-}
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
import Data.Aeson (FromJSON)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Yaml qualified as Decode
import Deps (Evaluation (EvRun), Judgment (Morphing), Term (TeExpression))
import Files (allPathsIn)
import Fixtures (defaultReduceContext, fixtureLambdas, recorded, withLambdas, withLambdasOf)
import GHC.Generics (Generic)
import Lambdas (readLambdas)
import Matcher (substEmpty)
import Morph (ReduceContext (..), Steps (..), execBuildTerm, morph)
import Parser (parseExpressionThrows)
import Printer (printExpression)
import System.FilePath (makeRelative)
import Tau (seedTaus)
import Test.Hspec
import Yaml (ExtraArgument (..))

-- One case of a λ function answered by the '--symbolic' file, as a pack of
-- 'test-resources/evaluate-packs' spells it: the file itself under
-- 'symbolic', the program it is fired against under 'input', the whole protocol
-- of '--protocol' under 'protocol' and, where the answer is small enough to be
-- worth spelling, the program 𝕄 lands on under 'result' — or the failure under
-- 'fails'. The protocol is one block of text rather than a list of lines, so a
-- pack holds the file a user of the option reads back and the case compares the
-- two of them verbatim.
data SymbolPack = SymbolPack
  { symbolic :: String
  , location :: Maybe String
  , input :: String
  , deep :: Maybe Bool
  , partial :: Maybe Bool
  , acyclic :: Maybe Bool
  , steps :: Maybe Int
  , protocol :: String
  , result :: Maybe String
  , fails :: Maybe String
  }
  deriving (Generic, Show, FromJSON)

-- Fire one symbol pack and check both what it answers and what its firings
-- wrote to the protocol, since a λ function is as much what it reports as what
-- it hands back. The run opens the protocol with itself, the way the command
-- opens it, so a pack reads as the file a user of '--protocol' reads back.
testSymbols :: FilePath -> Expectation
testSymbols pth = do
  SymbolPack{..} <- Decode.decodeFileThrow pth
  expr <- parseExpressionThrows input
  seedTaus expr
  loc <- parseExpressionThrows (fromMaybe "Q" location)
  withLambdasOf (T.pack symbolic) $ \file -> do
    known <- readLambdas file
    (_, written) <- recorded $ \record -> do
      let ctx =
            (defaultReduceContext loc)
              { _deep = deep == Just True
              , _partial = partial == Just True
              , _acyclic = acyclic == Just True
              , _steps = Steps (fromMaybe 250 steps) 0
              , _symbolic = known
              , _saveEval = record
              }
      record (EvRun Morphing (T.pack (printExpression loc)))
      case fails of
        Just message ->
          morph expr (started expr) ctx `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))
        Nothing -> do
          (morphed, _, _) <- morph expr (started expr) ctx
          forM_ result $ \res -> do
            expected <- parseExpressionThrows res
            morphed `shouldBe` expected
    written `shouldBe` protocol

spec :: Spec
spec = do
  -- Every λ function a case may fire comes from the fixture file, read once
  -- here: phino carries none of its own (see 'Fixtures').
  known <- runIO fixtureLambdas

  -- The whole of what a λ function answered by the '--symbolic' file does, pack
  -- by pack: the file itself, the program it is fired against, every line the
  -- protocol of '--protocol' writes and the program 𝕄 lands on.
  describe "evaluate with the λ functions of '--symbolic'" $ do
    let resources = "test-resources/evaluate-packs"
    packs <- runIO (allPathsIn resources)
    forM_ packs (\pth -> it (makeRelative resources pth) (testSymbols pth))

  -- 'execBuildTerm's "evaluate" case exposes 𝔼 to the matcher's condition path
  -- (guards in 'when'/'having'). No built-in rule's guard actually calls the
  -- function, so these error paths — reachable only by malformed arguments —
  -- are exercised here directly through the exported 'execBuildTerm', the same
  -- way the matcher would call it.
  describe "execBuildTerm 'evaluate'" $ do
    let univ = ExFormation []
        ctx = withLambdas known (defaultReduceContext ExRoot)
        runEvaluate args = execBuildTerm univ ctx "evaluate" args substEmpty
    forM_
      [
        ( "the first argument is not a formation"
        , [ArgExpression ExRoot, ArgExpression univ]
        , "Function evaluate() expects a formation"
        )
      ,
        ( "the only λ binding carries a symbol rather than a name"
        , [ArgExpression (ExFormation [BiLambda (FnSymbol 1)]), ArgExpression univ]
        , "a single λ binding naming a function"
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

    -- Two λ bindings never reach 𝔼: the builder refuses to make a formation out
    -- of them first. The case is here anyway, since what matters is that such a
    -- formation fails rather than answering ⊥ the way a λ-less one does.
    it "throws when the formation carries more than one λ binding" $
      runEvaluate [ArgExpression (ExFormation [BiLambda (Function "L_one"), BiLambda (Function "L_two")]), ArgExpression univ]
        `shouldThrow` (\e -> "Duplicated attribute 'λ'" `isInfixOf` show (e :: SomeException))

    -- A formation with no λ binding has nothing to fire, which is a question
    -- the calculus answers rather than a malformed one: ⊥ is what 𝕄 hands back
    -- for a term nobody can reduce further, and 𝔼 says the same. Only a λ 𝔼
    -- cannot make sense of — several of them, or one standing for anything but
    -- a plain name — is malformed and throws (see above).
    forM_
      [ ("carries no binding at all", ExFormation [])
      , ("carries bindings but none of them a λ", ExFormation [BiVoid AtRho])
      ]
      ( \(desc, form) ->
          it ("answers ⊥ for a formation that " ++ desc) $ do
            answered <- runEvaluate [ArgExpression form, ArgExpression univ]
            case answered of
              TeExpression expr -> expr `shouldBe` ExTermination
              _ -> expectationFailure "expected TeExpression"
      )
    it "evaluates a λ-bearing formation to the answer of its entry, normalized" $ do
      let form = ExFormation [BiLambda (Function "L_answer"), BiTau AtRho (ExFormation [BiDelta (BtOne "00")])]
      answered <- withLambdasOf "- λ: L_answer\n  𝑛: ⟦ Δ ⤍ FF- ⟧\n" readLambdas
      result <- execBuildTerm univ (withLambdas answered ctx) "evaluate" [ArgExpression form, ArgExpression univ] substEmpty
      case result of
        TeExpression expr -> expr `shouldBe` ExFormation [BiDelta (BtOne "FF"), BiVoid AtRho]
        _ -> expectationFailure "expected TeExpression"
