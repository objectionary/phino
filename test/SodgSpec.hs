{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module SodgSpec where

import AST
import Control.Monad (forM_)
import Data.Aeson
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Yaml qualified as Yaml
import Files (allPathsIn)
import GHC.Generics (Generic)
import Grown (Grown (..))
import Parser (parseExpression, parseExpressionThrows)
import Printer (printExpression)
import Sodg
import System.FilePath
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, conjoin, counterexample, (===))
import Text.Printf (printf)

data YamlPack = YamlPack
  { input :: String
  , output :: Maybe String
  , fails :: Maybe String
  }
  deriving (Generic, Show, FromJSON)

spec :: Spec
spec = do
  describe "sodg packs" $ do
    let resources = "test-resources/sodg-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          YamlPack{..} <- Yaml.decodeFileThrow pth
          expr <- parseExpressionThrows input
          case (output, fails, commands expr) of
            (Just expected, Nothing, Right graph) -> printCommands graph `shouldBe` expected
            (Just _, Nothing, Left err) -> expectationFailure (printf "The term failed with '%s', while the pack expects commands" (show err))
            (Nothing, Just message, Left err) -> show err `shouldContain` message
            (Nothing, Just _, Right graph) -> expectationFailure (printf "The term gave commands, while the pack expects it to fail:\n%s" (printCommands graph))
            _ -> expectationFailure "The pack holds neither a single 'output' nor a single 'fails'"
      )

  describe "sodg keeps its four promises on any program" $ do
    prop "every F, D, A and T makes a fresh name and a command uses only names made above it" $ \(Grown grown) ->
      let expr = scrubbed False grown
       in either (counterexample "the walk failed" . const False) (ordered expr) (commands expr)
    prop "every attached attribute of every formation gets exactly one P" $ \(Grown grown) ->
      let expr = scrubbed False grown
       in either (counterexample "the walk failed" . const False) (attached expr) (commands expr)
    prop "the same input gives the same output, byte for byte" $ \(Grown grown) ->
      let expr = scrubbed False grown
       in case parseExpression (printExpression expr) of
            Left err -> counterexample err False
            Right parsed -> either show printCommands (commands parsed) === either show printCommands (commands expr)

ordered :: Expression -> [Command] -> Property
ordered expr graph = counterexample (printExpression expr ++ "\n" ++ printCommands graph) (go [] graph)
  where
    go :: [Int] -> [Command] -> Bool
    go _ [] = True
    go made (cmd : rest) =
      all (`elem` made) (used cmd)
        && all (`notElem` made) (defined cmd)
        && go (defined cmd ++ made) rest
    defined :: Command -> [Int]
    defined = \case
      Formation self _ -> [self]
      Dispatch self _ _ -> [self]
      Application self _ _ _ -> [self]
      Terminator self -> [self]
      _ -> []
    used :: Command -> [Int]
    used = \case
      Formation _ _ -> []
      Dispatch _ base _ -> [base]
      Application _ proto _ arg -> [proto, arg]
      Put self _ kid -> [self, kid]
      Delta self _ -> [self]
      Lambda self _ -> [self]
      Terminator _ -> []

attached :: Expression -> [Command] -> Property
attached expr graph =
  counterexample
    (printExpression expr ++ "\n" ++ printCommands graph)
    ( conjoin
        [ length puts === taus expr
        , nub puts === puts
        , counterexample "a P puts an attribute its F does not list" (all listed puts)
        , length [() | Formation _ _ <- graph] === formations expr + (case expr of ExFormation _ -> 0; _ -> 1)
        ]
    )
  where
    puts :: [(Int, Text)]
    puts = [(self, attr) | Put self attr _ <- graph]
    listed :: (Int, Text) -> Bool
    listed (self, attr) = or [attr `elem` names | Formation self' names <- graph, self' == self]
    taus :: Expression -> Int
    taus = \case
      ExFormation bds -> sum [1 + taus inner | BiTau _ inner <- bds]
      ExDispatch inner _ -> taus inner
      ExApplication inner arg -> taus inner + taus (held arg)
      _ -> 0
    formations :: Expression -> Int
    formations = \case
      ExFormation bds -> 1 + sum [formations inner | BiTau _ inner <- bds]
      ExDispatch inner _ -> formations inner
      ExApplication inner arg -> formations inner + formations (held arg)
      _ -> 0
    held :: Argument -> Expression
    held (ArTau _ inner) = inner
    held (ArAlpha _ inner) = inner

scrubbed :: Bool -> Expression -> Expression
scrubbed inside = \case
  ExFormation bds -> ExFormation (mapMaybe binding bds)
  ExDispatch inner attr -> ExDispatch (scrubbed inside inner) (label attr)
  ExApplication inner (ArTau attr arg) -> ExApplication (scrubbed inside inner) (ArTau (label attr) (scrubbed inside arg))
  ExApplication inner (ArAlpha alpha arg) -> ExApplication (scrubbed inside inner) (ArAlpha (indexed alpha) (scrubbed inside arg))
  ExXi | inside -> ExXi
  ExTermination -> ExTermination
  _ -> ExRoot
  where
    binding :: Binding -> Maybe Binding
    binding = \case
      BiTau (AtMeta _) _ -> Nothing
      BiVoid (AtMeta _) -> Nothing
      BiTau attr inner -> Just (BiTau attr (scrubbed True inner))
      BiDelta (BtMeta _) -> Just (BiDelta BtEmpty)
      BiLambda (FnMeta _) -> Just (BiLambda (Function "L_scrubbed"))
      BiMeta _ -> Nothing
      other -> Just other
    indexed :: Alpha -> Alpha
    indexed (AlMeta _) = Alpha 0
    indexed alpha = alpha
    label :: Attribute -> Attribute
    label (AtMeta _) = AtLabel "scrubbed"
    label attr = attr
