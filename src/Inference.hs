{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Inference (Conclusion (..), Inference, Premises (..), Way (..), dataizationOf, dataizationSpine, direct, morphingOf, morphingSpine) where

import AST
import Builder (buildBytesThrows, buildExpressionThrows)
import Control.Exception (throwIO)
import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Deps (Judgment (..))
import Matcher (MetaValue (..), Subst, combine, matchExpression', substSingle)
import Rule (RuleContext, matchExpressionWithRule')
import Text.Printf (printf)
import qualified Yaml as Y

type Inference value = RuleContext -> Expression -> Expression -> IO (Maybe (Premises value))

data Premises value
  = Morphs Expression Expression (Expression -> IO (Premises value))
  | Evaluates Expression Expression (Expression -> IO (Premises value))
  | Contextualizes Expression Expression (Expression -> IO (Premises value))
  | Concludes (Conclusion value)

data Conclusion value
  = Answered (Judgment, String) value
  | Onward Way Expression Expression
  deriving (Eq, Show)

data Way
  = Taken (Judgment, String)
  | Normalized (Judgment, String)
  | Named (Judgment, String)
  | Staged Expression
  deriving (Eq, Show)

morphingSpine :: Y.MorphRule -> Either String ([Y.Premise], Conclusion Expression)
morphingSpine rule = case producer rule.nresult rule.premises of
  Nothing -> Right (rule.premises, Answered step rule.nresult)
  Just concl@(Y.Premise _ (Y.OpMorph arg universe)) -> case producer arg rule.premises of
    Just normal@(Y.Premise _ (Y.OpNormalize inner)) ->
      Right (rule.premises `excluding` [concl, normal], Onward ((if inner == rule.ematch then Named else Normalized) step) inner universe)
    _ -> Right (rule.premises `excluding` [concl], Onward (Taken step) arg universe)
  Just _ -> Left "it concludes with no 'morph' premise"
  where
    step :: (Judgment, String)
    step = (Morphing, rule.name)

dataizationSpine :: Y.DataizeRule -> Either String ([Y.Premise], Conclusion Bytes)
dataizationSpine rule = case bytesProducer rule.dresult of
  Nothing -> Right (rule.premises, Answered (Dataization, rule.name) rule.dresult)
  Just concl@(Y.Premise _ (Y.OpDataize arg universe)) -> case producer arg rule.premises of
    Just normal@(Y.Premise _ (Y.OpNormalize inner)) ->
      let side = rule.premises `excluding` [concl, normal]
       in Right (side, Onward (Normalized (labelled (Dataization, "") side)) inner universe)
    Just morphed@(Y.Premise _ (Y.OpMorph inner scene)) ->
      Right (rule.premises `excluding` [concl, morphed], Onward (Staged scene) inner universe)
    _ ->
      let side = rule.premises `excluding` [concl]
       in Right (side, Onward (Taken (labelled (label concl.operation) side)) arg universe)
  Just _ -> Left "it concludes with no 'dataize' premise"
  where
    bytesProducer :: Bytes -> Maybe Y.Premise
    bytesProducer (BtMeta name) = find (\premise -> premise.result == name) rule.premises
    bytesProducer _ = Nothing
    labelled :: (Judgment, String) -> [Y.Premise] -> (Judgment, String)
    labelled _ (premise : _) = label premise.operation
    labelled fallback [] = fallback

producer :: Expression -> [Y.Premise] -> Maybe Y.Premise
producer (ExMeta name) = find (\premise -> premise.result == name)
producer _ = const Nothing

excluding :: [Y.Premise] -> [Y.Premise] -> [Y.Premise]
excluding premises removed = filter (\premise -> premise.result `notElem` map (.result) removed) premises

label :: Y.Operation -> (Judgment, String)
label (Y.OpMorph _ _) = (Morphing, "morph")
label (Y.OpNormalize _) = (Normalization, "normalize")
label (Y.OpEvaluate _ _) = (Evaluation, "evaluate")
label (Y.OpContextualize _ _) = (Contextualization, "contextualize")
label (Y.OpDataize _ _) = (Dataization, "dataize")

morphingOf :: Y.MorphRule -> Inference Expression
morphingOf rule = interpreted buildExpressionThrows (Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing) rule.ematch (morphingSpine rule)

dataizationOf :: Y.DataizeRule -> Inference Bytes
dataizationOf rule = interpreted buildBytesThrows (Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing) rule.ematch (dataizationSpine rule)

interpreted :: forall value. (value -> Subst -> IO value) -> Y.Rule -> Expression -> Either String ([Y.Premise], Conclusion value) -> Inference value
interpreted build rule ematch spine ctx term univ = do
  matched <- matchExpressionWithRule' (matchExpression' ematch univ) term rule ctx
  case (matched, spine) of
    ([], _) -> pure Nothing
    (_, Left reason) -> refuse reason
    (subst : _, Right (sides, conclusion)) -> Just <$> premised sides conclusion subst
  where
    premised :: [Y.Premise] -> Conclusion value -> Subst -> IO (Premises value)
    premised [] conclusion subst = Concludes <$> concluded conclusion subst
    premised (premise : rest) conclusion subst = case premise.operation of
      Y.OpMorph expr universe -> do
        world <- buildExpressionThrows universe subst
        morphed <- buildExpressionThrows expr subst
        pure (Morphs morphed world next)
      Y.OpEvaluate expr universe -> Evaluates <$> buildExpressionThrows expr subst <*> buildExpressionThrows universe subst <*> pure next
      Y.OpContextualize expr context -> Contextualizes <$> buildExpressionThrows expr subst <*> buildExpressionThrows context subst <*> pure next
      _ -> refuse (printf "its premise '%s' runs beside the spine, which only a 'morph', an 'evaluate' or a 'contextualize' can" (T.unpack premise.result))
      where
        next :: Expression -> IO (Premises value)
        next answer = case combine (substSingle premise.result (MvExpression answer)) subst of
          Just subst' -> premised rest conclusion subst'
          Nothing -> throwIO (userError (printf "premise meta '%s' clashes with an existing binding" (T.unpack premise.result)))
    concluded :: Conclusion value -> Subst -> IO (Conclusion value)
    concluded (Answered step value) subst = Answered step <$> build value subst
    concluded (Onward (Staged stage) expr world) subst = Onward . Staged <$> buildExpressionThrows stage subst <*> buildExpressionThrows expr subst <*> buildExpressionThrows world subst
    concluded (Onward way expr world) subst = Onward way <$> buildExpressionThrows expr subst <*> buildExpressionThrows world subst
    refuse :: String -> IO a
    refuse reason = throwIO (userError (printf "The rule '%s' cannot be run, since %s" rule.name reason))

direct :: (Expression -> Expression -> [Premises value]) -> Inference value
direct rule _ term univ = pure (listToMaybe (rule term univ))
