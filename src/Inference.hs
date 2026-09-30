{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The rules of 𝕄 and 𝔻 the way a run takes them, whichever engine runs them.
-- A rule matched against a term and its universe comes to the premises it
-- runs beside its spine, in the order it lists them, each handed on to what
-- the rule builds of its answer, and to the conclusion it reaches once they
-- all ran. The engine of YAML interprets a rule into them ('morphingOf',
-- 'dataizationOf'), the one 'phino compile' writes builds them in Haskell out
-- of the very same spine ('morphingSpine', 'dataizationSpine'), and 'Morph'
-- and 'Dataize' run them, so the chain a run makes does not depend on which
-- of the two built them (#1628).
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

-- One rule of 𝕄 or 𝔻 ready to run: what it makes of a term in a universe,
-- which is nothing where the two do not match it and the premises it runs
-- where they do, the first way they match it. The context tells its
-- conditions the world and the normal forms of the engine.
type Inference value = RuleContext -> Expression -> Expression -> IO (Maybe (Premises value))

-- The premises a rule runs beside its spine, in the order it lists them: 𝕄
-- of a term in a universe, 𝔼 of a formation in one, or 𝒞 of a term in a
-- context, each handed on to what the rule builds of its answer, and the
-- conclusion the rule comes to once they all ran.
data Premises value
  = Morphs Expression Expression (Expression -> IO (Premises value))
  | Evaluates Expression Expression (Expression -> IO (Premises value))
  | Contextualizes Expression Expression (Expression -> IO (Premises value))
  | Concludes (Conclusion value)

-- What a rule concludes with: the value it answers, which a step of the
-- label takes the chain to; or its judgment asked again, in the universe the
-- rule names, of the term it built, once that term is reached the way the
-- rule says.
data Conclusion value
  = Answered (Judgment, String) value
  | Onward Way Expression Expression
  deriving (Eq, Show)

-- How the term a judgment is asked about again is reached from the one its
-- rule built: by a step of the label; by a step of the label and 𝒩 after it;
-- the same, where the term is the universe the rule matched, which the run
-- has named already, so the step takes the chain to that world and nothing is
-- normalized (#1453); or by 𝕄 in the universe given, its steps spliced into
-- the chain.
data Way
  = Taken (Judgment, String)
  | Normalized (Judgment, String)
  | Named (Judgment, String)
  | Staged Expression
  deriving (Eq, Show)

-- What a rule of 𝕄 does once it matched, read off its premises: the ones it
-- runs beside its spine, in the order it lists them, and the conclusion it
-- comes to, the terms of both written with the metas of the rule; or why no
-- run can take it. A rule no premise produces the conclusion of answers it,
-- and a rule of any other kind is asked again of the argument of the 'morph'
-- premise producing its conclusion, normalized where a 'normalize' premise
-- produces that argument.
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

-- What a rule of 𝔻 does once it matched, read off its premises the way
-- 'morphingSpine' reads those of 𝕄, where 'morph' may produce the argument of
-- the 'dataize' premise too. A step of 𝔻 is labelled by the first premise the
-- rule runs beside its spine — 'box' by its 'contextualize', 'fire' by its
-- 'evaluate' — and taken by the judgment that premise runs; with none it is
-- labelled blank where it normalizes and by the verb of its conclusion
-- otherwise.
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

-- The premise binding the given expression meta, if any. The conclusion of a
-- rule and the argument of a continuation premise are looked up here to find
-- the premise that produces them.
producer :: Expression -> [Y.Premise] -> Maybe Y.Premise
producer (ExMeta name) = find (\premise -> premise.result == name)
producer _ = const Nothing

-- The premises whose result meta is not bound by any of the given ones — the
-- side-computations left once the spine premises are removed.
excluding :: [Y.Premise] -> [Y.Premise] -> [Y.Premise]
excluding premises removed = filter (\premise -> premise.result `notElem` map (.result) removed) premises

-- What a step a premise takes is labelled with in the chain: the judgment the
-- premise runs, which picks the arrow of the step in LaTeX (#1536), and its
-- verb, which names the step.
label :: Y.Operation -> (Judgment, String)
label (Y.OpMorph _ _) = (Morphing, "morph")
label (Y.OpNormalize _) = (Normalization, "normalize")
label (Y.OpEvaluate _ _) = (Evaluation, "evaluate")
label (Y.OpContextualize _ _) = (Contextualization, "contextualize")
label (Y.OpDataize _ _) = (Dataization, "dataize")

-- A rule of 𝕄 as the engine of YAML runs it (see 'interpreted').
morphingOf :: Y.MorphRule -> Inference Expression
morphingOf rule = interpreted buildExpressionThrows (Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing) rule.ematch (morphingSpine rule)

-- A rule of 𝔻 as the engine of YAML runs it (see 'interpreted').
dataizationOf :: Y.DataizeRule -> Inference Bytes
dataizationOf rule = interpreted buildBytesThrows (Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing) rule.ematch (dataizationSpine rule)

-- A rule of 𝕄 or 𝔻 the matcher matches, the pattern against the term and the
-- pattern of the universe against the universe, its 'when' and its '𝑛' and
-- '𝑘' metas checked the way those of a rewriting rule are, and the premises
-- of its spine built out of the metas the first match bound, each binding its
-- own meta to the answer it is handed. Only 𝕄, 𝔼 and 𝒞 run beside a spine.
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

-- A rule of 𝕄 or 𝔻 'phino compile' turned into Haskell: a function telling
-- every way the term and the universe match it, of which a run takes the
-- first, as it takes the first match of a rule of YAML.
direct :: (Expression -> Expression -> [Premises value]) -> Inference value
direct rule _ term univ = pure (listToMaybe (rule term univ))
