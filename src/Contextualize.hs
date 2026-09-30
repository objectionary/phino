{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The Contextualization function 𝒞 of the calculus, carried out by the rules
-- of 'resources/contextualization' and by nothing else, the way the other
-- judgments are carried out by theirs: 𝒞(n, c) is the conclusion of the one
-- rule whose 'match' matches the term and whose 'c-match' matches the context,
-- every premise of it a 𝒞 of a smaller term. A change to a rule is a change to
-- what runs, and 'explain --contextualize' prints the rules that fire (#1618).
-- The letters '𝑛' and '𝑘' of the rules are only names here, since the match is
-- the plain one, which demands neither a normal form nor an absolute term.
module Contextualize (concluded, contextualize, ContextualizeException (..)) where

import AST
import Builder (buildExpression)
import Control.Exception (Exception, throwIO)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.List (intercalate)
import qualified Data.Text as T
import Matcher (MetaValue (..), Subst, combine, matchExpression', substSingle)
import Printer (printExpression)
import Text.Printf (printf)
import qualified Yaml as Y

-- 𝒞 has no single conclusion for a term: no rule matches it, more than one
-- does, or the one that does cannot be carried out on it. It carries the term
-- and the reason, which is a clause of the sentence it is shown in.
data ContextualizeException = Uncontextualizable Expression String
  deriving (Exception)

instance Show ContextualizeException where
  show (Uncontextualizable term reason) = printf "Contextualization has no single conclusion, since %s: %s" reason (printExpression term)

-- The conclusion of the one rule that matched the term, among the names of the
-- rules that matched it beside what each of them concludes, or the failure of
-- a term no rule or more than one rule matches. Only the conclusion of the one
-- rule is ever worked out, so the other judgments of 'contextualize' and of
-- the function 'phino compile' writes for 𝒞 are never asked for (#1617).
concluded :: Expression -> [(String, Either ContextualizeException Expression)] -> Either ContextualizeException Expression
concluded _ [(_, answer)] = answer
concluded term [] = Left (Uncontextualizable term "no contextualization rule matches the term")
concluded term several = Left (Uncontextualizable term (printf "the contextualization rules %s all match the term" (intercalate ", " (map fst several))))

-- The term with every ξ outside the formations nested in it standing for the
-- context, or the failure of the term that has no single conclusion, which may
-- be a part of the term rather than the term itself.
contextualize :: Expression -> Expression -> IO Expression
contextualize expr context = either throwIO pure (contextualized expr context)
  where
    contextualized :: Expression -> Expression -> Either ContextualizeException Expression
    contextualized term around =
      concluded
        term
        [ (rule.name, foldM (premised term rule) subst rule.premises >>= built term rule rule.cresult)
        | (rule, subst) <- matching term around
        ]
    -- Every rule matching the term and the context, once for every way it
    -- matches them, so a rule matching in two ways is as ambiguous as two rules.
    matching :: Expression -> Expression -> [(Y.ContextualizeRule, Subst)]
    matching term around =
      [ (rule, subst)
      | rule <- Y.contextualizationRules
      , matched <- matchExpression' rule.match term
      , surrounding <- matchExpression' rule.cmatch around
      , Just subst <- [combine matched surrounding]
      ]
    premised :: Expression -> Y.ContextualizeRule -> Subst -> Y.Premise -> Either ContextualizeException Subst
    premised term rule subst (Y.Premise result (Y.OpContextualize inner outer)) = do
      part <- built term rule inner subst
      surrounding <- built term rule outer subst
      answer <- contextualized part surrounding
      maybe
        (Left (Uncontextualizable term (printf "the premise '%s' of the contextualization rule '%s' clashes with a binding" (T.unpack result) rule.name)))
        Right
        (combine (substSingle result (MvExpression answer)) subst)
    premised term rule _ premise =
      Left (Uncontextualizable term (printf "the premise '%s' of the contextualization rule '%s' is not a contextualization" (T.unpack premise.result) rule.name))
    built :: Expression -> Y.ContextualizeRule -> Expression -> Subst -> Either ContextualizeException Expression
    built term rule template subst =
      first
        (Uncontextualizable term . printf "the contextualization rule '%s' cannot be built, %s" rule.name)
        (buildExpression template subst)
