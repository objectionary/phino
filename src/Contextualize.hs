{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

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

data ContextualizeException = Uncontextualizable Expression String
  deriving (Exception)

instance Show ContextualizeException where
  show (Uncontextualizable term reason) = printf "Contextualization has no single conclusion, since %s: %s" reason (printExpression term)

concluded :: Expression -> [(String, Either ContextualizeException Expression)] -> Either ContextualizeException Expression
concluded _ [(_, answer)] = answer
concluded term [] = Left (Uncontextualizable term "no contextualization rule matches the term")
concluded term several = Left (Uncontextualizable term (printf "the contextualization rules %s all match the term" (intercalate ", " (map fst several))))

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
