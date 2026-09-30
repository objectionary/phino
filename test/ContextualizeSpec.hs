{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ContextualizeSpec where

import AST
import Contextualize (contextualize)
import Control.Exception (SomeException)
import Control.Monad (forM_)
import Data.List (isInfixOf)
import Test.Hspec (Spec, describe, it, shouldReturn, shouldThrow)

spec :: Spec
spec = do
  describe "contextualize" $
    let commonContext :: Expression
        commonContext = ExFormation [BiVoid AtRho]
     in forM_
          [ ("replaces a xi expression with the context", ExXi, commonContext, commonContext)
          , ("keeps a root expression untouched", ExRoot, commonContext, ExRoot)
          ,
            ( "keeps an empty formation untouched"
            , ExFormation [BiVoid AtRho]
            , ExFormation [BiVoid AtRho, BiVoid AtRho]
            , ExFormation [BiVoid AtRho]
            )
          ,
            ( "recurses into a dispatch application"
            , ExDispatch ExXi (AtLabel "z")
            , commonContext
            , ExDispatch commonContext (AtLabel "z")
            )
          , ("keeps a termination untouched", ExTermination, commonContext, ExTermination)
          ,
            ( "recurses into both sides of an application with a tau argument"
            , ExApplication ExXi (ArTau (AtLabel "x") ExXi)
            , commonContext
            , ExApplication commonContext (ArTau (AtLabel "x") commonContext)
            )
          ,
            ( "recurses into both sides of an application with an alpha argument"
            , ExApplication ExXi (ArAlpha (Alpha 0) ExXi)
            , commonContext
            , ExApplication commonContext (ArAlpha (Alpha 0) commonContext)
            )
          ]
          (\(desc, expr, context, expected) -> it desc (contextualize expr context `shouldReturn` expected))
  describe "contextualize a term no rule matches" $ do
    it "refuses a meta, since no contextualization rule matches it" $
      contextualize (ExMeta "e7") (ExFormation [BiVoid AtRho])
        `shouldThrow` (\err -> "no contextualization rule matches the term: 𝑒7" `isInfixOf` show (err :: SomeException))
    it "refuses a dispatch off a meta, naming the meta rather than the dispatch" $
      contextualize (ExDispatch (ExMeta "e13") (AtLabel "kvo")) (ExFormation [BiVoid AtRho])
        `shouldThrow` (\err -> "the term: 𝑒13" `isInfixOf` show (err :: SomeException))
