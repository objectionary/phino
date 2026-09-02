{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module BuilderSpec where

import AST
import Builder
import Control.Exception (SomeException)
import Control.Monad
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Matcher
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, anyException, describe, it, shouldBe, shouldSatisfy, shouldThrow)

test :: (Show a, Eq a) => (a -> Subst -> Either String a) -> [(String, a, [(T.Text, MetaValue)], Either String a)] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, expr, mp, res) ->
    it desc $ function expr (Subst (Map.fromList mp)) `shouldBe` res

spec :: Spec
spec = do
  describe "buildExpression" $
    test
      buildExpression
      [
        ( "Q.!t => (!t >> x) => Q.x"
        , ExDispatch ExRoot (AtMeta "t")
        , [("t", MvAttribute (AtLabel "x"))]
        , Right (ExDispatch ExRoot (AtLabel "x"))
        )
      ,
        ( "Q.c(!t -> !e) => (!t >> x, !e >> $.y.z) => Q.c(x -> $.y.z)"
        , ExApplication (ExDispatch ExRoot (AtLabel "c")) (ArTau (AtMeta "t") (ExMeta "e"))
        , [("t", MvAttribute (AtLabel "x")), ("e", MvExpression (ExDispatch (ExDispatch ExXi (AtLabel "y")) (AtLabel "z")))]
        , Right (ExApplication (ExDispatch ExRoot (AtLabel "c")) (ArTau (AtLabel "x") (ExDispatch (ExDispatch ExXi (AtLabel "y")) (AtLabel "z"))))
        )
      ,
        ( "[[!t -> $.x, !B]] => (!t >> y, !B >> [[b -> ?, L> Func]]) => [[y -> $.x, b -> ?, L> Func]]"
        , ExFormation [BiTau (AtMeta "t") (ExDispatch ExXi (AtLabel "x")), BiMeta "B"]
        , [("t", MvAttribute (AtLabel "y")), ("B", MvBindings [BiVoid (AtLabel "b"), BiLambda (Function "Func")])]
        , Right
            ( ExFormation
                [ BiTau (AtLabel "y") (ExDispatch ExXi (AtLabel "x"))
                , BiVoid (AtLabel "b")
                , BiLambda (Function "Func")
                ]
            )
        )
      ,
        ( "Q.!t => () => X"
        , ExDispatch ExRoot (AtMeta "t")
        , []
        , Left "meta 't' is either does not exist or refers to an inappropriate term"
        )
      ,
        ( "!e0(!t1 -> !e1, !t2 => !e2) => (!e0 >> [[]], !t1 >> x, !e1 >> Q, !t2 >> y, !e2 >> $) => [[]](x -> Q, y -> $)"
        , ExApplication (ExApplication (ExMeta "e0") (ArTau (AtMeta "t1") (ExMeta "e1"))) (ArTau (AtMeta "t2") (ExMeta "e2"))
        ,
          [ ("e0", MvExpression (ExFormation []))
          , ("t1", MvAttribute (AtLabel "x"))
          , ("e1", MvExpression ExRoot)
          , ("t2", MvAttribute (AtLabel "y"))
          , ("e2", MvExpression ExXi)
          ]
        , Right (ExApplication (ExApplication (ExFormation []) (ArTau (AtLabel "x") ExRoot)) (ArTau (AtLabel "y") ExXi))
        )
      ,
        ( "⟦!t ↦ ∅, !B⟧.!t => (!t >> t, !B >> ⟦ x ↦ ξ.t ⟧ ) => ⟦ t ↦ ∅, x ↦ ξ.t ⟧.t"
        , ExDispatch (ExFormation [BiVoid (AtMeta "t"), BiMeta "B"]) (AtMeta "t")
        ,
          [ ("t", MvAttribute (AtLabel "t"))
          , ("B", MvBindings [BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))])
          ]
        , Right
            ( ExDispatch
                ( ExFormation
                    [ BiVoid (AtLabel "t")
                    , BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                    ]
                )
                (AtLabel "t")
            )
        )
      ,
        ( "Q.c(α!i -> !e) => (!i >> 2, !e >> $) => Q.c(α2 -> $)"
        , ExApplication (ExDispatch ExRoot (AtLabel "c")) (ArAlpha (AlMeta "i") (ExMeta "e"))
        , [("i", MvIndex 2), ("e", MvExpression ExXi)]
        , Right (ExApplication (ExDispatch ExRoot (AtLabel "c")) (ArAlpha (Alpha 2) ExXi))
        )
      ,
        ( "Q.c(α!i -> Q) => () => X"
        , ExApplication (ExDispatch ExRoot (AtLabel "c")) (ArAlpha (AlMeta "i") ExRoot)
        , []
        , Left "meta 'i' is either does not exist or refers to an inappropriate term"
        )
      ]

  describe "buildExpressions" $ do
    it "!e => [(!e >> Q.x), (!e >> $.y)] => [Q.x, $.y]" $ do
      built <-
        buildExpressionsThrows
          (ExMeta "e")
          [ substSingle "e" (MvExpression (ExDispatch ExRoot (AtLabel "x")))
          , substSingle "e" (MvExpression (ExDispatch ExXi (AtLabel "y")))
          ]
      built `shouldBe` [ExDispatch ExRoot (AtLabel "x"), ExDispatch ExXi (AtLabel "y")]
    it "!e => [(!e1 >> Q.x)] => X" $
      buildExpressionsThrows
        (ExMeta "e")
        [substSingle "e1" (MvExpression (ExDispatch ExRoot (AtLabel "x")))]
        `shouldThrow` anyException

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
          , ("leaves any other expression untouched", ExMeta "e", commonContext, ExMeta "e")
          ]
          (\(desc, expr, context, expected) -> it desc (contextualize expr context `shouldBe` expected))

  describe "buildBinding: lambda and delta bindings from metas" $
    forM_
      [
        ( "builds a lambda binding from a bound function meta"
        , BiLambda (FnMeta "f")
        , substSingle "f" (MvFunction "Func")
        , Right [BiLambda (Function "Func")]
        )
      ,
        ( "fails to build a lambda binding from an unbound function meta"
        , BiLambda (FnMeta "f")
        , substEmpty
        , Left "meta 'f' is either does not exist or refers to an inappropriate term"
        )
      ,
        ( "builds a delta binding from a bound bytes meta"
        , BiDelta (BtMeta "b")
        , substSingle "b" (MvBytes (BtOne "00"))
        , Right [BiDelta (BtOne "00")]
        )
      ,
        ( "fails to build a delta binding from an unbound bytes meta"
        , BiDelta (BtMeta "b")
        , substEmpty
        , Left "meta 'b' is either does not exist or refers to an inappropriate term"
        )
      ,
        ( "fails to build a meta binding that is unbound"
        , BiMeta "B"
        , substEmpty
        , Left "meta 'B' is either does not exist or refers to an inappropriate term"
        )
      ]
      (\(desc, binding, subst, expected) -> it desc (buildBinding binding subst `shouldBe` expected))

  describe "the throwing builders report a descriptive message" $
    forM_
      [
        ( "buildBytesThrows names the bytes it could not build"
        , void (buildBytesThrows (BtMeta "b") substEmpty)
        , "Couldn't build bytes"
        )
      ,
        ( "buildBindingThrows names the binding it could not build"
        , void (buildBindingThrows (BiMeta "B") substEmpty)
        , "Couldn't build binding"
        )
      ,
        ( "buildAttributeThrows names the attribute it could not build"
        , void (buildAttributeThrows (AtMeta "t") substEmpty)
        , "Couldn't build attribute"
        )
      ,
        ( "buildExpressionThrows names the expression it could not build"
        , void (buildExpressionThrows (ExMeta "e") substEmpty)
        , "Couldn't build expression"
        )
      ]
      (\(desc, action, message) -> it desc (action `shouldThrow` (\exc -> message `isInfixOf` show (exc :: SomeException))))

  describe "build with duplicate attributes in bindings" $ do
    it "build binding with duplicates" $
      buildBinding (BiMeta "B") (substSingle "B" (MvBindings [BiVoid AtRho, BiVoid AtRho])) `shouldSatisfy` isLeft
    it "build formation with duplicates" $
      buildExpression (ExMeta "e") (substSingle "e" (MvExpression (ExFormation [BiVoid AtRho, BiVoid AtRho]))) `shouldSatisfy` isLeft
