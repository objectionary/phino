{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MatcherSpec where

import AST
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Matcher
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

class Expected e where
  type ExpectedResult e
  toExpected :: e -> ExpectedResult e

instance Expected [[(String, MetaValue)]] where
  type ExpectedResult [[(String, MetaValue)]] = [Subst]
  toExpected = map (Subst . Map.fromList)

maybeCombined :: Subst -> Subst -> Subst
maybeCombined first second =
  fromMaybe
    (error "combine returned Nothing")
    (combine first second)

test ::
  (Expected e, ExpectedResult e ~ r, Eq r, Show r) =>
  (a -> a -> b -> r) ->
  [(String, a, a, b, e)] ->
  SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, ptn, tgt, scope, mp) ->
    it desc $ function ptn tgt scope `shouldBe` toExpected mp

spec :: Spec
spec = do
  describe "matchExpressionDeep: expression => expression => [substitution]" $
    test
      matchExpressionDeep
      [
        ( "[[!a -> Q.org.!a]] => [[f -> [[x -> Q.org.x]], t -> [[y -> Q.org.y]] => [(!a >> x), (!a >> y)]"
        , ExFormation [BiTau (AtMeta "a") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtMeta "a"))]
        , ExFormation
            [ BiTau (AtLabel "f") (ExFormation [BiTau (AtLabel "x") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "x"))])
            , BiTau (AtLabel "t") (ExFormation [BiTau (AtLabel "y") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "y"))])
            ]
        , defaultScope
        , [[("a", MvAttribute (AtLabel "x"))], [("a", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "!e => [[x -> Q]] => [(!e >> [[x -> Q]] ), (!e >> Q)]"
        , ExMeta "e"
        , ExFormation [BiTau (AtLabel "x") ExGlobal]
        , defaultScope
        ,
          [ [("e", MvExpression (ExFormation [BiTau (AtLabel "x") ExGlobal]) defaultScope)]
          , [("e", MvExpression ExGlobal (ExFormation [BiTau (AtLabel "x") ExGlobal]))]
          ]
        )
      ,
        ( "!e.!a => Q.org.eolang => [(!e >> Q.org, !a >> eolang), (!e >> Q, !a >> org)]"
        , ExDispatch (ExMeta "e") (AtMeta "a")
        , ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")
        , defaultScope
        ,
          [ [("e", MvExpression (ExDispatch ExGlobal (AtLabel "org")) defaultScope), ("a", MvAttribute (AtLabel "eolang"))]
          , [("e", MvExpression ExGlobal defaultScope), ("a", MvAttribute (AtLabel "org"))]
          ]
        )
      ,
        ( "⟦!B1, !a ↦ ∅, !B2⟧.!a => ⟦ x ↦ ξ.t, t ↦ ∅ ⟧.t(ρ ↦ ⟦ x ↦ ξ.t, t ↦ ∅ ⟧) => [(!B1 >> ⟦x ↦ ξ.t⟧, !a >> t, !B2 >> ⟦⟧ )]"
        , ExDispatch (ExFormation [BiMeta "B1", BiVoid (AtMeta "a"), BiMeta "B2"]) (AtMeta "a")
        , ExApplication
            ( ExDispatch
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                    , BiVoid (AtLabel "t")
                    ]
                )
                (AtLabel "t")
            )
            ( BiTau
                AtRho
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                    , BiVoid (AtLabel "t")
                    ]
                )
            )
        , defaultScope
        ,
          [
            [ ("B1", MvBindings [BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))])
            , ("a", MvAttribute (AtLabel "t"))
            , ("B2", MvBindings [])
            ]
          ]
        )
      ,
        ( "somebody"
        , ExFormation
            [ BiTau
                (AtLabel "i1")
                ( ExFormation
                    [ BiTau (AtLabel "a") (ExMeta "e0")
                    , BiTau (AtLabel "b") (ExMeta "e-first")
                    ]
                )
            , BiTau
                (AtLabel "i2")
                ( ExFormation
                    [ BiTau (AtLabel "a") (ExMeta "e0")
                    , BiTau (AtLabel "b") (ExMeta "e-second")
                    ]
                )
            ]
        , ExFormation
            [ BiTau
                (AtLabel "i1")
                ( ExFormation
                    [ BiTau (AtLabel "a") ExGlobal
                    , BiTau (AtLabel "b") ExThis
                    ]
                )
            , BiTau
                (AtLabel "i2")
                ( ExFormation
                    [ BiTau (AtLabel "a") ExGlobal
                    , BiTau (AtLabel "b") (ExFormation [BiVoid AtPhi])
                    ]
                )
            ]
        , defaultScope
        ,
          [
            [
              ( "e0"
              , MvExpression
                  ExGlobal
                  ( ExFormation
                      [ BiTau (AtLabel "a") ExGlobal
                      , BiTau (AtLabel "b") ExThis
                      ]
                  )
              )
            ,
              ( "e-first"
              , MvExpression
                  ExThis
                  ( ExFormation
                      [ BiTau (AtLabel "a") ExGlobal
                      , BiTau (AtLabel "b") ExThis
                      ]
                  )
              )
            ,
              ( "e-second"
              , MvExpression
                  (ExFormation [BiVoid AtPhi])
                  ( ExFormation
                      [ BiTau (AtLabel "a") ExGlobal
                      , BiTau (AtLabel "b") (ExFormation [BiVoid AtPhi])
                      ]
                  )
              )
            ]
          ]
        )
      ]

  describe "matchAttribute: attribute => attribute => substitution" $
    forM_
      [ ("~1 => ~1 => [()]", AtAlpha 1, AtAlpha 1, [[]])
      , ("!a => ^ => [(!a >> ^)]", AtMeta "a", AtRho, [[("a", MvAttribute AtRho)]])
      , ("!a => @ => [(!a >> @)]", AtMeta "a", AtPhi, [[("a", MvAttribute AtPhi)]])
      , ("~0 => [] => [()]", AtAlpha 0, AtLabel "x", [])
      ]
      ( \(desc, ptn, tgt, mp) ->
          it desc $ matchAttribute ptn tgt `shouldBe` toExpected mp
      )

  describe "matchBindings: [binding] => [binding] => substitution" $
    test
      matchBindings
      [
        ( "[[]] => [[]] => ()"
        , []
        , []
        , defaultScope
        , [[]]
        )
      ,
        ( "[[!B]] => T:[[x -> ?, D> 01-, L> Func]] => (!B >> T)"
        , [BiMeta "B"]
        , [BiVoid (AtLabel "x"), BiDelta (BtOne "01"), BiLambda "Func"]
        , defaultScope
        , [[("B", MvBindings [BiVoid (AtLabel "x"), BiDelta (BtOne "01"), BiLambda "Func"])]]
        )
      ,
        ( "[[D> 00-]] => [[D> 00-, L> Func]] => []"
        , [BiDelta (BtOne "00")]
        , [BiDelta (BtOne "00"), BiLambda "Func"]
        , defaultScope
        , []
        )
      ,
        ( "[[y -> ?, !a -> ?]] => [[y -> ?, x -> ?]] => (!a >> x)"
        , [BiVoid (AtLabel "y"), BiVoid (AtMeta "a")]
        , [BiVoid (AtLabel "y"), BiVoid (AtLabel "x")]
        , defaultScope
        , [[("a", MvAttribute (AtLabel "x"))]]
        )
      ,
        ( "[[!B, x -> ?]] => [[x -> ?]] => (!B >> [[]])"
        , [BiMeta "B", BiVoid (AtLabel "x")]
        , [BiVoid (AtLabel "x")]
        , defaultScope
        , [[("B", MvBindings [])]]
        )
      ,
        ( "[[!B1, x -> ?, !B2]] => [[x -> ?, y -> ?]] => (!B1 >> [[]], !B2 >> [[y -> ?]])"
        , [BiMeta "B1", BiVoid (AtLabel "x"), BiMeta "B2"]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")]
        , defaultScope
        , [[("B1", MvBindings []), ("B2", MvBindings [BiVoid (AtLabel "y")])]]
        )
      ,
        ( "[[!B1, !x -> ?, !B2]] => [[y -> ?, D> -> 00-, L> Func]] => (!x >> y, !B1 >> [[]], !B2 >> [[D> -> 00-, L> Func]])"
        , [BiMeta "B1", BiVoid (AtMeta "x"), BiMeta "B2"]
        , [BiVoid (AtLabel "y"), BiDelta (BtOne "00"), BiLambda "Func"]
        , defaultScope
        , [[("B1", MvBindings []), ("B2", MvBindings [BiDelta (BtOne "00"), BiLambda "Func"]), ("x", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "[[!x -> ?, !y -> ?]] => [[a -> ?, b -> ?]] => (!x >> a, !y >> b)"
        , [BiVoid (AtMeta "x"), BiVoid (AtMeta "y")]
        , [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")]
        , defaultScope
        , [[("x", MvAttribute (AtLabel "a")), ("y", MvAttribute (AtLabel "b"))]]
        )
      ,
        ( "[[t -> ?, !B]] => [[t -> ?, x -> Q, y -> $]] => (!B >> [[x -> Q, y -> $]])"
        , [BiVoid (AtLabel "t"), BiMeta "B"]
        , [BiVoid (AtLabel "t"), BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis]
        , defaultScope
        , [[("B", MvBindings [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis])]]
        )
      ,
        ( "[[!B, z -> Q]] => [[x -> Q, y -> $, z -> Q]] => (!B >> [[x -> Q, y -> $]])"
        , [BiMeta "B", BiTau (AtLabel "z") ExGlobal]
        , [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis, BiTau (AtLabel "z") ExGlobal]
        , defaultScope
        , [[("B", MvBindings [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis])]]
        )
      ,
        ( "[[L> Func, D> 00-]] => [[D> 00-, L> Func]] => []"
        , [BiLambda "Func", BiDelta (BtOne "00")]
        , [BiDelta (BtOne "00"), BiLambda "Func"]
        , defaultScope
        , []
        )
      ,
        ( "[[t -> ?, !B]] => [[x -> ?, t -> ?]] => []"
        , [BiVoid (AtLabel "t"), BiMeta "B"]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "t")]
        , defaultScope
        , []
        )
      ,
        ( "[[!B, !a -> ?]] => [[x -> ?, y -> ?]] => (!a >> y, !B >> [[ x -> ? ]] )"
        , [BiMeta "B", BiVoid (AtMeta "a")]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")]
        , defaultScope
        , [[("a", MvAttribute (AtLabel "y")), ("B", MvBindings [BiVoid (AtLabel "x")])]]
        )
      ,
        ( "[[!B1, !a -> ?, !B2]] => [[ x -> ?, y -> ?, z -> ? ]] => [(!B1 >> [[]], !a >> x, !B2 >> [[ y -> ?, z -> ? ]]), (...), (...)]"
        , [BiMeta "B1", BiVoid (AtMeta "a"), BiMeta "B2"]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y"), BiVoid (AtLabel "z")]
        , defaultScope
        ,
          [
            [ ("B1", MvBindings [])
            , ("a", MvAttribute (AtLabel "x"))
            , ("B2", MvBindings [BiVoid (AtLabel "y"), BiVoid (AtLabel "z")])
            ]
          ,
            [ ("B1", MvBindings [BiVoid (AtLabel "x")])
            , ("a", MvAttribute (AtLabel "y"))
            , ("B2", MvBindings [BiVoid (AtLabel "z")])
            ]
          ,
            [ ("B1", MvBindings [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")])
            , ("a", MvAttribute (AtLabel "z"))
            , ("B2", MvBindings [])
            ]
          ]
        )
      ,
        ( "[[!B1, !a1 -> ?, !B2, !a2 -> ?, !B3]] => [[ a -> ?, b -> ?, x -> ?, y -> ?, z -> ? ]] => [10 substs]"
        , [BiMeta "B1", BiVoid (AtMeta "a1"), BiMeta "B2", BiVoid (AtMeta "a2"), BiMeta "B3"]
        ,
          [ BiVoid (AtLabel "a")
          , BiVoid (AtLabel "b")
          , BiVoid (AtLabel "x")
          , BiVoid (AtLabel "y")
          , BiVoid (AtLabel "z")
          ]
        , defaultScope
        ,
          [
            [ ("B1", MvBindings [])
            , ("a1", MvAttribute (AtLabel "a"))
            , ("B2", MvBindings [])
            , ("a2", MvAttribute (AtLabel "b"))
            , ("B3", MvBindings [BiVoid (AtLabel "x"), BiVoid (AtLabel "y"), BiVoid (AtLabel "z")])
            ]
          ,
            [ ("B1", MvBindings [])
            , ("a1", MvAttribute (AtLabel "a"))
            , ("B2", MvBindings [BiVoid (AtLabel "b")])
            , ("a2", MvAttribute (AtLabel "x"))
            , ("B3", MvBindings [BiVoid (AtLabel "y"), BiVoid (AtLabel "z")])
            ]
          ,
            [ ("B1", MvBindings [])
            , ("a1", MvAttribute (AtLabel "a"))
            , ("B2", MvBindings [BiVoid (AtLabel "b"), BiVoid (AtLabel "x")])
            , ("a2", MvAttribute (AtLabel "y"))
            , ("B3", MvBindings [BiVoid (AtLabel "z")])
            ]
          ,
            [ ("B1", MvBindings [])
            , ("a1", MvAttribute (AtLabel "a"))
            , ("B2", MvBindings [BiVoid (AtLabel "b"), BiVoid (AtLabel "x"), BiVoid (AtLabel "y")])
            , ("a2", MvAttribute (AtLabel "z"))
            , ("B3", MvBindings [])
            ]
          ,
            [ ("B1", MvBindings [BiVoid (AtLabel "a")])
            , ("a1", MvAttribute (AtLabel "b"))
            , ("B2", MvBindings [])
            , ("a2", MvAttribute (AtLabel "x"))
            , ("B3", MvBindings [BiVoid (AtLabel "y"), BiVoid (AtLabel "z")])
            ]
          ,
            [ ("B1", MvBindings [BiVoid (AtLabel "a")])
            , ("a1", MvAttribute (AtLabel "b"))
            , ("B2", MvBindings [BiVoid (AtLabel "x")])
            , ("a2", MvAttribute (AtLabel "y"))
            , ("B3", MvBindings [BiVoid (AtLabel "z")])
            ]
          ,
            [ ("B1", MvBindings [BiVoid (AtLabel "a")])
            , ("a1", MvAttribute (AtLabel "b"))
            , ("B2", MvBindings [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")])
            , ("a2", MvAttribute (AtLabel "z"))
            , ("B3", MvBindings [])
            ]
          ,
            [ ("B1", MvBindings [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")])
            , ("a1", MvAttribute (AtLabel "x"))
            , ("B2", MvBindings [])
            , ("a2", MvAttribute (AtLabel "y"))
            , ("B3", MvBindings [BiVoid (AtLabel "z")])
            ]
          ,
            [ ("B1", MvBindings [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")])
            , ("a1", MvAttribute (AtLabel "x"))
            , ("B2", MvBindings [BiVoid (AtLabel "y")])
            , ("a2", MvAttribute (AtLabel "z"))
            , ("B3", MvBindings [])
            ]
          ,
            [ ("B1", MvBindings [BiVoid (AtLabel "a"), BiVoid (AtLabel "b"), BiVoid (AtLabel "x")])
            , ("a1", MvAttribute (AtLabel "y"))
            , ("B2", MvBindings [])
            , ("a2", MvAttribute (AtLabel "z"))
            , ("B3", MvBindings [])
            ]
          ]
        )
      ]

  describe "matchExpression: expression => pattern => substitution" $
    test
      matchExpression
      [ ("$ => $ => [()]", ExThis, ExThis, defaultScope, [[]])
      , ("Q => Q => [()]", ExGlobal, ExGlobal, defaultScope, [[]])
      ,
        ( "!e => Q => [(!e >> Q)]"
        , ExMeta "e"
        , ExGlobal
        , defaultScope
        , [[("e", MvExpression ExGlobal defaultScope)]]
        )
      ,
        ( "!e => Q.org(x -> $) => [(!e >> Q.org(x -> $))]"
        , ExMeta "e"
        , ExApplication (ExDispatch ExGlobal (AtLabel "org")) (BiTau (AtLabel "x") ExThis)
        , defaultScope
        , [[("e", MvExpression (ExApplication (ExDispatch ExGlobal (AtLabel "org")) (BiTau (AtLabel "x") ExThis)) defaultScope)]]
        )
      ,
        ( "!e1.x => Q.org.x => [(!e1 >> Q.org)]"
        , ExDispatch (ExMeta "e1") (AtLabel "x")
        , ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "x")
        , defaultScope
        , [[("e1", MvExpression (ExDispatch ExGlobal (AtLabel "org")) defaultScope)]]
        )
      ,
        ( "!e.org.!a => $.org.x => [(!e >> $, !a >> x)]"
        , ExDispatch (ExDispatch (ExMeta "e") (AtLabel "org")) (AtMeta "a")
        , ExDispatch (ExDispatch ExThis (AtLabel "org")) (AtLabel "x")
        , defaultScope
        , [[("e", MvExpression ExThis defaultScope), ("a", MvAttribute (AtLabel "x"))]]
        )
      ,
        ( "[[!a -> !e, !B]].!a => [[x -> Q, y -> $]].x => [(!a >> x, !e >> Q, !B >> [y -> $])]"
        , ExDispatch (ExFormation [BiTau (AtMeta "a") (ExMeta "e"), BiMeta "B"]) (AtMeta "a")
        , ExDispatch
            ( ExFormation
                [ BiTau (AtLabel "x") ExGlobal
                , BiTau (AtLabel "y") ExThis
                ]
            )
            (AtLabel "x")
        , defaultScope
        ,
          [
            [ ("a", MvAttribute (AtLabel "x"))
            ,
              ( "e"
              , MvExpression
                  ExGlobal
                  ( ExFormation
                      [ BiTau (AtLabel "x") ExGlobal
                      , BiTau (AtLabel "y") ExThis
                      ]
                  )
              )
            , ("B", MvBindings [BiTau (AtLabel "y") ExThis])
            ]
          ]
        )
      ,
        ( "Q * !t => Q.org => [(!t >> [.org])]"
        , ExMetaTail ExGlobal "t"
        , ExDispatch ExGlobal (AtLabel "x")
        , defaultScope
        , [[("t", MvTail [TaDispatch (AtLabel "x")])]]
        )
      ,
        ( "Q * !t => Q.org(x -> [[]]) => [(!t >> [.org, (x -> [[]])])]"
        , ExMetaTail ExGlobal "t"
        , ExApplication (ExDispatch ExGlobal (AtLabel "org")) (BiTau (AtLabel "x") defaultScope)
        , defaultScope
        , [[("t", MvTail [TaDispatch (AtLabel "org"), TaApplication (BiTau (AtLabel "x") defaultScope)])]]
        )
      ,
        ( "Q.!a * !t => Q.org.eolang(x -> [[]]) => [(!a >> org, !t >> [ .eolang, ( x -> [[ ]] ) ])]"
        , ExMetaTail (ExDispatch ExGlobal (AtMeta "a")) "t"
        , ExApplication (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (BiTau (AtLabel "x") defaultScope)
        , defaultScope
        , [[("a", MvAttribute (AtLabel "org")), ("t", MvTail [TaDispatch (AtLabel "eolang"), TaApplication (BiTau (AtLabel "x") defaultScope)])]]
        )
      ,
        ( "Q.x(y -> $ * !t1) * !t2 => Q.x(y -> $.q).p => [(!t1 >> [.q], !t2 >> [.p])]"
        , ExMetaTail (ExApplication (ExDispatch ExGlobal (AtLabel "x")) (BiTau (AtLabel "y") (ExMetaTail ExThis "t1"))) "t2"
        , ExDispatch (ExApplication (ExDispatch ExGlobal (AtLabel "x")) (BiTau (AtLabel "y") (ExDispatch ExThis (AtLabel "q")))) (AtLabel "p")
        , defaultScope
        , [[("t1", MvTail [TaDispatch (AtLabel "q")]), ("t2", MvTail [TaDispatch (AtLabel "p")])]]
        )
      ,
        ( "[[!B1, !a ↦ !e1, !B2]](!a ↦ !e2) => ⟦ t ↦ ξ.k, x ↦ ξ.t, k ↦ ∅ ⟧(x ↦ ξ) => [(!B1 >> [[ t -> $.k ]], !a >> x, !B2 >> [[ k -> ? ]], !e1 >> $.t, !e2 >> $)]"
        , ExApplication (ExFormation [BiMeta "B1", BiTau (AtMeta "a") (ExMeta "e1"), BiMeta "B2"]) (BiTau (AtMeta "a") (ExMeta "e2"))
        , ExApplication
            ( ExFormation
                [ BiTau (AtLabel "t") (ExDispatch ExThis (AtLabel "k"))
                , BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                , BiVoid (AtLabel "k")
                ]
            )
            (BiTau (AtLabel "x") ExThis)
        , defaultScope
        ,
          [
            [ ("B1", MvBindings [BiTau (AtLabel "t") (ExDispatch ExThis (AtLabel "k"))])
            , ("a", MvAttribute (AtLabel "x"))
            , ("B2", MvBindings [BiVoid (AtLabel "k")])
            ,
              ( "e1"
              , MvExpression
                  (ExDispatch ExThis (AtLabel "t"))
                  ( ExFormation
                      [ BiTau (AtLabel "t") (ExDispatch ExThis (AtLabel "k"))
                      , BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                      , BiVoid (AtLabel "k")
                      ]
                  )
              )
            , ("e2", MvExpression ExThis defaultScope)
            ]
          ]
        )
      ]

  describe "combine" $ do
    it "combines empty substitutions" $
      combine substEmpty substEmpty `shouldBe` Just substEmpty
    it "combines two empty substs from list" $
      combine (Subst Map.empty) (Subst Map.empty) `shouldBe` Just substEmpty
    it "combines empty subst with single one" $ do
      let Subst joined = maybeCombined substEmpty (Subst (Map.singleton "at" (MvAttribute AtPhi)))
      Map.lookup "at" joined `shouldBe` Just (MvAttribute AtPhi)
    it "combines two different subst" $ do
      let Subst joined =
            maybeCombined
              (Subst (Map.singleton "first" (MvAttribute AtPhi)))
              (Subst (Map.singleton "second" (MvBytes (BtOne "00"))))
      Map.lookup "first" joined `shouldBe` Just (MvAttribute AtPhi)
      Map.lookup "second" joined `shouldBe` Just (MvBytes (BtOne "00"))
    it "leave values in the same substs" $ do
      let rho = MvAttribute AtRho
          first =
            Subst
              ( Map.fromList
                  [ ("first", rho)
                  , ("second", MvAttribute AtPhi)
                  ]
              )
          second = Subst (Map.singleton "first" rho)
          Subst joined = maybeCombined first second
      Map.lookup "first" joined `shouldBe` Just (MvAttribute AtRho)
    it "returns Nothing if values are different" $
      combine (Subst (Map.singleton "x" (MvAttribute AtPhi))) (Subst (Map.singleton "x" (MvAttribute AtRho))) `shouldBe` Nothing
    it "clears all the values" $ do
      let first =
            Subst
              ( Map.fromList
                  [ ("x", MvAttribute AtRho)
                  , ("y", MvBytes (BtOne "1F"))
                  ]
              )
          second = Subst (Map.singleton "x" (MvAttribute AtPhi))
      combine first second `shouldBe` Nothing
