{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MatcherSpec where

import AST
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Matcher
import Parser (parseExpressionThrows)
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe, shouldSatisfy)

substs :: [[(T.Text, MetaValue)]] -> [Subst]
substs = map (\pairs -> Subst (Map.fromList [(k, [v]) | (k, v) <- pairs]))

maybeCombined :: Subst -> Subst -> Subst
maybeCombined first second =
  fromMaybe
    (error "combine returned Nothing")
    (combine first second)

test ::
  (a -> a -> b -> [Subst]) ->
  [(String, a, a, b, [Subst])] ->
  SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, ptn, tgt, scope, expected) ->
    it desc $ function ptn tgt scope `shouldBe` expected

spec :: Spec
spec = do
  describe "matchExpressionDeep: expression => expression => [substitution]" $
    test
      matchExpressionDeep
      [
        ( "Q => [[ @ -> Q, ^ -> Q ]] => [(), ()]"
        , ExGlobal
        , ExFormation [BiTau AtPhi ExGlobal, BiTau AtRho ExGlobal]
        , defaultScope
        , substs [[], []]
        )
      ,
        ( "Q.!a => [[ @ -> Q.y, ^ -> [[ a -> Q.w ]], @ -> Q.y ]] => [(a >> y), (a >> w), (a >> y)]"
        , ExDispatch ExGlobal (AtMeta (Just "a"))
        , ExFormation
            [ BiTau AtPhi (ExDispatch ExGlobal (AtLabel "y"))
            , BiTau AtRho (ExFormation [BiTau (AtLabel "a") (ExDispatch ExGlobal (AtLabel "w"))])
            , BiTau AtPhi (ExDispatch ExGlobal (AtLabel "y"))
            ]
        , defaultScope
        , substs [[("a", MvAttribute (AtLabel "y"))], [("a", MvAttribute (AtLabel "w"))], [("a", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "[[!a -> Q.org.!a]] => [[f -> [[x -> Q.org.x]], t -> [[y -> Q.org.y]] => [(!a >> x), (!a >> y)]"
        , ExFormation [BiTau (AtMeta (Just "a")) (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtMeta (Just "a")))]
        , ExFormation
            [ BiTau (AtLabel "f") (ExFormation [BiTau (AtLabel "x") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "x"))])
            , BiTau (AtLabel "t") (ExFormation [BiTau (AtLabel "y") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "y"))])
            ]
        , defaultScope
        , substs [[("a", MvAttribute (AtLabel "x"))], [("a", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "!e => [[x -> Q]] => [(!e >> [[x -> Q]] ), (!e >> Q)]"
        , ExMeta (Just "e")
        , ExFormation [BiTau (AtLabel "x") ExGlobal]
        , defaultScope
        , substs
            [ [("e", MvExpression (ExFormation [BiTau (AtLabel "x") ExGlobal]) defaultScope)]
            , [("e", MvExpression ExGlobal (ExFormation [BiTau (AtLabel "x") ExGlobal]))]
            ]
        )
      ,
        ( "!e.!a => Q.org.eolang => [(!e >> Q.org, !a >> eolang), (!e >> Q, !a >> org)]"
        , ExDispatch (ExMeta (Just "e")) (AtMeta (Just "a"))
        , ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")
        , defaultScope
        , substs
            [ [("e", MvExpression (ExDispatch ExGlobal (AtLabel "org")) defaultScope), ("a", MvAttribute (AtLabel "eolang"))]
            , [("e", MvExpression ExGlobal defaultScope), ("a", MvAttribute (AtLabel "org"))]
            ]
        )
      ,
        ( "⟦!B1, !a ↦ ∅, !B2⟧.!a => ⟦ x ↦ ξ.t, t ↦ ∅ ⟧.t(ρ ↦ ⟦ x ↦ ξ.t, t ↦ ∅ ⟧) => [(!B1 >> ⟦x ↦ ξ.t⟧, !a >> t, !B2 >> ⟦⟧ )]"
        , ExDispatch (ExFormation [BiMeta (Just "B1"), BiVoid (AtMeta (Just "a")), BiMeta (Just "B2")]) (AtMeta (Just "a"))
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
        , substs
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
                    [ BiTau (AtLabel "a") (ExMeta (Just "e0"))
                    , BiTau (AtLabel "b") (ExMeta (Just "e-first"))
                    ]
                )
            , BiTau
                (AtLabel "i2")
                ( ExFormation
                    [ BiTau (AtLabel "a") (ExMeta (Just "e0"))
                    , BiTau (AtLabel "b") (ExMeta (Just "e-second"))
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
        , substs
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
      [ ("~1 => ~1 => [()]", AtAlpha 1, AtAlpha 1, substs [[]])
      , ("!a => ^ => [(!a >> ^)]", AtMeta (Just "a"), AtRho, substs [[("a", MvAttribute AtRho)]])
      , ("!a => @ => [(!a >> @)]", AtMeta (Just "a"), AtPhi, substs [[("a", MvAttribute AtPhi)]])
      , ("~0 => [] => [()]", AtAlpha 0, AtLabel "x", substs [])
      ]
      ( \(desc, ptn, tgt, expected) ->
          it desc $ matchAttribute ptn tgt `shouldBe` expected
      )

  describe "matchBindings: [binding] => [binding] => substitution" $
    test
      matchBindings
      [
        ( "[[]] => [[]] => ()"
        , []
        , []
        , defaultScope
        , substs [[]]
        )
      ,
        ( "[[!B]] => T:[[x -> ?, D> 01-, L> Func]] => (!B >> T)"
        , [BiMeta (Just "B")]
        , [BiVoid (AtLabel "x"), BiDelta (BtOne "01"), BiLambda "Func"]
        , defaultScope
        , substs [[("B", MvBindings [BiVoid (AtLabel "x"), BiDelta (BtOne "01"), BiLambda "Func"])]]
        )
      ,
        ( "[[D> 00-]] => [[D> 00-, L> Func]] => []"
        , [BiDelta (BtOne "00")]
        , [BiDelta (BtOne "00"), BiLambda "Func"]
        , defaultScope
        , substs []
        )
      ,
        ( "[[y -> ?, !a -> ?]] => [[y -> ?, x -> ?]] => (!a >> x)"
        , [BiVoid (AtLabel "y"), BiVoid (AtMeta (Just "a"))]
        , [BiVoid (AtLabel "y"), BiVoid (AtLabel "x")]
        , defaultScope
        , substs [[("a", MvAttribute (AtLabel "x"))]]
        )
      ,
        ( "[[!B, x -> ?]] => [[x -> ?]] => (!B >> [[]])"
        , [BiMeta (Just "B"), BiVoid (AtLabel "x")]
        , [BiVoid (AtLabel "x")]
        , defaultScope
        , substs [[("B", MvBindings [])]]
        )
      ,
        ( "[[!B1, x -> ?, !B2]] => [[x -> ?, y -> ?]] => (!B1 >> [[]], !B2 >> [[y -> ?]])"
        , [BiMeta (Just "B1"), BiVoid (AtLabel "x"), BiMeta (Just "B2")]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")]
        , defaultScope
        , substs [[("B1", MvBindings []), ("B2", MvBindings [BiVoid (AtLabel "y")])]]
        )
      ,
        ( "[[!B1, !x -> ?, !B2]] => [[y -> ?, D> -> 00-, L> Func]] => (!x >> y, !B1 >> [[]], !B2 >> [[D> -> 00-, L> Func]])"
        , [BiMeta (Just "B1"), BiVoid (AtMeta (Just "x")), BiMeta (Just "B2")]
        , [BiVoid (AtLabel "y"), BiDelta (BtOne "00"), BiLambda "Func"]
        , defaultScope
        , substs [[("B1", MvBindings []), ("B2", MvBindings [BiDelta (BtOne "00"), BiLambda "Func"]), ("x", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "[[!x -> ?, !y -> ?]] => [[a -> ?, b -> ?]] => (!x >> a, !y >> b)"
        , [BiVoid (AtMeta (Just "x")), BiVoid (AtMeta (Just "y"))]
        , [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")]
        , defaultScope
        , substs [[("x", MvAttribute (AtLabel "a")), ("y", MvAttribute (AtLabel "b"))]]
        )
      ,
        ( "[[t -> ?, !B]] => [[t -> ?, x -> Q, y -> $]] => (!B >> [[x -> Q, y -> $]])"
        , [BiVoid (AtLabel "t"), BiMeta (Just "B")]
        , [BiVoid (AtLabel "t"), BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis]
        , defaultScope
        , substs [[("B", MvBindings [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis])]]
        )
      ,
        ( "[[!B, z -> Q]] => [[x -> Q, y -> $, z -> Q]] => (!B >> [[x -> Q, y -> $]])"
        , [BiMeta (Just "B"), BiTau (AtLabel "z") ExGlobal]
        , [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis, BiTau (AtLabel "z") ExGlobal]
        , defaultScope
        , substs [[("B", MvBindings [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis])]]
        )
      ,
        ( "[[L> Func, D> 00-]] => [[D> 00-, L> Func]] => []"
        , [BiLambda "Func", BiDelta (BtOne "00")]
        , [BiDelta (BtOne "00"), BiLambda "Func"]
        , defaultScope
        , substs []
        )
      ,
        ( "[[t -> ?, !B]] => [[x -> ?, t -> ?]] => []"
        , [BiVoid (AtLabel "t"), BiMeta (Just "B")]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "t")]
        , defaultScope
        , substs []
        )
      ,
        ( "[[!B, !a -> ?]] => [[x -> ?, y -> ?]] => (!a >> y, !B >> [[ x -> ? ]] )"
        , [BiMeta (Just "B"), BiVoid (AtMeta (Just "a"))]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")]
        , defaultScope
        , substs [[("a", MvAttribute (AtLabel "y")), ("B", MvBindings [BiVoid (AtLabel "x")])]]
        )
      ,
        ( "[[!B1, !a -> ?, !B2]] => [[ x -> ?, y -> ?, z -> ? ]] => [(!B1 >> [[]], !a >> x, !B2 >> [[ y -> ?, z -> ? ]]), (...), (...)]"
        , [BiMeta (Just "B1"), BiVoid (AtMeta (Just "a")), BiMeta (Just "B2")]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y"), BiVoid (AtLabel "z")]
        , defaultScope
        , substs
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
        , [BiMeta (Just "B1"), BiVoid (AtMeta (Just "a1")), BiMeta (Just "B2"), BiVoid (AtMeta (Just "a2")), BiMeta (Just "B3")]
        ,
          [ BiVoid (AtLabel "a")
          , BiVoid (AtLabel "b")
          , BiVoid (AtLabel "x")
          , BiVoid (AtLabel "y")
          , BiVoid (AtLabel "z")
          ]
        , defaultScope
        , substs
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
      matchExpression'
      [ ("$ => $ => [()]", ExThis, ExThis, defaultScope, substs [[]])
      , ("Q => Q => [()]", ExGlobal, ExGlobal, defaultScope, substs [[]])
      ,
        ( "!e => Q => [(!e >> Q)]"
        , ExMeta (Just "e")
        , ExGlobal
        , defaultScope
        , substs [[("e", MvExpression ExGlobal defaultScope)]]
        )
      ,
        ( "!e => Q.org(x -> $) => [(!e >> Q.org(x -> $))]"
        , ExMeta (Just "e")
        , ExApplication (ExDispatch ExGlobal (AtLabel "org")) (BiTau (AtLabel "x") ExThis)
        , defaultScope
        , substs [[("e", MvExpression (ExApplication (ExDispatch ExGlobal (AtLabel "org")) (BiTau (AtLabel "x") ExThis)) defaultScope)]]
        )
      ,
        ( "!e1.x => Q.org.x => [(!e1 >> Q.org)]"
        , ExDispatch (ExMeta (Just "e1")) (AtLabel "x")
        , ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "x")
        , defaultScope
        , substs [[("e1", MvExpression (ExDispatch ExGlobal (AtLabel "org")) defaultScope)]]
        )
      ,
        ( "!e.org.!a => $.org.x => [(!e >> $, !a >> x)]"
        , ExDispatch (ExDispatch (ExMeta (Just "e")) (AtLabel "org")) (AtMeta (Just "a"))
        , ExDispatch (ExDispatch ExThis (AtLabel "org")) (AtLabel "x")
        , defaultScope
        , substs [[("e", MvExpression ExThis defaultScope), ("a", MvAttribute (AtLabel "x"))]]
        )
      ,
        ( "[[!a -> !e, !B]].!a => [[x -> Q, y -> $]].x => [(!a >> x, !e >> Q, !B >> [y -> $])]"
        , ExDispatch (ExFormation [BiTau (AtMeta (Just "a")) (ExMeta (Just "e")), BiMeta (Just "B")]) (AtMeta (Just "a"))
        , ExDispatch
            ( ExFormation
                [ BiTau (AtLabel "x") ExGlobal
                , BiTau (AtLabel "y") ExThis
                ]
            )
            (AtLabel "x")
        , defaultScope
        , substs
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
        , ExMetaTail ExGlobal (Just "t")
        , ExDispatch ExGlobal (AtLabel "x")
        , defaultScope
        , substs [[("t", MvTail [TaDispatch (AtLabel "x")])]]
        )
      ,
        ( "Q * !t => Q.org(x -> [[]]) => [(!t >> [.org, (x -> [[]])])]"
        , ExMetaTail ExGlobal (Just "t")
        , ExApplication (ExDispatch ExGlobal (AtLabel "org")) (BiTau (AtLabel "x") defaultScope)
        , defaultScope
        , substs [[("t", MvTail [TaDispatch (AtLabel "org"), TaApplication (BiTau (AtLabel "x") defaultScope)])]]
        )
      ,
        ( "Q.!a * !t => Q.org.eolang(x -> [[]]) => [(!a >> org, !t >> [ .eolang, ( x -> [[ ]] ) ])]"
        , ExMetaTail (ExDispatch ExGlobal (AtMeta (Just "a"))) (Just "t")
        , ExApplication (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (BiTau (AtLabel "x") defaultScope)
        , defaultScope
        , substs [[("a", MvAttribute (AtLabel "org")), ("t", MvTail [TaDispatch (AtLabel "eolang"), TaApplication (BiTau (AtLabel "x") defaultScope)])]]
        )
      ,
        ( "Q.x(y -> $ * !t1) * !t2 => Q.x(y -> $.q).p => [(!t1 >> [.q], !t2 >> [.p])]"
        , ExMetaTail (ExApplication (ExDispatch ExGlobal (AtLabel "x")) (BiTau (AtLabel "y") (ExMetaTail ExThis (Just "t1")))) (Just "t2")
        , ExDispatch (ExApplication (ExDispatch ExGlobal (AtLabel "x")) (BiTau (AtLabel "y") (ExDispatch ExThis (AtLabel "q")))) (AtLabel "p")
        , defaultScope
        , substs [[("t1", MvTail [TaDispatch (AtLabel "q")]), ("t2", MvTail [TaDispatch (AtLabel "p")])]]
        )
      ,
        ( "[[!B1, !a ↦ !e1, !B2]](!a ↦ !e2) => ⟦ t ↦ ξ.k, x ↦ ξ.t, k ↦ ∅ ⟧(x ↦ ξ) => [(!B1 >> [[ t -> $.k ]], !a >> x, !B2 >> [[ k -> ? ]], !e1 >> $.t, !e2 >> $)]"
        , ExApplication (ExFormation [BiMeta (Just "B1"), BiTau (AtMeta (Just "a")) (ExMeta (Just "e1")), BiMeta (Just "B2")]) (BiTau (AtMeta (Just "a")) (ExMeta (Just "e2")))
        , ExApplication
            ( ExFormation
                [ BiTau (AtLabel "t") (ExDispatch ExThis (AtLabel "k"))
                , BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                , BiVoid (AtLabel "k")
                ]
            )
            (BiTau (AtLabel "x") ExThis)
        , defaultScope
        , substs
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
      let Subst joined = maybeCombined substEmpty (Subst (Map.singleton "at" [MvAttribute AtPhi]))
      Map.lookup "at" joined `shouldBe` Just [MvAttribute AtPhi]
    it "combines two different subst" $ do
      let Subst joined =
            maybeCombined
              (Subst (Map.singleton "first" [MvAttribute AtPhi]))
              (Subst (Map.singleton "second" [MvBytes (BtOne "00")]))
      Map.lookup "first" joined `shouldBe` Just [MvAttribute AtPhi]
      Map.lookup "second" joined `shouldBe` Just [MvBytes (BtOne "00")]
    it "leave values in the same substs" $ do
      let rho = MvAttribute AtRho
          first =
            Subst
              ( Map.fromList
                  [ ("first", [rho])
                  , ("second", [MvAttribute AtPhi])
                  ]
              )
          second = Subst (Map.singleton "first" [rho])
          Subst joined = maybeCombined first second
      Map.lookup "first" joined `shouldBe` Just [MvAttribute AtRho]
    it "returns Nothing if values are different" $
      combine (Subst (Map.singleton "x" [MvAttribute AtPhi])) (Subst (Map.singleton "x" [MvAttribute AtRho])) `shouldBe` Nothing
    it "clears all the values" $ do
      let first =
            Subst
              ( Map.fromList
                  [ ("x", [MvAttribute AtRho])
                  , ("y", [MvBytes (BtOne "1F")])
                  ]
              )
          second = Subst (Map.singleton "x" [MvAttribute AtPhi])
      combine first second `shouldBe` Nothing

  describe "anonymous meta variables (bare 𝜏 / 𝐵 / 𝑒, no index)" $ do
    it "anonymous attribute parses as AtMeta Nothing" $ do
      ptn <- parseExpressionThrows "Q.𝜏"
      case ptn of
        ExDispatch _ (AtMeta Nothing) -> pure ()
        _ -> error ("expected AtMeta Nothing, got " ++ show ptn)
    it "two bare 𝜏 in a pattern match without conflict" $ do
      ptn <- parseExpressionThrows "⟦𝜏 ↦ Q, 𝜏 ↦ $, 𝐵⟧"
      tgt <- parseExpressionThrows "⟦x ↦ Q, y ↦ $⟧"
      matchExpression ptn tgt `shouldSatisfy` (not . null)
    it "named 𝜏1 used twice still requires equality" $ do
      ptn <- parseExpressionThrows "⟦𝜏1 ↦ 𝑒1, 𝐵⟧.𝜏1"
      consistent <- parseExpressionThrows "⟦x ↦ Q⟧.x"
      conflicting <- parseExpressionThrows "⟦x ↦ Q⟧.y"
      matchExpression ptn consistent `shouldSatisfy` (not . null)
      matchExpression ptn conflicting `shouldBe` []
