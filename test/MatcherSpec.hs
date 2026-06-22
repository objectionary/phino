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
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

substs :: [[(T.Text, MetaValue)]] -> [Subst]
substs = map (Subst . Map.fromList)

maybeCombined :: Subst -> Subst -> Subst
maybeCombined first second =
  fromMaybe
    (error "combine returned Nothing")
    (combine first second)

test ::
  (a -> a -> [Subst]) ->
  [(String, a, a, [Subst])] ->
  SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, ptn, tgt, expected) ->
    it desc $ function ptn tgt `shouldBe` expected

spec :: Spec
spec = do
  describe "matchExpressionDeep: expression => expression => [substitution]" $
    test
      matchExpressionDeep
      [
        ( "Q => [[ @ -> Q, ^ -> Q ]] => [(), ()]"
        , ExRoot
        , ExFormation [BiTau AtPhi ExRoot, BiTau AtRho ExRoot]
        , substs [[], []]
        )
      ,
        ( "Q.!a => [[ @ -> Q.y, ^ -> [[ a -> Q.w ]], @ -> Q.y ]] => [(a >> y), (a >> w), (a >> y)]"
        , ExDispatch ExRoot (AtMeta "a")
        , ExFormation
            [ BiTau AtPhi (ExDispatch ExRoot (AtLabel "y"))
            , BiTau AtRho (ExFormation [BiTau (AtLabel "a") (ExDispatch ExRoot (AtLabel "w"))])
            , BiTau AtPhi (ExDispatch ExRoot (AtLabel "y"))
            ]
        , substs [[("a", MvAttribute (AtLabel "y"))], [("a", MvAttribute (AtLabel "w"))], [("a", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "[[!a -> Q.org.!a]] => [[f -> [[x -> Q.org.x]], t -> [[y -> Q.org.y]] => [(!a >> x), (!a >> y)]"
        , ExFormation [BiTau (AtMeta "a") (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtMeta "a"))]
        , ExFormation
            [ BiTau (AtLabel "f") (ExFormation [BiTau (AtLabel "x") (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "x"))])
            , BiTau (AtLabel "t") (ExFormation [BiTau (AtLabel "y") (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "y"))])
            ]
        , substs [[("a", MvAttribute (AtLabel "x"))], [("a", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "!e => [[x -> Q]] => [(!e >> [[x -> Q]] ), (!e >> Q)]"
        , ExMeta "e"
        , ExFormation [BiTau (AtLabel "x") ExRoot]
        , substs
            [ [("e", MvExpression (ExFormation [BiTau (AtLabel "x") ExRoot]))]
            , [("e", MvExpression ExRoot)]
            ]
        )
      ,
        ( "!e.!a => Q.org.eolang => [(!e >> Q.org, !a >> eolang), (!e >> Q, !a >> org)]"
        , ExDispatch (ExMeta "e") (AtMeta "a")
        , ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")
        , substs
            [ [("e", MvExpression (ExDispatch ExRoot (AtLabel "org"))), ("a", MvAttribute (AtLabel "eolang"))]
            , [("e", MvExpression ExRoot), ("a", MvAttribute (AtLabel "org"))]
            ]
        )
      ,
        ( "⟦!B1, !a ↦ ∅, !B2⟧.!a => ⟦ x ↦ ξ.t, t ↦ ∅ ⟧.t(ρ ↦ ⟦ x ↦ ξ.t, t ↦ ∅ ⟧) => [(!B1 >> ⟦x ↦ ξ.t⟧, !a >> t, !B2 >> ⟦⟧ )]"
        , ExDispatch (ExFormation [BiMeta "B1", BiVoid (AtMeta "a"), BiMeta "B2"]) (AtMeta "a")
        , ExApplication
            ( ExDispatch
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                    , BiVoid (AtLabel "t")
                    ]
                )
                (AtLabel "t")
            )
            ( ArTau
                AtRho
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                    , BiVoid (AtLabel "t")
                    ]
                )
            )
        , substs
            [
              [ ("B1", MvBindings [BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))])
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
                    [ BiTau (AtLabel "a") ExRoot
                    , BiTau (AtLabel "b") ExXi
                    ]
                )
            , BiTau
                (AtLabel "i2")
                ( ExFormation
                    [ BiTau (AtLabel "a") ExRoot
                    , BiTau (AtLabel "b") (ExFormation [BiVoid AtPhi])
                    ]
                )
            ]
        , substs
            [
              [ ("e0", MvExpression ExRoot)
              , ("e-first", MvExpression ExXi)
              , ("e-second", MvExpression (ExFormation [BiVoid AtPhi]))
              ]
            ]
        )
      ]

  describe "matchAttribute: attribute => attribute => substitution" $
    forM_
      [ ("!a => ^ => [(!a >> ^)]", AtMeta "a", AtRho, substs [[("a", MvAttribute AtRho)]])
      , ("!a => @ => [(!a >> @)]", AtMeta "a", AtPhi, substs [[("a", MvAttribute AtPhi)]])
      ]
      ( \(desc, ptn, tgt, expected) ->
          it desc $ matchAttribute ptn tgt `shouldBe` expected
      )

  describe "matchAlpha: alpha => alpha => substitution" $
    forM_
      [ ("α1 => α1 => [()]", Alpha 1, Alpha 1, substs [[]])
      , ("α0 => α1 => []", Alpha 0, Alpha 1, substs [])
      ]
      ( \(desc, ptn, tgt, expected) ->
          it desc $ matchAlpha ptn tgt `shouldBe` expected
      )

  describe "matchBindings: [binding] => [binding] => substitution" $
    test
      matchBindings
      [
        ( "[[]] => [[]] => ()"
        , []
        , []
        , substs [[]]
        )
      ,
        ( "[[!B]] => T:[[x -> ?, D> 01-, L> Func]] => (!B >> T)"
        , [BiMeta "B"]
        , [BiVoid (AtLabel "x"), BiDelta (BtOne "01"), BiLambda (Function "Func")]
        , substs [[("B", MvBindings [BiVoid (AtLabel "x"), BiDelta (BtOne "01"), BiLambda (Function "Func")])]]
        )
      ,
        ( "[[D> 00-]] => [[D> 00-, L> Func]] => []"
        , [BiDelta (BtOne "00")]
        , [BiDelta (BtOne "00"), BiLambda (Function "Func")]
        , substs []
        )
      ,
        ( "[[y -> ?, !a -> ?]] => [[y -> ?, x -> ?]] => (!a >> x)"
        , [BiVoid (AtLabel "y"), BiVoid (AtMeta "a")]
        , [BiVoid (AtLabel "y"), BiVoid (AtLabel "x")]
        , substs [[("a", MvAttribute (AtLabel "x"))]]
        )
      ,
        ( "[[!B, x -> ?]] => [[x -> ?]] => (!B >> [[]])"
        , [BiMeta "B", BiVoid (AtLabel "x")]
        , [BiVoid (AtLabel "x")]
        , substs [[("B", MvBindings [])]]
        )
      ,
        ( "[[!B1, x -> ?, !B2]] => [[x -> ?, y -> ?]] => (!B1 >> [[]], !B2 >> [[y -> ?]])"
        , [BiMeta "B1", BiVoid (AtLabel "x"), BiMeta "B2"]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")]
        , substs [[("B1", MvBindings []), ("B2", MvBindings [BiVoid (AtLabel "y")])]]
        )
      ,
        ( "[[!B1, !x -> ?, !B2]] => [[y -> ?, D> -> 00-, L> Func]] => (!x >> y, !B1 >> [[]], !B2 >> [[D> -> 00-, L> Func]])"
        , [BiMeta "B1", BiVoid (AtMeta "x"), BiMeta "B2"]
        , [BiVoid (AtLabel "y"), BiDelta (BtOne "00"), BiLambda (Function "Func")]
        , substs [[("B1", MvBindings []), ("B2", MvBindings [BiDelta (BtOne "00"), BiLambda (Function "Func")]), ("x", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "[[!x -> ?, !y -> ?]] => [[a -> ?, b -> ?]] => (!x >> a, !y >> b)"
        , [BiVoid (AtMeta "x"), BiVoid (AtMeta "y")]
        , [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")]
        , substs [[("x", MvAttribute (AtLabel "a")), ("y", MvAttribute (AtLabel "b"))]]
        )
      ,
        ( "[[t -> ?, !B]] => [[t -> ?, x -> Q, y -> $]] => (!B >> [[x -> Q, y -> $]])"
        , [BiVoid (AtLabel "t"), BiMeta "B"]
        , [BiVoid (AtLabel "t"), BiTau (AtLabel "x") ExRoot, BiTau (AtLabel "y") ExXi]
        , substs [[("B", MvBindings [BiTau (AtLabel "x") ExRoot, BiTau (AtLabel "y") ExXi])]]
        )
      ,
        ( "[[!B, z -> Q]] => [[x -> Q, y -> $, z -> Q]] => (!B >> [[x -> Q, y -> $]])"
        , [BiMeta "B", BiTau (AtLabel "z") ExRoot]
        , [BiTau (AtLabel "x") ExRoot, BiTau (AtLabel "y") ExXi, BiTau (AtLabel "z") ExRoot]
        , substs [[("B", MvBindings [BiTau (AtLabel "x") ExRoot, BiTau (AtLabel "y") ExXi])]]
        )
      ,
        ( "[[L> Func, D> 00-]] => [[D> 00-, L> Func]] => []"
        , [BiLambda (Function "Func"), BiDelta (BtOne "00")]
        , [BiDelta (BtOne "00"), BiLambda (Function "Func")]
        , substs []
        )
      ,
        ( "[[t -> ?, !B]] => [[x -> ?, t -> ?]] => []"
        , [BiVoid (AtLabel "t"), BiMeta "B"]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "t")]
        , substs []
        )
      ,
        ( "[[!B, !a -> ?]] => [[x -> ?, y -> ?]] => (!a >> y, !B >> [[ x -> ? ]] )"
        , [BiMeta "B", BiVoid (AtMeta "a")]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")]
        , substs [[("a", MvAttribute (AtLabel "y")), ("B", MvBindings [BiVoid (AtLabel "x")])]]
        )
      ,
        ( "[[!B1, !a -> ?, !B2]] => [[ x -> ?, y -> ?, z -> ? ]] => [(!B1 >> [[]], !a >> x, !B2 >> [[ y -> ?, z -> ? ]]), (...), (...)]"
        , [BiMeta "B1", BiVoid (AtMeta "a"), BiMeta "B2"]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y"), BiVoid (AtLabel "z")]
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
        , [BiMeta "B1", BiVoid (AtMeta "a1"), BiMeta "B2", BiVoid (AtMeta "a2"), BiMeta "B3"]
        ,
          [ BiVoid (AtLabel "a")
          , BiVoid (AtLabel "b")
          , BiVoid (AtLabel "x")
          , BiVoid (AtLabel "y")
          , BiVoid (AtLabel "z")
          ]
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
      [ ("$ => $ => [()]", ExXi, ExXi, substs [[]])
      , ("Q => Q => [()]", ExRoot, ExRoot, substs [[]])
      ,
        ( "!e => Q => [(!e >> Q)]"
        , ExMeta "e"
        , ExRoot
        , substs [[("e", MvExpression ExRoot)]]
        )
      ,
        ( "!e => Q.org(x -> $) => [(!e >> Q.org(x -> $))]"
        , ExMeta "e"
        , ExApplication (ExDispatch ExRoot (AtLabel "org")) (ArTau (AtLabel "x") ExXi)
        , substs [[("e", MvExpression (ExApplication (ExDispatch ExRoot (AtLabel "org")) (ArTau (AtLabel "x") ExXi)))]]
        )
      ,
        ( "!e1.x => Q.org.x => [(!e1 >> Q.org)]"
        , ExDispatch (ExMeta "e1") (AtLabel "x")
        , ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "x")
        , substs [[("e1", MvExpression (ExDispatch ExRoot (AtLabel "org")))]]
        )
      ,
        ( "!e.org.!a => $.org.x => [(!e >> $, !a >> x)]"
        , ExDispatch (ExDispatch (ExMeta "e") (AtLabel "org")) (AtMeta "a")
        , ExDispatch (ExDispatch ExXi (AtLabel "org")) (AtLabel "x")
        , substs [[("e", MvExpression ExXi), ("a", MvAttribute (AtLabel "x"))]]
        )
      ,
        ( "[[!a -> !e, !B]].!a => [[x -> Q, y -> $]].x => [(!a >> x, !e >> Q, !B >> [y -> $])]"
        , ExDispatch (ExFormation [BiTau (AtMeta "a") (ExMeta "e"), BiMeta "B"]) (AtMeta "a")
        , ExDispatch
            ( ExFormation
                [ BiTau (AtLabel "x") ExRoot
                , BiTau (AtLabel "y") ExXi
                ]
            )
            (AtLabel "x")
        , substs
            [
              [ ("a", MvAttribute (AtLabel "x"))
              , ("e", MvExpression ExRoot)
              , ("B", MvBindings [BiTau (AtLabel "y") ExXi])
              ]
            ]
        )
      ,
        ( "[[!B1, !a ↦ !e1, !B2]](!a ↦ !e2) => ⟦ t ↦ ξ.k, x ↦ ξ.t, k ↦ ∅ ⟧(x ↦ ξ) => [(!B1 >> [[ t -> $.k ]], !a >> x, !B2 >> [[ k -> ? ]], !e1 >> $.t, !e2 >> $)]"
        , ExApplication (ExFormation [BiMeta "B1", BiTau (AtMeta "a") (ExMeta "e1"), BiMeta "B2"]) (ArTau (AtMeta "a") (ExMeta "e2"))
        , ExApplication
            ( ExFormation
                [ BiTau (AtLabel "t") (ExDispatch ExXi (AtLabel "k"))
                , BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                , BiVoid (AtLabel "k")
                ]
            )
            (ArTau (AtLabel "x") ExXi)
        , substs
            [
              [ ("B1", MvBindings [BiTau (AtLabel "t") (ExDispatch ExXi (AtLabel "k"))])
              , ("a", MvAttribute (AtLabel "x"))
              , ("B2", MvBindings [BiVoid (AtLabel "k")])
              , ("e1", MvExpression (ExDispatch ExXi (AtLabel "t")))
              , ("e2", MvExpression ExXi)
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
