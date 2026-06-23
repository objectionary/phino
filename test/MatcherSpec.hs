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
        ( "Q.!t => [[ @ -> Q.y, ^ -> [[ a -> Q.w ]], @ -> Q.y ]] => [(t >> y), (t >> w), (t >> y)]"
        , ExDispatch ExRoot (AtMeta "t")
        , ExFormation
            [ BiTau AtPhi (ExDispatch ExRoot (AtLabel "y"))
            , BiTau AtRho (ExFormation [BiTau (AtLabel "a") (ExDispatch ExRoot (AtLabel "w"))])
            , BiTau AtPhi (ExDispatch ExRoot (AtLabel "y"))
            ]
        , substs [[("t", MvAttribute (AtLabel "y"))], [("t", MvAttribute (AtLabel "w"))], [("t", MvAttribute (AtLabel "y"))]]
        )
      ,
        ( "[[!t -> Q.org.!t]] => [[f -> [[x -> Q.org.x]], t -> [[y -> Q.org.y]] => [(!t >> x), (!t >> y)]"
        , ExFormation [BiTau (AtMeta "t") (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtMeta "t"))]
        , ExFormation
            [ BiTau (AtLabel "f") (ExFormation [BiTau (AtLabel "x") (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "x"))])
            , BiTau (AtLabel "t") (ExFormation [BiTau (AtLabel "y") (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "y"))])
            ]
        , substs [[("t", MvAttribute (AtLabel "x"))], [("t", MvAttribute (AtLabel "y"))]]
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
        ( "!e.!t => Q.org.eolang => [(!e >> Q.org, !t >> eolang), (!e >> Q, !t >> org)]"
        , ExDispatch (ExMeta "e") (AtMeta "t")
        , ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")
        , substs
            [ [("e", MvExpression (ExDispatch ExRoot (AtLabel "org"))), ("t", MvAttribute (AtLabel "eolang"))]
            , [("e", MvExpression ExRoot), ("t", MvAttribute (AtLabel "org"))]
            ]
        )
      ,
        ( "⟦!B1, !t ↦ ∅, !B2⟧.!t => ⟦ x ↦ ξ.t, t ↦ ∅ ⟧.t(ρ ↦ ⟦ x ↦ ξ.t, t ↦ ∅ ⟧) => [(!B1 >> ⟦x ↦ ξ.t⟧, !t >> t, !B2 >> ⟦⟧ )]"
        , ExDispatch (ExFormation [BiMeta "B1", BiVoid (AtMeta "t"), BiMeta "B2"]) (AtMeta "t")
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
              , ("t", MvAttribute (AtLabel "t"))
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
      [ ("!t => ^ => [(!t >> ^)]", AtMeta "t", AtRho, substs [[("t", MvAttribute AtRho)]])
      , ("!t => @ => [(!t >> @)]", AtMeta "t", AtPhi, substs [[("t", MvAttribute AtPhi)]])
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
        ( "[[y -> ?, !t -> ?]] => [[y -> ?, x -> ?]] => (!t >> x)"
        , [BiVoid (AtLabel "y"), BiVoid (AtMeta "t")]
        , [BiVoid (AtLabel "y"), BiVoid (AtLabel "x")]
        , substs [[("t", MvAttribute (AtLabel "x"))]]
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
        ( "[[!B, !t -> ?]] => [[x -> ?, y -> ?]] => (!t >> y, !B >> [[ x -> ? ]] )"
        , [BiMeta "B", BiVoid (AtMeta "t")]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")]
        , substs [[("t", MvAttribute (AtLabel "y")), ("B", MvBindings [BiVoid (AtLabel "x")])]]
        )
      ,
        ( "[[!B1, !t -> ?, !B2]] => [[ x -> ?, y -> ?, z -> ? ]] => [(!B1 >> [[]], !t >> x, !B2 >> [[ y -> ?, z -> ? ]]), (...), (...)]"
        , [BiMeta "B1", BiVoid (AtMeta "t"), BiMeta "B2"]
        , [BiVoid (AtLabel "x"), BiVoid (AtLabel "y"), BiVoid (AtLabel "z")]
        , substs
            [
              [ ("B1", MvBindings [])
              , ("t", MvAttribute (AtLabel "x"))
              , ("B2", MvBindings [BiVoid (AtLabel "y"), BiVoid (AtLabel "z")])
              ]
            ,
              [ ("B1", MvBindings [BiVoid (AtLabel "x")])
              , ("t", MvAttribute (AtLabel "y"))
              , ("B2", MvBindings [BiVoid (AtLabel "z")])
              ]
            ,
              [ ("B1", MvBindings [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")])
              , ("t", MvAttribute (AtLabel "z"))
              , ("B2", MvBindings [])
              ]
            ]
        )
      ,
        ( "[[!B1, !t1 -> ?, !B2, !t2 -> ?, !B3]] => [[ a -> ?, b -> ?, x -> ?, y -> ?, z -> ? ]] => [10 substs]"
        , [BiMeta "B1", BiVoid (AtMeta "t1"), BiMeta "B2", BiVoid (AtMeta "t2"), BiMeta "B3"]
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
              , ("t1", MvAttribute (AtLabel "a"))
              , ("B2", MvBindings [])
              , ("t2", MvAttribute (AtLabel "b"))
              , ("B3", MvBindings [BiVoid (AtLabel "x"), BiVoid (AtLabel "y"), BiVoid (AtLabel "z")])
              ]
            ,
              [ ("B1", MvBindings [])
              , ("t1", MvAttribute (AtLabel "a"))
              , ("B2", MvBindings [BiVoid (AtLabel "b")])
              , ("t2", MvAttribute (AtLabel "x"))
              , ("B3", MvBindings [BiVoid (AtLabel "y"), BiVoid (AtLabel "z")])
              ]
            ,
              [ ("B1", MvBindings [])
              , ("t1", MvAttribute (AtLabel "a"))
              , ("B2", MvBindings [BiVoid (AtLabel "b"), BiVoid (AtLabel "x")])
              , ("t2", MvAttribute (AtLabel "y"))
              , ("B3", MvBindings [BiVoid (AtLabel "z")])
              ]
            ,
              [ ("B1", MvBindings [])
              , ("t1", MvAttribute (AtLabel "a"))
              , ("B2", MvBindings [BiVoid (AtLabel "b"), BiVoid (AtLabel "x"), BiVoid (AtLabel "y")])
              , ("t2", MvAttribute (AtLabel "z"))
              , ("B3", MvBindings [])
              ]
            ,
              [ ("B1", MvBindings [BiVoid (AtLabel "a")])
              , ("t1", MvAttribute (AtLabel "b"))
              , ("B2", MvBindings [])
              , ("t2", MvAttribute (AtLabel "x"))
              , ("B3", MvBindings [BiVoid (AtLabel "y"), BiVoid (AtLabel "z")])
              ]
            ,
              [ ("B1", MvBindings [BiVoid (AtLabel "a")])
              , ("t1", MvAttribute (AtLabel "b"))
              , ("B2", MvBindings [BiVoid (AtLabel "x")])
              , ("t2", MvAttribute (AtLabel "y"))
              , ("B3", MvBindings [BiVoid (AtLabel "z")])
              ]
            ,
              [ ("B1", MvBindings [BiVoid (AtLabel "a")])
              , ("t1", MvAttribute (AtLabel "b"))
              , ("B2", MvBindings [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")])
              , ("t2", MvAttribute (AtLabel "z"))
              , ("B3", MvBindings [])
              ]
            ,
              [ ("B1", MvBindings [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")])
              , ("t1", MvAttribute (AtLabel "x"))
              , ("B2", MvBindings [])
              , ("t2", MvAttribute (AtLabel "y"))
              , ("B3", MvBindings [BiVoid (AtLabel "z")])
              ]
            ,
              [ ("B1", MvBindings [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")])
              , ("t1", MvAttribute (AtLabel "x"))
              , ("B2", MvBindings [BiVoid (AtLabel "y")])
              , ("t2", MvAttribute (AtLabel "z"))
              , ("B3", MvBindings [])
              ]
            ,
              [ ("B1", MvBindings [BiVoid (AtLabel "a"), BiVoid (AtLabel "b"), BiVoid (AtLabel "x")])
              , ("t1", MvAttribute (AtLabel "y"))
              , ("B2", MvBindings [])
              , ("t2", MvAttribute (AtLabel "z"))
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
        ( "!e.org.!t => $.org.x => [(!e >> $, !t >> x)]"
        , ExDispatch (ExDispatch (ExMeta "e") (AtLabel "org")) (AtMeta "t")
        , ExDispatch (ExDispatch ExXi (AtLabel "org")) (AtLabel "x")
        , substs [[("e", MvExpression ExXi), ("t", MvAttribute (AtLabel "x"))]]
        )
      ,
        ( "[[!t -> !e, !B]].!t => [[x -> Q, y -> $]].x => [(!t >> x, !e >> Q, !B >> [y -> $])]"
        , ExDispatch (ExFormation [BiTau (AtMeta "t") (ExMeta "e"), BiMeta "B"]) (AtMeta "t")
        , ExDispatch
            ( ExFormation
                [ BiTau (AtLabel "x") ExRoot
                , BiTau (AtLabel "y") ExXi
                ]
            )
            (AtLabel "x")
        , substs
            [
              [ ("t", MvAttribute (AtLabel "x"))
              , ("e", MvExpression ExRoot)
              , ("B", MvBindings [BiTau (AtLabel "y") ExXi])
              ]
            ]
        )
      ,
        ( "[[!B1, !t ↦ !e1, !B2]](!t ↦ !e2) => ⟦ t ↦ ξ.k, x ↦ ξ.t, k ↦ ∅ ⟧(x ↦ ξ) => [(!B1 >> [[ t -> $.k ]], !t >> x, !B2 >> [[ k -> ? ]], !e1 >> $.t, !e2 >> $)]"
        , ExApplication (ExFormation [BiMeta "B1", BiTau (AtMeta "t") (ExMeta "e1"), BiMeta "B2"]) (ArTau (AtMeta "t") (ExMeta "e2"))
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
              , ("t", MvAttribute (AtLabel "x"))
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
