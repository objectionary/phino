{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MatcherSpec where

import Ast
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Matcher
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

class Expected e where
  type ExpectedResult e
  toExpected :: e -> ExpectedResult e

instance Expected (Maybe [(String, MetaValue)]) where
  type ExpectedResult (Maybe [(String, MetaValue)]) = Maybe Subst
  toExpected = fmap (Subst . Map.fromList)

instance Expected ([Binding], [(String, MetaValue)]) where
  type ExpectedResult ([Binding], [(String, MetaValue)]) = ([Binding], Maybe Subst)
  toExpected (rest, pairs) = (rest, Just (Subst (Map.fromList pairs)))

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
  (a -> a -> r) ->
  [(String, a, a, e)] ->
  SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, ptn, tgt, mp) ->
    it desc $ function ptn tgt `shouldBe` toExpected mp

spec :: Spec
spec = do
  describe "matchExpressionDeep: expression => expression => [substitution]" $
    test
      matchExpressionDeep
      [ ( "[[!a -> Q.org.!a]] => [[f -> [[x -> Q.org.x]], t -> [[y -> Q.org.y]] => [(!a >> x), (!a >> y)]",
          ExFormation [BiTau (AtMeta "a") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtMeta "a"))],
          ExFormation
            [ BiTau (AtLabel "f") (ExFormation [BiTau (AtLabel "x") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "x"))]),
              BiTau (AtLabel "t") (ExFormation [BiTau (AtLabel "y") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "y"))])
            ],
          [[("a", MvAttribute (AtLabel "x"))], [("a", MvAttribute (AtLabel "y"))]]
        ),
        ( "!e => [[x -> Q]] => [(!e >> [[x -> Q]]), (!e >> Q)]",
          ExMeta "e",
          ExFormation [BiTau (AtLabel "x") ExGlobal],
          [[("e", MvExpression (ExFormation [BiTau (AtLabel "x") ExGlobal]))], [("e", MvExpression ExGlobal)]]
        ),
        ( "!e.!a => Q.org.eolang => [(!e >> Q.org, !a >> eolang), (!e >> Q, !a >> org)]",
          ExDispatch (ExMeta "e") (AtMeta "a"),
          ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"),
          [ [("e", MvExpression (ExDispatch ExGlobal (AtLabel "org"))), ("a", MvAttribute (AtLabel "eolang"))],
            [("e", MvExpression ExGlobal), ("a", MvAttribute (AtLabel "org"))]
          ]
        )
      ]

  describe "matchAttribute: attribute => attribute => substitution" $
    test
      matchAttribute
      [ ("~1 => ~1 => ()", AtAlpha 1, AtAlpha 1, Just []),
        ("!a => ^ => (!a >> ^)", AtMeta "a", AtRho, Just [("a", MvAttribute AtRho)]),
        ("!a => @ => (!a >> @)", AtMeta "a", AtPhi, Just [("a", MvAttribute AtPhi)]),
        ("~0 => x => X", AtAlpha 0, AtLabel "x", Nothing)
      ]

  describe "matchBindings: [binding] => [binding] => substitution" $
    test
      matchBindings
      [ ( "[[]] => [[]] => ()",
          [],
          [],
          Just []
        ),
        ( "[[!B]] => T:[[x -> ?, D> 01-, L> Func]] => (!B >> T)",
          [BiMeta "B"],
          [BiVoid (AtLabel "x"), BiDelta "01-", BiLambda "Func"],
          Just [("B", MvBindings [BiVoid (AtLabel "x"), BiDelta "01-", BiLambda "Func"])]
        ),
        ( "[[D> 00-]] => [[D> 00-, L> Func]] => X",
          [BiDelta "00-"],
          [BiDelta "00-", BiLambda "Func"],
          Nothing
        ),
        ( "[[y -> ?, !a -> ?]] => [[y -> ?, x -> ?]] => (!a >> x)",
          [BiVoid (AtLabel "y"), BiVoid (AtMeta "a")],
          [BiVoid (AtLabel "y"), BiVoid (AtLabel "x")],
          Just [("a", MvAttribute (AtLabel "x"))]
        ),
        ( "[[!B, x -> ?]] => [[x -> ?]] => (!B >> [[]])",
          [BiMeta "B", BiVoid (AtLabel "x")],
          [BiVoid (AtLabel "x")],
          Just [("B", MvBindings [])]
        ),
        ( "[[!B1, x -> ?, !B2]] => [[x -> ?, y -> ?]] => (!B1 >> [[]], !B2 >> [[y -> ?]])",
          [BiMeta "B1", BiVoid (AtLabel "x"), BiMeta "B2"],
          [BiVoid (AtLabel "x"), BiVoid (AtLabel "y")],
          Just [("B1", MvBindings []), ("B2", MvBindings [BiVoid (AtLabel "y")])]
        ),
        ( "[[!B1, !x -> ?, !B2]] => [[y -> ?, D> -> 00-, L> Func]] => (!x >> y, !B1 >> [[]], !B2 >> [[D> -> 00-, L> Func]])",
          [BiMeta "B1", BiVoid (AtMeta "x"), BiMeta "B2"],
          [BiVoid (AtLabel "y"), BiDelta "00-", BiLambda "Func"],
          Just [("B1", MvBindings []), ("B2", MvBindings [BiDelta "00-", BiLambda "Func"]), ("x", MvAttribute (AtLabel "y"))]
        ),
        ( "[[!x -> ?, !y -> ?]] => [[a -> ?, b -> ?]] => (!x >> a, !y >> b)",
          [BiVoid (AtMeta "x"), BiVoid (AtMeta "y")],
          [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")],
          Just [("x", MvAttribute (AtLabel "a")), ("y", MvAttribute (AtLabel "b"))]
        ),
        ( "[[t -> ?, !B]] => [[t -> ?, x -> Q, y -> $]] => (!B >> [[x -> Q, y -> $]])",
          [BiVoid (AtLabel "t"), BiMeta "B"],
          [BiVoid (AtLabel "t"), BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis],
          Just [("B", MvBindings [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis])]
        ),
        ( "[[!B, z -> Q]] => [[x -> Q, y -> $, z -> Q]] => (!B >> [[x -> Q, y -> $]])",
          [BiMeta "B", BiTau (AtLabel "z") ExGlobal],
          [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis, BiTau (AtLabel "z") ExGlobal],
          Just [("B", MvBindings [BiTau (AtLabel "x") ExGlobal, BiTau (AtLabel "y") ExThis])]
        ),
        ( "[[L> Func, D> 00-]] => [[D> 00-, L> Func]] => X",
          [BiLambda "Func", BiDelta "00-"],
          [BiDelta "00-", BiLambda "Func"],
          Nothing
        ),
        ("[[t -> ?, !B]] => [[x -> ?, t -> ?]] => X",
          [BiVoid (AtLabel "t"), BiMeta "B"],
          [BiVoid (AtLabel "x"), BiVoid (AtLabel "t")],
          Nothing
        )
      ]

  describe "matchExpression: expression => pattern => substitution" $
    test
      matchExpression
      [ ("$ => $ => ()", ExThis, ExThis, Just []),
        ("Q => Q => ()", ExGlobal, ExGlobal, Just []),
        ( "!e => Q => (!e >> Q)",
          ExMeta "e",
          ExGlobal,
          Just [("e", MvExpression ExGlobal)]
        ),
        ( "!e => Q.org(x -> $) => (!e >> Q.org(x -> $))",
          ExMeta "e",
          ExApplication (ExDispatch ExGlobal (AtLabel "org")) [BiTau (AtLabel "x") ExThis],
          Just [("e", MvExpression (ExApplication (ExDispatch ExGlobal (AtLabel "org")) [BiTau (AtLabel "x") ExThis]))]
        ),
        ( "!e1.x => Q.org.x => (!e1 >> Q.org)",
          ExDispatch (ExMeta "e1") (AtLabel "x"),
          ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "x"),
          Just [("e1", MvExpression (ExDispatch ExGlobal (AtLabel "org")))]
        ),
        ( "!e.org.!a => $.org.x => (!e >> $, !a >> x)",
          ExDispatch (ExDispatch (ExMeta "e") (AtLabel "org")) (AtMeta "a"),
          ExDispatch (ExDispatch ExThis (AtLabel "org")) (AtLabel "x"),
          Just [("e", MvExpression ExThis), ("a", MvAttribute (AtLabel "x"))]
        ),
        ( "[[!a -> !e, !B]].!a => [[x -> Q, y -> $]].x => (!a >> x, !e >> Q, !B >> [y -> $])",
          ExDispatch (ExFormation [BiTau (AtMeta "a") (ExMeta "e"), BiMeta "B"]) (AtMeta "a"),
          ExDispatch
            ( ExFormation
                [ BiTau (AtLabel "x") ExGlobal,
                  BiTau (AtLabel "y") ExThis
                ]
            )
            (AtLabel "x"),
          Just
            [ ("a", MvAttribute (AtLabel "x")),
              ("e", MvExpression ExGlobal),
              ("B", MvBindings [BiTau (AtLabel "y") ExThis])
            ]
        ),
        ( "Q * !t => Q.org => (!t >> [.org])",
          ExMetaTail ExGlobal "t",
          ExDispatch ExGlobal (AtLabel "x"),
          Just [("t", MvTail [TaDispatch (AtLabel "x")])]
        ),
        ( "Q * !t => Q.org(x -> [[]]) => (!t >> [.org, (x -> [[]])])",
          ExMetaTail ExGlobal "t",
          ExApplication (ExDispatch ExGlobal (AtLabel "org")) [BiTau (AtLabel "x") (ExFormation [])],
          Just [("t", MvTail [TaDispatch (AtLabel "org"), TaApplication [BiTau (AtLabel "x") (ExFormation [])]])]
        ),
        ( "Q.!a * !t => Q.org(x -> [[]]) => (!a >> org, !t >> [(x -> [[]])])",
          ExMetaTail (ExDispatch ExGlobal (AtMeta "a")) "t",
          ExApplication (ExDispatch ExGlobal (AtLabel "org")) [BiTau (AtLabel "x") (ExFormation [])],
          Just [("a", MvAttribute (AtLabel "org")), ("t", MvTail [TaApplication [BiTau (AtLabel "x") (ExFormation [])]])]
        ),
        ( "Q.x(y -> $ * !t1) * !t2 => Q.x(y -> $.q).p => (!t1 >> [.q], !t2 >> [.p])",
          ExMetaTail (ExApplication (ExDispatch ExGlobal (AtLabel "x")) [BiTau (AtLabel "y") (ExMetaTail ExThis "t1")]) "t2",
          ExDispatch (ExApplication (ExDispatch ExGlobal (AtLabel "x")) [BiTau (AtLabel "y") (ExDispatch ExThis (AtLabel "q"))]) (AtLabel "p"),
          Just [("t1", MvTail [TaDispatch (AtLabel "q")]), ("t2", MvTail [TaDispatch (AtLabel "p")])]
        )
      ]

  describe "combine" $ do
    it "combines empty substitutions" $ do
      combine substEmpty substEmpty `shouldBe` Just substEmpty
    it "combines two empty substs from list" $ do
      combine (Subst Map.empty) (Subst Map.empty) `shouldBe` Just substEmpty
    it "combines empty subst with single one" $ do
      let Subst joined = maybeCombined substEmpty (Subst (Map.singleton "at" (MvAttribute AtPhi)))
      Map.lookup "at" joined `shouldBe` Just (MvAttribute AtPhi)
    it "combines two different subst" $ do
      let Subst joined =
            maybeCombined
              (Subst (Map.singleton "first" (MvAttribute AtPhi)))
              (Subst (Map.singleton "second" (MvBytes "00-")))
      Map.lookup "first" joined `shouldBe` Just (MvAttribute AtPhi)
      Map.lookup "second" joined `shouldBe` Just (MvBytes "00-")
    it "leave values in the same substs" $ do
      let rho = MvAttribute AtRho
          first =
            Subst
              ( Map.fromList
                  [ ("first", rho),
                    ("second", MvAttribute AtPhi)
                  ]
              )
          second = Subst (Map.singleton "first" rho)
          Subst joined = maybeCombined first second
      Map.lookup "first" joined `shouldBe` Just (MvAttribute AtRho)
    it "returns Nothing if values are different" $ do
      combine (Subst (Map.singleton "x" (MvAttribute AtPhi))) (Subst (Map.singleton "x" (MvAttribute AtRho))) `shouldBe` Nothing
    it "clears all the values" $ do
      let first =
            Subst
              ( Map.fromList
                  [ ("x", MvAttribute AtRho),
                    ("y", MvBytes "1F-")
                  ]
              )
          second = Subst (Map.singleton "x" (MvAttribute AtPhi))
      combine first second `shouldBe` Nothing
