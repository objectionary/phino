{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import AST
import Control.Exception (SomeException)
import Control.Monad
import Data.List (find, isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Dataize (DataizeContext (DataizeContext), dataize, dataize', emptyState, execBuildTerm, morph)
import Deps (dontSaveStep)
import Functions (buildTerm)
import Matcher (substEmpty)
import Parser (parseExpressionThrows, parseProgramThrows)
import Rewriter (Rewritten)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Test.Hspec
import Yaml qualified

-- Shuffle is enabled so the suite exercises the order-independence of the
-- dataization rules (#909): a hidden overlap surfaces as a nondeterministic
-- failure instead of staying silently green.
defaultDataizeContext :: Expression -> DataizeContext
defaultDataizeContext loc = DataizeContext loc 25 25 False True buildTerm dontSaveStep

test :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> String -> DataizeContext -> IO ((a, [Rewritten]), String)) -> [(String, Expression, Expression, a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      let prog = Program expr
      ((res, _), _) <- func (input, (prog, Nothing) :| []) expr emptyState (defaultDataizeContext ExRoot)
      res `shouldBe` output

test' :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> String -> DataizeContext -> IO ((a, NonEmpty Rewritten), String)) -> [(String, Expression, Expression, a)] -> Spec
test' func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      let prog = Program expr
      ((res, _), _) <- func (input, (prog, Nothing) :| []) expr emptyState (defaultDataizeContext ExRoot)
      res `shouldBe` output

testDataize :: [(String, String, String, Bytes)] -> Spec
testDataize useCases =
  forM_ useCases $ \(name, loc, prog, res) ->
    it name $ do
      prog' <- parseProgramThrows prog
      loc' <- parseExpressionThrows loc
      (value, _) <- dataize prog' (defaultDataizeContext loc')
      value `shouldBe` res

spec :: Spec
spec = do
  describe "morph" $
    test'
      morph
      [ ("[[ D> 00- ]] => [[ D> 00- ]]", ExFormation [BiDelta (BtOne "00")], ExRoot, ExFormation [BiDelta (BtOne "00")])
      , ("T => T", ExTermination, ExRoot, ExTermination)
      , ("$ => X", ExXi, ExRoot, ExTermination)
      , ("Q => X", ExRoot, ExRoot, ExTermination)
      ,
        ( "Q.x (Q -> [[ x -> [[]] ]]) => [[ ρ -> Q ]]"
        , ExDispatch ExRoot (AtLabel "x")
        , ExFormation [BiTau (AtLabel "x") (ExFormation [])]
        , ExFormation [BiTau AtRho (ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho]), BiVoid AtRho])]
        )
      ]

  -- 'defaultDataizeContext' runs with '_shuffle' on, so 'morph' walks the
  -- morphing rules in a random order on every step. Every clause is
  -- order-independent (the known overlaps were removed in #856 and #860), so the
  -- outcome must never depend on that order: morphing each input many times under
  -- a shuffling context yields exactly the formation the fixed declaration order
  -- does, proving the rules may be applied in any order with the same result.
  -- Were a hidden overlap re-introduced, some of these random orders would
  -- disagree and 'nub' would collect more than the single expected form.
  describe "morphing is order-independent under --shuffle" $ do
    let cases =
          [ ("a byte formation", ExFormation [BiDelta (BtOne "00")], ExRoot, ExFormation [BiDelta (BtOne "00")])
          , ("termination", ExTermination, ExRoot, ExTermination)
          , ("xi", ExXi, ExRoot, ExTermination)
          , ("the global object", ExRoot, ExRoot, ExTermination)
          ,
            ( "a dispatch over a formation"
            , ExDispatch ExRoot (AtLabel "x")
            , ExFormation [BiTau (AtLabel "x") (ExFormation [])]
            , ExFormation [BiTau AtRho (ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho]), BiVoid AtRho])]
            )
          ]
    forM_ cases $ \(desc, input, univ, expected) ->
      it ("morphs " ++ desc ++ " to the same form across 100 random rule orders") $ do
        let prog = Program univ
        results <- replicateM 100 (fst . fst <$> morph (input, (prog, Nothing) :| []) univ emptyState (defaultDataizeContext ExRoot))
        nub results `shouldBe` [expected]

  -- 'dispatch' fires only when its head is not a formation ('not (formation 𝑛)'),
  -- so a formation head — λ-bearing or not — is left to 'lambda'/'prim'. The
  -- two clauses are mutually exclusive and their order in 'morphing.yaml'
  -- cannot change behavior.
  describe "morphing 'dispatch' is disjoint from 'lambda'" $ do
    let rctx = RuleContext (execBuildTerm ExRoot (defaultDataizeContext ExRoot))
        morphRule :: String -> Yaml.MorphRule
        morphRule nm = fromMaybe (error ("no morphing rule named " ++ nm)) (find (\r -> r.name == nm) Yaml.morphingRules)
        asRule :: Yaml.MorphRule -> Yaml.Rule
        asRule r = Yaml.Rule r.name Nothing Nothing r.match ExRoot r.when Nothing Nothing
        lambdaFormation = ExFormation [BiLambda (Function "L_dummy"), BiVoid AtRho]
    it "does not fire on a λ-bearing formation dispatch" $ do
      substs <- matchExpressionWithRule' [substEmpty] (ExDispatch lambdaFormation (AtLabel "x")) (asRule (morphRule "dispatch")) rctx
      substs `shouldBe` []
    it "still fires on a non-λ-formation dispatch" $ do
      substs <- matchExpressionWithRule' [substEmpty] (ExDispatch ExXi (AtLabel "x")) (asRule (morphRule "dispatch")) rctx
      null substs `shouldBe` False
    -- ⟦λ ⤍ F⟧.a.b.c : 'dispatch' peels .c then .b (their heads are dispatches,
    -- not λ-formations, so 'λ ∉ 𝐵' holds), then 'lambda' handles the base
    -- ⟦λ ⤍ F⟧.a and fires the atom. The chain therefore routes
    -- dispatch → dispatch → lambda; firing the undefined atom 'F' is what
    -- raises the error, proving the base λ-formation reached 'lambda'.
    it "drills a chained λ-formation dispatch down to the base 'lambda'" $ do
      let base = ExFormation [BiLambda (Function "F")]
          chain = ExDispatch (ExDispatch (ExDispatch base (AtLabel "a")) (AtLabel "b")) (AtLabel "c")
      morph (chain, (Program ExRoot, Nothing) :| []) ExRoot emptyState (defaultDataizeContext ExRoot)
        `shouldThrow` (\e -> "Atom 'F' does not exist" `isInfixOf` show (e :: SomeException))

  -- 'norm' matches the bare meta 𝑛, which unifies with any expression, so it is
  -- guarded to fire only when 𝑛 is neither a formation ('not (formation 𝑛)',
  -- left to 'delta'/'box'/'fire'/'none') nor the termination ⊥ ('not (𝑛 = ⊥)',
  -- left to 'bott'). The dataization clauses are therefore disjoint and their
  -- order in 'dataization.yaml' cannot change behavior.
  describe "dataization 'norm' is disjoint from the specific clauses" $ do
    let rctx = RuleContext (execBuildTerm ExRoot (defaultDataizeContext ExRoot))
        dataizeRule :: String -> Yaml.DataizeRule
        dataizeRule nm = fromMaybe (error ("no dataization rule named " ++ nm)) (find (\r -> r.name == nm) Yaml.dataizationRules)
        asRule :: Yaml.DataizeRule -> Yaml.Rule
        asRule r = Yaml.Rule r.name Nothing Nothing r.match ExRoot r.when Nothing Nothing
    it "does not fire on a formation" $ do
      substs <- matchExpressionWithRule' [substEmpty] (ExFormation [BiDelta (BtOne "00")]) (asRule (dataizeRule "norm")) rctx
      substs `shouldBe` []
    it "does not fire on the termination ⊥" $ do
      substs <- matchExpressionWithRule' [substEmpty] ExTermination (asRule (dataizeRule "norm")) rctx
      substs `shouldBe` []
    it "still fires on a non-formation, non-termination normal form" $ do
      substs <- matchExpressionWithRule' [substEmpty] (ExDispatch ExXi (AtLabel "x")) (asRule (dataizeRule "norm")) rctx
      null substs `shouldBe` False

  describe "dataize" $
    test
      dataize'
      [ ("[[ D> 00- ]] => 00-", ExFormation [BiDelta (BtOne "00")], ExRoot, BtOne "00")
      , ("T => --", ExTermination, ExRoot, BtEmpty)
      , ("[[ ]] => --", ExFormation [], ExRoot, BtEmpty)
      ,
        ( "[[ @ -> [[ D> 00-]] ]] => 00-"
        , ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "00"), BiVoid AtRho]), BiVoid AtRho]
        , ExRoot
        , BtOne "00"
        )
      ,
        ( "[[ @ -> [[ x -> [[ D> 01-, y -> ? ]](y -> [[ ]]) ]].x ]] => 01-"
        , ExFormation
            [ BiTau
                AtPhi
                ( ExDispatch
                    ( ExFormation
                        [ BiTau
                            (AtLabel "x")
                            ( ExApplication
                                ( ExFormation
                                    [ BiDelta (BtOne "01")
                                    , BiVoid (AtLabel "y")
                                    , BiVoid AtRho
                                    ]
                                )
                                (ArTau (AtLabel "y") (ExFormation []))
                            )
                        ]
                    )
                    (AtLabel "x")
                )
            ]
        , ExRoot
        , BtOne "01"
        )
      ]

  describe "labels every step with a defined rule or operation" $ do
    let verb op = case op of
          Yaml.OpMorph _ -> "morph"
          Yaml.OpNormalize _ -> "normalize"
          Yaml.OpEvaluate _ -> "evaluate"
          Yaml.OpContextualize _ _ -> "contextualize"
          Yaml.OpDataize _ -> "dataize"
        allowed =
          map (.name) Yaml.morphingRules
            ++ map (.name) Yaml.dataizationRules
            ++ map (.name) Yaml.normalizationRules
            ++ concatMap (map (verb . (.operation)) . (.premises)) Yaml.morphingRules
            ++ concatMap (map (verb . (.operation)) . (.premises)) Yaml.dataizationRules
    it "uses no step label without a defining rule or operation" $ do
      prog <-
        parseProgramThrows
          ( unlines
              [ "Q -> [["
              , "  bytes(data) -> [[ @ -> $.data ]],"
              , "  number(as-bytes) -> [[ @ -> $.as-bytes, plus(x) -> [[ L> L_number_plus ]] ]],"
              , "  @ -> 5.plus(6)"
              , "]]"
              ]
          )
      loc <- parseExpressionThrows "Q"
      (_, chain) <- dataize prog (defaultDataizeContext loc)
      let orphans = nub [label | (_, Just label) <- chain, label `notElem` allowed]
      unless
        (null orphans)
        (expectationFailure ("Dataization emitted step labels with no defining rule or operation: " ++ show orphans))

  describe "names every rule uniquely across rule sets" $
    it "shares no rule name between morphing, dataization, normalization and contextualization" $ do
      let names =
            map (.name) Yaml.morphingRules
              ++ map (.name) Yaml.dataizationRules
              ++ map (.name) Yaml.normalizationRules
              ++ map (.name) Yaml.contextualizationRules
          clashes = nub (filter (\n -> length (filter (== n) names) > 1) names)
      clashes `shouldBe` []

  describe "preserves the reduction label sequence" $ do
    let labelsOf loc src = do
          prog <- parseProgramThrows src
          loc' <- parseExpressionThrows loc
          (_, chain) <- dataize prog (defaultDataizeContext loc')
          pure [label | (_, Just label) <- chain]
    it "dataizes 5.plus(6) through the expected rules" $ do
      labels <-
        labelsOf
          "Q"
          "Q -> [[ bytes(data) -> [[ @ -> $.data ]], number(as-bytes) -> [[ @ -> $.as-bytes, plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
      labels
        `shouldBe` [ "contextualize"
                   , "applicationa"
                   , "alpha"
                   , "copy"
                   , "prim"
                   , "evaluate"
                   , "applicationa"
                   , "alpha"
                   , "copy"
                   , "prim"
                   , "contextualize"
                   , "dot"
                   , "application"
                   , "stay"
                   , "prim"
                   , "contextualize"
                   , "dot"
                   , "copy"
                   ]
    it "dataizes a located reference through the expected rules" $ do
      labels <- labelsOf "Q.foo.bar" "Q -> [[ foo -> [[ bar -> [[ @ -> Q.x ]] ]], x -> [[ D> 42- ]] ]]"
      labels `shouldBe` ["contextualize", "dispatch", "dot", "copy", "prim"]

  testDataize
    [
      ( "5.plus(6)"
      , "Q"
      , unlines
          [ "Q -> [["
          , "  bytes(data) -> [["
          , "    @ -> $.data"
          , "  ]],"
          , "  number(as-bytes) -> [["
          , "    @ -> $.as-bytes,"
          , "    plus(x) -> [[ L> L_number_plus ]]"
          , "  ]],"
          , "  @ -> 5.plus(6)"
          , "]]"
          ]
      , BtMany ["40", "26", "00", "00", "00", "00", "00", "00"]
      )
    ,
      ( "Fahrenheit"
      , "Q"
      , unlines
          [ "Q -> [["
          , "  bytes -> [["
          , "    data -> ?,"
          , "    @ -> $.data"
          , "  ]],"
          , "  number -> [["
          , "    as-bytes -> ?,"
          , "    @ -> $.as-bytes,"
          , "    plus -> [[ x -> ?, L> L_number_plus ]],"
          , "    times -> [[ x -> ?, L> L_number_times ]]"
          , "  ]],"
          , "  @ -> $.c.times(1.8).plus(32),"
          , "  c -> 25"
          , "]]"
          ]
      , BtMany ["40", "53", "40", "00", "00", "00", "00", "00"]
      )
    ,
      ( "Factorial"
      , "Q"
      , unlines
          [ "Q -> [["
          , "  bytes -> [["
          , "    data -> ?,"
          , "    @ -> $.data"
          , "  ]],"
          , "  number -> [["
          , "    as-bytes -> ?,"
          , "    @ -> $.as-bytes,"
          , "    times -> [[ x -> ?, L> L_number_times ]],"
          , "    plus -> [[ x -> ?, L> L_number_plus ]],"
          , "    eq -> [[ x -> ?, y -> ?, L> L_number_eq ]]"
          , "  ]],"
          , "  fac -> [["
          , "    x -> ?,"
          , "    @ -> $.x.eq("
          , "      1,"
          , "      $.x.times($.^.fac($.x.plus(-1)))"
          , "    )"
          , "  ]],"
          , "  @ -> $.fac(3)"
          , "]]"
          ]
      , BtMany ["40", "18", "00", "00", "00", "00", "00", "00"]
      )
    ,
      ( "Located"
      , "Q.foo.bar"
      , unlines
          [ "Q -> [["
          , "  foo -> [["
          , "    bar -> [["
          , "      @ -> Q.x"
          , "    ]]"
          , "  ]],"
          , "  x -> [[ D> 42- ]]"
          , "]]"
          ]
      , BtOne "42"
      )
    ,
      ( "Five"
      , "Q.x"
      , unlines
          [ "Q -> [["
          , "  number(as-bytes) -> [[ @ -> as-bytes ]],"
          , "  bytes(data) -> [[ @ -> data ]],"
          , "  x -> 5"
          , "]]"
          ]
      , BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]
      )
    ]
