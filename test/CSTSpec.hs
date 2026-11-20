{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CSTSpec (spec) where

import AST
import Bytes (NonFinite (..))
import CST
import Control.Monad (forM_)
import Data.Aeson
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Encoding (Encoding (ASCII), withEncoding)
import Files (allPathsIn)
import GHC.Generics (Generic)
import Lining (LineFormat (SINGLELINE), withLineFormat)
import Margin (defaultMargin, withMargin)
import Parser (Form (MONOFORM), parseExpressionThrows, parseExpressionThrows')
import Render (Render (render))
import Sugar
import System.FilePath
import Test.Hspec
import Yaml qualified as Y

data CSTPack = CSTPack
  { expression :: String
  , result :: T.Text
  }
  deriving (Generic, Show, FromJSON)

cstPack :: FilePath -> IO CSTPack
cstPack = Yaml.decodeFileThrow

spec :: Spec
spec = do
  describe "builds valid CST" $
    forM_
      [ ("Q", EX_GLOBAL Φ)
      ,
        ( "[[ x -> Q.y ]]"
        , EX_SINGLE
            (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_GLOBAL Φ) NO_SPACE (AT_LABEL "y")))
            NO_SPACE
            ( EX_FORMATION
                LSB
                EOL
                (TAB 1)
                (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_GLOBAL Φ) NO_SPACE (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1))
                EOL
                (TAB 0)
                RSB
            )
        )
      ,
        ( "[[ x -> Q.y, z -> Q ]]"
        , EX_FORMATION
            LSB
            EOL
            (TAB 1)
            ( BI_PAIR
                (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_GLOBAL Φ) NO_SPACE (AT_LABEL "y")))
                (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "z") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1)))
                (TAB 1)
            )
            EOL
            (TAB 0)
            RSB
        )
      ]
      ( \(desc, cst) -> it desc $ do
          ast <- parseExpressionThrows desc
          expressionToCST ast `shouldBe` cst
      )

  describe "build valid CST with wrapped phinoAgain{} " $ do
    let number = BaseObject "number"
        again = ExPhiAgain Nothing 1
        bts = BaseObject "bytes"
        bt = ArAlpha (Alpha 0)
        app = ExApplication
        form = ExFormation [BiDelta (BtMany ["40", "18", "00", "00", "00", "00", "00", "00"])]
        isCSTNumber (EX_NUMBER{}) = True
        isCSTNumber _ = False
    forM_
      [ ("number(bytes(data))", app number (bt (app bts (bt form))))
      , ("again(number)(bytes(data))", app (again number) (bt (app bts (bt form))))
      , ("number(again(bytes(data)))", app number (bt (again (app bts (bt form)))))
      , ("number(again(bytes)(data))", app number (bt (app (again bts) (bt form))))
      , ("again(number)(again(bytes)(data))", app (again number) (bt (app (again bts) (bt form))))
      , ("number(bytes(again(data)))", app number (bt (app bts (bt (again form)))))
      , ("again(number)(again(bytes)(again(data)))", app (again number) (bt (app (again bts) (bt (again form)))))
      ]
      (\(desc, ex) -> it desc (toCST ex (0, EOL) `shouldSatisfy` isCSTNumber))

  describe "CST printing packs" $ do
    let resources = "test-resources/cst/printing-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          parsed <- parseExpressionThrows' MONOFORM (expression pack)
          render (withMargin defaultMargin (expressionToCST parsed)) `shouldBe` result pack
      )

  describe "converts to salty CST" $ do
    let resources = "test-resources/cst/to-salty-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          parsed <- parseExpressionThrows' MONOFORM (expression pack)
          let cst = expressionToCST parsed
              salty = toSalty cst
          render salty `shouldBe` result pack
      )

  describe "converts to ascii CST" $ do
    let resources = "test-resources/cst/to-ascii-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          parsed <- parseExpressionThrows' MONOFORM (expression pack)
          let cst = expressionToCST parsed
              ascii = withMargin defaultMargin (withEncoding ASCII cst)
          render ascii `shouldBe` result pack
      )

  describe "converts to singleline CST" $ do
    let resources = "test-resources/cst/to-singleline-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          parsed <- parseExpressionThrows' MONOFORM (expression pack)
          let cst = expressionToCST parsed
              ascii = withLineFormat SINGLELINE cst
          render ascii `shouldBe` result pack
      )

  describe "expressionToCSTFrom lays out a formation from a given base indent"
    $ it "nests the body one level below the given tabs and closes at it"
    $ expressionToCSTFrom 2 (ExFormation [BiTau (AtLabel "x") ExRoot, BiVoid (AtLabel "y")])
      `shouldBe` EX_FORMATION
        LSB
        EOL
        (TAB 3)
        (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_GLOBAL Φ)) (BDS_PAIR EOL (TAB 3) (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 3))) (TAB 3))
        EOL
        (TAB 2)
        RSB

  describe "sweetNumber" $
    forM_
      [ ("is true for a finite integral value", BtMany ["40", "45", "00", "00", "00", "00", "00", "00"], True)
      , ("is true for a finite fractional value", BtMany ["BF", "D0", "00", "00", "00", "00", "00", "00"], True)
      , ("is true for NaN", BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"], True)
      , ("is true for positive infinity", BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"], True)
      , ("is true for negative infinity", BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"], True)
      , ("is false for a NaN carrying a payload", BtMany ["7F", "F8", "00", "00", "00", "00", "00", "01"], False)
      , ("is false for the negative quiet NaN", BtMany ["FF", "F8", "00", "00", "00", "00", "00", "00"], False)
      , ("is false for fewer than eight bytes", BtOne "21", False)
      , ("is false for empty bytes", BtEmpty, False)
      ]
      (\(desc, bts, expected) -> it desc (sweetNumber bts `shouldBe` expected))

  describe "sweetString" $
    forM_
      [ ("is true for a valid ASCII datum", BtMany ["77", "6F", "72", "6C", "64"], True)
      , ("is true for a valid multi-byte datum", BtMany ["D0", "B0"], True)
      , ("is true for empty bytes", BtEmpty, True)
      , ("is false for a lone continuation byte", BtOne "A0", False)
      , ("is false for a truncated surrogate sequence", BtMany ["F0", "90", "80", "41"], False)
      , ("is false for a lone 0xFE byte", BtOne "FE", False)
      ]
      (\(desc, bts, expected) -> it desc (sweetString bts `shouldBe` expected))

  describe "sweetCollapsible" $
    forM_
      [
        ( "delegates to sweetNumber for a data number"
        , DataNumber (BtMany ["7F", "F8", "00", "00", "00", "00", "00", "01"])
        , False
        )
      ,
        ( "is true for a data number with a sweet literal"
        , DataNumber (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
        , True
        )
      , ("is true for any other expression", ExXi, True)
      ]
      (\(desc, expressionValue, expected) -> it desc (sweetCollapsible expressionValue `shouldBe` expected))

  describe "metaTail drops the leading kind character" $
    forM_
      [ ("single char meta", "x", "")
      , ("two char meta", "ex", "x")
      ]
      (\(desc, metaName, expected) -> it desc (metaTail metaName `shouldBe` expected))

  describe "exMetaHead classifies a meta name by its leading character" $
    forM_
      [ ("n-prefixed becomes a normal-form meta", "nx", N)
      , ("k-prefixed becomes an absolute meta", "kx", K)
      , ("anything else becomes an ordinary meta", "ex", E)
      , ("a name with neither prefix also becomes ordinary", "tx", E)
      ]
      (\(desc, metaName, expected) -> it desc (exMetaHead metaName `shouldBe` expected))

  describe "expressionToCST on the non-finite doubles" $
    forM_
      [ ("NaN becomes Φ.nan", BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"], NfNan)
      , ("positive infinity becomes Φ.pinf", BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"], NfPinf)
      , ("negative infinity becomes Φ.ninf", BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"], NfNinf)
      ]
      ( \(desc, bts, value) ->
          it desc (expressionToCST (DataNumber bts) `shouldBe` EX_NONFINITE Φ value (TAB 0) [])
      )

  describe "expressionToCST keeps a non-canonical non-finite double in byte form" $
    it "leaves a NaN carrying a payload as an application" $ do
      let payloaded = DataNumber (BtMany ["7F", "F8", "00", "00", "00", "00", "00", "01"])
          isApplication :: EXPRESSION -> Bool
          isApplication EX_APPLICATION{} = True
          isApplication _ = False
      expressionToCST payloaded `shouldSatisfy` isApplication

  describe "expressionToCST on rendering-only and meta nodes" $
    forM_
      [ ("ExBytes becomes a bare EX_BYTES chain node", ExBytes (BtOne "1F"), EX_BYTES (BT_ONE "1F"))
      , ("an n-prefixed ExMeta becomes a normal-form meta", ExMeta "nX", EX_META (META NO_EXCL N "X"))
      , ("a k-prefixed ExMeta becomes an absolute meta", ExMeta "kX", EX_META (META NO_EXCL K "X"))
      , ("any other ExMeta becomes an ordinary meta", ExMeta "eX", EX_META (META NO_EXCL E "X"))
      , ("ExPhiMeet keeps its prefix", ExPhiMeet (Just "p") 3 ExXi, EX_PHI_MEET (Just "p") 3 (EX_XI XI))
      , ("ExPhiAgain keeps its prefix", ExPhiAgain (Just "p") 3 ExXi, EX_PHI_AGAIN (Just "p") 3 (EX_XI XI))
      ]
      (\(desc, expressionValue, expected) -> it desc (expressionToCST expressionValue `shouldBe` expected))

  describe "attributeToCST and bindingsToCST" $ do
    forM_
      [ ("attributeToCST on a label", AtLabel "x", AT_LABEL "x")
      , ("attributeToCST on rho", AtRho, AT_RHO RHO)
      ]
      (\(desc, attribute, expected) -> it desc (attributeToCST attribute `shouldBe` expected))

    forM_
      [ ("bindingsToCST on an empty list", [], BI_EMPTY (TAB 0))
      ,
        ( "bindingsToCST on a single binding"
        , [BiVoid (AtLabel "y")]
        , BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0)
        )
      ,
        ( "bindingsToCST on a meta binding collapses the whole list to BI_META"
        , [BiMeta "B"]
        , BI_META (META NO_EXCL B "") (BDS_EMPTY (TAB 0)) (TAB 0)
        )
      ,
        ( "bindingsToCST on a delta binding"
        , [BiDelta (BtOne "01")]
        , BI_PAIR (PA_DELTA (BT_ONE "01")) (BDS_EMPTY (TAB 0)) (TAB 0)
        )
      ,
        ( "bindingsToCST recurses into a meta binding that is not the head"
        , [BiVoid (AtLabel "x"), BiMeta "B"]
        , BI_PAIR
            (PA_VOID (AT_LABEL "x") ARROW EMPTY)
            (BDS_META EOL (TAB 0) (META NO_EXCL B "") (BDS_EMPTY (TAB 0)))
            (TAB 0)
        )
      ]
      (\(desc, bindingsList, expected) -> it desc (bindingsToCST bindingsList `shouldBe` expected))

  describe "expressionToCST on formation and dispatch edge cases" $ do
    it
      "a single void rho binding stays apart from an empty formation"
      (expressionToCST (ExFormation [BiVoid AtRho]) `shouldNotBe` expressionToCST (ExFormation []))
    it
      "dispatch on Xi becomes a bare attribute"
      (expressionToCST (ExDispatch ExXi (AtLabel "foo")) `shouldBe` EX_ATTR (AT_LABEL "foo"))

  describe "expressionToCST on non-primitive applications" $ do
    it
      "an application with only named (tau) arguments and no primitive base"
      ( expressionToCST
          ( ExApplication
              (ExApplication (ExDispatch ExRoot (AtLabel "bar")) (ArTau (AtLabel "x") ExRoot))
              (ArTau (AtLabel "y") ExXi)
          )
          `shouldSatisfy` isApplicationWithTaus
      )
    it
      "an application with positional (alpha) arguments and no primitive base"
      ( expressionToCST
          ( ExApplication
              (ExApplication (ExDispatch ExRoot (AtLabel "bar")) (ArAlpha (Alpha 0) ExRoot))
              (ArAlpha (Alpha 1) ExXi)
          )
          `shouldSatisfy` isApplicationWithExprs
      )

  describe "conditionToCST on every Y.Condition constructor" $ do
    let voidYBinding :: BINDING
        voidYBinding = BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0)
    forM_
      [ ("In", Y.In [AtLabel "x"] [BiVoid (AtLabel "y")], CO_BELONGS (AT_LABEL "x") IN (ST_BINDING voidYBinding))
      ,
        ( "Not (In ...) flips the belonging"
        , Y.Not (Y.In [AtLabel "x"] [BiVoid (AtLabel "y")])
        , CO_BELONGS (AT_LABEL "x") NOT_IN (ST_BINDING voidYBinding)
        )
      ,
        ( "Eq"
        , Y.Eq (Y.CmpAttr (AtLabel "x")) (Y.CmpNum (Y.Literal 3))
        , CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_NUM (LITERAL 3))
        )
      ,
        ( "Not (Eq ...) becomes a not-equal comparison"
        , Y.Not (Y.Eq (Y.CmpAttr (AtLabel "x")) (Y.CmpNum (Y.Literal 3)))
        , CO_COMPARE (CMP_ATTR (AT_LABEL "x")) NOT_EQUAL (CMP_NUM (LITERAL 3))
        )
      ,
        ( "Gt"
        , Y.Gt (Y.CmpAttr (AtLabel "x")) (Y.CmpNum (Y.Literal 3))
        , CO_COMPARE (CMP_ATTR (AT_LABEL "x")) GREATER (CMP_NUM (LITERAL 3))
        )
      ,
        ( "Not (Gt ...) becomes a not-greater comparison"
        , Y.Not (Y.Gt (Y.CmpAttr (AtLabel "x")) (Y.CmpNum (Y.Literal 3)))
        , CO_COMPARE (CMP_ATTR (AT_LABEL "x")) NOT_GREATER (CMP_NUM (LITERAL 3))
        )
      , ("Absolute", Y.Absolute ExXi, CO_ABSOLUTE (EX_XI XI) IN)
      , ("Not (Absolute ...) flips membership", Y.Not (Y.Absolute ExXi), CO_ABSOLUTE (EX_XI XI) NOT_IN)
      , ("Disjoint", Y.Disjoint [AtLabel "a"] [BiVoid (AtLabel "y")], CO_DISJOINT [AT_LABEL "a"] [voidYBinding])
      ,
        ( "In over many attributes becomes a subset"
        , Y.In [AtLabel "a", AtLabel "x"] [BiVoid (AtLabel "y")]
        , CO_SUBSET [AT_LABEL "a", AT_LABEL "x"] IN [voidYBinding]
        )
      ,
        ( "Not (In ...) over many binding metas flips the subset"
        , Y.Not (Y.In [AtLabel "x"] [BiVoid (AtLabel "y"), BiVoid (AtLabel "y")])
        , CO_SUBSET [AT_LABEL "x"] NOT_IN [voidYBinding, voidYBinding]
        )
      , ("And on an empty list collapses to CO_EMPTY", Y.And [], CO_EMPTY)
      , ("And on a non-empty list wraps every condition", Y.And [Y.NF ExXi], CO_LOGIC [CO_NF (EX_XI XI)] AND)
      , ("Or on an empty list collapses to CO_EMPTY", Y.Or [], CO_EMPTY)
      , ("Or on a non-empty list wraps every condition", Y.Or [Y.NF ExXi], CO_LOGIC [CO_NF (EX_XI XI)] OR)
      , ("NF", Y.NF ExXi, CO_NF (EX_XI XI))
      ,
        ( "Not on any other condition falls back to a generic negation"
        , Y.Not (Y.NF ExXi)
        , CO_NOT (CO_NF (EX_XI XI))
        )
      , ("Matches", Y.Matches "abc" ExXi, CO_MATCHES "abc" (EX_XI XI))
      , ("PartOf", Y.PartOf ExXi (BiVoid (AtLabel "y")), CO_PART_OF (EX_XI XI) voidYBinding)
      , ("IsFormation", Y.IsFormation ExXi, CO_FORMATION (EX_XI XI))
      ]
      (\(desc, yamlCondition, expected) -> it desc (conditionToCST yamlCondition `shouldBe` expected))

  describe "comparableToCST on every Y.Comparable constructor" $
    forM_
      [ ("CmpAttr", Y.CmpAttr (AtLabel "x"), CMP_ATTR (AT_LABEL "x"))
      , ("CmpExpr", Y.CmpExpr ExXi, CMP_EXPR (EX_XI XI))
      , ("CmpNum", Y.CmpNum (Y.Literal 3), CMP_NUM (LITERAL 3))
      ]
      (\(desc, comparable, expected) -> it desc (comparableToCST comparable `shouldBe` expected))

  describe "numberToCST on every Y.Number constructor" $
    forM_
      [ ("MetaIndex", Y.MetaIndex "i1", IDX_META (META NO_EXCL I "1"))
      ,
        ( "Length"
        , Y.Length (BiVoid (AtLabel "y"))
        , LENGTH (BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0))
        )
      ,
        ( "Domain"
        , Y.Domain (BiVoid (AtLabel "y"))
        , DOMAIN (BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0))
        )
      , ("Literal", Y.Literal 5, LITERAL 5)
      ]
      (\(desc, number, expected) -> it desc (numberToCST number `shouldBe` expected))

  describe "extraToCST on every Y.ExtraArgument constructor" $
    it
      "converts the meta and every kind of argument"
      ( extraToCST
          (Y.Extra (Y.ArgAttribute (AtLabel "m")) "g" [Y.ArgExpression ExXi, Y.ArgBinding (BiVoid (AtLabel "y")), Y.ArgBytes (BtOne "1F")])
          `shouldBe` EXTRA
            (ARG_ATTR (AT_LABEL "m"))
            "g"
            [ ARG_EXPR (EX_XI XI)
            , ARG_BINDING (BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0))
            , ARG_BYTES (BT_ONE "1F")
            ]
      )

  describe "CST token derived instances" $
    forM_
      [ ("LCB", shouldShowAndEqSelf "LCB" LCB)
      , ("BIG_LCB", shouldShowAndEqSelf "BIG_LCB" BIG_LCB)
      , ("RCB", shouldShowAndEqSelf "RCB" RCB)
      , ("BIG_RCB", shouldShowAndEqSelf "BIG_RCB" BIG_RCB)
      , ("LSB", shouldShowAndEqSelf "LSB" LSB)
      , ("LSB'", shouldShowAndEqSelf "LSB'" LSB')
      , ("RSB", shouldShowAndEqSelf "RSB" RSB)
      , ("RSB'", shouldShowAndEqSelf "RSB'" RSB')
      , ("COMMA", shouldShowAndEqSelf "COMMA" COMMA)
      , ("NO_COMMA", shouldShowAndEqSelf "NO_COMMA" NO_COMMA)
      , ("ARROW", shouldShowAndEqSelf "ARROW" ARROW)
      , ("ARROW'", shouldShowAndEqSelf "ARROW'" ARROW')
      , ("DASHED_ARROW", shouldShowAndEqSelf "DASHED_ARROW" DASHED_ARROW)
      , ("EMPTY", shouldShowAndEqSelf "EMPTY" EMPTY)
      , ("QUESTION", shouldShowAndEqSelf "QUESTION" QUESTION)
      , ("PHI", shouldShowAndEqSelf "PHI" PHI)
      , ("AT", shouldShowAndEqSelf "AT" AT)
      , ("RHO", shouldShowAndEqSelf "RHO" RHO)
      , ("CARET", shouldShowAndEqSelf "CARET" CARET)
      , ("RHO'", shouldShowAndEqSelf "RHO'" RHO')
      , ("DELTA", shouldShowAndEqSelf "DELTA" DELTA)
      , ("DELTA'", shouldShowAndEqSelf "DELTA'" DELTA')
      , ("XI", shouldShowAndEqSelf "XI" XI)
      , ("DOLLAR", shouldShowAndEqSelf "DOLLAR" DOLLAR)
      , ("XI'", shouldShowAndEqSelf "XI'" XI')
      , ("LAMBDA", shouldShowAndEqSelf "LAMBDA" LAMBDA)
      , ("LAMBDA'", shouldShowAndEqSelf "LAMBDA'" LAMBDA')
      , ("Q", shouldShowAndEqSelf "Q" Q)
      , ("DEAD", shouldShowAndEqSelf "DEAD" DEAD)
      , ("T", shouldShowAndEqSelf "T" T)
      , ("SPACE", shouldShowAndEqSelf "SPACE" SPACE)
      , ("NO_SPACE", shouldShowAndEqSelf "NO_SPACE" NO_SPACE)
      , ("EOL", shouldShowAndEqSelf "EOL" EOL)
      , ("NO_EOL", shouldShowAndEqSelf "NO_EOL" NO_EOL)
      , ("DOTS", shouldShowAndEqSelf "DOTS" DOTS)
      , ("DOTS'", shouldShowAndEqSelf "DOTS'" DOTS')
      , ("BT_EMPTY", shouldShowAndEqSelf "BT_EMPTY" BT_EMPTY)
      , ("E", shouldShowAndEqSelf "E" E)
      , ("EXCL", shouldShowAndEqSelf "EXCL" EXCL)
      , ("NO_EXCL", shouldShowAndEqSelf "NO_EXCL" NO_EXCL)
      , ("IN", shouldShowAndEqSelf "IN" IN)
      , ("NOT_IN", shouldShowAndEqSelf "NOT_IN" NOT_IN)
      , ("AND", shouldShowAndEqSelf "AND" AND)
      , ("OR", shouldShowAndEqSelf "OR" OR)
      , ("EQUAL", shouldShowAndEqSelf "EQUAL" EQUAL)
      , ("NOT_EQUAL", shouldShowAndEqSelf "NOT_EQUAL" NOT_EQUAL)
      , ("GREATER", shouldShowAndEqSelf "GREATER" GREATER)
      , ("NOT_GREATER", shouldShowAndEqSelf "NOT_GREATER" NOT_GREATER)
      ]
      (uncurry it)

  describe "META field accessors" $
    it "exposes every META field via its accessor" $ do
      let metaVal = META{excl = EXCL, hd = TAU, rest = "x"}
      metaVal.excl `shouldBe` EXCL
      metaVal.hd `shouldBe` TAU
      metaVal.rest `shouldBe` "x"
      shouldShowAndEqSelf "META" metaVal

  describe "TAB field accessors" $
    it "exposes every TAB field via its accessor" $ do
      let tabVal = TAB{indent = 3}
      case tabVal of
        actual@TAB{} ->
          actual.indent `shouldBe` 3
        _ -> expectationFailure "Expected TAB"
      shouldShowAndEqSelf "TAB" tabVal
      shouldShowAndEqSelf "TAB'" TAB'
      shouldShowAndEqSelf "NO_TAB" NO_TAB

  describe "ALPHA field accessors" $
    it "exposes every ALPHA field via its accessor" $ do
      let idxAlpha = AL_IDX{sym = ALPHA, idx = 2}
          metaAlpha = AL_META{sym = ALPHA', meta = META EXCL TAU "x"}
      case idxAlpha of
        actual@AL_IDX{} -> do
          actual.sym `shouldBe` ALPHA
          actual.idx `shouldBe` 2
        _ -> expectationFailure "Expected AL_IDX"
      case metaAlpha of
        actual@AL_META{} -> do
          actual.sym `shouldBe` ALPHA'
          actual.meta `shouldBe` META EXCL TAU "x"
        _ -> expectationFailure "Expected AL_META"
      shouldShowAndEqSelf "AL_IDX" idxAlpha
      shouldShowAndEqSelf "AL_META" metaAlpha

  describe "PAIR field accessors" $
    it "exposes every PAIR constructor's fields via their accessors" $ do
      let attrLabel = AT_LABEL "x"
          exprGlobal = EX_GLOBAL Φ
          metaVal = META EXCL TAU "x"
          pairTau = PA_TAU{attr = attrLabel, arrow = ARROW, expr = exprGlobal}
          pairAlpha = PA_ALPHA{alpha = AL_IDX ALPHA 0, arrow = ARROW, expr = exprGlobal}
          pairFormation = PA_FORMATION{attr = attrLabel, voids = [AT_RHO RHO], arrow = ARROW, expr = exprGlobal}
          pairVoid = PA_VOID{attr = attrLabel, arrow = ARROW, void = EMPTY}
          pairLambda = PA_LAMBDA{func = "fn"}
          pairLambda' = PA_LAMBDA'{func = "fn"}
          pairMetaLambda = PA_META_LAMBDA{meta = metaVal}
          pairMetaLambda' = PA_META_LAMBDA'{meta = metaVal}
          pairDelta = PA_DELTA{bytes = BT_ONE "40"}
          pairDelta' = PA_DELTA'{bytes = BT_ONE "40"}
          pairMetaDelta = PA_META_DELTA{meta = metaVal}
          pairMetaDelta' = PA_META_DELTA'{meta = metaVal}
      case pairTau of
        actual@PA_TAU{} -> do
          actual.attr `shouldBe` attrLabel
          actual.arrow `shouldBe` ARROW
          actual.expr `shouldBe` exprGlobal
        _ -> expectationFailure "Expected PA_TAU"
      case pairAlpha of
        actual@PA_ALPHA{} -> do
          actual.alpha `shouldBe` AL_IDX ALPHA 0
          actual.arrow `shouldBe` ARROW
          actual.expr `shouldBe` exprGlobal
        _ -> expectationFailure "Expected PA_ALPHA"
      case pairFormation of
        actual@PA_FORMATION{} -> do
          actual.attr `shouldBe` attrLabel
          actual.voids `shouldBe` [AT_RHO RHO]
          actual.arrow `shouldBe` ARROW
          actual.expr `shouldBe` exprGlobal
        _ -> expectationFailure "Expected PA_FORMATION"
      case pairVoid of
        actual@PA_VOID{} -> do
          actual.attr `shouldBe` attrLabel
          actual.arrow `shouldBe` ARROW
          actual.void `shouldBe` EMPTY
        _ -> expectationFailure "Expected PA_VOID"
      case pairLambda of
        actual@PA_LAMBDA{} ->
          actual.func `shouldBe` "fn"
        _ -> expectationFailure "Expected PA_LAMBDA"
      case pairLambda' of
        actual@PA_LAMBDA'{} ->
          actual.func `shouldBe` "fn"
        _ -> expectationFailure "Expected PA_LAMBDA'"
      case pairMetaLambda of
        actual@PA_META_LAMBDA{} ->
          actual.meta `shouldBe` metaVal
        _ -> expectationFailure "Expected PA_META_LAMBDA"
      case pairMetaLambda' of
        actual@PA_META_LAMBDA'{} ->
          actual.meta `shouldBe` metaVal
        _ -> expectationFailure "Expected PA_META_LAMBDA'"
      case pairDelta of
        actual@PA_DELTA{} ->
          actual.bytes `shouldBe` BT_ONE "40"
        _ -> expectationFailure "Expected PA_DELTA"
      case pairDelta' of
        actual@PA_DELTA'{} ->
          actual.bytes `shouldBe` BT_ONE "40"
        _ -> expectationFailure "Expected PA_DELTA'"
      case pairMetaDelta of
        actual@PA_META_DELTA{} ->
          actual.meta `shouldBe` metaVal
        _ -> expectationFailure "Expected PA_META_DELTA"
      case pairMetaDelta' of
        actual@PA_META_DELTA'{} ->
          actual.meta `shouldBe` metaVal
        _ -> expectationFailure "Expected PA_META_DELTA'"
      shouldShowAndEqSelf "PA_TAU" pairTau
      shouldShowAndEqSelf "PA_ALPHA" pairAlpha
      shouldShowAndEqSelf "PA_FORMATION" pairFormation
      shouldShowAndEqSelf "PA_VOID" pairVoid
      shouldShowAndEqSelf "PA_LAMBDA" pairLambda
      shouldShowAndEqSelf "PA_LAMBDA'" pairLambda'
      shouldShowAndEqSelf "PA_META_LAMBDA" pairMetaLambda
      shouldShowAndEqSelf "PA_META_LAMBDA'" pairMetaLambda'
      shouldShowAndEqSelf "PA_DELTA" pairDelta
      shouldShowAndEqSelf "PA_DELTA'" pairDelta'
      shouldShowAndEqSelf "PA_META_DELTA" pairMetaDelta
      shouldShowAndEqSelf "PA_META_DELTA'" pairMetaDelta'

  describe "APP_BINDING field accessors" $
    it "exposes every APP_BINDING field via its accessor" $ do
      let pairTau = PA_TAU{attr = AT_LABEL "x", arrow = ARROW, expr = EX_GLOBAL Φ}
          appBinding = APP_BINDING{pair = pairTau}
      appBinding.pair `shouldBe` pairTau
      shouldShowAndEqSelf "APP_BINDING" appBinding

  describe "BINDING field accessors" $
    it "exposes every BINDING constructor's fields via their accessors" $ do
      let pairTau = PA_TAU{attr = AT_LABEL "x", arrow = ARROW, expr = EX_GLOBAL Φ}
          metaVal = META EXCL TAU "x"
          bindingsEmpty = BDS_EMPTY{tab = TAB 0}
          biPair = BI_PAIR{pair = pairTau, bindings = bindingsEmpty, tab = TAB 1}
          biMeta = BI_META{meta = metaVal, bindings = bindingsEmpty, tab = TAB 1}
      case biPair of
        actual@BI_PAIR{} -> do
          actual.pair `shouldBe` pairTau
          actual.bindings `shouldBe` bindingsEmpty
          actual.tab `shouldBe` TAB 1
        _ -> expectationFailure "Expected BI_PAIR"
      case biMeta of
        actual@BI_META{} -> do
          actual.meta `shouldBe` metaVal
          actual.bindings `shouldBe` bindingsEmpty
          actual.tab `shouldBe` TAB 1
        _ -> expectationFailure "Expected BI_META"
      shouldShowAndEqSelf "BI_PAIR" biPair
      shouldShowAndEqSelf "BI_META" biMeta

  describe "BINDINGS field accessors" $
    it "exposes every BINDINGS constructor's fields via their accessors" $ do
      let pairTau = PA_TAU{attr = AT_LABEL "x", arrow = ARROW, expr = EX_GLOBAL Φ}
          metaVal = META EXCL TAU "x"
          bindingsEmpty = BDS_EMPTY{tab = TAB 0}
          bdsPair = BDS_PAIR{eol = EOL, tab = TAB 1, pair = pairTau, bindings = bindingsEmpty}
          bdsMeta = BDS_META{eol = EOL, tab = TAB 1, meta = metaVal, bindings = bindingsEmpty}
      case bdsPair of
        actual@BDS_PAIR{} -> do
          actual.eol `shouldBe` EOL
          actual.tab `shouldBe` TAB 1
          actual.pair `shouldBe` pairTau
          actual.bindings `shouldBe` bindingsEmpty
        _ -> expectationFailure "Expected BDS_PAIR"
      case bdsMeta of
        actual@BDS_META{} -> do
          actual.eol `shouldBe` EOL
          actual.tab `shouldBe` TAB 1
          actual.meta `shouldBe` metaVal
          actual.bindings `shouldBe` bindingsEmpty
        _ -> expectationFailure "Expected BDS_META"
      shouldShowAndEqSelf "BDS_PAIR" bdsPair
      shouldShowAndEqSelf "BDS_META" bdsMeta

  describe "APP_ARG field accessors" $
    it "exposes every APP_ARG field via its accessor" $ do
      let exprGlobal = EX_GLOBAL Φ
          appArgsEmpty = AAS_EMPTY
          appArg = APP_ARG{expr = exprGlobal, args = appArgsEmpty}
      appArg.expr `shouldBe` exprGlobal
      appArg.args `shouldBe` appArgsEmpty
      shouldShowAndEqSelf "APP_ARG" appArg

  describe "APP_ARGS field accessors" $
    it "exposes every APP_ARGS constructor's fields via their accessors" $ do
      let exprGlobal = EX_GLOBAL Φ
          appArgsEmpty = AAS_EMPTY
          appArgsExpr = AAS_EXPR{eol = EOL, tab = TAB 1, expr = exprGlobal, args = appArgsEmpty}
      case appArgsExpr of
        actual@AAS_EXPR{} -> do
          actual.eol `shouldBe` EOL
          actual.tab `shouldBe` TAB 1
          actual.expr `shouldBe` exprGlobal
          actual.args `shouldBe` appArgsEmpty
        _ -> expectationFailure "Expected AAS_EXPR"
      shouldShowAndEqSelf "AAS_EXPR" appArgsExpr

  describe "APP_ARGUMENT derived instances" $
    it "derives Eq and Show for every APP_ARGUMENT constructor" $ do
      let pairTau = PA_TAU{attr = AT_LABEL "x", arrow = ARROW, expr = EX_GLOBAL Φ}
      shouldShowAndEqSelf "AA_TAU" (AA_TAU (APP_BINDING pairTau))
      shouldShowAndEqSelf "AA_TAUS" (AA_TAUS (BI_EMPTY (TAB 0)))
      shouldShowAndEqSelf "AA_EXPRS" (AA_EXPRS (APP_ARG (EX_GLOBAL Φ) AAS_EMPTY))

  describe "EXPRESSION field accessors" $
    it "exposes every EXPRESSION constructor's fields via their accessors" $ do
      let attrLabel = AT_LABEL "x"
          bindingEmpty = BI_EMPTY{tab = TAB 1}
          metaVal = META EXCL TAU "x"
          argumentVal = AA_EXPRS (APP_ARG (EX_GLOBAL Φ) AAS_EMPTY)
          exGlobal = EX_GLOBAL{global = Φ}
          exXi = EX_XI{xi = XI}
          exAttr = EX_ATTR{attr = attrLabel}
          exTermination = EX_TERMINATION{termination = DEAD}
          exFormation =
            EX_FORMATION
              { lsb = LSB
              , eol = EOL
              , tab = TAB 1
              , binding = bindingEmpty
              , eol' = EOL
              , tab' = TAB 0
              , rsb = RSB
              }
          exDispatch = EX_DISPATCH{expr = exGlobal, space = NO_SPACE, attr = attrLabel}
          exApplication =
            EX_APPLICATION
              { expr = exGlobal
              , space = NO_SPACE
              , eol = EOL
              , tab = TAB 1
              , argument = argumentVal
              , eol' = EOL
              , tab' = TAB 0
              , indent = 1
              }
          exString = EX_STRING{str = "hi", tab = TAB 0, rhos = []}
          exNumber = EX_NUMBER{num = 5, tab = TAB 0, rhos = []}
          exNonFinite = EX_NONFINITE{global = Φ, nonfinite = NfNan, tab = TAB 0, rhos = []}
          exMeta = EX_META{meta = metaVal}
          exPhiMeet = EX_PHI_MEET{prefix = Just "p", idx = 1, expr = exGlobal}
          exBytes = EX_BYTES{bytes = BT_ONE "40"}
      case exGlobal of
        actual@EX_GLOBAL{} ->
          actual.global `shouldBe` Φ
        _ -> expectationFailure "Expected EX_GLOBAL"
      case exXi of
        actual@EX_XI{} ->
          actual.xi `shouldBe` XI
        _ -> expectationFailure "Expected EX_XI"
      case exAttr of
        actual@EX_ATTR{} ->
          actual.attr `shouldBe` attrLabel
        _ -> expectationFailure "Expected EX_ATTR"
      case exTermination of
        actual@EX_TERMINATION{} ->
          actual.termination `shouldBe` DEAD
        _ -> expectationFailure "Expected EX_TERMINATION"
      case exFormation of
        actual@EX_FORMATION{} -> do
          actual.lsb `shouldBe` LSB
          actual.eol `shouldBe` EOL
          actual.tab `shouldBe` TAB 1
          actual.binding `shouldBe` bindingEmpty
          actual.eol' `shouldBe` EOL
          actual.tab' `shouldBe` TAB 0
          actual.rsb `shouldBe` RSB
        _ -> expectationFailure "Expected EX_FORMATION"
      case exDispatch of
        actual@EX_DISPATCH{} -> do
          actual.expr `shouldBe` exGlobal
          actual.space `shouldBe` NO_SPACE
          actual.attr `shouldBe` attrLabel
        _ -> expectationFailure "Expected EX_DISPATCH"
      case exApplication of
        actual@EX_APPLICATION{} -> do
          actual.expr `shouldBe` exGlobal
          actual.space `shouldBe` NO_SPACE
          actual.eol `shouldBe` EOL
          actual.tab `shouldBe` TAB 1
          actual.argument `shouldBe` argumentVal
          actual.eol' `shouldBe` EOL
          actual.tab' `shouldBe` TAB 0
          actual.indent `shouldBe` 1
        _ -> expectationFailure "Expected EX_APPLICATION"
      case exString of
        actual@EX_STRING{} -> do
          actual.str `shouldBe` "hi"
          actual.tab `shouldBe` TAB 0
          actual.rhos `shouldBe` []
        _ -> expectationFailure "Expected EX_STRING"
      case exNumber of
        actual@EX_NUMBER{} -> do
          actual.num `shouldBe` 5
          actual.tab `shouldBe` TAB 0
          actual.rhos `shouldBe` []
        _ -> expectationFailure "Expected EX_NUMBER"
      case exNonFinite of
        actual@EX_NONFINITE{} -> do
          actual.global `shouldBe` Φ
          actual.nonfinite `shouldBe` NfNan
          actual.tab `shouldBe` TAB 0
          actual.rhos `shouldBe` []
        _ -> expectationFailure "Expected EX_NONFINITE"
      case exMeta of
        actual@EX_META{} ->
          actual.meta `shouldBe` metaVal
        _ -> expectationFailure "Expected EX_META"
      case exPhiMeet of
        actual@EX_PHI_MEET{} -> do
          actual.prefix `shouldBe` Just "p"
          actual.idx `shouldBe` 1
          actual.expr `shouldBe` exGlobal
        _ -> expectationFailure "Expected EX_PHI_MEET"
      case exBytes of
        actual@EX_BYTES{} ->
          actual.bytes `shouldBe` BT_ONE "40"
        _ -> expectationFailure "Expected EX_BYTES"
      shouldShowAndEqSelf "EX_GLOBAL" exGlobal
      shouldShowAndEqSelf "EX_XI" exXi
      shouldShowAndEqSelf "EX_ATTR" exAttr
      shouldShowAndEqSelf "EX_TERMINATION" exTermination
      shouldShowAndEqSelf "EX_FORMATION" exFormation
      shouldShowAndEqSelf "EX_DISPATCH" exDispatch
      shouldShowAndEqSelf "EX_APPLICATION" exApplication
      shouldShowAndEqSelf "EX_STRING" exString
      shouldShowAndEqSelf "EX_NUMBER" exNumber
      shouldShowAndEqSelf "EX_NONFINITE" exNonFinite
      shouldShowAndEqSelf "EX_META" exMeta
      shouldShowAndEqSelf "EX_PHI_MEET" exPhiMeet
      shouldShowAndEqSelf "EX_BYTES" exBytes

  describe "ATTRIBUTE field accessors" $
    it "exposes every ATTRIBUTE constructor's fields via their accessors" $ do
      let metaVal = META EXCL TAU "x"
          atLabel = AT_LABEL{label = "x"}
          atRho = AT_RHO{rho = RHO}
          atPhi = AT_PHI{phi = PHI}
          atLambda = AT_LAMBDA{lambda = LAMBDA}
          atDelta = AT_DELTA{delta = DELTA}
          atMeta = AT_META{meta = metaVal}
          atRest = AT_REST{dots = DOTS}
      case atLabel of
        actual@AT_LABEL{} ->
          actual.label `shouldBe` "x"
        _ -> expectationFailure "Expected AT_LABEL"
      case atRho of
        actual@AT_RHO{} ->
          actual.rho `shouldBe` RHO
        _ -> expectationFailure "Expected AT_RHO"
      case atPhi of
        actual@AT_PHI{} ->
          actual.phi `shouldBe` PHI
        _ -> expectationFailure "Expected AT_PHI"
      case atLambda of
        actual@AT_LAMBDA{} ->
          actual.lambda `shouldBe` LAMBDA
        _ -> expectationFailure "Expected AT_LAMBDA"
      case atDelta of
        actual@AT_DELTA{} ->
          actual.delta `shouldBe` DELTA
        _ -> expectationFailure "Expected AT_DELTA"
      case atMeta of
        actual@AT_META{} ->
          actual.meta `shouldBe` metaVal
        _ -> expectationFailure "Expected AT_META"
      case atRest of
        actual@AT_REST{} ->
          actual.dots `shouldBe` DOTS
        _ -> expectationFailure "Expected AT_REST"
      shouldShowAndEqSelf "AT_LABEL" atLabel
      shouldShowAndEqSelf "AT_RHO" atRho
      shouldShowAndEqSelf "AT_PHI" atPhi
      shouldShowAndEqSelf "AT_LAMBDA" atLambda
      shouldShowAndEqSelf "AT_DELTA" atDelta
      shouldShowAndEqSelf "AT_META" atMeta
      shouldShowAndEqSelf "AT_REST" atRest

  describe "BELONGING derived instances" $
    it "derives Eq and Show for every BELONGING constructor" $ do
      shouldShowAndEqSelf "IN" IN
      shouldShowAndEqSelf "NOT_IN" NOT_IN

  describe "SET field accessors" $
    it "exposes every SET constructor's fields via their accessors" $ do
      let bindingEmpty = BI_EMPTY{tab = TAB 0}
          stBinding = ST_BINDING{binding = bindingEmpty}
          stAttributes = ST_ATTRIBUTES{attrs = [AT_LABEL "x"]}
      case stBinding of
        actual@ST_BINDING{} ->
          actual.binding `shouldBe` bindingEmpty
        _ -> expectationFailure "Expected ST_BINDING"
      case stAttributes of
        actual@ST_ATTRIBUTES{} ->
          actual.attrs `shouldBe` [AT_LABEL "x"]
        _ -> expectationFailure "Expected ST_ATTRIBUTES"
      shouldShowAndEqSelf "ST_BINDING" stBinding
      shouldShowAndEqSelf "ST_ATTRIBUTES" stAttributes

  describe "LOGIC_OPERATOR derived instances" $
    it "derives Eq and Show for every LOGIC_OPERATOR constructor" $ do
      shouldShowAndEqSelf "AND" AND
      shouldShowAndEqSelf "OR" OR

  describe "EQUAL derived instances" $
    it "derives Eq and Show for every EQUAL constructor" $ do
      shouldShowAndEqSelf "EQUAL" EQUAL
      shouldShowAndEqSelf "NOT_EQUAL" NOT_EQUAL
      shouldShowAndEqSelf "GREATER" GREATER
      shouldShowAndEqSelf "NOT_GREATER" NOT_GREATER

  describe "NUMBER field accessors" $
    it "exposes every NUMBER constructor's fields via their accessors" $ do
      let metaVal = META EXCL TAU "x"
          bindingEmpty = BI_EMPTY{tab = TAB 0}
          idxMeta = IDX_META{meta = metaVal}
          lengthVal = LENGTH{binding = bindingEmpty}
          literalVal = LITERAL{num = 5}
      case idxMeta of
        actual@IDX_META{} ->
          actual.meta `shouldBe` metaVal
        _ -> expectationFailure "Expected IDX_META"
      case lengthVal of
        actual@LENGTH{} ->
          actual.binding `shouldBe` bindingEmpty
        _ -> expectationFailure "Expected LENGTH"
      case literalVal of
        actual@LITERAL{} ->
          actual.num `shouldBe` 5
        _ -> expectationFailure "Expected LITERAL"
      shouldShowAndEqSelf "IDX_META" idxMeta
      shouldShowAndEqSelf "LENGTH" lengthVal
      shouldShowAndEqSelf "LITERAL" literalVal

  describe "COMPARABLE field accessors" $
    it "exposes every COMPARABLE constructor's fields via their accessors" $ do
      let attrLabel = AT_LABEL "x"
          exprGlobal = EX_GLOBAL Φ
          cmpAttr = CMP_ATTR{attr = attrLabel}
          cmpExpr = CMP_EXPR{expr = exprGlobal}
          cmpNum = CMP_NUM{num = LITERAL 5}
      case cmpAttr of
        actual@CMP_ATTR{} ->
          actual.attr `shouldBe` attrLabel
        _ -> expectationFailure "Expected CMP_ATTR"
      case cmpExpr of
        actual@CMP_EXPR{} ->
          actual.expr `shouldBe` exprGlobal
        _ -> expectationFailure "Expected CMP_EXPR"
      case cmpNum of
        actual@CMP_NUM{} ->
          actual.num `shouldBe` LITERAL 5
        _ -> expectationFailure "Expected CMP_NUM"
      shouldShowAndEqSelf "CMP_ATTR" cmpAttr
      shouldShowAndEqSelf "CMP_EXPR" cmpExpr
      shouldShowAndEqSelf "CMP_NUM" cmpNum

  describe "CONDITION field accessors" $
    it "exposes every CONDITION constructor's fields via their accessors" $ do
      let attrLabel = AT_LABEL "x"
          exprGlobal = EX_GLOBAL Φ
          bindingEmpty = BI_EMPTY{tab = TAB 0}
          cmpAttr = CMP_ATTR attrLabel
          coBelongs = CO_BELONGS{attr = attrLabel, belongs = IN, set = ST_BINDING bindingEmpty}
          coLogic = CO_LOGIC{conditions = [CO_EMPTY], operator = AND}
          coNf = CO_NF{expr = exprGlobal}
          coNot = CO_NOT{condition = CO_EMPTY}
          coCompare = CO_COMPARE{left = cmpAttr, equal = EQUAL, right = cmpAttr}
          coMatches = CO_MATCHES{regex = "x*", expr = exprGlobal}
          coPartOf = CO_PART_OF{expr = exprGlobal, binding = bindingEmpty}
          coDisjoint = CO_DISJOINT{attrs = [attrLabel], groups = [bindingEmpty]}
      case coBelongs of
        actual@CO_BELONGS{} -> do
          actual.attr `shouldBe` attrLabel
          actual.belongs `shouldBe` IN
          actual.set `shouldBe` ST_BINDING bindingEmpty
        _ -> expectationFailure "Expected CO_BELONGS"
      case coLogic of
        actual@CO_LOGIC{} -> do
          actual.conditions `shouldBe` [CO_EMPTY]
          actual.operator `shouldBe` AND
        _ -> expectationFailure "Expected CO_LOGIC"
      case coNf of
        actual@CO_NF{} ->
          actual.expr `shouldBe` exprGlobal
        _ -> expectationFailure "Expected CO_NF"
      case coNot of
        actual@CO_NOT{} ->
          actual.condition `shouldBe` CO_EMPTY
        _ -> expectationFailure "Expected CO_NOT"
      case coCompare of
        actual@CO_COMPARE{} -> do
          actual.left `shouldBe` cmpAttr
          actual.equal `shouldBe` EQUAL
          actual.right `shouldBe` cmpAttr
        _ -> expectationFailure "Expected CO_COMPARE"
      case coMatches of
        actual@CO_MATCHES{} -> do
          actual.regex `shouldBe` "x*"
          actual.expr `shouldBe` exprGlobal
        _ -> expectationFailure "Expected CO_MATCHES"
      case coPartOf of
        actual@CO_PART_OF{} -> do
          actual.expr `shouldBe` exprGlobal
          actual.binding `shouldBe` bindingEmpty
        _ -> expectationFailure "Expected CO_PART_OF"
      case coDisjoint of
        actual@CO_DISJOINT{} -> do
          actual.attrs `shouldBe` [attrLabel]
          actual.groups `shouldBe` [bindingEmpty]
        _ -> expectationFailure "Expected CO_DISJOINT"
      shouldShowAndEqSelf "CO_BELONGS" coBelongs
      shouldShowAndEqSelf "CO_LOGIC" coLogic
      shouldShowAndEqSelf "CO_NF" coNf
      shouldShowAndEqSelf "CO_NOT" coNot
      shouldShowAndEqSelf "CO_COMPARE" coCompare
      shouldShowAndEqSelf "CO_MATCHES" coMatches
      shouldShowAndEqSelf "CO_PART_OF" coPartOf
      shouldShowAndEqSelf "CO_DISJOINT" coDisjoint

  describe "EXTRA_ARG field accessors" $
    it "exposes every EXTRA_ARG constructor's fields via their accessors" $ do
      let attrLabel = AT_LABEL "x"
          exprGlobal = EX_GLOBAL Φ
          bindingEmpty = BI_EMPTY{tab = TAB 0}
          argExpr = ARG_EXPR{expr = exprGlobal}
          argAttr = ARG_ATTR{attr = attrLabel}
          argBinding = ARG_BINDING{binding = bindingEmpty}
          argBytes = ARG_BYTES{bytes = BT_ONE "40"}
      case argExpr of
        actual@ARG_EXPR{} ->
          actual.expr `shouldBe` exprGlobal
        _ -> expectationFailure "Expected ARG_EXPR"
      case argAttr of
        actual@ARG_ATTR{} ->
          actual.attr `shouldBe` attrLabel
        _ -> expectationFailure "Expected ARG_ATTR"
      case argBinding of
        actual@ARG_BINDING{} ->
          actual.binding `shouldBe` bindingEmpty
        _ -> expectationFailure "Expected ARG_BINDING"
      case argBytes of
        actual@ARG_BYTES{} ->
          actual.bytes `shouldBe` BT_ONE "40"
        _ -> expectationFailure "Expected ARG_BYTES"
      shouldShowAndEqSelf "ARG_EXPR" argExpr
      shouldShowAndEqSelf "ARG_ATTR" argAttr
      shouldShowAndEqSelf "ARG_BINDING" argBinding
      shouldShowAndEqSelf "ARG_BYTES" argBytes

  describe "EXTRA field accessors" $
    it "exposes every EXTRA field via its accessor" $ do
      let exprGlobal = EX_GLOBAL Φ
          extraArgExpr = ARG_EXPR exprGlobal
          extra = EXTRA{meta = extraArgExpr, func = "fn", args = [extraArgExpr]}
      extra.meta `shouldBe` extraArgExpr
      extra.func `shouldBe` "fn"
      extra.args `shouldBe` [extraArgExpr]
      shouldShowAndEqSelf "EXTRA" extra
  where
    shouldShowAndEqSelf :: (Eq node, Show node) => String -> node -> Expectation
    shouldShowAndEqSelf expectedName node = do
      show node `shouldContain` expectedName
      node `shouldBe` node
    isApplicationWithTaus :: EXPRESSION -> Bool
    isApplicationWithTaus EX_APPLICATION{argument = AA_TAUS{}} = True
    isApplicationWithTaus _ = False
    isApplicationWithExprs :: EXPRESSION -> Bool
    isApplicationWithExprs EX_APPLICATION{argument = AA_EXPRS{}} = True
    isApplicationWithExprs _ = False
