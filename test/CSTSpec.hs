{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CSTSpec (spec) where

import AST
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
import Parser (parseExpressionThrows)
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
        , EX_FORMATION
            LSB
            EOL
            (TAB 1)
            (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_GLOBAL Φ) NO_SPACE (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1))
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
        form = ExFormation [BiDelta (BtMany ["40", "18", "00", "00", "00", "00", "00", "00"]), BiVoid AtRho]
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
          parsed <- parseExpressionThrows (expression pack)
          render (withMargin defaultMargin (expressionToCST parsed)) `shouldBe` result pack
      )

  describe "converts to salty CST" $ do
    let resources = "test-resources/cst/to-salty-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          parsed <- parseExpressionThrows (expression pack)
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
          parsed <- parseExpressionThrows (expression pack)
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
          parsed <- parseExpressionThrows (expression pack)
          let cst = expressionToCST parsed
              ascii = withLineFormat SINGLELINE cst
          render ascii `shouldBe` result pack
      )

  describe "expressionToCSTFrom lays out a formation from a given base indent" $
    it "nests the body one level below the given tabs and closes at it" $
      expressionToCSTFrom 2 (ExFormation [BiTau (AtLabel "x") ExRoot])
        `shouldBe` EX_FORMATION
          LSB
          EOL
          (TAB 3)
          (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 3)) (TAB 3))
          EOL
          (TAB 2)
          RSB

  describe "sweetNumber" $ do
    it "is true for a finite integral value" (sweetNumber (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"]) `shouldBe` True)
    it "is true for a finite fractional value" (sweetNumber (BtMany ["BF", "D0", "00", "00", "00", "00", "00", "00"]) `shouldBe` True)
    it "is false for NaN" (sweetNumber (BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"]) `shouldBe` False)
    it "is false for positive infinity" (sweetNumber (BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"]) `shouldBe` False)
    it "is false for negative infinity" (sweetNumber (BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"]) `shouldBe` False)

  describe "sweetCollapsible" $ do
    it "delegates to sweetNumber for a data number" (sweetCollapsible (DataNumber (BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"])) `shouldBe` False)
    it "is true for a data number with a sweet literal" (sweetCollapsible (DataNumber (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])) `shouldBe` True)
    it "is true for any other expression" (sweetCollapsible ExXi `shouldBe` True)

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

  describe "expressionToCST on rendering-only and meta nodes" $ do
    it "ExBytes becomes a bare EX_BYTES chain node" (expressionToCST (ExBytes (BtOne "1F")) `shouldBe` EX_BYTES (BT_ONE "1F"))
    it "an n-prefixed ExMeta becomes a normal-form meta" (expressionToCST (ExMeta "nX") `shouldBe` EX_META (META NO_EXCL N "X"))
    it "a k-prefixed ExMeta becomes an absolute meta" (expressionToCST (ExMeta "kX") `shouldBe` EX_META (META NO_EXCL K "X"))
    it "any other ExMeta becomes an ordinary meta" (expressionToCST (ExMeta "eX") `shouldBe` EX_META (META NO_EXCL E "X"))
    it
      "ExPhiMeet keeps its prefix"
      (expressionToCST (ExPhiMeet (Just "p") 3 ExXi) `shouldBe` EX_PHI_MEET (Just "p") 3 (EX_XI XI))
    it
      "ExPhiAgain keeps its prefix"
      (expressionToCST (ExPhiAgain (Just "p") 3 ExXi) `shouldBe` EX_PHI_AGAIN (Just "p") 3 (EX_XI XI))

  describe "attributeToCST and bindingsToCST" $ do
    it "attributeToCST on a label" (attributeToCST (AtLabel "x") `shouldBe` AT_LABEL "x")
    it "attributeToCST on rho" (attributeToCST AtRho `shouldBe` AT_RHO RHO)
    it "bindingsToCST on an empty list" (bindingsToCST [] `shouldBe` BI_EMPTY (TAB 0))
    it
      "bindingsToCST on a single binding"
      (bindingsToCST [BiVoid (AtLabel "y")] `shouldBe` BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0))

  describe "conditionToCST on every Y.Condition constructor" $ do
    let voidYBinding :: BINDING
        voidYBinding = BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0)
    it
      "In"
      (conditionToCST (Y.In (AtLabel "x") (BiVoid (AtLabel "y"))) `shouldBe` CO_BELONGS (AT_LABEL "x") IN (ST_BINDING voidYBinding))
    it
      "Not (In ...) flips the belonging"
      (conditionToCST (Y.Not (Y.In (AtLabel "x") (BiVoid (AtLabel "y")))) `shouldBe` CO_BELONGS (AT_LABEL "x") NOT_IN (ST_BINDING voidYBinding))
    it
      "Eq"
      (conditionToCST (Y.Eq (Y.CmpAttr (AtLabel "x")) (Y.CmpNum (Y.Literal 3))) `shouldBe` CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_NUM (LITERAL 3)))
    it
      "Not (Eq ...) becomes a not-equal comparison"
      (conditionToCST (Y.Not (Y.Eq (Y.CmpAttr (AtLabel "x")) (Y.CmpNum (Y.Literal 3)))) `shouldBe` CO_COMPARE (CMP_ATTR (AT_LABEL "x")) NOT_EQUAL (CMP_NUM (LITERAL 3)))
    it
      "Gt"
      (conditionToCST (Y.Gt (Y.CmpAttr (AtLabel "x")) (Y.CmpNum (Y.Literal 3))) `shouldBe` CO_COMPARE (CMP_ATTR (AT_LABEL "x")) GREATER (CMP_NUM (LITERAL 3)))
    it
      "Not (Gt ...) becomes a not-greater comparison"
      (conditionToCST (Y.Not (Y.Gt (Y.CmpAttr (AtLabel "x")) (Y.CmpNum (Y.Literal 3)))) `shouldBe` CO_COMPARE (CMP_ATTR (AT_LABEL "x")) NOT_GREATER (CMP_NUM (LITERAL 3)))
    it "Absolute" (conditionToCST (Y.Absolute ExXi) `shouldBe` CO_ABSOLUTE (EX_XI XI) IN)
    it
      "Not (Absolute ...) flips membership"
      (conditionToCST (Y.Not (Y.Absolute ExXi)) `shouldBe` CO_ABSOLUTE (EX_XI XI) NOT_IN)
    it
      "Disjoint"
      (conditionToCST (Y.Disjoint [AtLabel "a"] [BiVoid (AtLabel "y")]) `shouldBe` CO_DISJOINT [AT_LABEL "a"] [voidYBinding])
    it "And on an empty list collapses to CO_EMPTY" (conditionToCST (Y.And []) `shouldBe` CO_EMPTY)
    it "And on a non-empty list wraps every condition" (conditionToCST (Y.And [Y.NF ExXi]) `shouldBe` CO_LOGIC [CO_NF (EX_XI XI)] AND)
    it "Or on an empty list collapses to CO_EMPTY" (conditionToCST (Y.Or []) `shouldBe` CO_EMPTY)
    it "Or on a non-empty list wraps every condition" (conditionToCST (Y.Or [Y.NF ExXi]) `shouldBe` CO_LOGIC [CO_NF (EX_XI XI)] OR)
    it "NF" (conditionToCST (Y.NF ExXi) `shouldBe` CO_NF (EX_XI XI))
    it
      "Not on any other condition falls back to a generic negation"
      (conditionToCST (Y.Not (Y.NF ExXi)) `shouldBe` CO_NOT (CO_NF (EX_XI XI)))
    it "Matches" (conditionToCST (Y.Matches "abc" ExXi) `shouldBe` CO_MATCHES "abc" (EX_XI XI))
    it "PartOf" (conditionToCST (Y.PartOf ExXi (BiVoid (AtLabel "y"))) `shouldBe` CO_PART_OF (EX_XI XI) voidYBinding)
    it "IsFormation" (conditionToCST (Y.IsFormation ExXi) `shouldBe` CO_FORMATION (EX_XI XI))

  describe "comparableToCST on every Y.Comparable constructor" $ do
    it "CmpAttr" (comparableToCST (Y.CmpAttr (AtLabel "x")) `shouldBe` CMP_ATTR (AT_LABEL "x"))
    it "CmpExpr" (comparableToCST (Y.CmpExpr ExXi) `shouldBe` CMP_EXPR (EX_XI XI))
    it "CmpNum" (comparableToCST (Y.CmpNum (Y.Literal 3)) `shouldBe` CMP_NUM (LITERAL 3))

  describe "numberToCST on every Y.Number constructor" $ do
    it "MetaIndex" (numberToCST (Y.MetaIndex "i1") `shouldBe` IDX_META (META NO_EXCL I "1"))
    it
      "Length"
      (numberToCST (Y.Length (BiVoid (AtLabel "y"))) `shouldBe` LENGTH (BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0)))
    it
      "Domain"
      (numberToCST (Y.Domain (BiVoid (AtLabel "y"))) `shouldBe` DOMAIN (BI_PAIR (PA_VOID (AT_LABEL "y") ARROW EMPTY) (BDS_EMPTY (TAB 0)) (TAB 0)))
    it "Literal" (numberToCST (Y.Literal 5) `shouldBe` LITERAL 5)

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
