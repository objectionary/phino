{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CSTSpec (spec) where

import AST
import CST
import Control.Monad (forM_)
import Data.Aeson
import Data.Yaml qualified as Yaml
import Encoding (Encoding (ASCII), withEncoding)
import GHC.Generics (Generic)
import Lining (LineFormat (SINGLELINE), withLineFormat)
import Misc
import Parser (parseProgramThrows)
import Render (Render (render))
import Sugar
import System.FilePath
import Test.Hspec

data CSTPack = CSTPack
  { program :: String
  , result :: String
  }
  deriving (Generic, Show, FromJSON)

cstPack :: FilePath -> IO CSTPack
cstPack = Yaml.decodeFileThrow

spec :: Spec
spec = do
  describe "builds valid CST" $
    forM_
      [ ("Q -> Q", PR_SWEET LCB (EX_GLOBAL Φ) RCB)
      ,
        ( "{[[ x -> Q.y ]]}"
        , PR_SWEET
            LCB
            ( EX_FORMATION
                LSB
                EOL
                (TAB 1)
                (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_GLOBAL Φ) (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1))
                EOL
                (TAB 0)
                RSB
            )
            RCB
        )
      ]
      ( \(prog, cst) -> it prog $ do
          ast <- parseProgramThrows prog
          programToCST ast `shouldBe` cst
      )

  describe "CST printing packs" $ do
    let resources = "test-resources/cst/printing-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          prog <- parseProgramThrows (program pack)
          render (programToCST prog) `shouldBe` result pack
      )

  describe "converts to salty CST" $ do
    let resources = "test-resources/cst/to-salty-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          prog <- parseProgramThrows (program pack)
          let cst = programToCST prog
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
          prog <- parseProgramThrows (program pack)
          let cst = programToCST prog
              ascii = withEncoding ASCII cst
          render ascii `shouldBe` result pack
      )

  describe "converts to singleline CST" $ do
    let resources = "test-resources/cst/to-singleline-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          prog <- parseProgramThrows (program pack)
          let cst = programToCST prog
              ascii = withLineFormat SINGLELINE cst
          render ascii `shouldBe` result pack
      )

  describe "expressionToCST" $
    forM_
      [ ("ExGlobal", ExGlobal, EX_GLOBAL Φ)
      , ("ExThis", ExThis, EX_XI XI)
      , ("ExTermination", ExTermination, EX_TERMINATION DEAD)
      , ("ExMeta", ExMeta "!expr", EX_META (MT_EXPRESSION "expr"))
      , ("ExMetaTail", ExMetaTail ExGlobal "!tail", EX_META_TAIL (EX_GLOBAL Φ) (MT_TAIL "tail"))
      , ("ExPhiMeet with prefix", ExPhiMeet (Just "pfx") 42 ExGlobal, EX_PHI_MEET (Just "pfx") 42 (EX_GLOBAL Φ))
      , ("ExPhiMeet without prefix", ExPhiMeet Nothing 7 ExThis, EX_PHI_MEET Nothing 7 (EX_XI XI))
      , ("ExPhiAgain with prefix", ExPhiAgain (Just "pfx") 42 ExGlobal, EX_PHI_AGAIN (Just "pfx") 42 (EX_GLOBAL Φ))
      , ("ExPhiAgain without prefix", ExPhiAgain Nothing 7 ExThis, EX_PHI_AGAIN Nothing 7 (EX_XI XI))
      , ("empty ExFormation", ExFormation [], EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY NO_TAB) NO_EOL NO_TAB RSB)
      , ("ExFormation with only void rho", ExFormation [BiVoid AtRho], EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY NO_TAB) NO_EOL NO_TAB RSB)
      , ("ExDispatch with ExThis", ExDispatch ExThis (AtLabel "foo"), EX_ATTR (AT_LABEL "foo"))
      , ("dispatch to default package", ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"), EX_DEF_PACKAGE Φ̇)
      ]
      ( \(desc, input, expected) ->
          it desc $ expressionToCST input `shouldBe` expected
      )

  describe "toCST converts Bytes" $
    forM_
      [ ("empty", BtEmpty, BT_EMPTY)
      , ("one byte", BtOne "FF", BT_ONE "FF")
      , ("many bytes", BtMany ["00", "01", "02"], BT_MANY ["00", "01", "02"])
      , ("meta bytes", BtMeta "!bts", BT_META (MT_BYTES "bts"))
      ]
      ( \(desc, input, expected) ->
          it desc $ (toCST input 0 EOL :: BYTES) `shouldBe` expected
      )

  describe "toCST converts Attribute" $
    forM_
      [ ("label", AtLabel "xyz", AT_LABEL "xyz")
      , ("alpha", AtAlpha 5, AT_ALPHA ALPHA 5)
      , ("phi", AtPhi, AT_PHI PHI)
      , ("rho", AtRho, AT_RHO RHO)
      , ("delta", AtDelta, AT_DELTA DELTA)
      , ("lambda", AtLambda, AT_LAMBDA LAMBDA)
      , ("meta", AtMeta "!attr", AT_META (MT_ATTRIBUTE "attr"))
      ]
      ( \(desc, input, expected) ->
          it desc $ (toCST input 0 EOL :: ATTRIBUTE) `shouldBe` expected
      )

  describe "toCST converts BiLambda" $
    it "produces PA_LAMBDA" $
      (toCST (BiLambda "MyFunc") 0 EOL :: PAIR) `shouldBe` PA_LAMBDA "MyFunc"

  describe "toCST converts BiMetaLambda" $
    it "produces PA_META_LAMBDA" $
      (toCST (BiMetaLambda "!func") 0 EOL :: PAIR) `shouldBe` PA_META_LAMBDA (MT_FUNCTION "func")

  describe "toCST converts BiDelta" $
    forM_
      [ ("with empty bytes", BiDelta BtEmpty, PA_DELTA BT_EMPTY)
      , ("with one byte", BiDelta (BtOne "AB"), PA_DELTA (BT_ONE "AB"))
      , ("with many bytes", BiDelta (BtMany ["01", "02"]), PA_DELTA (BT_MANY ["01", "02"]))
      ]
      ( \(desc, input, expected) ->
          it desc $ (toCST input 0 EOL :: PAIR) `shouldBe` expected
      )

  describe "toCST converts BiVoid" $
    it "produces PA_VOID with EMPTY" $
      (toCST (BiVoid (AtLabel "x")) 0 EOL :: PAIR) `shouldBe` PA_VOID (AT_LABEL "x") ARROW EMPTY

  describe "toCST converts binding list to BINDING" $
    forM_
      [ ("empty list", [], BI_EMPTY (TAB 0))
      , ("with meta", [BiMeta "!B"], BI_META (MT_BINDING "B") (BDS_EMPTY (TAB 0)) (TAB 0))
      ]
      ( \(desc, input, expected) ->
          it desc $ (toCST input 0 EOL :: BINDING) `shouldBe` expected
      )

  describe "toCST converts binding list to BINDINGS" $
    forM_
      [ ("empty list", [], BDS_EMPTY (TAB 0))
      , ("with meta", [BiMeta "!B"], BDS_META EOL (TAB 0) (MT_BINDING "B") (BDS_EMPTY (TAB 0)))
      ]
      ( \(desc, input, expected) ->
          it desc $ (toCST input 0 EOL :: BINDINGS) `shouldBe` expected
      )

  describe "inlinedEOL returns correct EOL" $
    forM_
      [ ("True gives NO_EOL", True, NO_EOL)
      , ("False gives EOL", False, EOL)
      ]
      ( \(desc, input, expected) ->
          it desc $ inlinedEOL input `shouldBe` expected
      )

  describe "tabOfEOL returns correct TAB" $
    forM_
      [ ("EOL gives TAB with indent", EOL, 3, TAB 3)
      , ("NO_EOL gives TAB'", NO_EOL, 5, TAB')
      ]
      ( \(desc, ln, lvl, expected) ->
          it desc $ tabOfEOL ln lvl `shouldBe` expected
      )

  describe "hasEOL returns False for empty binding list" $
    it "empty list has no EOL" $
      hasEOL ([] :: [Binding]) `shouldBe` False

  describe "hasEOL returns False for empty expression list" $
    it "empty list has no EOL" $
      hasEOL ([] :: [Expression]) `shouldBe` False

  describe "hasEOL returns False for ExGlobal" $
    it "global has no EOL" $
      hasEOL ExGlobal `shouldBe` False

  describe "hasEOL returns False for ExThis" $
    it "this has no EOL" $
      hasEOL ExThis `shouldBe` False

  describe "hasEOL returns False for ExTermination" $
    it "termination has no EOL" $
      hasEOL ExTermination `shouldBe` False

  describe "hasEOL returns False for empty ExFormation" $
    it "empty formation has no EOL" $
      hasEOL (ExFormation []) `shouldBe` False

  describe "hasEOL returns True for non-empty ExFormation" $
    it "formation with binding has EOL" $
      hasEOL (ExFormation [BiVoid AtRho]) `shouldBe` True

  describe "hasEOL returns True for dispatch with formation" $
    it "dispatch on formation has EOL" $
      hasEOL (ExDispatch (ExFormation [BiVoid AtRho]) (AtLabel "x")) `shouldBe` True

  describe "hasEOL returns True for application with formation" $
    it "application with formation has EOL" $
      hasEOL (ExApplication (ExFormation [BiVoid AtRho]) (BiVoid AtPhi)) `shouldBe` True

  describe "hasEOL returns True for binding with formation" $
    it "tau binding with formation has EOL" $
      hasEOL (BiTau AtRho (ExFormation [BiVoid AtPhi])) `shouldBe` True

  describe "hasEOL returns False for BiVoid binding" $
    it "void binding has no EOL" $
      hasEOL (BiVoid AtRho) `shouldBe` False

  describe "hasEOL returns False for BiDelta binding" $
    it "delta binding has no EOL" $
      hasEOL (BiDelta BtEmpty) `shouldBe` False

  describe "hasEOL returns True for expression list with formation" $
    it "list with formation has EOL" $
      hasEOL [ExGlobal, ExFormation [BiVoid AtRho]] `shouldBe` True

  describe "hasEOL returns True for binding list with formation" $
    it "list with formation has EOL" $
      hasEOL [BiTau AtRho (ExFormation [BiVoid AtPhi])] `shouldBe` True
