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

  describe "build valid CST with wrapped phiAgain{} " $ do
    let number = BaseObject "number"
        again = ExPhiAgain Nothing 1
        bts = BaseObject "bytes"
        bt = BiTau (AtAlpha 0)
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
      , ("again(number)(again(bytes)(again(bytes)))", app (again number) (bt (app (again bts) (bt (again form)))))
      ]
      ( \(desc, ex) -> it desc (toCST ex 0 EOL `shouldSatisfy` isCSTNumber)
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
