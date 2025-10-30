{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CSTSpec (spec) where

import CST
import Control.Monad (forM_)
import Data.Aeson
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Misc
import Parser (parseProgramThrows)
import System.FilePath
import Test.Hspec
import Encoding (ToASCII(toASCII))
import Lining (ToSingleLine(toSingleLine))

data CSTPack = CSTPack
  { program :: String,
    result :: String
  }
  deriving (Generic, Show, FromJSON)

cstPack :: FilePath -> IO CSTPack
cstPack = Yaml.decodeFileThrow

spec :: Spec
spec = do
  describe "builds valid CST" $
    forM_
      [ ("Q -> Q", PR_SWEET (EX_GLOBAL Φ)),
        ( "{[[ x -> Q.y ]]}",
          PR_SWEET
            ( EX_FORMATION
                LSB
                EOL
                (TAB 1)
                (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_GLOBAL Φ) (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1))
                EOL
                (TAB 0)
                RSB
            )
        )
      ]
      ( \(prog, cst) -> it prog $ do
          ast <- parseProgramThrows prog
          astToCst ast `shouldBe` cst
      )

  describe "CST printing packs" $ do
    let resources = "test-resources/cst/printing-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          prog <- parseProgramThrows (program pack)
          pretty (astToCst prog) `shouldBe` result pack
      )

  describe "converts to salty CST" $ do
    let resources = "test-resources/cst/to-salty-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          prog <- parseProgramThrows (program pack)
          let cst = astToCst prog
              salty = toSalty cst
          pretty salty `shouldBe` result pack
      )

  describe "converts to ascii CST" $ do
    let resources = "test-resources/cst/to-ascii-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          prog <- parseProgramThrows (program pack)
          let cst = astToCst prog
              ascii = toASCII cst
          pretty ascii `shouldBe` result pack
      )

  describe "converts to singleline CST" $ do
    let resources = "test-resources/cst/to-singleline-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- cstPack pth
          prog <- parseProgramThrows (program pack)
          let cst = astToCst prog
              ascii = toSingleLine cst
          pretty ascii `shouldBe` result pack
      )
