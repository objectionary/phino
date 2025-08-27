{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIRSpec where

import Control.Monad (forM_)
import Data.Aeson
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Misc (allPathsIn)
import Parser (parseProgramThrows)
import System.FilePath (makeRelative)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldThrow, anyException)
import XMIR (parseXMIRThrows, xmirToPhi)

data XMIRPack = XMIRPack
  { failure :: Maybe Bool,
    xmir :: String,
    phi :: String
  }
  deriving (Generic, Show, FromJSON)

xmirPack :: FilePath -> IO XMIRPack
xmirPack = Yaml.decodeFileThrow

-- @todo #126:30min Introduce XMIR printing test. It's not possible anymore to compare XMIRs like strings
--  because they contain random data, e.g. system time. We need to introduce some convenient
--  test system for testing XML and use it here here.
spec :: Spec
spec =
  describe "XMIR parsing packs" $ do
    let resources = "test-resources/xmir-parsing-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- xmirPack pth
          let xmir' = do
                doc <- parseXMIRThrows (xmir pack)
                xmirToPhi doc
          case failure pack of
            Just True -> xmir' `shouldThrow` anyException
            _ -> do
              xmir'' <- xmir'
              phi' <- parseProgramThrows (phi pack)
              xmir'' `shouldBe` phi'
      )
