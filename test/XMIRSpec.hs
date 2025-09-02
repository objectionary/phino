{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIRSpec where

import Control.Exception (bracket)
import Control.Monad (filterM, forM_, unless)
import Data.Aeson
import Data.List (intercalate)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Misc (allPathsIn)
import Parser (parseProgramThrows)
import Pretty (PrintMode (SWEET))
import System.Directory (removeFile)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (makeRelative)
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, pendingWith, runIO, shouldBe, shouldThrow)
import XMIR (XmirContext (XmirContext), parseXMIRThrows, printXMIR, programToXMIR, xmirToPhi)

data ParsePack = ParsePack
  { failure :: Maybe Bool,
    xmir :: String,
    phi :: String
  }
  deriving (Generic, Show, FromJSON)

data PrintPack = PrintPack
  { phi :: String,
    xpaths :: [String]
  }
  deriving (Generic, Show, FromJSON)

parsePack :: FilePath -> IO ParsePack
parsePack = Yaml.decodeFileThrow

printPack :: FilePath -> IO PrintPack
printPack = Yaml.decodeFileThrow

-- Check if xmllint is available on the system
isXmllintAvailable :: Bool
isXmllintAvailable =
  let (exitCode, _, _) = unsafePerformIO (readProcessWithExitCode "xmllint" ["--version"] "")
   in (exitCode == ExitSuccess)

spec :: Spec
spec = do
  describe "XMIR parsing packs" $ do
    let resources = "test-resources/xmir-parsing-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- parsePack pth
          let ParsePack {phi = phi'} = pack
              xmir' = do
                doc <- parseXMIRThrows (xmir pack)
                xmirToPhi doc
          case failure pack of
            Just True -> xmir' `shouldThrow` anyException
            _ -> do
              xmir'' <- xmir'
              phi'' <- parseProgramThrows phi'
              xmir'' `shouldBe` phi''
      )

  describe "XMIR printing packs" $ do
    let resources = "test-resources/xmir-printing-packs"
        available = isXmllintAvailable
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth ->
          it (makeRelative resources pth) $
            if not available
              then pendingWith "The 'xmllint' is not available"
              else do
                pack <- printPack pth
                let PrintPack {phi = phi', xpaths = xpaths'} = pack
                prog <- parseProgramThrows phi'
                xmir' <- programToXMIR prog (XmirContext True True SWEET)
                let xml = printXMIR xmir'
                bracket
                  (openTempFile "." "xmirXXXXXX.tmp")
                  (\(fp, _) -> removeFile fp)
                  ( \(path, hTmp) -> do
                      hPutStr hTmp xml
                      hClose hTmp
                      failed <-
                        filterM
                          ( \xpath -> do
                              (code, _, _) <- readProcessWithExitCode "xmllint" ["--xpath", xpath, path] ""
                              pure (code /= ExitSuccess)
                          )
                          xpaths'
                      unless
                        (null failed)
                        (expectationFailure ("Failed xpaths:\n - " ++ intercalate "\n - " failed ++ "\nXMIR is:\n" ++ xml))
                  )
      )
