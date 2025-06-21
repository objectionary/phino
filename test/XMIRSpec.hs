-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIRSpec where

import Test.Hspec (Spec, it, shouldBe, runIO)
import XMIR
import Misc (ensuredFile)
import Parser (parseProgramThrows)
import qualified Data.Text as T

spec :: Spec
spec = do
  phi <- runIO $ readFile =<< ensuredFile "test-resources/xmir/program.phi"
  xmir <- runIO $ readFile =<< ensuredFile "test-resources/xmir/program.xmir"
  prog <- runIO (parseProgramThrows phi)
  doc <- runIO $ programToXMIR prog
  let xmir' = printXMIR doc
  it "prints valid xmir" $ T.stripEnd (T.pack xmir) `shouldBe` T.stripEnd (T.pack xmir')
