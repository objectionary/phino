-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIRSpec where

import Test.Hspec (Spec, it, shouldBe, runIO, pending)
import XMIR
import Misc (ensuredFile)
import Parser (parseProgramThrows)
import qualified Data.Text as T
import Pretty (PrintMode(SALTY))

-- @todo #126:30min Enable XMIR test. It's not possible anymore to compare XMIRs like strings
--  because they contain random data, e.g. system time. We need to introduce some convenient
--  test system for testing XML and use it here here.
spec :: Spec
spec = do
  phi <- runIO $ readFile =<< ensuredFile "test-resources/xmir/program.phi"
  xmir <- runIO $ readFile =<< ensuredFile "test-resources/xmir/program.xmir"
  prog <- runIO (parseProgramThrows phi)
  doc <- runIO $ programToXMIR prog SALTY
  let xmir' = printXMIR doc
  it "prints valid xmir" $ do
    pending
    T.stripEnd (T.pack xmir) `shouldBe` T.stripEnd (T.pack xmir')
