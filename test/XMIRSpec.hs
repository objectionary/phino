-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIRSpec where

import Test.Hspec (Spec, it, shouldBe, runIO, pending)
import XMIR
import Misc (ensuredFile)
import Parser (parseProgramThrows)
import qualified Data.Text as T
import Pretty (PrintMode(SALTY))
import Control.Monad (when)

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
  
  it "omits redundant alpha attributes in XMIR" $ do
    let phiCode = "Q -> [[org -> [[eolang -> [[version -> Q.org.eolang.number(α0 -> Q.org.eolang.bytes(α0 -> \"test\")), λ ⤍ Package]], λ ⤍ Package]]]]"
    program <- parseProgramThrows phiCode
    document <- programToXMIR program SALTY
    let xmlOutput = printXMIR document
    -- Check that alpha attributes are omitted - there should be no 'as="α0"' in the output
    xmlOutput `shouldNotContain` "as=\"α0\""
    -- Check that the structure is preserved with proper base attributes
    xmlOutput `shouldContain` "base=\"Q.org.eolang.number\""
    xmlOutput `shouldContain` "base=\"Q.org.eolang.bytes\""

shouldNotContain :: String -> String -> IO ()
shouldNotContain haystack needle = 
  when (T.pack needle `T.isInfixOf` T.pack haystack) $
    fail $ "Expected not to contain: " ++ needle

shouldContain :: String -> String -> IO ()  
shouldContain haystack needle =
  if T.pack needle `T.isInfixOf` T.pack haystack
    then return ()
    else fail $ "Expected to contain: " ++ needle
