-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLITypesSpec (spec) where

import CLI.Types
import Control.Monad (forM_)
import Test.Hspec

spec :: Spec
spec = do
  describe "Show CmdException" $
    forM_
      [ ("InvalidCLIArguments", show (InvalidCLIArguments "bad args"), "Invalid set of arguments: bad args")
      ,
        ( "CouldNotReadFromStdin"
        , show (CouldNotReadFromStdin "reason")
        , "Could not read input from stdin\nReason: reason"
        )
      , ("CouldNotDataize", show CouldNotDataize, "Could not dataize given expression")
      ,
        ( "CouldNotPrintExpressionInXMIR"
        , show CouldNotPrintExpressionInXMIR
        , "Could not print expression with --output=xmir, only expression printing is allowed"
        )
      ,
        ( "EmptySubstsOnMatch"
        , show EmptySubstsOnMatch
        , "Provided pattern was not matched, no substitutions are built"
        )
      ,
        ( "VersionMismatch"
        , show (VersionMismatch "1.0.0" "2.0.0")
        , "Version mismatch: --pin requires '1.0.0', but this is phino 2.0.0"
        )
      ]
      (\(name, actual, expected) -> it ("shows " ++ name) (actual `shouldBe` expected))

  describe "Show IOFormat" $
    forM_
      [ (XMIR, "xmir")
      , (PHI, "phi")
      , (LATEX, "latex")
      ]
      (\(format, expected) -> it ("shows " ++ show format) (show format `shouldBe` expected))
