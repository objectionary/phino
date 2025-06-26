-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Printer where

import Ast
import Pretty (prettyProgram', PrintMode)
import XMIR (programToXMIR, printXMIR)

data PrintFormat = XMIR | PHI
  deriving (Eq)

instance Show PrintFormat where
  show XMIR = "xmir"
  show PHI = "phi"

printProgram :: Program -> PrintFormat -> PrintMode -> Bool -> IO String
printProgram prog PHI mode _ = pure (prettyProgram' prog mode)
printProgram prog XMIR mode omitListing = do
  xmir <- programToXMIR prog mode omitListing
  pure (printXMIR xmir)
