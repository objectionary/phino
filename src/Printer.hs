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

printProgram :: Program -> PrintFormat -> PrintMode -> IO String
printProgram prog PHI mode = pure (prettyProgram' prog mode)
printProgram prog XMIR mode = do
  xmir <- programToXMIR prog mode
  pure (printXMIR xmir)
