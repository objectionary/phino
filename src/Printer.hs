-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Printer where

import Ast
import Pretty (prettyProgram)
import XMIR (programToXMIR, printXMIR)

data PrintFormat = XMIR | PHI
  deriving (Eq)

instance Show PrintFormat where
  show XMIR = "xmir"
  show PHI = "phi"

printProgram :: Program -> PrintFormat -> IO String
printProgram prog PHI = pure (prettyProgram prog)
printProgram prog XMIR = do
  xmir <- programToXMIR prog
  pure (printXMIR xmir)
