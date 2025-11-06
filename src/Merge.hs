{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Merge where

import AST
import Control.Exception (throwIO)
import Control.Exception.Base
import Deps (BuildTermFunc, Term (TeBindings))
import Matcher (substEmpty)
import Printer (printProgram)
import Text.Printf (printf)
import qualified Yaml as Y

newtype MergeContext = MergeContext
  { _buildTerm :: BuildTermFunc
  }

data MergeException
  = WrongProgramFormat {program :: Program}
  deriving (Exception)

instance Show MergeException where
  show WrongProgramFormat {..} =
    printf
      "Invalid program format, only programs with top level formations are supported for 'merge' command, given:\n"
      (printProgram program)

merge :: [Program] -> MergeContext -> IO Program
merge progs = merge' (reverse progs)

merge' :: [Program] -> MergeContext -> IO Program
merge' [prog@(Program (ExFormation bds))] _ = pure prog
merge' (prog@(Program (ExFormation bds)) : rest) ctx@MergeContext {..} = do
  Program (ExFormation bds') <- merge' rest ctx
  let bindings = map Y.ArgBinding (bds ++ bds')
  TeBindings joined <- _buildTerm "join" bindings substEmpty (Program (ExFormation []))
  pure (Program (ExFormation joined))
merge' (prog : rest) _ = throwIO (WrongProgramFormat prog)
