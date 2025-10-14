-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The main goal of this module is breaking cyclic dependency:
-- Dataize -> Functions -> Rewriter -> Dataize
-- Here we provide custom type BuildTermFunc and add it to
-- RewriteContext and DataizeContext. Now Dataize and Rewrite depends
-- only on Term module. This allows us to use Rewriter and Dataize in
-- Functions module because Rewriter does not depend on Functions anymore.
module Deps where

import Ast
import Logger (logDebug)
import Matcher
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import Text.Printf (printf)
import Yaml

data Term
  = TeExpression Expression
  | TeAttribute Attribute
  | TeBytes Bytes
  | TeBindings [Binding]

type BuildTermMethod = [ExtraArgument] -> Subst -> Program -> IO Term

type BuildTermFunc = String -> BuildTermMethod

type SaveStepFunc = Program -> Integer -> IO ()

saveStep :: Maybe FilePath -> String -> (Program -> IO String) -> SaveStepFunc
saveStep Nothing _ _ _ _ = pure ()
saveStep (Just dir) ext print' prog step = do
  createDirectoryIfMissing True dir
  let path = dir </> printf "%05d.%s" step ext
  content <- print' prog
  writeFile path content
  logDebug (printf "Saved step '%d' to '%s'" step path)

dontSaveStep :: SaveStepFunc
dontSaveStep = saveStep Nothing "" (\_ -> pure "")
