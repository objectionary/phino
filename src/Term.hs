-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The main goal of this module is breaking cyclic dependency:
-- Dataize -> Functions -> Rewriter -> Dataize
-- Here we provide custom type BuildTermFunc and add it to
-- RewriteContext and DataizeContext. Now Dataize and Rewrite depends
-- only on Term module. This allows us to use Rewriter and Dataize in
-- Functions module because Rewriter does not depend on Functions anymore.
module Term where
  
import Yaml
import Matcher
import Ast

data Term = TeExpression Expression | TeAttribute Attribute | TeBytes Bytes

type BuildTermMethod = [ExtraArgument] -> Subst -> Program -> IO Term

type BuildTermFunc = String -> BuildTermMethod
