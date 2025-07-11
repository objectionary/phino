-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Term where
  
import Yaml
import Matcher
import Ast

data Term = TeExpression Expression | TeAttribute Attribute

type BuildTermFunc = String -> [ExtraArgument] -> Subst -> Program -> Maybe Term
