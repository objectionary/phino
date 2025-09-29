-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LaTeX (explainRules, programToLaTeX) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Yaml as Y
import Ast (Program)
import Pretty (PrintMode, prettyProgram', Encoding (ASCII))

programToLaTeX :: Program -> PrintMode -> String
programToLaTeX prog mode = unlines
  [
    "\\documentclass{article}",
    "\\usepackage{eolang}",
    "\\begin{document}",
    "\\begin{phiquation}",
    prettyProgram' prog mode ASCII,
    "\\end{phiquation}",
    "\\end{document}"
  ]

-- @todo #114:30min Implement LaTeX conversion for rules.
--  Convert Rule data structure to LaTeX inference rule format.
--  Each rule should be formatted as a LaTeX inference rule with
--  pattern, result, and optional conditions.
--  Tests must be added for LaTeX conversion logic.
explainRule :: Y.Rule -> String
explainRule rule = "\\rule{" ++ fromMaybe "unnamed" (Y.name rule) ++ "}"

-- @todo #114:30min Create LaTeX document wrapper.
--  Generate proper LaTeX document with tabular format for rules.
--  Each rule should be   in its own tabular environment.
--  Include tests for document structure generation.
explainRules :: [Y.Rule] -> String
explainRules rules' =
  unlines
    [ "\\documentclass{article}",
      "\\usepackage{amsmath}",
      "\\begin{document}"
    ]
    ++ unlines (map explainRule rules')
    ++ "\\end{document}"
