-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LaTeX (ruleToLaTeX, rulesToLaTeXDocument) where

import Data.Maybe (fromMaybe)
import qualified Yaml as Y

-- @todo #114:30min Implement LaTeX conversion for rules.
--  Convert Rule data structure to LaTeX inference rule format.
--  Each rule should be formatted as a LaTeX inference rule with
--  pattern, result, and optional conditions.
--  Tests must be added for LaTeX conversion logic.
ruleToLaTeX :: Y.Rule -> String
ruleToLaTeX rule = "\\rule{" ++ fromMaybe "unnamed" (Y.name rule) ++ "}\n"

-- @todo #114:30min Create LaTeX document wrapper.
--  Generate proper LaTeX document with tabular format for rules.
--  Each rule should be in its own tabular environment.
--  Include tests for document structure generation.
rulesToLaTeXDocument :: [Y.Rule] -> String
rulesToLaTeXDocument rules' = unlines
  [ "\\documentclass{article}",
    "\\usepackage{amsmath}",
    "\\begin{document}",
    unlines (map ruleToLaTeX rules'),
    "\\end{document}"
  ]