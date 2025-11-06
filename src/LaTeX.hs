{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LaTeX (explainRules, rewrittensToLatex, programToLaTeX, LatexContext (..)) where

import AST (Program)
import CST
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Encoding (Encoding (ASCII), withEncoding)
import Lining (LineFormat (MULTILINE, SINGLELINE), withLineFormat)
import Printer (printProgram')
import Render (Render (render))
import Rewriter (Rewritten (..))
import Sugar (SugarType (SWEET), withSugarType)
import Text.Printf (printf)
import qualified Yaml as Y

data LatexContext = LatexContext
  { sugar :: SugarType,
    line :: LineFormat,
    nonumber :: Bool
  }

renderToLatex :: Program -> LatexContext -> String
renderToLatex prog LatexContext {..} = render (toLaTeX $ withLineFormat line $ withEncoding ASCII $ withSugarType sugar $ programToCST prog)

phiquation :: LatexContext -> String
phiquation LatexContext {nonumber = True} = "phiquation*"
phiquation LatexContext {nonumber = False} = "phiquation"

rewrittensToLatex :: [Rewritten] -> LatexContext -> String
rewrittensToLatex rewrittens ctx =
  let equation = phiquation ctx
   in concat
        [ printf "\\begin{%s}\n" equation,
          intercalate
            "\n  \\leadsto "
            ( map
                ( \Rewritten {..} ->
                    let prog = renderToLatex program ctx
                        unknown = "unknown"
                        maybeTo = case maybeRule of
                          Just (Y.Rule {..}) -> Just (map toLower (fromMaybe unknown name))
                          _ -> Nothing
                     in case maybeTo of
                          Just name -> printf "%s \\leadsto_{\\nameref{r:%s}}" prog name
                          _ -> prog
                )
                rewrittens
            ),
          printf "\n\\end{%s}" equation
        ]

programToLaTeX :: Program -> LatexContext -> String
programToLaTeX prog ctx =
  let equation = phiquation ctx
   in concat
        [ "\\begin{",
          equation,
          "}\n",
          renderToLatex prog ctx,
          "\n\\end{",
          equation,
          "}"
        ]

class ToLaTeX a where
  toLaTeX :: a -> a

instance ToLaTeX PROGRAM where
  toLaTeX PR_SWEET {..} = PR_SWEET (toLaTeX expr)
  toLaTeX PR_SALTY {..} = PR_SALTY global arrow (toLaTeX expr)

instance ToLaTeX EXPRESSION where
  toLaTeX EX_ATTR {..} = EX_ATTR (toLaTeX attr)
  toLaTeX EX_FORMATION {..} = EX_FORMATION lsb eol tab (toLaTeX binding) eol' tab' rsb
  toLaTeX EX_APPLICATION {..} = EX_APPLICATION (toLaTeX expr) eol tab (toLaTeX bindings) eol' tab'
  toLaTeX EX_APPLICATION' {..} = EX_APPLICATION' (toLaTeX expr) eol tab (toLaTeX args) eol' tab'
  toLaTeX EX_DISPATCH{..} = EX_DISPATCH (toLaTeX expr) (toLaTeX attr)
  toLaTeX expr = expr

instance ToLaTeX ATTRIBUTE where
  toLaTeX AT_LABEL {..} = AT_LABEL (piped (toLaTeX label))
    where
      piped :: String -> String
      piped str = "|" <> str <> "|"
  toLaTeX attr = attr

instance ToLaTeX BINDING where
  toLaTeX BI_PAIR {..} = BI_PAIR (toLaTeX pair) (toLaTeX bindings) tab
  toLaTeX bd = bd

instance ToLaTeX BINDINGS where
  toLaTeX BDS_PAIR {..} = BDS_PAIR eol tab (toLaTeX pair) (toLaTeX bindings)
  toLaTeX bds = bds

instance ToLaTeX PAIR where
  toLaTeX PA_DELTA {..} = PA_DELTA' bytes
  toLaTeX PA_LAMBDA {..} = PA_LAMBDA' func
  toLaTeX PA_VOID {..} = PA_VOID (toLaTeX attr) arrow void
  toLaTeX PA_TAU {..} = PA_TAU (toLaTeX attr) arrow (toLaTeX expr)
  toLaTeX pair = pair

instance ToLaTeX APP_ARG where
  toLaTeX APP_ARG {..} = APP_ARG (toLaTeX expr) (toLaTeX args)

instance ToLaTeX APP_ARGS where
  toLaTeX AAS_EXPR {..} = AAS_EXPR eol tab (toLaTeX expr) (toLaTeX args)
  toLaTeX args = args

instance ToLaTeX String where
  toLaTeX = escapeUnprintedChars
    where
      escapeUnprintedChars :: String -> String
      escapeUnprintedChars [] = []
      escapeUnprintedChars (ch : rest) = case ch of
        '$' -> "\\char36{}" <> escapeUnprintedChars rest
        '@' -> "\\char64{}" <> escapeUnprintedChars rest
        '^' -> "\\char94{}" <> escapeUnprintedChars rest
        _ -> ch : escapeUnprintedChars rest

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
