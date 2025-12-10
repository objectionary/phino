{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LaTeX
  ( explainRules
  , rewrittensToLatex
  , programToLaTeX
  , defaultLatexContext
  , LatexContext (..)
  , meetInPrograms
  , meetInProgram
  ) where

import AST
import CST
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import Encoding (Encoding (ASCII), withEncoding)
import Lining (LineFormat (MULTILINE, SINGLELINE), withLineFormat)
import Matcher
import Misc
import Printer (printProgram')
import Render (Render (render))
import Replacer (ReplaceContext (ReplaceCtx), replaceProgram)
import Rewriter (Rewritten (..))
import Sugar (SugarType (SWEET), withSugarType)
import Text.Printf (printf)
import qualified Yaml as Y

data LatexContext = LatexContext
  { sugar :: SugarType
  , line :: LineFormat
  , nonumber :: Bool
  , compress :: Bool
  , expression :: Maybe String
  , label :: Maybe String
  }

defaultLatexContext :: LatexContext
defaultLatexContext = LatexContext SWEET MULTILINE False False Nothing Nothing

meetInProgram :: Program -> Program -> [Expression]
meetInProgram (Program expr) = meetInExpression expr
  where
    meetInExpression :: Expression -> Program -> [Expression]
    meetInExpression ExGlobal _ = []
    meetInExpression ExThis _ = []
    meetInExpression ExTermination _ = []
    meetInExpression (ExFormation [BiVoid AtRho]) _ = []
    meetInExpression (ExFormation [BiDelta _, BiVoid AtRho]) _ = []
    meetInExpression (ExFormation []) _ = []
    meetInExpression (ExDispatch ExGlobal _) _ = []
    meetInExpression (ExDispatch ExThis _) _ = []
    meetInExpression (ExDispatch ExTermination _) _ = []
    meetInExpression (DataString _) _ = []
    meetInExpression (DataNumber _) _ = []
    meetInExpression (ExPhiMeet _ _) _ = []
    meetInExpression (ExPhiAgain _ _) _ = []
    meetInExpression expr prog =
      map (const expr) (matchProgram expr prog) ++ case expr of
        ExDispatch exp _ -> meetInExpression exp prog
        ExApplication exp (BiTau _ arg) -> meetInExpression exp prog ++ meetInExpression arg prog
        ExFormation bds -> meetInBindings bds prog
        _ -> []
    meetInBindings :: [Binding] -> Program -> [Expression]
    meetInBindings [] _ = []
    meetInBindings (BiTau _ expr : bds) prog = meetInExpression expr prog ++ meetInBindings bds prog
    meetInBindings (_ : bds) prog = meetInBindings bds prog

{- | Here we're trying to compress sequence of programs with \phiMeet{} and \phiAgain LaTeX functions.
We process the sequence of programs and trying to find all expressions in first program which are present
in following programs. Then we find ONE expression which is the most frequently encountered.
If it's encountered in more than 50% of following programs - we replace it with \phiAgain{} in following
programs and with \phiMeet{} in first program.
-}
meetInPrograms :: [Program] -> [Program]
meetInPrograms = meetInPrograms' 1
  where
    meetInPrograms' :: Int -> [Program] -> [Program]
    meetInPrograms' _ [prog] = [prog]
    meetInPrograms' idx progs@(first : rest) =
      let met = map (meetInProgram first) rest
          unique = nub (concat met)
          (frequent, _) =
            foldl
              ( \(best, count) cur ->
                  let len = length (filter (elem cur) met)
                   in if len > count
                        then (Just cur, len)
                        else (best, count)
              )
              (Nothing, 0)
              unique
          next = first : meetInPrograms' idx rest
       in case frequent of
            Just expr ->
              let met' = map (filter (== expr)) met
                  substs = matchProgram expr first
                  prog = replaceProgram (first, map (const expr) substs, map (const (ExPhiMeet idx)) substs)
                  rest' = zipWith (\prgm exprs -> replaceProgram (prgm, exprs, map (const (ExPhiAgain idx)) exprs)) rest met'
                  found = filter (not . null) met'
               in if length met' > 1 && toDouble (length found) / toDouble (length met') >= 0.5
                    then prog : meetInPrograms' (idx + 1) rest'
                    else next
            _ -> next

renderToLatex :: Program -> LatexContext -> String
renderToLatex prog LatexContext{..} = render (toLaTeX $ withLineFormat line $ withEncoding ASCII $ withSugarType sugar $ programToCST prog)

phiquation :: LatexContext -> String
phiquation LatexContext{nonumber = True} = "phiquation*"
phiquation LatexContext{nonumber = False} = "phiquation"

rewrittensToLatex :: [Rewritten] -> LatexContext -> String
rewrittensToLatex rewrittens ctx@LatexContext{..} =
  let equation = phiquation ctx
      (progs, rules) = unzip rewrittens
      rewrittens' = if compress then zip (meetInPrograms progs) rules else rewrittens
   in concat
        [ printf "\\begin{%s}\n" equation
        , maybe "" (printf "\\label{%s}\n") label
        , maybe "" (printf "\\phiExpression{%s} ") expression
        , intercalate
            "\n  \\leadsto "
            ( map
                ( \(program, maybeName) ->
                    let prog = renderToLatex program ctx
                     in maybe prog (printf "%s \\leadsto_{\\nameref{r:%s}}" prog) maybeName
                )
                rewrittens'
            )
        , printf ".\n\\end{%s}" equation
        ]

programToLaTeX :: Program -> LatexContext -> String
programToLaTeX prog ctx =
  let equation = phiquation ctx
   in concat
        [ "\\begin{"
        , equation
        , "}\n"
        , renderToLatex prog ctx
        , "\n\\end{"
        , equation
        , "}"
        ]

piped :: String -> String
piped str = "|" <> str <> "|"

class ToLaTeX a where
  toLaTeX :: a -> a

instance ToLaTeX PROGRAM where
  toLaTeX PR_SWEET{..} = PR_SWEET BIG_LCB (toLaTeX expr) BIG_RCB
  toLaTeX PR_SALTY{..} = PR_SALTY global arrow (toLaTeX expr)

instance ToLaTeX EXPRESSION where
  toLaTeX EX_ATTR{..} = EX_ATTR (toLaTeX attr)
  toLaTeX EX_FORMATION{..} = EX_FORMATION lsb eol tab (toLaTeX binding) eol' tab' rsb
  toLaTeX EX_APPLICATION{..} = EX_APPLICATION (toLaTeX expr) eol tab (toLaTeX tau) eol' tab' indent
  toLaTeX EX_APPLICATION_TAUS{..} = EX_APPLICATION_TAUS (toLaTeX expr) eol tab (toLaTeX taus) eol' tab' indent
  toLaTeX EX_APPLICATION_EXPRS{..} = EX_APPLICATION_EXPRS (toLaTeX expr) eol tab (toLaTeX args) eol' tab' indent
  toLaTeX EX_DISPATCH{..} = EX_DISPATCH (toLaTeX expr) (toLaTeX attr)
  toLaTeX EX_PHI_MEET{..} = EX_PHI_MEET idx (toLaTeX expr)
  toLaTeX EX_PHI_AGAIN{..} = EX_PHI_AGAIN idx (toLaTeX expr)
  toLaTeX expr = expr

instance ToLaTeX ATTRIBUTE where
  toLaTeX AT_LABEL{..} = AT_LABEL (piped (toLaTeX label))
  toLaTeX attr = attr

instance ToLaTeX APP_BINDING where
  toLaTeX APP_BINDING{..} = APP_BINDING (toLaTeX pair)

instance ToLaTeX BINDING where
  toLaTeX BI_PAIR{..} = BI_PAIR (toLaTeX pair) (toLaTeX bindings) tab
  toLaTeX bd = bd

instance ToLaTeX BINDINGS where
  toLaTeX BDS_PAIR{..} = BDS_PAIR eol tab (toLaTeX pair) (toLaTeX bindings)
  toLaTeX bds = bds

instance ToLaTeX PAIR where
  toLaTeX PA_DELTA{..} = PA_DELTA' bytes
  toLaTeX PA_LAMBDA{..} = PA_LAMBDA' (piped (toLaTeX func))
  toLaTeX PA_LAMBDA'{..} = PA_LAMBDA' (piped (toLaTeX func))
  toLaTeX PA_VOID{..} = PA_VOID (toLaTeX attr) arrow void
  toLaTeX PA_TAU{..} = PA_TAU (toLaTeX attr) arrow (toLaTeX expr)
  toLaTeX PA_FORMATION{..} = PA_FORMATION (toLaTeX attr) (map toLaTeX voids) arrow (toLaTeX expr)
  toLaTeX pair = pair

instance ToLaTeX APP_ARG where
  toLaTeX APP_ARG{..} = APP_ARG (toLaTeX expr) (toLaTeX args)

instance ToLaTeX APP_ARGS where
  toLaTeX AAS_EXPR{..} = AAS_EXPR eol tab (toLaTeX expr) (toLaTeX args)
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
        '_' -> "\\char95{}" <> escapeUnprintedChars rest
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
    [ "\\documentclass{article}"
    , "\\usepackage{amsmath}"
    , "\\begin{document}"
    ]
    ++ unlines (map explainRule rules')
    ++ "\\end{document}"
