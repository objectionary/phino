{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LaTeX
  ( explainRules
  , rewrittensToLatex
  , programToLaTeX
  , expressionToLaTeX
  , defaultLatexContext
  , defaultMeetLength
  , defaultMeetPopularity
  , LatexContext (..)
  , meetInPrograms
  , meetInProgram
  ) where

import AST
import CST
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import Encoding (Encoding (ASCII), ToASCII, withEncoding)
import Lining (LineFormat (MULTILINE), ToSingleLine, withLineFormat)
import Locator (locatedExpression)
import Margin (WithMargin, defaultMargin, withMargin)
import Matcher
import Misc
import Render (Render (render))
import Replacer (replaceProgram)
import Rewriter (Rewritten, Rewrittens)
import Sugar (SugarType (SWEET), ToSalty, withSugarType)
import Text.Printf (printf)
import qualified Yaml as Y

data LatexContext = LatexContext
  { _sugar :: SugarType
  , _line :: LineFormat
  , _margin :: Int
  , _nonumber :: Bool
  , _compress :: Bool
  , _meetPopularity :: Int
  , _meetLength :: Int
  , _focus :: Expression
  , _expression :: Maybe String
  , _label :: Maybe String
  , _meetPrefix :: Maybe String
  }

defaultLatexContext :: LatexContext
defaultLatexContext = LatexContext SWEET MULTILINE defaultMargin False False defaultMeetPopularity defaultMeetLength ExGlobal Nothing Nothing Nothing

defaultMeetPopularity :: Int
defaultMeetPopularity = 50

defaultMeetLength :: Int
defaultMeetLength = 8

meetInProgram :: Program -> Int -> Program -> [Expression]
meetInProgram (Program expr) len = meetInExpression expr
  where
    meetInExpression :: Expression -> Program -> [Expression]
    meetInExpression (DataString _) _ = []
    meetInExpression (DataNumber _) _ = []
    meetInExpression (ExPhiMeet{}) _ = []
    meetInExpression (ExPhiAgain{}) _ = []
    meetInExpression ex prog =
      let matched = if countNodes ex >= len then map (const ex) (matchProgram ex prog) else []
       in matched ++ case ex of
            ExDispatch ex' _ -> meetInExpression ex' prog
            ExApplication ex' (BiTau _ arg) -> meetInExpression ex' prog ++ meetInExpression arg prog
            ExFormation bds -> meetInBindings bds prog
            _ -> []
    meetInBindings :: [Binding] -> Program -> [Expression]
    meetInBindings [] _ = []
    meetInBindings (BiTau _ ex : bds) prog = meetInExpression ex prog ++ meetInBindings bds prog
    meetInBindings (_ : bds) prog = meetInBindings bds prog

{- | Here we're trying to compress sequence of programs with \phiMeet{} and \phiAgain LaTeX functions.
We process the sequence of programs and trying to find all expressions in first program which are present
in following programs. Then we find ONE expression which is the most frequently encountered.
If it's encountered in more than specific percentage (_meetPopularity) of following programs - we replace
it with \phiAgain{} in following programs and with \phiMeet{} in first program.
-}
meetInPrograms :: [Program] -> LatexContext -> [Program]
meetInPrograms prog LatexContext{..} = meetInPrograms' prog 1
  where
    meetInPrograms' :: [Program] -> Int -> [Program]
    meetInPrograms' [] _ = []
    meetInPrograms' [program] _ = [program]
    meetInPrograms' (first : rest) idx =
      let met = map (meetInProgram first _meetLength) rest
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
          next = first : meetInPrograms' rest idx
       in case frequent of
            Just expr ->
              case matchProgram expr first of
                (_ : substs) ->
                  let met' = map (filter (== expr)) met
                      program = replaceProgram (first, [expr], [ExPhiMeet _meetPrefix idx])
                      program' = replaceProgram (program, map (const expr) substs, map (const (ExPhiAgain _meetPrefix idx)) substs)
                      rest' = zipWith (\prgm exprs -> replaceProgram (prgm, exprs, map (const (ExPhiAgain _meetPrefix idx)) exprs)) rest met'
                      found = filter (not . null) met'
                   in if length met' > 1 && toDouble (length found) / toDouble (length met') >= popularity
                        then program' : meetInPrograms' rest' (idx + 1)
                        else next
                [] -> next
            _ -> next
    popularity :: Double
    popularity = toDouble _meetPopularity / 100.0

renderToLatex :: (ToSalty a, ToASCII a, ToSingleLine a, ToLaTeX a, WithMargin a, Render a) => a -> LatexContext -> String
renderToLatex renderable LatexContext{..} = render (toLaTeX $ withLineFormat _line $ withMargin _margin $ withEncoding ASCII $ withSugarType _sugar renderable)

phiquation :: LatexContext -> String
phiquation LatexContext{_nonumber = True} = "phiquation*"
phiquation LatexContext{_nonumber = False} = "phiquation"

preamble :: LatexContext -> String
preamble ctx@LatexContext{..} =
  concat
    [ printf "\\begin{%s}\n" (phiquation ctx)
    , maybe "" (printf "\\label{%s}\n") _label
    , maybe "" (printf "\\phiExpression{%s} ") _expression
    ]

body :: [(a, Maybe String)] -> (a -> String) -> String
body printed toLatex =
  intercalate
    "\n  \\leadsto "
    ( map
        ( \(item, maybeName) ->
            let item' = toLatex item
             in maybe item' (printf "%s \\leadsto_{\\nameref{r:%s}}" item') maybeName
        )
        printed
    )

ending :: Bool -> LatexContext -> String
ending True ctx = printf "\n  \\leadsto \\dots\n\\end{%s}" (phiquation ctx)
ending False ctx = printf "{.}\n\\end{%s}" (phiquation ctx)

compressedRewrittens :: [Rewritten] -> LatexContext -> [Rewritten]
compressedRewrittens rewrittens ctx@LatexContext{..} =
  let (progs, rules) = unzip rewrittens
   in if _compress then zip (meetInPrograms progs ctx) rules else rewrittens

rewrittensToLatex :: Rewrittens -> LatexContext -> IO String
rewrittensToLatex (rewrittens, exceeded) ctx@LatexContext{_focus = ExGlobal} =
  pure
    ( concat
        [ preamble ctx
        , body (compressedRewrittens rewrittens ctx) (\prog -> renderToLatex (programToCST prog) ctx)
        , ending exceeded ctx
        ]
    )
rewrittensToLatex (rewrittens, exceeded) ctx@LatexContext{..} = do
  let (progs, rules) = unzip (compressedRewrittens rewrittens ctx)
  exprs <- mapM (locatedExpression _focus) progs
  pure
    ( concat
        [ preamble ctx
        , body (zip exprs rules) (\expr -> renderToLatex (expressionToCST expr) ctx)
        , ending exceeded ctx
        ]
    )

programToLaTeX :: Program -> LatexContext -> String
programToLaTeX prog ctx =
  concat
    [ preamble ctx
    , renderToLatex (programToCST prog) ctx
    , ending False ctx
    ]

expressionToLaTeX :: Expression -> LatexContext -> String
expressionToLaTeX ex ctx =
  concat
    [ preamble ctx
    , renderToLatex (expressionToCST ex) ctx
    , ending False ctx
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
  toLaTeX EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toLaTeX expr)
  toLaTeX EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (toLaTeX expr)
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
