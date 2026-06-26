{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LaTeX
  ( explainRules
  , explainMorphRules
  , explainDataizeRules
  , rewrittensToLatex
  , programToLaTeX
  , expressionToLaTeX
  , defaultLatexContext
  , defaultMeetLength
  , defaultMeetPopularity
  , LatexContext (..)
  , meetInPrograms
  , meetInProgram
  , conditionToLatex
  ) where

import AST
import CST
import Data.List (intercalate, nub)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Encoding
import Lining
import Locator (locatedExpression)
import Margin (WithMargin, defaultMargin, withMargin)
import Matcher
import Misc
import Render (Render (render))
import Replacer (replaceProgram)
import Rewriter (Rewritten, Rewrittens')
import Sugar (SugarType (SWEET), ToSalty, withSugarType)
import Text.Printf (printf)
import Text.Read (readMaybe)
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
defaultLatexContext = LatexContext SWEET SINGLELINE defaultMargin False False defaultMeetPopularity defaultMeetLength ExRoot Nothing Nothing Nothing

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
            ExApplication ex' arg -> meetInExpression ex' prog ++ meetInExpression (argExpr arg) prog
            ExFormation bds -> meetInBindings bds prog
            _ -> []
    meetInBindings :: [Binding] -> Program -> [Expression]
    meetInBindings [] _ = []
    meetInBindings (BiTau _ ex : bds) prog = meetInExpression ex prog ++ meetInBindings bds prog
    meetInBindings (_ : bds) prog = meetInBindings bds prog
    argExpr :: Argument -> Expression
    argExpr (ArTau _ ex) = ex
    argExpr (ArAlpha _ ex) = ex

{- | Here we're trying to compress sequence of programs with \phinoMeet{} and \phinoAgain LaTeX functions.
We process the sequence of programs and trying to find all expressions in first program which are present
in following programs. Then we find ONE expression which is the most frequently encountered.
If it's encountered in more than specific percentage (_meetPopularity) of following programs - we replace
it with \phinoAgain{} in following programs and with \phinoMeet{} in first program.
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
renderToLatex renderable LatexContext{..} = T.unpack $ render (toLaTeX $ withLineFormat _line $ withMargin _margin $ withEncoding ASCII $ withSugarType _sugar renderable)

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
ending True ctx = printf " \\leadsto\n  \\leadsto \\dots\n\\end{%s}" (phiquation ctx)
ending False ctx = printf "{.}\n\\end{%s}" (phiquation ctx)

compressedRewrittens :: [Rewritten] -> LatexContext -> [Rewritten]
compressedRewrittens rewrittens ctx@LatexContext{..} =
  let (progs, rules) = unzip rewrittens
   in if _compress then zip (meetInPrograms progs ctx) rules else rewrittens

-- Compress a sequence of focused sub-expressions the way 'compressedRewrittens'
-- compresses whole programs: each expression is wrapped as a program so the
-- meet machinery factors recurring sub-expressions out across the sequence.
-- Focusing happens before this, so the meet never replaces a program root the
-- focus must still descend through.
compressedExpressions :: [Expression] -> LatexContext -> [Expression]
compressedExpressions exprs ctx@LatexContext{..} =
  if _compress then map unwrap (meetInPrograms (map Program exprs) ctx) else exprs
  where
    unwrap (Program expr) = expr

rewrittensToLatex :: Rewrittens' -> LatexContext -> IO String
rewrittensToLatex (rewrittens, exceeded) ctx@LatexContext{_focus = ExRoot} =
  pure
    ( concat
        [ preamble ctx
        , body (compressedRewrittens rewrittens ctx) (\prog -> renderToLatex (programToCST prog) ctx)
        , ending exceeded ctx
        ]
    )
rewrittensToLatex (rewrittens, exceeded) ctx@LatexContext{..} = do
  let (progs, rules) = unzip rewrittens
  focused <- mapM (locatedExpression _focus) progs
  pure
    ( concat
        [ preamble ctx
        , body (zip (compressedExpressions focused ctx) rules) (\expr -> renderToLatex (expressionToCST expr) ctx)
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

piped :: T.Text -> T.Text
piped str = "|" <> toLaTeX str <> "|"

class ToLaTeX a where
  toLaTeX :: a -> a

instance ToLaTeX PROGRAM where
  toLaTeX PR_SWEET{..} = PR_SWEET BIG_LCB (toLaTeX expr) BIG_RCB SPACE
  toLaTeX PR_SALTY{..} = PR_SALTY global arrow (toLaTeX expr)

instance ToLaTeX EXPRESSION where
  toLaTeX EX_ATTR{..} = EX_ATTR (toLaTeX attr)
  toLaTeX EX_FORMATION{..} = EX_FORMATION lsb eol tab (toLaTeX binding) eol' tab' rsb
  toLaTeX EX_APPLICATION{..} = EX_APPLICATION (toLaTeX expr) SPACE eol tab (toLaTeX tau) eol' tab' indent
  toLaTeX EX_APPLICATION_TAUS{..} = EX_APPLICATION_TAUS (toLaTeX expr) SPACE eol tab (toLaTeX taus) eol' tab' indent
  toLaTeX EX_APPLICATION_EXPRS{..} = EX_APPLICATION_EXPRS (toLaTeX expr) SPACE eol tab (toLaTeX args) eol' tab' indent
  toLaTeX EX_DISPATCH{..} = EX_DISPATCH (toLaTeX expr) SPACE (toLaTeX attr)
  toLaTeX EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toLaTeX expr)
  toLaTeX EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (toLaTeX expr)
  toLaTeX EX_META{..} = EX_META (toLaTeX meta)
  toLaTeX EX_XI{} = EX_XI XI'
  toLaTeX expr = expr

instance ToLaTeX ATTRIBUTE where
  toLaTeX AT_LABEL{..} = AT_LABEL (piped label)
  toLaTeX AT_META{..} = AT_META (toLaTeX meta)
  toLaTeX AT_LAMBDA{} = AT_LAMBDA LAMBDA'
  toLaTeX AT_DELTA{} = AT_DELTA DELTA'
  toLaTeX AT_REST{} = AT_REST DOTS'
  toLaTeX AT_RHO{} = AT_RHO RHO'
  toLaTeX attr = attr

instance ToLaTeX APP_BINDING where
  toLaTeX APP_BINDING{..} = APP_BINDING (toLaTeX pair)

instance ToLaTeX BINDING where
  toLaTeX BI_PAIR{..} = BI_PAIR (toLaTeX pair) (toLaTeX bindings) tab
  toLaTeX BI_META{..} = BI_META (toLaTeX meta) (toLaTeX bindings) tab
  toLaTeX bd = bd

instance ToLaTeX BINDINGS where
  toLaTeX BDS_PAIR{..} = BDS_PAIR eol tab (toLaTeX pair) (toLaTeX bindings)
  toLaTeX BDS_META{..} = BDS_META eol tab (toLaTeX meta) (toLaTeX bindings)
  toLaTeX bds = bds

instance ToLaTeX PAIR where
  toLaTeX PA_DELTA{..} = toLaTeX (PA_DELTA' bytes)
  toLaTeX PA_DELTA'{..} = PA_DELTA' (toLaTeX bytes)
  toLaTeX PA_LAMBDA{..} = PA_LAMBDA' (piped func)
  toLaTeX PA_LAMBDA'{..} = PA_LAMBDA' (piped func)
  toLaTeX PA_VOID{..} = PA_VOID (toLaTeX attr) arrow void
  toLaTeX PA_TAU{..} = PA_TAU (toLaTeX attr) arrow (toLaTeX expr)
  toLaTeX PA_ALPHA{..} =
    let subscript = case alpha of
          AL_IDX _ n -> render n
          AL_META _ mt -> render (hd mt) <> rest mt
     in PA_TAU (AT_LABEL ("\\phiTerminal{\\alpha_{" <> subscript <> "}}")) arrow (toLaTeX expr)
  toLaTeX PA_FORMATION{..} = PA_FORMATION (toLaTeX attr) (map toLaTeX voids) arrow (toLaTeX expr)
  toLaTeX PA_META_DELTA{..} = toLaTeX (PA_META_DELTA' meta)
  toLaTeX PA_META_DELTA'{..} = PA_META_DELTA' (toLaTeX meta)
  toLaTeX PA_META_LAMBDA{..} = toLaTeX (PA_META_LAMBDA' meta)
  toLaTeX PA_META_LAMBDA'{..} = PA_META_LAMBDA' (toLaTeX meta)

instance ToLaTeX META where
  toLaTeX META{..} =
    let idx = readMaybe (T.unpack rest) :: Maybe Int
        rest' = if not (T.null rest) && T.length rest <= 2 && isJust idx then T.cons '_' rest else rest
     in META NO_EXCL (toLaTeX hd) rest'

instance ToLaTeX META_HEAD where
  toLaTeX E = E'
  toLaTeX N = N'
  toLaTeX K = K'
  toLaTeX A = TAU'
  toLaTeX TAU = TAU'
  toLaTeX B = B'
  toLaTeX D = D'
  toLaTeX F = F'
  toLaTeX mh = mh

instance ToLaTeX BYTES where
  toLaTeX (BT_META meta) = BT_META (toLaTeX meta)
  toLaTeX bts = bts

instance ToLaTeX APP_ARG where
  toLaTeX APP_ARG{..} = APP_ARG (toLaTeX expr) (toLaTeX args)

instance ToLaTeX APP_ARGS where
  toLaTeX AAS_EXPR{..} = AAS_EXPR eol tab (toLaTeX expr) (toLaTeX args)
  toLaTeX args = args

instance ToLaTeX T.Text where
  toLaTeX = T.concatMap escape
    where
      escape '$' = "\\char36{}"
      escape '@' = "\\char64{}"
      escape '^' = "\\char94{}"
      escape '_' = "\\char95{}"
      escape ch = T.singleton ch

instance ToLaTeX SET where
  toLaTeX ST_BINDING{..} = ST_BINDING (toLaTeX binding)
  toLaTeX ST_ATTRIBUTES{..} = ST_ATTRIBUTES (map toLaTeX attrs)

instance ToLaTeX NUMBER where
  toLaTeX IDX_META{..} = IDX_META (toLaTeX meta)
  toLaTeX LENGTH{..} = LENGTH (toLaTeX binding)
  toLaTeX DOMAIN{..} = DOMAIN (toLaTeX binding)
  toLaTeX literal@LITERAL{} = literal

instance ToLaTeX COMPARABLE where
  toLaTeX CMP_EXPR{..} = CMP_EXPR (toLaTeX expr)
  toLaTeX CMP_ATTR{..} = CMP_ATTR (toLaTeX attr)
  toLaTeX CMP_NUM{..} = CMP_NUM (toLaTeX num)

instance ToLaTeX CONDITION where
  toLaTeX CO_BELONGS{..} = CO_BELONGS (toLaTeX attr) belongs (toLaTeX set)
  toLaTeX CO_LOGIC{..} = CO_LOGIC (map toLaTeX conditions) operator
  toLaTeX CO_NF{..} = CO_NF (toLaTeX expr)
  toLaTeX CO_ABSOLUTE{..} = CO_ABSOLUTE (toLaTeX expr) belongs
  toLaTeX CO_NOT{..} = CO_NOT (toLaTeX condition)
  toLaTeX CO_COMPARE{..} = CO_COMPARE (toLaTeX left) equal (toLaTeX right)
  toLaTeX CO_MATCHES{..} = CO_MATCHES regex (toLaTeX expr)
  toLaTeX CO_PART_OF{..} = CO_PART_OF (toLaTeX expr) (toLaTeX binding)
  toLaTeX CO_DISJOINT{..} = CO_DISJOINT (map toLaTeX attrs) (map toLaTeX groups)
  toLaTeX CO_FORMATION{..} = CO_FORMATION (toLaTeX expr)
  toLaTeX CO_EMPTY = CO_EMPTY

instance ToLaTeX EXTRA_ARG where
  toLaTeX ARG_ATTR{..} = ARG_ATTR (toLaTeX attr)
  toLaTeX ARG_EXPR{..} = ARG_EXPR (toLaTeX expr)
  toLaTeX ARG_BINDING{..} = ARG_BINDING (toLaTeX binding)
  toLaTeX bts@ARG_BYTES{} = bts

instance ToLaTeX EXTRA where
  toLaTeX EXTRA{..} = EXTRA (toLaTeX meta) func (map toLaTeX args)

explainRule :: Y.Rule -> String
explainRule rule =
  trrule
    "\\phinoNormalizationRule"
    rule.label
    rule.name
    (renderToLatex (expressionToCST rule.pattern) defaultLatexContext)
    (renderToLatex (expressionToCST rule.result) defaultLatexContext)
    (joinedConditions rule.when rule.having)
    rule.where_
  where
    -- Join two maybe conditions into single one using Y.And if at least one is just.
    joinedConditions :: Maybe Y.Condition -> Maybe Y.Condition -> Maybe Y.Condition
    joinedConditions Nothing Nothing = Nothing
    joinedConditions first@(Just _) Nothing = first
    joinedConditions Nothing second@(Just _) = second
    joinedConditions (Just first) (Just second) = Just (Y.And [first, second])

-- Render a morphing rule as a LaTeX inference rule: each premise becomes a
-- judgment above the line and the conclusion is 𝕄(match, e) ⟿ n-result below.
explainMorphRule :: Y.MorphRule -> String
explainMorphRule rule =
  inference
    rule.name
    rule.label
    rule.when
    (map premiseToLatex rule.premises)
    (phinoMorph (renderExpr rule.match) rule.euniverse (renderExpr rule.nresult))

-- Render a dataization rule as a LaTeX inference rule, with 𝔻(match, e) ⟿
-- d-result as the conclusion below the line.
explainDataizeRule :: Y.DataizeRule -> String
explainDataizeRule rule =
  inference
    rule.name
    rule.label
    rule.when
    (map premiseToLatex rule.premises)
    (phinoDataize (renderExpr rule.match) rule.euniverse (renderBytes rule.dresult))

-- One premise judgment, rendered per its operation. 𝕄 ('morph') and 𝔻
-- ('dataize') are binary and carry the universe; the rest are unary.
premiseToLatex :: Y.Premise -> String
premiseToLatex premise = case premise.operation of
  Y.OpMorph arg -> phinoMorph (renderExpr arg) premise.universe (renderExpr (ExMeta premise.result))
  Y.OpDataize arg -> phinoDataize (renderExpr arg) premise.universe (renderBytes (BtMeta premise.result))
  Y.OpNormalize arg -> phinoNormalize (renderExpr arg) (renderExpr (ExMeta premise.result))
  Y.OpLambda arg -> phinoEvaluate (renderExpr arg) (renderExpr (ExMeta premise.result))
  Y.OpContextualize arg context -> phinoContextualize (renderExpr arg) (renderExpr context) (renderExpr (ExMeta premise.result))

-- Assemble an inference block from a name, optional label, optional side
-- condition, the premise judgments and the conclusion judgment.
inference :: String -> Maybe String -> Maybe Y.Condition -> [String] -> String -> String
inference name label cond premises conclusion =
  intercalate "\n" $
    ["\\begin{phinoInference}", "  \\phinoName{" ++ name ++ "}"]
      ++ maybe [] (\symbol -> ["  \\phinoLabel{" ++ symbol ++ "}"]) label
      ++ maybe [] (\rendered -> ["  \\phinoCondition{ " ++ rendered ++ " }"]) (conditionInLatex cond)
      ++ map (\premise -> "  \\phinoPremise{ " ++ premise ++ " }") premises
      ++ ["  \\phinoConclusion{ " ++ conclusion ++ " }", "\\end{phinoInference}"]

renderExpr :: Expression -> String
renderExpr expr = renderToLatex (expressionToCST expr) defaultLatexContext

renderBytes :: Bytes -> String
renderBytes bytes = T.unpack (render (toLaTeX (toCST' bytes :: BYTES)))

-- Render a single normalization rule row through the \phinoNormalizationRule
-- macro: an optional typeset label, name, left-hand side, right-hand side, the
-- optional 'if' condition and 'where' extras. When the label is present it
-- becomes the macro's first optional argument ('\macro[label]{name}'); when
-- absent the optional argument is omitted entirely ('\macro{name}').
-- Morphing and dataization rules render as inference rules instead (see
-- 'explainMorphRule' and 'explainDataizeRule').
trrule :: String -> Maybe String -> String -> String -> String -> Maybe Y.Condition -> Maybe [Y.Extra] -> String
trrule macro label name lhs rhs cond extras =
  intercalate
    "\n  "
    [ macro ++ labelArg ++ "{" ++ name ++ "}"
    , braced lhs
    , braced rhs
    , conditionToLatex cond
    , extraArgumentsToLatex extras
    ]
  where
    labelArg = maybe "" (\symbol -> "[" ++ symbol ++ "]") label

-- 𝕄 and 𝔻 are binary, so they render with the universe as a middle argument:
-- \phinoMorph{ input }{ universe }{ output }, mirroring 𝕄(input, universe) ⟿
-- output. 𝒩, 𝔼 and 𝒞 carry no universe.
phinoMorph :: String -> Maybe Expression -> String -> String
phinoMorph input univ output = printf "\\phinoMorph{ %s }{ %s }{ %s }" input (universeInLatex univ) output

phinoDataize :: String -> Maybe Expression -> String -> String
phinoDataize input univ output = printf "\\phinoDataize{ %s }{ %s }{ %s }" input (universeInLatex univ) output

phinoNormalize :: String -> String -> String
phinoNormalize input = printf "\\phinoNormalize{ %s }{ %s }" input

phinoEvaluate :: String -> String -> String
phinoEvaluate input = printf "\\phinoEvaluate{ %s }{ %s }" input

phinoContextualize :: String -> String -> String -> String
phinoContextualize input context = printf "\\phinoContextualize{ %s }{ %s }{ %s }" input context

universeInLatex :: Maybe Expression -> String
universeInLatex = maybe "e" renderExpr

conditionInLatex :: Maybe Y.Condition -> Maybe String
conditionInLatex Nothing = Nothing
conditionInLatex (Just cond) = case conditionToCST cond of
  CO_EMPTY -> Nothing
  cond' -> Just (renderToLatex cond' defaultLatexContext)

braced :: String -> String
braced = printf "{ %s }"

conditionToLatex :: Maybe Y.Condition -> String
conditionToLatex Nothing = "{ }"
conditionToLatex (Just cond) = case conditionToCST cond of
  CO_EMPTY -> "{ }"
  cond' -> braced (renderToLatex cond' defaultLatexContext)

extraArgumentsToLatex :: Maybe [Y.Extra] -> String
extraArgumentsToLatex Nothing = "{ }"
extraArgumentsToLatex (Just extras) =
  let extras' = map ((`renderToLatex` defaultLatexContext) . extraToCST) extras
   in braced (intercalate " and " extras')

explainRules :: [Y.Rule] -> String
explainRules = intercalate "\n" . map explainRule

explainMorphRules :: [Y.MorphRule] -> String
explainMorphRules = intercalate "\n" . map explainMorphRule

explainDataizeRules :: [Y.DataizeRule] -> String
explainDataizeRules = intercalate "\n" . map explainDataizeRule
