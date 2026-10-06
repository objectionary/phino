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
  , explainContextualizeRules
  , rewrittensToLatex
  , expressionToLaTeX
  , defaultLatexContext
  , defaultMeetLength
  , defaultMeetPopularity
  , LatexContext (..)
  , meetInExpressions
  , meetInExpression
  , conditionToLatex
  ) where

import AST
import Bytes (nonFiniteName)
import CST
import Canonizer (canonize, canonizeExpr)
import Data.List (intercalate, nub, zipWith4)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Deps (Judgment (..))
import Encoding
import Lining
import Locator (locatedExpression)
import Margin (WithMargin, defaultMargin, withMargin)
import Matcher
import Metas (lonely)
import Misc
import Render (Render (render))
import Replacer (replaceExpression)
import Rewriter (Rewritten, Rewrittens', stepHeaders)
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
  , _canonize :: Bool
  , _meetPopularity :: Int
  , _meetLength :: Int
  , _focus :: Expression
  , _expression :: Maybe String
  , _label :: Maybe String
  , _meetPrefix :: Maybe String
  , _headers :: Bool
  }

defaultLatexContext :: LatexContext
defaultLatexContext = LatexContext SWEET SINGLELINE defaultMargin False False False defaultMeetPopularity defaultMeetLength ExRoot Nothing Nothing Nothing False

defaultMeetPopularity :: Int
defaultMeetPopularity = 50

defaultMeetLength :: Int
defaultMeetLength = 8

meetInExpression :: Expression -> Int -> Expression -> [Expression]
meetInExpression expr len = meetIn expr
  where
    meetIn :: Expression -> Expression -> [Expression]
    meetIn (DataString _) _ = []
    meetIn (DataNumber _) _ = []
    meetIn (ExPhiMeet{}) _ = []
    meetIn (ExPhiAgain{}) _ = []
    meetIn ex target =
      let matched = if countNodes ex >= len then map (const ex) (matchExpression ex target) else []
       in matched ++ case ex of
            ExDispatch ex' _ -> meetIn ex' target
            ExApplication ex' arg -> meetIn ex' target ++ meetIn (argExpr arg) target
            ExFormation bds -> meetInBindings bds target
            _ -> []
    meetInBindings :: [Binding] -> Expression -> [Expression]
    meetInBindings [] _ = []
    meetInBindings (BiTau _ ex : bds) target = meetIn ex target ++ meetInBindings bds target
    meetInBindings (_ : bds) target = meetInBindings bds target
    argExpr :: Argument -> Expression
    argExpr (ArTau _ ex) = ex
    argExpr (ArAlpha _ ex) = ex

meetInExpressions :: [Expression] -> LatexContext -> [Expression]
meetInExpressions exprs LatexContext{..} = go exprs 1
  where
    go :: [Expression] -> Int -> [Expression]
    go [] _ = []
    go [single] _ = [single]
    go (first : rest) idx =
      let met = map (meetInExpression first _meetLength) rest
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
          next = first : go rest idx
       in case frequent of
            Just expr ->
              case matchExpression expr first of
                (_ : substs) ->
                  let met' = map (filter (== expr)) met
                      withMeet = replaceExpression (first, [expr], [ExPhiMeet _meetPrefix idx])
                      withAgain = replaceExpression (withMeet, map (const expr) substs, map (const (ExPhiAgain _meetPrefix idx)) substs)
                      rest' = zipWith (\other exprs' -> replaceExpression (other, exprs', map (const (ExPhiAgain _meetPrefix idx)) exprs')) rest met'
                      found = filter (not . null) met'
                   in if length met' > 1 && toDouble (length found) / toDouble (length met') >= popularity
                        then go (withAgain : rest') (idx + 1)
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
    , maybe "" (printf "\\phiExpression{%s} " . escaped) _expression
    ]

body :: [String] -> [(a, Maybe (Judgment, String))] -> (Int -> a -> String) -> String
body comments printed toLatex =
  intercalate
    "\n"
    ( zipWith4
        ( \idx comment (item, rule) reached ->
            let item' = toLatex (baseTab idx) item
                opening = if idx == 0 then item' else printf "  %s %s" (relation reached) item'
             in comment ++ maybe opening (\(judgment, name) -> printf "%s %s[\\nameref{r:%s}]" opening (relation judgment) (escaped name)) rule
        )
        [0 ..]
        comments
        printed
        (arrows (map snd printed))
    )
  where
    baseTab :: Int -> Int
    baseTab 0 = 0
    baseTab _ = 1

arrows :: [Maybe (Judgment, String)] -> [Judgment]
arrows = scanl (\current rule -> maybe current fst rule) Normalization

relation :: Judgment -> String
relation Normalization = "\\phiNormalize"
relation Morphing = "\\phiMorph"
relation Dataization = "\\phiDataize"
relation Evaluation = "\\phiEvaluate"
relation Contextualization = "\\phiContextualize"

stepComments :: [Rewritten] -> LatexContext -> [String]
stepComments rewrittens LatexContext{_headers = enabled} =
  if enabled
    then map (printf "%% %s\n") (stepHeaders rewrittens)
    else map (const "") rewrittens

ending :: Bool -> Judgment -> LatexContext -> String
ending True judgment ctx = printf " %s\n  %s \\dots\n\\end{%s}" (relation judgment) (relation judgment) (phiquation ctx)
ending False _ ctx = period ctx

period :: LatexContext -> String
period ctx = printf "{.}\n\\end{%s}" (phiquation ctx)

compressedRewrittens :: [Rewritten] -> LatexContext -> [Rewritten]
compressedRewrittens rewrittens ctx@LatexContext{..} =
  let (exprs, rules) = unzip rewrittens
   in if _compress then zip (meetInExpressions exprs ctx) rules else rewrittens

canonizedRewrittens :: [Rewritten] -> LatexContext -> [Rewritten]
canonizedRewrittens rewrittens LatexContext{_canonize = shouldCanonize} =
  if shouldCanonize then canonize rewrittens else rewrittens

canonizedExpressions :: [Expression] -> LatexContext -> [Expression]
canonizedExpressions exprs LatexContext{_canonize = shouldCanonize} =
  if shouldCanonize then map canonizeExpr exprs else exprs

compressedExpressions :: [Expression] -> LatexContext -> [Expression]
compressedExpressions exprs ctx@LatexContext{..} =
  if _compress then meetInExpressions exprs ctx else exprs

rewrittensToLatex :: Rewrittens' -> LatexContext -> IO String
rewrittensToLatex (rewrittens, exceeded) ctx@LatexContext{_focus = ExRoot} =
  pure
    ( concat
        [ preamble ctx
        , body (stepComments rewrittens ctx) (canonizedRewrittens (compressedRewrittens rewrittens ctx) ctx) (\tabs expr -> renderToLatex (expressionToCSTFrom tabs expr) ctx)
        , ending exceeded (last (arrows (map snd rewrittens))) ctx
        ]
    )
rewrittensToLatex (rewrittens, exceeded) ctx@LatexContext{..} = do
  let (exprs, rules) = unzip rewrittens
  focused <- mapM (locatedExpression _focus) exprs
  pure
    ( concat
        [ preamble ctx
        , body (stepComments rewrittens ctx) (zip (canonizedExpressions (compressedExpressions focused ctx) ctx) rules) (\tabs expr -> renderToLatex (expressionToCSTFrom tabs expr) ctx)
        , ending exceeded (last (arrows (map snd rewrittens))) ctx
        ]
    )

expressionToLaTeX :: Expression -> LatexContext -> String
expressionToLaTeX ex ctx =
  concat
    [ preamble ctx
    , renderToLatex (expressionToCST ex) ctx
    , period ctx
    ]

piped :: T.Text -> T.Text
piped str = "|" <> toLaTeX str <> "|"

class ToLaTeX a where
  toLaTeX :: a -> a

instance ToLaTeX EXPRESSION where
  toLaTeX EX_ATTR{..} = EX_ATTR (toLaTeX attr)
  toLaTeX EX_FORMATION{..} = EX_FORMATION lsb eol tab (toLaTeX binding) eol' tab' rsb
  toLaTeX EX_APPLICATION{..} = EX_APPLICATION (toLaTeX expr) SPACE eol tab (toLaTeX argument) eol' tab' indent
  toLaTeX EX_DISPATCH{..} = EX_DISPATCH (toLaTeX expr) SPACE (toLaTeX attr)
  toLaTeX EX_PHI_MEET{..} = EX_PHI_MEET prefix idx (toLaTeX expr)
  toLaTeX EX_PHI_AGAIN{..} = EX_PHI_AGAIN prefix idx (toLaTeX expr)
  toLaTeX EX_META{..} = EX_META (toLaTeX meta)
  toLaTeX EX_XI{} = EX_XI XI'
  toLaTeX EX_NONFINITE{..} = EX_DISPATCH (EX_GLOBAL global) SPACE (toLaTeX (AT_LABEL (nonFiniteName nonfinite)))
  toLaTeX EX_BYTES{..} = EX_BYTES (toLaTeX bytes)
  toLaTeX EX_SINGLE{..} = EX_SINGLE (toLaTeX pair) SPACE (toLaTeX formation)
  toLaTeX EX_STRING{..} = EX_STRING (T.unpack (toLaTeX (T.pack str))) tab rhos
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
  toLaTeX folded@PA_FOLDED{} = folded

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
  toLaTeX D'' = D'
  toLaTeX F = F''
  toLaTeX F' = F''
  toLaTeX S = S''
  toLaTeX S' = S''
  toLaTeX mh = mh

instance ToLaTeX BYTES where
  toLaTeX (BT_META meta) = BT_META (toLaTeX meta)
  toLaTeX bts = BT_PIPED bts

instance ToLaTeX APP_ARGUMENT where
  toLaTeX (AA_TAU tau) = AA_TAU (toLaTeX tau)
  toLaTeX (AA_TAUS taus) = AA_TAUS (toLaTeX taus)
  toLaTeX (AA_EXPRS args) = AA_EXPRS (toLaTeX args)

instance ToLaTeX APP_ARG where
  toLaTeX APP_ARG{..} = APP_ARG (toLaTeX expr) (toLaTeX args)

instance ToLaTeX APP_ARGS where
  toLaTeX AAS_EXPR{..} = AAS_EXPR eol tab (toLaTeX expr) (toLaTeX args)
  toLaTeX args = args

instance ToLaTeX T.Text where
  toLaTeX = T.concatMap escape
    where
      escape '#' = "\\char35{}"
      escape '$' = "\\char36{}"
      escape '%' = "\\char37{}"
      escape '&' = "\\char38{}"
      escape '@' = "\\char64{}"
      escape '^' = "\\char94{}"
      escape '\\' = "\\char92{}"
      escape '_' = "\\char95{}"
      escape '{' = "\\char123{}"
      escape '}' = "\\char125{}"
      escape '~' = "\\char126{}"
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
  toLaTeX CO_MATCHES{..} = CO_MATCHES (T.unpack (toLaTeX (T.pack regex))) (toLaTeX expr)
  toLaTeX CO_PART_OF{..} = CO_PART_OF (toLaTeX expr) (toLaTeX binding)
  toLaTeX CO_DISJOINT{..} = CO_DISJOINT (map toLaTeX attrs) (map toLaTeX groups)
  toLaTeX CO_SUBSET{..} = CO_SUBSET (map toLaTeX attrs) belongs (map toLaTeX groups)
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
    joinedConditions :: Maybe Y.Condition -> Maybe Y.Condition -> Maybe Y.Condition
    joinedConditions Nothing Nothing = Nothing
    joinedConditions first@(Just _) Nothing = first
    joinedConditions Nothing second@(Just _) = second
    joinedConditions (Just first) (Just second) = Just (Y.And [first, second])

explainMorphRule :: Y.MorphRule -> String
explainMorphRule rule =
  inference
    "phinoMorphingInference"
    rule.name
    rule.label
    rule.when
    premises
    (phinoMorph (renderExpr rule.match) (renderExpr rule.ematch) (conclusionStateName final 1) (conclusionStateName final final) (renderExpr rule.nresult))
  where
    (premises, final) = premisesToLatex rule.premises

explainDataizeRule :: Y.DataizeRule -> String
explainDataizeRule rule =
  inference
    "phinoDataizationInference"
    rule.name
    rule.label
    rule.when
    premises
    (phinoDataize (renderExpr rule.match) (renderExpr rule.ematch) (conclusionStateName final 1) (conclusionStateName final final) (renderBytes rule.dresult))
  where
    (premises, final) = premisesToLatex rule.premises

explainContextualizeRule :: Y.ContextualizeRule -> String
explainContextualizeRule rule =
  inference
    "phinoContextualizationInference"
    rule.name
    rule.label
    Nothing
    (fst (premisesToLatex rule.premises))
    (phinoContextualize (renderExpr rule.match) (renderExpr rule.cmatch) (renderExpr rule.cresult))

stateName :: Int -> String
stateName n = "s_" ++ show n

conclusionStateName :: Int -> Int -> String
conclusionStateName final index
  | final == 1 = "s"
  | otherwise = stateName index

premisesToLatex :: [Y.Premise] -> ([String], Int)
premisesToLatex = go 1
  where
    go :: Int -> [Y.Premise] -> ([String], Int)
    go index [] = ([], index)
    go index (premise : rest) = (rendered : more, final)
      where
        (rendered, next) = premiseToLatex index premise
        (more, final) = go next rest

premiseToLatex :: Int -> Y.Premise -> (String, Int)
premiseToLatex index premise = case premise.operation of
  Y.OpMorph arg universe -> (phinoMorph (renderExpr arg) (renderExpr universe) (stateName index) (stateName (index + 1)) (renderExpr (ExMeta premise.result)), index + 1)
  Y.OpDataize arg universe -> (phinoDataize (renderExpr arg) (renderExpr universe) (stateName index) (stateName (index + 1)) (renderBytes (BtMeta premise.result)), index + 1)
  Y.OpNormalize arg -> (phinoNormalize (renderExpr arg) (renderExpr (ExMeta premise.result)), index)
  Y.OpEvaluate arg evalUniverse -> (phinoEvaluate (renderExpr arg) (renderExpr evalUniverse) (stateName index) (stateName (index + 1)) (renderExpr (ExMeta premise.result)), index + 1)
  Y.OpContextualize arg context -> (phinoContextualize (renderExpr arg) (renderExpr context) (renderExpr (ExMeta premise.result)), index)

inference :: String -> String -> Maybe String -> Maybe Y.Condition -> [String] -> String -> String
inference env name label cond premises conclusion =
  intercalate "\n" $
    ["\\begin{" ++ env ++ "}", "  \\phinoName{" ++ escaped name ++ "}"]
      ++ maybe [] (\symbol -> ["  \\phinoLabel{" ++ symbol ++ "}"]) label
      ++ maybe [] (\rendered -> ["  \\phinoCondition{ " ++ rendered ++ " }"]) (conditionInLatex cond)
      ++ map (\premise -> "  \\phinoPremise{ " ++ premise ++ " }") premises
      ++ ["  \\phinoConclusion{ " ++ conclusion ++ " }", "\\end{" ++ env ++ "}"]

escaped :: String -> String
escaped = T.unpack . toLaTeX . T.pack

renderExpr :: Expression -> String
renderExpr expr = renderToLatex (expressionToCST expr) defaultLatexContext

renderBytes :: Bytes -> String
renderBytes bytes = T.unpack (render (toLaTeX (toCST' bytes :: BYTES)))

trrule :: String -> Maybe String -> String -> String -> String -> Maybe Y.Condition -> Maybe [Y.Extra] -> String
trrule macro label name lhs rhs cond extras =
  intercalate
    "\n  "
    [ macro ++ labelArg ++ "{" ++ escaped name ++ "}"
    , braced lhs
    , braced rhs
    , conditionToLatex cond
    , extraArgumentsToLatex extras
    ]
  where
    labelArg = maybe "" (\symbol -> "[" ++ symbol ++ "]") label

phinoMorph :: String -> String -> String -> String -> String -> String
phinoMorph input univ sIn sOut output = printf "\\phinoMorph{ %s }{ %s }{ %s }{ %s }{ %s }" input univ sIn output sOut

phinoDataize :: String -> String -> String -> String -> String -> String
phinoDataize input univ sIn sOut output = printf "\\phinoDataize{ %s }{ %s }{ %s }{ %s }{ %s }" input univ sIn output sOut

phinoNormalize :: String -> String -> String
phinoNormalize input = printf "\\phinoNormalize{ %s }{ %s }" input

phinoEvaluate :: String -> String -> String -> String -> String -> String
phinoEvaluate input univ sIn sOut output = printf "\\phinoEvaluate{ %s }{ %s }{ %s }{ %s }{ %s }" input univ sIn output sOut

phinoContextualize :: String -> String -> String -> String
phinoContextualize input context = printf "\\phinoContextualize{ %s }{ %s }{ %s }" input context

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
   in braced (intercalate (" " <> T.unpack (render AND) <> " ") extras')

explainRules :: [Y.Rule] -> String
explainRules = intercalate "\n" . map (explainRule . lonely)

explainMorphRules :: [Y.MorphRule] -> String
explainMorphRules = intercalate "\n" . map (explainMorphRule . lonely)

explainDataizeRules :: [Y.DataizeRule] -> String
explainDataizeRules = intercalate "\n" . map (explainDataizeRule . lonely)

explainContextualizeRules :: [Y.ContextualizeRule] -> String
explainContextualizeRules = intercalate "\n" . map (explainContextualizeRule . lonely)
