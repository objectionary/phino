{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The Haskell module 'phino compile' writes out of the rules of YAML: every
-- rewriting rule a function of the term it may match as a whole, answering
-- what it rewrites the term to, the normal form a test of whether any
-- built-in one matches anywhere, 𝒞 one function with an equation per rule
-- of 'resources/contextualization' (#1617), and every rule of 𝕄 and of 𝔻 a
-- function of the term and the universe it may match, answering the premises
-- it runs and the conclusion it comes to (#1628). A pattern becomes the
-- generators of a list comprehension, a meta the variable a generator binds,
-- a meta met twice a guard of equality, the 'when' of a rule a guard, a
-- function of its 'where' a binding, a premise a function of the answer it is
-- handed, and its result the constructors that build it. No substitution is
-- made and no template is filled, which is what a rule of YAML costs at every
-- step. What the module does is what the matcher, the builder and the
-- replacer do for the same rule, in the same order, so the steps a chain is
-- made of do not depend on which of the two ran; a rule the module could not
-- run that way is refused, with the reason.
module Emit (emitted) where

import AST
import Control.Monad (zipWithM)
import Data.Char (isAlphaNum, isDigit, toLower, toUpper)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import Deps (Judgment)
import qualified Inference as In
import Matcher (Meta (..))
import Rewriter (fast)
import Rule (redex)
import Text.Printf (printf)
import qualified Yaml as Y

-- What the emitter knows while it walks one rule: the variable every meta
-- matched so far is held in, and the number the next fresh variable takes.
data Scope = Scope (Map.Map Meta String) Int

-- A walk over one rule, which either writes a piece of Haskell or refuses the
-- rule, saying why.
newtype Emitting a = Emitting (Scope -> Either String (a, Scope))

instance Functor Emitting where
  fmap func (Emitting walk) = Emitting (fmap (\(value, scope) -> (func value, scope)) . walk)

instance Applicative Emitting where
  pure value = Emitting (\scope -> Right (value, scope))
  Emitting left <*> Emitting right = Emitting $ \scope -> do
    (func, scope') <- left scope
    (value, scope'') <- right scope'
    Right (func value, scope'')

instance Monad Emitting where
  Emitting walk >>= next = Emitting $ \scope -> do
    (value, scope') <- walk scope
    let Emitting walk' = next value
    walk' scope'

-- One qualifier of a list comprehension: a generator binding a pattern to
-- every element of a list, a guard, or a binding of a variable.
data Qual
  = Gen Pat String
  | Guard String
  | Let String String

-- A pattern a generator matches an element with.
data Pat
  = PVar String
  | PCon String [Pat]
  | PPair Pat Pat
  | PCons Pat Pat
  | PNil

-- The module of the given rules: the built-in rules of normalization, the
-- rules of '--rule', the rules of contextualization, of morphing and of
-- dataization, and the texts of the built-in rules the engine is compiled
-- from; or the reason one of the rules cannot be compiled.
emitted :: [Y.Rule] -> [Y.Rule] -> [Y.ContextualizeRule] -> [Y.MorphRule] -> [Y.DataizeRule] -> [String] -> Either String String
emitted builtin custom contextual morphs datas sources = do
  let rules = zip (named (map (.name) (builtin ++ custom))) (builtin ++ custom)
  functions <- mapM rewriting rules
  equations <- zipWithM contextualizing (named (map (.name) contextual)) contextual
  morphings <- zipWithM morphing (named (map (.name) morphs)) morphs
  dataizations <- zipWithM dataizing (named (map (.name) datas)) datas
  let body =
        unlines
          ( [ "compiled :: Maybe En.Engine"
            , "compiled ="
            , "  Just"
            , "    En.Engine"
            , "      { En._normalization = normalization"
            , "      , En._rules = steps"
            , "      , En._normal = nf"
            , "      , En._contextualize = \\term context -> either E.throwIO pure (contextualize term context)"
            , "      , En._morphing = morphings"
            , "      , En._dataization = dataizations"
            , "      , En._sources = sources"
            , "      }"
            , ""
            , "-- The steps of the built-in rules of normalization, in the order of the rules."
            , "normalization :: [Ru.Step]"
            , "normalization = " ++ listed (map (("step" ++) . fst) (take (length builtin) rules))
            , ""
            , "-- The steps of every rule compiled, by the text of the rule."
            , "steps :: Map.Map String Ru.Step"
            , "steps ="
            , "  Map.fromList " ++ listed [printf "(%s, step%s)" (show (show rule)) name | (name, rule) <- rules]
            , ""
            , "-- The rules of 𝕄, in the order of their files."
            , "morphings :: [In.Inference Expression]"
            , "morphings = " ++ listed (map ("In.direct morphing" ++) (named (map (.name) morphs)))
            , ""
            , "-- The rules of 𝔻, in the order of their files."
            , "dataizations :: [In.Inference Bytes]"
            , "dataizations = " ++ listed (map ("In.direct dataization" ++) (named (map (.name) datas)))
            , ""
            , "-- The texts of the built-in rules of all four judgments this module is made of."
            , "sources :: [String]"
            , "sources = " ++ listed (map show sources)
            , ""
            , "-- Whether the term is a normal form: no built-in rule of normalization"
            , "-- matches anywhere inside it."
            , "nf :: Expression -> Bool"
            , "nf ="
            , "  Ru.normalWith"
            , "    ( \\term ->"
            , "        " ++ intercalate "\n          || " [printf "M.anywhere %s (not . null . rewrite%s Nothing) term" (show (redex rule)) name | (name, rule) <- take (length builtin) rules]
            , "    )"
            , ""
            , "-- The Contextualization function 𝒞, the conclusion of the one rule matching"
            , "-- the term and the context."
            , "contextualize :: Expression -> Expression -> Either C.ContextualizeException Expression"
            , "contextualize term context ="
            , "  C.concluded"
            , "    term"
            , "    ( concat"
            , "        " ++ listed' 8 [printf "contextualize%s term context" name | name <- named (map (.name) contextual)]
            , "    )"
            , ""
            ]
              ++ functions
              ++ equations
              ++ morphings
              ++ dataizations
          )
  Right (header ++ imports body ++ "\n" ++ body)
  where
    header :: String
    header =
      unlines
        [ "-- The built-in rules of phino, and the rules of '--rule' it was given, as"
        , "-- Haskell: this module is written by 'phino compile' and a build with the"
        , "-- flag 'compiled' links it in (#1617). The next 'phino compile' writes it"
        , "-- anew, so a change belongs to the rules of YAML and not to it."
        , "module Compiled (compiled) where"
        , ""
        ]
    imports :: String -> String
    imports body =
      unlines
        ( "import AST"
            : [ line
              | (qualifier, line) <-
                  [ ("B.", "import qualified Builder as B")
                  , ("C.", "import qualified Contextualize as C")
                  , ("D.", "import qualified Deps as D")
                  , ("E.", "import qualified Control.Exception as E")
                  , ("En.", "import qualified Engine as En")
                  , ("In.", "import qualified Inference as In")
                  , ("M.", "import qualified Matcher as M")
                  , ("Map.", "import qualified Data.Map.Strict as Map")
                  , ("R.", "import qualified Rewriter as R")
                  , ("Ru.", "import qualified Rule as Ru")
                  , ("T.", "import qualified Data.Text as T")
                  ]
              , qualifier `elem` qualifiers body
              ]
        )
    qualifiers :: String -> [String]
    qualifiers body = [takeWhile (/= '.') word ++ "." | word <- words (map spaced body), '.' `elem` word, isUpperStart word]
    spaced :: Char -> Char
    spaced char
      | isAlphaNum char || char == '.' || char == '_' = char
      | otherwise = ' '
    isUpperStart :: String -> Bool
    isUpperStart (first : _) = first `elem` ['A' .. 'Z']
    isUpperStart [] = False

-- The names the functions of the rules go by, one per rule, told apart where
-- two rules carry the same name.
named :: [String] -> [String]
named names = zipWith unique [0 :: Int ..] (map camel names)
  where
    unique :: Int -> String -> String
    unique idx name
      | length (filter (== name) (map camel names)) > 1 = name ++ show idx
      | otherwise = name
    camel :: String -> String
    camel name = case filter isAlphaNum (concatMap upper (words (map (\char -> if isAlphaNum char then char else ' ') name))) of
      [] -> "Rule"
      word@(first : _)
        | isDigit first -> "Rule" ++ word
        | otherwise -> word
    upper :: String -> String
    upper (first : rest) = toUpper first : rest
    upper [] = []

-- The functions of one rewriting rule: its step and what it rewrites a term
-- matching it as a whole to.
--
-- @todo #1628:30min Ask the normal forms of a rewriting rule the way the
--  matcher asks them, with 'Ru.normalHeld' as the rules of 𝕄 and 𝔻 do. The
--  guard 'nf' tells a term that is itself a meta a normal form, which the
--  matcher does not, so a term holding a meta may rewrite under one engine
--  and stay under the other; 'CompiledSpec' should compare terms with metas.
rewriting :: (String, Y.Rule) -> Either String String
rewriting (name, rule) = do
  refused
  (quals, result) <- walked $ do
    pattern' <- matching rule.pattern "term"
    when' <- maybe (pure []) (fmap (pure . Guard) . condition) rule.when
    absolute <- mapM (fmap (\var -> Guard ("Ru.xiFree " ++ var)) . held) (prefixed "k" rule.pattern)
    normal <- mapM (fmap (\var -> Guard ("nf " ++ var)) . held) (prefixed "n" rule.pattern ++ prefixed "k" rule.pattern)
    extras <- concat <$> mapM extended (fromMaybe [] rule.where_)
    result <- built True rule.result >>= maybe (refuse "its result names a meta its pattern does not bind") pure
    pure (pattern' ++ when' ++ absolute ++ normal ++ extras, result)
  let body = comprehension quals result
      universe = if "universe" `elem` tokens body then "universe" else "_"
  Right
    ( unlines
        [ printf "-- The rule '%s'." rule.name
        , printf "step%s :: Ru.Step" name
        , printf "step%s = R.direct %s %s rewrite%s" name (show rule.name) (show (redex rule)) name
        , ""
        , printf "rewrite%s :: Maybe Expression -> Expression -> [Expression]" name
        , printf "rewrite%s %s %s =" name universe (if "term" `elem` tokens body then "term" else "_")
        , body
        ]
    )
  where
    refused :: Either String ()
    refused
      | isJust rule.having = Left (printf "The rule '%s' cannot be compiled, since it has a 'having' condition" rule.name)
      | fast rule.pattern rule.result = Left (printf "The rule '%s' cannot be compiled, since it rewrites a formation into a formation the fast way" rule.name)
      | rooted rule.pattern = Left (printf "The rule '%s' cannot be compiled, since its pattern applies Φ to a ρ" rule.name)
      | otherwise = Right ()
    walked :: Emitting a -> Either String a
    walked (Emitting walk) = either (Left . printf "The rule '%s' cannot be compiled, since %s" rule.name) (Right . fst) (walk (Scope Map.empty 1))
    extended :: Y.Extra -> Emitting [Qual]
    extended extra = case (extra.meta, extra.function, extra.args) of
      (Y.ArgExpression (ExMeta meta), "contextualize", [Y.ArgExpression expr, Y.ArgExpression context]) -> do
        expr' <- argument expr
        context' <- argument context
        into meta (printf "either E.throw id (contextualize %s %s)" expr' context')
      (Y.ArgExpression (ExMeta meta), "named", [Y.ArgExpression expr]) -> do
        expr' <- argument expr
        into meta (printf "B.nameIn universe %s" expr')
      (_, func, _) -> refuse (printf "its 'where' calls the function '%s', which only 'contextualize' and 'named' can be" func)
    argument :: Expression -> Emitting String
    argument expr = built True expr >>= maybe (refuse "its 'where' names a meta its pattern does not bind") (pure . parens)
    into :: T.Text -> String -> Emitting [Qual]
    into meta value =
      known (Named meta) >>= \case
        Just var -> pure [Guard (printf "%s == %s" var (parens value))]
        Nothing -> do
          let var = variable (Named meta)
          bind (Named meta) var
          pure [Let var value]

-- The function of one contextualization rule: the conclusion it comes to for
-- a term and a context matching it, once for every way they match it, beside
-- its name. The letters '𝑛' and '𝑘' of these rules are only names, so no
-- normal form is asked of anything (see 'Contextualize').
contextualizing :: String -> Y.ContextualizeRule -> Either String String
contextualizing name rule = do
  (quals, result) <- walked $ do
    term <- matching rule.match "term"
    context <- matching rule.cmatch "context"
    premises <- mapM premised rule.premises
    result <- built True rule.cresult >>= maybe (refuse "its conclusion names a meta nothing binds") pure
    pure (term ++ context, conclusion premises result)
  let body = comprehension quals result
      arg var = if var `elem` tokens body then var else "_"
  Right
    ( unlines
        [ printf "-- The contextualization rule '%s'." rule.name
        , printf "contextualize%s :: Expression -> Expression -> [(String, Either C.ContextualizeException Expression)]" name
        , printf "contextualize%s %s %s =" name (arg "term") (arg "context")
        , body
        ]
    )
  where
    walked :: Emitting a -> Either String a
    walked (Emitting walk) = either (Left . printf "The contextualization rule '%s' cannot be compiled, since %s" rule.name) (Right . fst) (walk (Scope Map.empty 1))
    premised :: Y.Premise -> Emitting (String, String)
    premised (Y.Premise result (Y.OpContextualize inner outer)) = do
      inner' <- built True inner >>= maybe (refuse "a premise names a meta nothing binds") (pure . parens)
      outer' <- built True outer >>= maybe (refuse "a premise names a meta nothing binds") (pure . parens)
      var <- introduced result
      pure (var, printf "contextualize %s %s" inner' outer')
    premised premise = refuse (printf "its premise '%s' is not a contextualization" (T.unpack premise.result))
    conclusion :: [(String, String)] -> String -> String
    conclusion [] result = printf "(%s, Right %s)" (show rule.name) (parens result)
    conclusion premises result =
      printf
        "(%s, do { %s; pure %s })"
        (show rule.name)
        (intercalate "; " [printf "%s <- %s" var call | (var, call) <- premises])
        (parens result)

-- The function of one rule of 𝕄: what it comes to for a term and a universe
-- matching it, once for every way they match it (see 'inferring').
morphing :: String -> Y.MorphRule -> Either String String
morphing name rule = inferring "morphing" ("morphing" ++ name, "Expression") (Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing) rule.ematch (built True) (In.morphingSpine rule)

-- The function of one rule of 𝔻, the way 'morphing' writes one of 𝕄.
dataizing :: String -> Y.DataizeRule -> Either String String
dataizing name rule = inferring "dataization" ("dataization" ++ name, "Bytes") (Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing) rule.ematch builtBytes (In.dataizationSpine rule)

-- The function of one rule of 𝕄 or 𝔻, of the given kind, name and type of
-- answer: the premises it runs beside its spine, each a function of the answer
-- it is handed, and the conclusion it comes to, once for every way the term
-- and the universe match it, checked the way the matcher checks them — the
-- pattern of the universe against the universe first, then the pattern of the
-- rule against the term, its 'when', and its '𝑛' and '𝑘' metas (see
-- 'matchExpressionWithRule''). What the premises and the conclusion are is
-- read off the rule by 'Inference', the very way the engine of YAML reads it.
inferring :: forall value. String -> (String, String) -> Y.Rule -> Expression -> (value -> Emitting (Maybe String)) -> Either String ([Y.Premise], In.Conclusion value) -> Either String String
inferring kind (name, answer) rule ematch builder spine = do
  (sides, conclusion) <- either (Left . refusal) Right spine
  (quals, result) <- walked $ do
    universe <- matching ematch "universe"
    term <- matching rule.pattern "term"
    when' <- maybe (pure []) (fmap (pure . Guard) . condition) rule.when
    absolute <- mapM (fmap (\var -> Guard ("Ru.xiFree " ++ var)) . held) (prefixed "k" rule.pattern)
    normal <- mapM (fmap (\var -> Guard ("Ru.normalHeld nf " ++ var)) . held) (prefixed "n" rule.pattern ++ prefixed "k" rule.pattern)
    result <- premised sides conclusion
    pure (universe ++ term ++ when' ++ absolute ++ normal, result)
  let body = comprehension quals result
      arg var = if var `elem` tokens body then var else "_"
  Right
    ( unlines
        [ printf "-- The %s rule '%s'." kind rule.name
        , printf "%s :: Expression -> Expression -> [In.Premises %s]" name answer
        , printf "%s %s %s =" name (arg "term") (arg "universe")
        , body
        ]
    )
  where
    refusal :: String -> String
    refusal = printf "The %s rule '%s' cannot be compiled, since %s" kind rule.name
    walked :: Emitting a -> Either String a
    walked (Emitting walk) = either (Left . refusal) (Right . fst) (walk (Scope Map.empty 1))
    premised :: [Y.Premise] -> In.Conclusion value -> Emitting String
    premised [] conclusion = ("In.Concludes " ++) . parens <$> concluded conclusion
    premised (premise : rest) conclusion = do
      (constructor, first, second) <- case premise.operation of
        Y.OpMorph expr world -> (,,) "In.Morphs" <$> argued "a premise" expr <*> argued "a premise" world
        Y.OpEvaluate expr world -> (,,) "In.Evaluates" <$> argued "a premise" expr <*> argued "a premise" world
        Y.OpContextualize expr context -> (,,) "In.Contextualizes" <$> argued "a premise" expr <*> argued "a premise" context
        _ -> refuse (printf "its premise '%s' runs beside the spine, which only a 'morph', an 'evaluate' or a 'contextualize' can" (T.unpack premise.result))
      var <- introduced premise.result
      next <- premised rest conclusion
      pure (printf "%s %s %s (\\%s -> pure %s)" constructor first second (if var `elem` tokens next then var else "_") (parens next))
    concluded :: In.Conclusion value -> Emitting String
    concluded (In.Answered step value) = printf "In.Answered %s %s" (stepped step) . parens <$> (builder value >>= maybe (refuse "its conclusion names a meta nothing binds") pure)
    concluded (In.Onward way expr world) = printf "In.Onward %s %s %s" <$> (parens <$> wayOf way) <*> argued "its conclusion" expr <*> argued "its conclusion" world
    wayOf :: In.Way -> Emitting String
    wayOf (In.Taken step) = pure ("In.Taken " ++ stepped step)
    wayOf (In.Normalized step) = pure ("In.Normalized " ++ stepped step)
    wayOf (In.Named step) = pure ("In.Named " ++ stepped step)
    wayOf (In.Staged stage) = ("In.Staged " ++) <$> argued "its conclusion" stage
    argued :: String -> Expression -> Emitting String
    argued place expr = built True expr >>= maybe (refuse (printf "%s names a meta nothing binds" place)) (pure . parens)
    stepped :: (Judgment, String) -> String
    stepped (judgment, verb) = printf "(D.%s, %s)" (show judgment) (show verb)

-- The comprehension of the qualifiers and the result, every variable a
-- generator binds and nothing after it reads written as a wildcard and every
-- binding nothing reads dropped, so the module compiles without a warning.
comprehension :: [Qual] -> String -> String
comprehension quals result = case fst (foldr written ([], Set.fromList (tokens result)) quals) of
  [] -> "  [" ++ result ++ "]"
  first : rest -> "  [ " ++ result ++ "\n  | " ++ first ++ concatMap ("\n  , " ++) rest ++ "\n  ]"
  where
    written :: Qual -> ([String], Set.Set String) -> ([String], Set.Set String)
    written (Gen pat source) (lines', used) = ((pattern' used pat ++ " <- " ++ source) : lines', grown source used)
    written (Guard guard) (lines', used) = (guard : lines', grown guard used)
    written (Let var value) (lines', used)
      | var `Set.member` used = (("let " ++ var ++ " = " ++ value) : lines', grown value used)
      | otherwise = (lines', used)
    grown :: String -> Set.Set String -> Set.Set String
    grown text used = foldr Set.insert used (tokens text)
    pattern' :: Set.Set String -> Pat -> String
    pattern' used (PVar var)
      | var `Set.member` used = var
      | otherwise = "_"
    pattern' _ (PCon con []) = con
    pattern' used (PCon con pats) = con ++ " " ++ unwords (map (atomic used) pats)
    pattern' used (PPair left right) = "(" ++ pattern' used left ++ ", " ++ pattern' used right ++ ")"
    pattern' used (PCons first rest) = "(" ++ pattern' used first ++ " : " ++ pattern' used rest ++ ")"
    pattern' _ PNil = "[]"
    atomic :: Set.Set String -> Pat -> String
    atomic used pat@(PCon _ (_ : _)) = "(" ++ pattern' used pat ++ ")"
    atomic used pat = pattern' used pat

-- The words of a piece of Haskell, which is how the emitter tells whether a
-- variable is read after it was bound.
tokens :: String -> [String]
tokens = words . map (\char -> if isAlphaNum char || char == '_' || char == '\'' then char else ' ')

-- The generators and guards matching the pattern against the term the
-- variable holds, in the order the matcher matches it (see 'matchExpression''):
-- the attribute of a dispatch before its head, the head of an application
-- before its argument, and the bindings of a formation left to right.
matching :: Expression -> String -> Emitting [Qual]
matching (ExMeta meta) var = meta' (Named meta) var
matching (ExAny slot) var = meta' (Anon slot) var
matching ExXi var = pure [Gen (PCon "ExXi" []) (single var)]
matching ExRoot var = pure [Gen (PCon "ExRoot" []) (single var)]
matching ExTermination var = pure [Gen (PCon "ExTermination" []) (single var)]
matching (ExFormation bds) var = do
  inner <- fresh
  rest <- bindings bds inner
  pure (Gen (PCon "ExFormation" [PVar inner]) (single var) : rest)
matching (ExDispatch expr attr) var = do
  head' <- fresh
  attr' <- fresh
  attribute' <- attribute attr attr'
  expression <- matching expr head'
  pure (Gen (PCon "ExDispatch" [PVar head', PVar attr']) (single var) : attribute' ++ expression)
matching (ExApplication expr (ArTau attr arg)) var = do
  head' <- fresh
  attr' <- fresh
  arg' <- fresh
  attribute' <- attribute attr attr'
  expression <- matching expr head'
  argument <- matching arg arg'
  pure (Gen (PCon "ExApplication" [PVar head', PCon "ArTau" [PVar attr', PVar arg']]) (single var) : attribute' ++ expression ++ argument)
matching (ExApplication expr (ArAlpha alpha arg)) var = do
  head' <- fresh
  alpha' <- fresh
  arg' <- fresh
  expression <- matching expr head'
  index <- indexed alpha alpha'
  argument <- matching arg arg'
  pure (Gen (PCon "ExApplication" [PVar head', PCon "ArAlpha" [PVar alpha', PVar arg']]) (single var) : expression ++ index ++ argument)
matching expr _ = refuse (printf "its pattern holds the term '%s', which only a rule of YAML can match" (show expr))

-- The generators and guards matching the bindings of a pattern against the
-- list the variable holds, a meta binding trying every leading run of it,
-- the shortest first, and taking the whole rest where it is the last one (see
-- 'matchBindingsMeta').
bindings :: [Binding] -> String -> Emitting [Qual]
bindings [] var = pure [Gen PNil (single var)]
bindings [BiMeta meta] var = meta' (Named meta) var
bindings [BiAny _] _ = pure []
bindings (BiMeta meta : rest) var = do
  before <- fresh
  after <- fresh
  bound <- meta' (Named meta) before
  others <- bindings rest after
  pure (Gen (PPair (PVar before) (PVar after)) ("M.splits " ++ var) : bound ++ others)
bindings (BiAny _ : rest) var = do
  after <- fresh
  others <- bindings rest after
  pure (Gen (PPair (PVar "_") (PVar after)) ("M.splits " ++ var) : others)
bindings (bd : rest) var = do
  first <- fresh
  after <- fresh
  binding' <- binding bd first
  others <- bindings rest after
  pure (Gen (PCons (PVar first) (PVar after)) (single var) : binding' ++ others)

-- The generators and guards matching one binding of a pattern against the
-- binding the variable holds (see 'matchBinding').
binding :: Binding -> String -> Emitting [Qual]
binding (BiVoid attr) var = do
  attr' <- fresh
  attribute' <- attribute attr attr'
  pure (Gen (PCon "BiVoid" [PVar attr']) (single var) : attribute')
binding (BiTau attr expr) var = do
  attr' <- fresh
  expr' <- fresh
  attribute' <- attribute attr attr'
  expression <- matching expr expr'
  pure (Gen (PCon "BiTau" [PVar attr', PVar expr']) (single var) : attribute' ++ expression)
binding (BiDelta (BtMeta meta)) var = do
  data' <- fresh
  bound <- meta' (Named meta) data'
  pure (Gen (PCon "BiDelta" [PVar data']) (single var) : bound)
binding (BiDelta (BtAny _)) var = pure [Gen (PCon "BiDelta" [PVar "_"]) (single var)]
binding (BiDelta bts) var = do
  data' <- fresh
  pure [Gen (PCon "BiDelta" [PVar data']) (single var), Guard (printf "%s == %s" data' (parens (show bts)))]
binding (BiLambda (FnMeta meta)) var = do
  func <- fresh
  bound <- meta' (Named meta) func
  pure (Gen (PCon "BiLambda" [PVar func]) (single var) : Guard ("M.named " ++ func) : bound)
binding (BiLambda (FnAny _)) var = do
  func <- fresh
  pure [Gen (PCon "BiLambda" [PVar func]) (single var), Guard ("M.named " ++ func)]
binding (BiLambda (FnFresh _)) _ = refuse "its pattern asks for a fresh symbol"
binding (BiLambda func) var = do
  func' <- fresh
  literal <- function func
  pure [Gen (PCon "BiLambda" [PVar func']) (single var), Guard (printf "%s == %s" func' literal)]
binding bd _ = refuse (printf "its pattern holds the binding '%s' where a single binding stands" (show bd))

-- The guards matching an attribute of a pattern against the attribute the
-- variable holds (see 'matchAttribute').
attribute :: Attribute -> String -> Emitting [Qual]
attribute (AtMeta meta) var = meta' (Named meta) var
attribute (AtAny _) _ = pure []
attribute attr var = (\literal -> [Guard (printf "%s == %s" var literal)]) <$> attributed attr

-- The generators and guards matching an index of a pattern against the one
-- the variable holds, which a meta matches only where it is a number (see
-- 'matchAlpha').
indexed :: Alpha -> String -> Emitting [Qual]
indexed (AlMeta meta) var = do
  index <- fresh
  bound <- meta' (Named meta) index
  pure (Gen (PCon "Alpha" [PVar index]) (single var) : bound)
indexed (AlAny _) var = pure [Gen (PCon "Alpha" [PVar "_"]) (single var)]
indexed (Alpha idx) var = pure [Guard (printf "%s == Alpha %d" var idx)]

-- A meta matched against what the variable holds: bound to it the first time,
-- and asked to equal what it was bound to every time after.
meta' :: Meta -> String -> Emitting [Qual]
meta' key var =
  known key >>= \case
    Just bound -> pure [Guard (printf "%s == %s" bound var)]
    Nothing -> bind key var >> pure []

-- The metas of the pattern a normal form or an absolute term is asked of,
-- '𝑛' or '𝑘' by the prefix, in the order they are met (see 'metasWithPrefix').
prefixed :: String -> Expression -> [Meta]
prefixed prefix = nub . go
  where
    go :: Expression -> [Meta]
    go (ExMeta meta)
      | T.pack prefix `T.isPrefixOf` meta = [Named meta]
    go (ExAny slot@(Slot kind _))
      | T.pack prefix `T.isPrefixOf` kind = [Anon slot]
    go (ExFormation bds) = concat [go expr | BiTau _ expr <- bds]
    go (ExApplication expr (ArTau _ arg)) = go expr ++ go arg
    go (ExApplication expr (ArAlpha _ arg)) = go expr ++ go arg
    go (ExDispatch expr _) = go expr
    go _ = []

-- The Haskell of a condition, which holds exactly where the condition of the
-- rule does (see 'meetCondition'''): a condition naming a meta the pattern
-- does not bind never holds.
condition :: Y.Condition -> Emitting String
condition (Y.And conds) = parens . intercalate " && " <$> mapM condition conds
condition (Y.Or conds) = parens . intercalate " || " <$> mapM condition conds
condition (Y.Not cond) = ("not " ++) . parens <$> condition cond
condition (Y.In attrs bds) = present True attrs bds
condition (Y.Disjoint attrs bds) = present False attrs bds
condition (Y.Eq (Y.CmpNum left) (Y.CmpNum right)) = compared "==" <$> number left <*> number right
condition (Y.Gt (Y.CmpNum left) (Y.CmpNum right)) = compared ">" <$> number left <*> number right
condition (Y.Eq (Y.CmpAttr left) (Y.CmpAttr right)) = compared "==" <$> attr' left <*> attr' right
  where
    attr' :: Attribute -> Emitting (Maybe String)
    attr' (AtMeta meta) = known (Named meta)
    attr' (AtAny _) = pure Nothing
    attr' attr = Just <$> attributed attr
condition (Y.Eq (Y.CmpExpr left) (Y.CmpExpr right)) = compared "==" <$> built False left <*> built False right
condition (Y.Eq _ _) = pure "False"
condition (Y.Gt _ _) = pure "False"
condition (Y.NF expr) = asked "nf" expr
condition (Y.Absolute expr) = asked "Ru.xiFree" expr
condition (Y.IsFormation (ExMeta meta)) = maybe "False" ("Ru.isFormation " ++) <$> known (Named meta)
condition (Y.IsFormation (ExFormation _)) = pure "True"
condition (Y.IsFormation _) = pure "False"
condition (Y.Matches _ _) = refuse "its condition 'matches' needs a run of dataization"
condition (Y.PartOf _ _) = refuse "its condition 'part-of' is not compiled yet"

-- A condition asking whether the attributes are present among the bindings
-- the metas hold, all of them or none of them, which never holds where an
-- attribute or a binding cannot be worked out.
present :: Bool -> [Attribute] -> [Binding] -> Emitting String
present every attrs bds = do
  attrs' <- mapM attr' attrs
  bds' <- mapM bindingsOf bds
  pure $
    if all isJust attrs' && all isJust bds'
      then (if every then id else ("not " ++) . parens) (printf "%s (`Ru.presentIn` concat %s) %s" (if every then "all" else "any") (listed (map (fromMaybe "") bds')) (listed (map (fromMaybe "") attrs')))
      else "False"
  where
    attr' :: Attribute -> Emitting (Maybe String)
    attr' (AtMeta meta) = known (Named meta)
    attr' (AtAny _) = pure Nothing
    attr' attr = Just <$> attributed attr
    bindingsOf :: Binding -> Emitting (Maybe String)
    bindingsOf (BiMeta meta) = known (Named meta)
    bindingsOf (BiAny _) = pure Nothing
    bindingsOf bd = fmap listed . sequence <$> mapM (builtBinding False) [bd]

-- A condition asking a question of the term a meta holds.
asked :: String -> Expression -> Emitting String
asked question (ExMeta meta) = maybe "False" ((question ++ " ") ++) <$> known (Named meta)
asked question (ExAny slot) = maybe "False" ((question ++ " ") ++) <$> known (Anon slot)
asked _ _ = refuse "its condition asks about a term that is no meta"

-- The comparison of the two sides, which never holds where either of them
-- cannot be worked out.
compared :: String -> Maybe String -> Maybe String -> String
compared operator (Just left) (Just right) = printf "%s %s %s" (parens left) operator (parens right)
compared _ _ _ = "False"

-- The Haskell of a number of a condition, where it can be worked out (see
-- 'numToInt').
number :: Y.Number -> Emitting (Maybe String)
number (Y.MetaIndex meta) = known (Named meta)
number (Y.Length (BiMeta meta)) = fmap ("length " ++) <$> known (Named meta)
number (Y.Domain (BiMeta meta)) = fmap ("Ru.domainOf " ++) <$> known (Named meta)
number (Y.Literal num) = pure (Just (show num))
number _ = pure Nothing

-- The Haskell building the term of a template out of the metas bound, the way
-- 'buildExpression' builds it, or nothing where a meta of it is not bound. A
-- formation is checked to carry no attribute twice where the flag says so,
-- which is what a result and a function of 'where' are built with, and a
-- condition is not.
built :: Bool -> Expression -> Emitting (Maybe String)
built _ (ExMeta meta) = known (Named meta)
built _ ExXi = pure (Just "ExXi")
built _ ExRoot = pure (Just "ExRoot")
built _ ExTermination = pure (Just "ExTermination")
built checked (ExApplication ExRoot (ArTau AtRho expr)) = fmap (const "ExRoot") <$> built checked expr
built checked (ExFormation bds) = do
  parts <- mapM part bds
  pure (formation <$> sequence parts)
  where
    part :: Binding -> Emitting (Maybe (Either String String))
    part (BiMeta meta) = fmap Left <$> known (Named meta)
    part bd = fmap Right <$> builtBinding checked bd
    formation :: [Either String String] -> String
    formation [Left var] = constructor ++ " " ++ var
    formation parts = constructor ++ " " ++ parens (joined parts)
    joined :: [Either String String] -> String
    joined parts
      | all isRight parts = listed [bd | Right bd <- parts]
      | otherwise = "concat " ++ listed (map (either id (\bd -> "[" ++ bd ++ "]")) parts)
    isRight :: Either String String -> Bool
    isRight (Right _) = True
    isRight (Left _) = False
    constructor :: String
    constructor = if checked then "B.formed" else "ExFormation"
built checked (ExDispatch expr attr) = do
  expr' <- built checked expr
  attr' <- builtAttribute attr
  pure (printf "ExDispatch %s %s" <$> fmap parens expr' <*> fmap parens attr')
built checked (ExApplication expr (ArTau attr arg)) = do
  expr' <- built checked expr
  attr' <- builtAttribute attr
  arg' <- built checked arg
  pure (printf "ExApplication %s (ArTau %s %s)" <$> fmap parens expr' <*> fmap parens attr' <*> fmap parens arg')
built checked (ExApplication expr (ArAlpha alpha arg)) = do
  expr' <- built checked expr
  alpha' <- builtAlpha alpha
  arg' <- built checked arg
  pure (printf "ExApplication %s (ArAlpha %s %s)" <$> fmap parens expr' <*> fmap parens alpha' <*> fmap parens arg')
built _ expr = refuse (printf "it builds the term '%s', which only a rule of YAML can build" (show expr))

-- The Haskell building one binding of a template (see 'built').
builtBinding :: Bool -> Binding -> Emitting (Maybe String)
builtBinding checked (BiTau attr expr) = do
  attr' <- builtAttribute attr
  expr' <- built checked expr
  pure (printf "BiTau %s %s" <$> fmap parens attr' <*> fmap parens expr')
builtBinding _ (BiVoid attr) = fmap (("BiVoid " ++) . parens) <$> builtAttribute attr
builtBinding _ (BiDelta (BtMeta meta)) = fmap ("BiDelta " ++) <$> known (Named meta)
builtBinding _ (BiDelta (BtAny _)) = pure Nothing
builtBinding _ (BiDelta bts) = pure (Just ("BiDelta " ++ parens (show bts)))
builtBinding _ (BiLambda (FnMeta meta)) = fmap ("BiLambda " ++) <$> known (Named meta)
builtBinding _ (BiLambda (FnAny _)) = pure Nothing
builtBinding _ (BiLambda (FnFresh _)) = refuse "it builds a fresh symbol"
builtBinding _ (BiLambda func) = Just . ("BiLambda " ++) . parens <$> function func
builtBinding _ bd = refuse (printf "it builds the binding '%s'" (show bd))

-- The Haskell of the data of a template (see 'buildBytes').
builtBytes :: Bytes -> Emitting (Maybe String)
builtBytes (BtMeta meta) = known (Named meta)
builtBytes (BtAny slot) = known (Anon slot)
builtBytes bts = pure (Just (parens (show bts)))

-- The Haskell of an attribute of a template (see 'buildAttribute').
builtAttribute :: Attribute -> Emitting (Maybe String)
builtAttribute (AtMeta meta) = known (Named meta)
builtAttribute (AtAny _) = pure Nothing
builtAttribute attr = Just <$> attributed attr

-- The Haskell of an index of a template (see 'buildAlpha').
builtAlpha :: Alpha -> Emitting (Maybe String)
builtAlpha (AlMeta meta) = fmap ("Alpha " ++) <$> known (Named meta)
builtAlpha (AlAny _) = pure Nothing
builtAlpha (Alpha idx) = pure (Just (printf "Alpha %d" idx))

-- The Haskell of an attribute no meta stands for.
attributed :: Attribute -> Emitting String
attributed (AtLabel label) = pure (printf "AtLabel (%s)" (texted label))
attributed AtPhi = pure "AtPhi"
attributed AtRho = pure "AtRho"
attributed AtLambda = pure "AtLambda"
attributed AtDelta = pure "AtDelta"
attributed attr = refuse (printf "it holds the attribute '%s' where a literal one stands" (show attr))

-- The Haskell of a λ function no meta stands for.
function :: Function -> Emitting String
function (Function name) = pure (printf "Function (%s)" (texted name))
function (FnSymbol idx) = pure (printf "FnSymbol %d" idx)
function func = refuse (printf "it holds the λ function '%s' where a literal one stands" (show func))

-- The Haskell of a text.
texted :: T.Text -> String
texted text = "T.pack " ++ show (T.unpack text)

-- Whether the pattern applies Φ to a ρ anywhere, which the builder turns into
-- Φ alone, so the place the matcher matched is not the term the replacer
-- looks for (see 'buildExpression').
rooted :: Expression -> Bool
rooted (ExApplication ExRoot (ArTau AtRho _)) = True
rooted (ExApplication expr (ArTau _ arg)) = rooted expr || rooted arg
rooted (ExApplication expr (ArAlpha _ arg)) = rooted expr || rooted arg
rooted (ExDispatch expr _) = rooted expr
rooted (ExFormation bds) = or [rooted expr | BiTau _ expr <- bds]
rooted _ = False

-- The variable a meta is held in where the pattern bound it.
known :: Meta -> Emitting (Maybe String)
known key = Emitting (\scope@(Scope bound _) -> Right (Map.lookup key bound, scope))

-- The variable a meta is held in, where the pattern bound it, or a refusal.
held :: Meta -> Emitting String
held key = known key >>= maybe (refuse "it asks a normal form of a meta its pattern does not bind") pure

-- Remember the variable a meta is held in.
bind :: Meta -> String -> Emitting ()
bind key var = Emitting (\(Scope bound next) -> Right ((), Scope (Map.insert key var bound) next))

-- A variable no meta and no other variable of the rule is held in.
fresh :: Emitting String
fresh = Emitting (\(Scope bound next) -> Right ("x" ++ show next, Scope bound (next + 1)))

-- The variable a premise binds its meta in, which neither the pattern nor a
-- premise before it may have bound.
introduced :: T.Text -> Emitting String
introduced result =
  known (Named result) >>= \case
    Just _ -> refuse (printf "its premise '%s' binds a meta bound already" (T.unpack result))
    Nothing -> do
      let var = variable (Named result)
      bind (Named result) var
      pure var

-- A refusal of the rule, saying why.
refuse :: String -> Emitting a
refuse reason = Emitting (const (Left reason))

-- The variable a meta a rule binds by itself is held in, named after the
-- meta where its name is a plain one.
variable :: Meta -> String
variable (Named meta) = case T.unpack meta of
  first : rest | all isDigit rest -> toLower first : rest
  name -> "m_" ++ map (\char -> if isAlphaNum char then char else '_') name
variable (Anon (Slot kind offset)) = printf "a_%s%d" (T.unpack kind) offset

-- A list of one element.
single :: String -> String
single var = "[" ++ var ++ "]"

-- The Haskell of a list of the elements.
listed :: [String] -> String
listed items = "[" ++ intercalate ", " items ++ "]"

-- The same, one element per line, indented by the given number of spaces.
listed' :: Int -> [String] -> String
listed' _ [] = "[]"
listed' indent items = "[ " ++ intercalate ("\n" ++ replicate indent ' ' ++ ", ") items ++ "\n" ++ replicate indent ' ' ++ "]"

-- The piece of Haskell in parentheses, unless it is one word.
parens :: String -> String
parens text
  | all (\char -> isAlphaNum char || char == '_' || char == '\'') text = text
  | otherwise = "(" ++ text ++ ")"
