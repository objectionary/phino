{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Yaml where

import AST
import Control.Applicative (asum)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import Data.Text (Text, unpack)
import Data.Yaml (Parser)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Metas
import Parser
import Slots
import Text.Printf (printf)

-- Fail unless the object names exactly one of the expected keys
validateYamlObject :: (MonadFail a) => Object -> [String] -> a ()
validateYamlObject v keys
  | length current > 1 = fail ("Exactly one condition type is expected, when multiple condition types specified: " ++ show current)
  | null present = fail (printf "Unknown condition type '%s', expected one of: %s" (show current) (show keys))
  | otherwise = pure ()
  where
    present :: [Key.Key]
    present = filter (`KeyMap.member` v) (map Key.fromString keys)
    current :: [Key.Key]
    current = KeyMap.keys v

parseJSON' :: String -> (String -> Either String a) -> Value -> Parser a
parseJSON' nm func =
  withText
    nm
    ( \txt -> case func (unpack txt) of
        Left err -> fail err
        Right parsed -> pure parsed
    )

instance FromJSON Attribute where
  parseJSON =
    withText
      "Attribute"
      ( \txt -> case unpack txt of
          "λ" -> pure AtLambda
          "Δ" -> pure AtDelta
          other -> case parseAttribute other of
            Left err -> fail err
            Right attr -> pure attr
      )

instance FromJSON Alpha where
  parseJSON = parseJSON' "Alpha" parseAlpha

instance FromJSON Bytes where
  parseJSON = parseJSON' "Bytes" parseBytes

instance FromJSON Expression where
  parseJSON = parseJSON' "Expression" parseExpression

instance FromJSON Binding where
  parseJSON = parseJSON' "Binding" parseBinding

instance FromJSON Number where
  parseJSON v = case v of
    Object o -> do
      validateYamlObject o ["length", "domain"]
      asum
        [ Length <$> o .: "length"
        , Domain <$> o .: "domain"
        ]
    Number num
      | toRational (round num :: Integer) == toRational num -> pure (Literal (round num))
      | otherwise -> fail (printf "Expected an integer, got a fractional number %s" (show num))
    String txt -> case parseIndex (unpack txt) of
      Right (Right mt) -> pure (MetaIndex mt)
      Right (Left slot) -> pure (AnyIndex slot)
      Left err -> fail err
    _ ->
      fail "Expected a numerable expression (object, number or index meta)"

instance FromJSON Comparable where
  parseJSON v =
    asum
      [ CmpAttr <$> parseJSON v
      , CmpNum <$> parseJSON v
      , CmpExpr <$> parseJSON v
      ]

instance FromJSON Condition where
  parseJSON = withObject "Condition" parseCondition

parseCondition :: Object -> Parser Condition
parseCondition v = do
  validateYamlObject v ["and", "or", "not", "nf", "absolute", "eq", "gt", "in", "matches", "part-of", "disjoint", "formation", "object"]
  case KeyMap.keys v of
    [key] -> case Key.toString key of
      "and" -> do
        conds <- v .: "and"
        if null conds
          then fail "The 'and' condition requires at least one element"
          else pure (And conds)
      "or" -> do
        conds <- v .: "or"
        if null conds
          then fail "The 'or' condition requires at least one element"
          else pure (Or conds)
      "not" -> Not <$> v .: "not"
      "nf" -> NF <$> v .: "nf"
      "absolute" -> Absolute <$> v .: "absolute"
      "formation" -> IsFormation <$> v .: "formation"
      "object" -> IsObject <$> v .: "object"
      "disjoint" -> do
        vals <- v .: "disjoint"
        case vals of
          [attrs_, bds_] -> Disjoint <$> parseJSON attrs_ <*> parseJSON bds_
          _ -> fail "'disjoint' expects exactly two arguments"
      "eq" -> do
        vals <- v .: "eq"
        case vals of
          [left_, right_] -> Eq <$> parseJSON left_ <*> parseJSON right_
          _ -> fail "'eq' expects exactly two arguments"
      "gt" -> do
        vals <- v .: "gt"
        case vals of
          [left_, right_] -> Gt <$> parseJSON left_ <*> parseJSON right_
          _ -> fail "'gt' expects exactly two arguments"
      "in" -> do
        vals <- v .: "in"
        case vals of
          [attr_, binding_] -> In <$> parseJSON attr_ <*> parseJSON binding_
          _ -> fail "'in' expects exactly two arguments"
      "matches" -> do
        vals <- v .: "matches"
        case vals of
          [pat, ex] -> Matches <$> parseJSON pat <*> parseJSON ex
          _ -> fail "'matches' expects exactly two arguments"
      "part-of" -> do
        vals <- v .: "part-of"
        case vals of
          [ex, bd] -> PartOf <$> parseJSON ex <*> parseJSON bd
          _ -> fail "'part-of' expects exactly two arguments"
      _ -> fail "Unknown condition type"
    _ -> fail "Exactly one condition type is expected"

instance FromJSON ExtraArgument where
  parseJSON v =
    asum
      [ ArgAttribute <$> parseJSON v
      , ArgBinding <$> parseJSON v
      , ArgExpression <$> parseJSON v
      , ArgBytes <$> parseJSON v
      ]

instance FromJSON Extra where
  parseJSON =
    withObject
      "Extra"
      ( \o ->
          Extra
            <$> o .: "meta"
            <*> o .: "function"
            <*> o .:? "args" .!= []
      )

instance FromJSON Rule where
  parseJSON value = do
    rule <-
      genericParseJSON
        defaultOptions
          { fieldLabelModifier = \case
              "where_" -> "where"
              "ematch" -> "e-match"
              other -> other
          }
        value
    referenceless rule.name "result" rule.result
    referenceless rule.name "when" rule.when
    referenceless rule.name "where" rule.where_
    referenceless rule.name "having" rule.having
    pure rule

data Number
  = MetaIndex Text
  | AnyIndex Slot
  | Length Binding
  | Domain Binding
  | Literal Int
  deriving (Eq, Generic, Show)

data Comparable
  = CmpAttr Attribute
  | CmpNum Number
  | CmpExpr Expression
  deriving (Eq, Generic, Show)

data Condition
  = And [Condition]
  | Or [Condition]
  | In Attribute Binding
  | Not Condition
  | Eq Comparable Comparable
  | Gt Comparable Comparable
  | NF Expression
  | Absolute Expression
  | Matches String Expression
  | PartOf Expression Binding
  | Disjoint [Attribute] [Binding]
  | IsFormation Expression
  | IsObject Expression
  deriving (Eq, Generic, Show)

data ExtraArgument
  = ArgAttribute Attribute
  | ArgExpression Expression
  | ArgBinding Binding
  | ArgBytes Bytes
  deriving (Generic, Show)

data Extra = Extra
  { meta :: ExtraArgument
  , function :: String
  , args :: [ExtraArgument]
  }
  deriving (Generic, Show)

data Rule = Rule
  { name :: String
  , label :: Maybe String
  , description :: Maybe String
  , pattern :: Expression
  , -- The universe-argument matcher, the one 'MorphRule' spells as 'ematch'.
    -- A rewriting rule is about a term and knows nothing of the world around
    -- it, so almost every rule leaves this out; a rule that does carry one is
    -- matched against the universe too and reads what it binds, which is how
    -- 'dot' tells the formation it dispatched from the whole program (#1318).
    ematch :: Maybe Expression
  , result :: Expression
  , when :: Maybe Condition
  , where_ :: Maybe [Extra]
  , having :: Maybe Condition
  }
  deriving (Generic, Show)

instance Slots Condition where
  slots (And conds) = slots conds
  slots (Or conds) = slots conds
  slots (Not cond) = slots cond
  slots (In attr bd) = slots attr ++ slots bd
  slots (Eq left right) = slots left ++ slots right
  slots (Gt left right) = slots left ++ slots right
  slots (NF expr) = slots expr
  slots (Absolute expr) = slots expr
  slots (Matches _ expr) = slots expr
  slots (PartOf expr bd) = slots expr ++ slots bd
  slots (Disjoint attrs bds) = slots attrs ++ slots bds
  slots (IsFormation expr) = slots expr
  slots (IsObject expr) = slots expr

instance Slots Comparable where
  slots (CmpAttr attr) = slots attr
  slots (CmpNum num) = slots num
  slots (CmpExpr expr) = slots expr

instance Slots Number where
  slots (AnyIndex slot) = [slot]
  slots (Length bd) = slots bd
  slots (Domain bd) = slots bd
  slots (MetaIndex _) = []
  slots (Literal _) = []

instance Slots ExtraArgument where
  slots (ArgAttribute attr) = slots attr
  slots (ArgExpression expr) = slots expr
  slots (ArgBinding bd) = slots bd
  slots (ArgBytes bts) = slots bts

instance Slots Extra where
  slots extra = slots extra.meta ++ slots extra.args

instance Slots Premise where
  slots premise = slots premise.operation

instance Slots Operation where
  slots (OpMorph expr) = slots expr
  slots (OpNormalize expr) = slots expr
  slots (OpEvaluate expr universe) = slots expr ++ slots universe
  slots (OpContextualize expr context) = slots expr ++ slots context
  slots (OpDataize expr) = slots expr
  slots (OpObject expr) = slots expr

instance Metas Condition where
  metas (And conds) = metas conds
  metas (Or conds) = metas conds
  metas (Not cond) = metas cond
  metas (In attr bd) = metas attr ++ metas bd
  metas (Eq left right) = metas left ++ metas right
  metas (Gt left right) = metas left ++ metas right
  metas (NF expr) = metas expr
  metas (Absolute expr) = metas expr
  metas (Matches _ expr) = metas expr
  metas (PartOf expr bd) = metas expr ++ metas bd
  metas (Disjoint attrs bds) = metas attrs ++ metas bds
  metas (IsFormation expr) = metas expr
  metas (IsObject expr) = metas expr
  bare names (And conds) = And (bare names conds)
  bare names (Or conds) = Or (bare names conds)
  bare names (Not cond) = Not (bare names cond)
  bare names (In attr bd) = In (bare names attr) (bare names bd)
  bare names (Eq left right) = Eq (bare names left) (bare names right)
  bare names (Gt left right) = Gt (bare names left) (bare names right)
  bare names (NF expr) = NF (bare names expr)
  bare names (Absolute expr) = Absolute (bare names expr)
  bare names (Matches regex expr) = Matches regex (bare names expr)
  bare names (PartOf expr bd) = PartOf (bare names expr) (bare names bd)
  bare names (Disjoint attrs bds) = Disjoint (bare names attrs) (bare names bds)
  bare names (IsFormation expr) = IsFormation (bare names expr)
  bare names (IsObject expr) = IsObject (bare names expr)

instance Metas Comparable where
  metas (CmpAttr attr) = metas attr
  metas (CmpNum num) = metas num
  metas (CmpExpr expr) = metas expr
  bare names (CmpAttr attr) = CmpAttr (bare names attr)
  bare names (CmpNum num) = CmpNum (bare names num)
  bare names (CmpExpr expr) = CmpExpr (bare names expr)

instance Metas Number where
  metas (MetaIndex named) = metas named
  metas (AnyIndex slot) = metas slot
  metas (Length bd) = metas bd
  metas (Domain bd) = metas bd
  metas (Literal _) = []
  bare names (MetaIndex named) = MetaIndex (bare names named)
  bare names (Length bd) = Length (bare names bd)
  bare names (Domain bd) = Domain (bare names bd)
  bare _ num = num

instance Metas ExtraArgument where
  metas (ArgAttribute attr) = metas attr
  metas (ArgExpression expr) = metas expr
  metas (ArgBinding bd) = metas bd
  metas (ArgBytes bts) = metas bts
  bare names (ArgAttribute attr) = ArgAttribute (bare names attr)
  bare names (ArgExpression expr) = ArgExpression (bare names expr)
  bare names (ArgBinding bd) = ArgBinding (bare names bd)
  bare names (ArgBytes bts) = ArgBytes (bare names bts)

instance Metas Extra where
  metas extra = metas extra.meta ++ metas extra.args
  bare names extra = extra{meta = bare names extra.meta, args = bare names extra.args}

instance Metas Premise where
  metas premise = metas premise.result ++ metas premise.operation
  bare names premise = premise{result = bare names premise.result, operation = bare names premise.operation}

instance Metas Operation where
  metas (OpMorph expr) = metas expr
  metas (OpNormalize expr) = metas expr
  metas (OpEvaluate expr universe) = metas expr ++ metas universe
  metas (OpContextualize expr context) = metas expr ++ metas context
  metas (OpDataize expr) = metas expr
  metas (OpObject expr) = metas expr
  bare names (OpMorph expr) = OpMorph (bare names expr)
  bare names (OpNormalize expr) = OpNormalize (bare names expr)
  bare names (OpEvaluate expr universe) = OpEvaluate (bare names expr) (bare names universe)
  bare names (OpContextualize expr context) = OpContextualize (bare names expr) (bare names context)
  bare names (OpDataize expr) = OpDataize (bare names expr)
  bare names (OpObject expr) = OpObject (bare names expr)

-- A rule is the scope an index counts in: the reader meets the metas of one
-- inference within it and nowhere else, so a kind the rule names just once
-- carries no index anywhere in the rule.
instance Metas Rule where
  metas rule = metas rule.pattern ++ metas rule.ematch ++ metas rule.result ++ metas rule.when ++ metas rule.having ++ metas rule.where_
  bare names rule =
    rule
      { pattern = bare names rule.pattern
      , ematch = bare names rule.ematch
      , result = bare names rule.result
      , when = bare names rule.when
      , having = bare names rule.having
      , where_ = bare names rule.where_
      }

instance Metas MorphRule where
  metas rule = metas rule.match ++ metas rule.ematch ++ metas rule.nresult ++ metas rule.when ++ metas rule.premises
  bare names rule =
    rule
      { match = bare names rule.match
      , ematch = bare names rule.ematch
      , nresult = bare names rule.nresult
      , when = bare names rule.when
      , premises = bare names rule.premises
      }

instance Metas DataizeRule where
  metas rule = metas rule.match ++ metas rule.ematch ++ metas rule.dresult ++ metas rule.when ++ metas rule.premises
  bare names rule =
    rule
      { match = bare names rule.match
      , ematch = bare names rule.ematch
      , dresult = bare names rule.dresult
      , when = bare names rule.when
      , premises = bare names rule.premises
      }

instance Metas ContextualizeRule where
  metas rule = metas rule.match ++ metas rule.cmatch ++ metas rule.cresult ++ metas rule.premises
  bare names rule =
    rule
      { match = bare names rule.match
      , cmatch = bare names rule.cmatch
      , cresult = bare names rule.cresult
      , premises = bare names rule.premises
      }

-- An anonymous meta-variable is bound by the pattern it stands in and is
-- forgotten as soon as that pattern matches, so it has no name for any other
-- part of a rule to read it back by. Writing one outside the pattern is
-- therefore a mistake in the rule, not a term to be resolved later, and the
-- rule is rejected as it loads.
referenceless :: (MonadFail m, Slots a) => String -> String -> a -> m ()
referenceless rule field term = case anonymous term of
  Nothing -> pure ()
  Just kind ->
    fail
      ( printf
          "anonymous meta '!%s' cannot be referenced in '%s' of rule '%s'"
          (unpack kind)
          field
          rule
      )

-- Decode one rule out of the file that carries it, naming that file when its
-- YAML is broken. A rule set is a directory 'embedDir' embeds wholesale, one
-- rule per file, the file named after the rule it carries.
decodeRule :: (FromJSON a) => (FilePath, BS.ByteString) -> a
decodeRule (path, bs) = case Yaml.decodeEither' bs of
  Right rule -> rule
  Left err -> error $ "YAML parse error in " ++ path ++ ": " ++ show err

normalizationRules :: [Rule]
{-# NOINLINE normalizationRules #-}
normalizationRules = map decodeRule $(embedDir "resources/normalize")

yamlRule :: FilePath -> IO Rule
yamlRule = Yaml.decodeFileThrow

-- One premise above the inference line of a morphing or dataization rule: bind
-- the meta named 'result' to the value of applying 'operation' to its argument.
-- The universe e is the fixed second argument of 𝕄 and 𝔻, not a per-premise
-- value, so it is not recorded here.
data Premise = Premise
  { result :: Text
  , operation :: Operation
  }
  deriving (Eq, Generic, Show)

-- The reduction a premise performs, mirroring the build-term functions and the
-- 𝒩 and 𝔻 reducers the engine already provides.
data Operation
  = OpMorph Expression
  | OpNormalize Expression
  | OpEvaluate Expression Expression
  | OpContextualize Expression Expression
  | OpDataize Expression
  | OpObject Expression
  deriving (Eq, Generic, Show)

-- One morphing rule in inference-rule form: when 'match' matches the term and
-- 'ematch' matches the universe (binding 'e'), the rule yields 'nresult' (a
-- premise meta or a literal) provided 'when' holds and the ordered 'premises'
-- reduce as stated. 'ematch' is the universe-argument matcher of 𝕄(n, e, s), in
-- practice always the '𝑒' meta.
data MorphRule = MorphRule
  { name :: String
  , label :: Maybe String
  , match :: Expression
  , ematch :: Expression
  , nresult :: Expression
  , when :: Maybe Condition
  , premises :: [Premise]
  }
  deriving (Generic, Show)

-- One dataization rule in inference-rule form, structured like 'MorphRule' but
-- terminating with bytes ('dresult').
data DataizeRule = DataizeRule
  { name :: String
  , label :: Maybe String
  , match :: Expression
  , ematch :: Expression
  , dresult :: Bytes
  , when :: Maybe Condition
  , premises :: [Premise]
  }
  deriving (Generic, Show)

-- One contextualization rule in inference-rule form, structured like 'MorphRule'
-- but binary in 𝒞(n, c): the second argument is the context 'c' ('cmatch',
-- always the 'c' meta) rather than the universe 'e', and the conclusion is the
-- contextualized term 'cresult'.
data ContextualizeRule = ContextualizeRule
  { name :: String
  , label :: Maybe String
  , match :: Expression
  , cmatch :: Expression
  , cresult :: Expression
  , premises :: [Premise]
  }
  deriving (Generic, Show)

instance FromJSON Premise where
  parseJSON =
    withObject
      "Premise"
      (\o -> Premise <$> premiseResult o <*> premiseOperation o)

-- The meta a premise binds, taken from its 'n-result' (an expression meta) or
-- 'd-result' (a bytes meta).
premiseResult :: Object -> Parser Text
premiseResult o = do
  expr <- o .:? "n-result"
  case expr of
    Just (ExMeta metaName) -> pure metaName
    Just (ExAny _) -> fail "an anonymous 'n-result' meta cannot be referenced"
    Just _ -> fail "'n-result' must be an expression meta"
    Nothing -> do
      bytes <- o .:? "d-result"
      case bytes of
        Just (BtMeta metaName) -> pure metaName
        Just (BtAny _) -> fail "an anonymous 'd-result' meta cannot be referenced"
        Just _ -> fail "'d-result' must be a bytes meta"
        Nothing -> fail "a premise needs an 'n-result' or 'd-result' meta"

-- The single verb of a premise.
premiseOperation :: Object -> Parser Operation
premiseOperation o =
  asum
    [ OpMorph <$> o .: "morph"
    , OpNormalize <$> o .: "normalize"
    , do
        vals <- o .: "evaluate"
        case vals of
          [expr, universe] -> OpEvaluate <$> parseJSON expr <*> parseJSON universe
          _ -> fail "'evaluate' expects exactly two arguments"
    , do
        vals <- o .: "contextualize"
        case vals of
          [expr, context] -> OpContextualize <$> parseJSON expr <*> parseJSON context
          _ -> fail "'contextualize' expects exactly two arguments"
    , OpDataize <$> o .: "dataize"
    , OpObject <$> o .: "object"
    ]

-- Parse the optional 'label', rejecting one that merely repeats the rule's
-- 'name'. A label equal to the name typesets the same token across two macros
-- and adds nothing, so it is forbidden: 'label' is meant to carry a symbol that
-- differs from the plain name (for example '\lambda' or 'disp').
parseLabel :: String -> Object -> Parser (Maybe String)
parseLabel ruleName o = do
  label' <- o .:? "label"
  if label' == Just ruleName
    then fail $ "'label' is redundant when it equals 'name' (" ++ ruleName ++ "); drop it"
    else pure label'

instance FromJSON MorphRule where
  parseJSON =
    withObject
      "MorphRule"
      ( \o -> do
          ruleName <- o .: "name"
          rule <-
            MorphRule ruleName
              <$> parseLabel ruleName o
              <*> o .: "match"
              <*> o .: "e-match"
              <*> o .: "n-result"
              <*> o .:? "when"
              <*> o .:? "premises" .!= []
          referenceless ruleName "n-result" rule.nresult
          referenceless ruleName "when" rule.when
          referenceless ruleName "premises" rule.premises
          pure rule
      )

instance FromJSON DataizeRule where
  parseJSON =
    withObject
      "DataizeRule"
      ( \o -> do
          ruleName <- o .: "name"
          rule <-
            DataizeRule ruleName
              <$> parseLabel ruleName o
              <*> o .: "match"
              <*> o .: "e-match"
              <*> o .: "d-result"
              <*> o .:? "when"
              <*> o .:? "premises" .!= []
          referenceless ruleName "d-result" rule.dresult
          referenceless ruleName "when" rule.when
          referenceless ruleName "premises" rule.premises
          pure rule
      )

instance FromJSON ContextualizeRule where
  parseJSON =
    withObject
      "ContextualizeRule"
      ( \o -> do
          ruleName <- o .: "name"
          rule <-
            ContextualizeRule ruleName
              <$> parseLabel ruleName o
              <*> o .: "match"
              <*> o .: "c-match"
              <*> o .: "c-result"
              <*> o .:? "premises" .!= []
          referenceless ruleName "c-result" rule.cresult
          referenceless ruleName "premises" rule.premises
          pure rule
      )

morphingRules :: [MorphRule]
{-# NOINLINE morphingRules #-}
morphingRules = map decodeRule $(embedDir "resources/morphing")

dataizationRules :: [DataizeRule]
{-# NOINLINE dataizationRules #-}
dataizationRules = map decodeRule $(embedDir "resources/dataization")

contextualizationRules :: [ContextualizeRule]
{-# NOINLINE contextualizationRules #-}
contextualizationRules = map decodeRule $(embedDir "resources/contextualization")
