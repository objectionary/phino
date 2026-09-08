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
import Data.FileEmbed (embedDir, embedFile)
import Data.Text (Text, unpack)
import Data.Yaml (Parser)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
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
  parseJSON =
    withObject
      "Condition"
      ( \v -> do
          validateYamlObject v ["and", "or", "not", "nf", "absolute", "eq", "gt", "in", "matches", "part-of", "disjoint", "formation"]
          asum
            [ do
                conds <- v .: "and"
                if null conds
                  then fail "The 'and' condition requires at least one element"
                  else pure (And conds)
            , do
                conds <- v .: "or"
                if null conds
                  then fail "The 'or' condition requires at least one element"
                  else pure (Or conds)
            , Not <$> v .: "not"
            , NF <$> v .: "nf"
            , Absolute <$> v .: "absolute"
            , IsFormation <$> v .: "formation"
            , do
                vals <- v .: "disjoint"
                case vals of
                  [attrs_, bds_] -> Disjoint <$> parseJSON attrs_ <*> parseJSON bds_
                  _ -> fail "'disjoint' expects exactly two arguments"
            , do
                vals <- v .: "eq"
                case vals of
                  [left_, right_] -> Eq <$> parseJSON left_ <*> parseJSON right_
                  _ -> fail "'eq' expects exactly two arguments"
            , do
                vals <- v .: "gt"
                case vals of
                  [left_, right_] -> Gt <$> parseJSON left_ <*> parseJSON right_
                  _ -> fail "'gt' expects exactly two arguments"
            , do
                vals <- v .: "in"
                case vals of
                  [attr_, binding_] -> do
                    attr <- parseJSON attr_
                    bd <- parseJSON binding_
                    pure (In attr bd)
                  _ -> fail "'in' expects exactly two arguments"
            , do
                vals <- v .: "matches"
                case vals of
                  [pat, ex] -> Matches <$> parseJSON pat <*> parseJSON ex
                  _ -> fail "'matches' expects exactly two arguments"
            , do
                vals <- v .: "part-of"
                case vals of
                  [ex, bd] -> PartOf <$> parseJSON ex <*> parseJSON bd
                  _ -> fail "'part-of' expects exactly two arguments"
            ]
      )

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

normalizationRules :: [Rule]
{-# NOINLINE normalizationRules #-}
normalizationRules = map decodeRule $(embedDir "resources/normalize")
  where
    decodeRule :: (FilePath, BS.ByteString) -> Rule
    decodeRule (path, bs) =
      case Yaml.decodeEither' bs of
        Right rule -> rule
        Left err -> error $ "YAML parse error in " ++ path ++ ": " ++ show err

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

decodeRules :: (FromJSON a) => FilePath -> BS.ByteString -> [a]
decodeRules path bs = case Yaml.decodeEither' bs of
  Right rs -> rs
  Left err -> error $ "YAML parse error in " ++ path ++ ": " ++ show err

morphingRules :: [MorphRule]
{-# NOINLINE morphingRules #-}
morphingRules = decodeRules "resources/morphing.yaml" $(embedFile "resources/morphing.yaml")

dataizationRules :: [DataizeRule]
{-# NOINLINE dataizationRules #-}
dataizationRules = decodeRules "resources/dataization.yaml" $(embedFile "resources/dataization.yaml")

contextualizationRules :: [ContextualizeRule]
{-# NOINLINE contextualizationRules #-}
contextualizationRules = decodeRules "resources/contextualization.yaml" $(embedFile "resources/contextualization.yaml")
