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
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir, embedFile)
import Data.List (find)
import Data.Text (Text, unpack)
import Data.Yaml (Parser)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Misc (validateYamlObject)
import Parser

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
    Number num -> pure (Literal (round num))
    String txt -> case parseIndex (unpack txt) of
      Right mt -> pure (MetaIndex mt)
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
            [ And <$> v .: "and"
            , Or <$> v .: "or"
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
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "where_" -> "where"
            other -> other
        }

data Number
  = MetaIndex Text
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

-- The operational right-hand side of a morphing reduction 𝕄(match) ⟿ then.
-- A mapping ('MoMorph') keeps reducing under 𝕄; a bare expression (including ⊥)
-- is the terminal result. It is no longer parsed from YAML directly: rules are
-- written in inference-rule form and 'morphReduction' recovers this shape.
data MorphOutcome
  = MoMorph MorphArg
  | MoStop Expression
  deriving (Eq, Generic, Show)

-- The argument of a morphing continuation: either a plain expression ('𝕄(e)')
-- or the normalization of one ('𝕄(𝒩(e))').
data MorphArg
  = MaExpr Expression
  | MaNormalize Expression
  deriving (Eq, Generic, Show)

-- The operational right-hand side of a dataization reduction 𝔻(match) ⟿ then.
-- A mapping ('DoDataize') keeps reducing under 𝔻; a bare bytes scalar yields
-- data. 𝔻 is total: every rule yields data or reduces further.
data DataizeOutcome
  = DoDataize DataizeArg
  | DoData Bytes
  deriving (Eq, Generic, Show)

-- The argument of a dataization continuation: a plain expression ('𝔻(e)'), the
-- morphing of one ('𝔻(𝕄(e))') or the normalization of one ('𝔻(𝒩(e))').
data DataizeArg
  = DaExpr Expression
  | DaMorph Expression
  | DaNormalize Expression
  deriving (Eq, Generic, Show)

-- One premise above the inference line of a morphing or dataization rule: bind
-- the meta named 'result' to the value of applying 'operation' to its argument,
-- under the universe 'universe' (present for the binary judgments 𝕄 and 𝔻).
data Premise = Premise
  { result :: Text
  , universe :: Maybe Expression
  , operation :: Operation
  }
  deriving (Eq, Generic, Show)

-- The reduction a premise performs, mirroring the build-term functions and the
-- 𝒩 and 𝔻 reducers the engine already provides.
data Operation
  = OpMorph Expression
  | OpNormalize Expression
  | OpLambda Expression
  | OpContextualize Expression Expression
  | OpDataize Expression
  deriving (Eq, Generic, Show)

-- One morphing rule in inference-rule form: under universe 'euniverse', 'match'
-- yields 'nresult' (a premise meta or a literal) provided 'when' holds and the
-- ordered 'premises' reduce as stated.
data MorphRule = MorphRule
  { name :: String
  , label :: Maybe String
  , match :: Expression
  , euniverse :: Maybe Expression
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
  , euniverse :: Maybe Expression
  , dresult :: Bytes
  , when :: Maybe Condition
  , premises :: [Premise]
  }
  deriving (Generic, Show)

instance FromJSON Premise where
  parseJSON =
    withObject
      "Premise"
      (\o -> Premise <$> premiseResult o <*> o .:? "e-result" <*> premiseOperation o)

-- The meta a premise binds, taken from its 'n-result' (an expression meta) or
-- 'd-result' (a bytes meta).
premiseResult :: Object -> Parser Text
premiseResult o = do
  expr <- o .:? "n-result"
  case expr of
    Just (ExMeta metaName) -> pure metaName
    Just _ -> fail "'n-result' must be an expression meta"
    Nothing -> do
      bytes <- o .:? "d-result"
      case bytes of
        Just (BtMeta metaName) -> pure metaName
        Just _ -> fail "'d-result' must be a bytes meta"
        Nothing -> fail "a premise needs an 'n-result' or 'd-result' meta"

-- The single verb of a premise.
premiseOperation :: Object -> Parser Operation
premiseOperation o =
  asum
    [ OpMorph <$> o .: "morph"
    , OpNormalize <$> o .: "normalize"
    , OpLambda <$> o .: "lambda"
    , do
        vals <- o .: "contextualize"
        case vals of
          [expr, context] -> OpContextualize <$> parseJSON expr <*> parseJSON context
          _ -> fail "'contextualize' expects exactly two arguments"
    , OpDataize <$> o .: "dataize"
    ]

instance FromJSON MorphRule where
  parseJSON =
    withObject
      "MorphRule"
      ( \o ->
          MorphRule
            <$> o .: "name"
            <*> o .:? "label"
            <*> o .: "match"
            <*> o .:? "e-result"
            <*> o .: "n-result"
            <*> o .:? "when"
            <*> o .:? "premises" .!= []
      )

instance FromJSON DataizeRule where
  parseJSON =
    withObject
      "DataizeRule"
      ( \o ->
          DataizeRule
            <$> o .: "name"
            <*> o .:? "label"
            <*> o .: "match"
            <*> o .:? "e-result"
            <*> o .: "d-result"
            <*> o .:? "when"
            <*> o .:? "premises" .!= []
      )

-- Recover the operational ('where', 'then') form of a morphing rule from its
-- inference-rule premises and conclusion. The conclusion meta is produced by a
-- trailing 'morph' premise; a 'morph(normalize(_))' continuation folds the
-- normalize and morph premises into 'then', leaving the rest as 'where'
-- side-computations. A literal conclusion is a terminal result.
morphReduction :: MorphRule -> (Maybe [Extra], MorphOutcome)
morphReduction rule = case producerOf (asMeta rule.nresult) rule.premises of
  Just (concl, OpMorph arg) -> case producerOf (asMeta arg) rule.premises of
    Just (normal, OpNormalize inner) ->
      (extras (rule.premises `without` [concl, normal]), MoMorph (MaNormalize inner))
    _ -> (extras (rule.premises `without` [concl]), MoMorph (MaExpr arg))
  _ -> (extras rule.premises, MoStop rule.nresult)

-- Recover the operational form of a dataization rule, mirroring
-- 'morphReduction' but with a 'dataize(morph(_))' or 'dataize(normalize(_))'
-- continuation and a bytes terminal.
dataizeReduction :: DataizeRule -> (Maybe [Extra], DataizeOutcome)
dataizeReduction rule = case bytesProducer rule.dresult rule.premises of
  Just (concl, OpDataize arg) -> case producerOf (asMeta arg) rule.premises of
    Just (normal, OpNormalize inner) ->
      (extras (rule.premises `without` [concl, normal]), DoDataize (DaNormalize inner))
    Just (morphed, OpMorph inner) ->
      (extras (rule.premises `without` [concl, morphed]), DoDataize (DaMorph inner))
    _ -> (extras (rule.premises `without` [concl]), DoDataize (DaExpr arg))
  _ -> (extras rule.premises, DoData rule.dresult)

-- The premise binding the named meta, with its operation, if any.
producerOf :: Maybe Text -> [Premise] -> Maybe (Premise, Operation)
producerOf (Just metaName) items = (\premise -> (premise, premise.operation)) <$> find (\premise -> premise.result == metaName) items
producerOf Nothing _ = Nothing

bytesProducer :: Bytes -> [Premise] -> Maybe (Premise, Operation)
bytesProducer (BtMeta metaName) = producerOf (Just metaName)
bytesProducer _ = const Nothing

asMeta :: Expression -> Maybe Text
asMeta (ExMeta metaName) = Just metaName
asMeta _ = Nothing

without :: [Premise] -> [Premise] -> [Premise]
without items removed = filter (\premise -> premise.result `notElem` map (.result) removed) items

-- Map the leftover side-computation premises onto 'where' extras, preserving
-- their order so the first one names the dataization step.
extras :: [Premise] -> Maybe [Extra]
extras [] = Nothing
extras items = Just (map asExtra items)
  where
    asExtra :: Premise -> Extra
    asExtra premise = case premise.operation of
      OpContextualize expr context -> Extra (boundMeta premise) "contextualize" [ArgExpression expr, ArgExpression context]
      OpMorph expr -> Extra (boundMeta premise) "morph" [ArgExpression expr]
      OpLambda expr -> Extra (boundMeta premise) "lambda" [ArgExpression expr]
      OpNormalize expr -> Extra (boundMeta premise) "normalize" [ArgExpression expr]
      OpDataize expr -> Extra (boundMeta premise) "dataize" [ArgExpression expr]
    boundMeta :: Premise -> ExtraArgument
    boundMeta premise = ArgExpression (ExMeta premise.result)

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
