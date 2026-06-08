{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Text (unpack)
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

instance FromJSON Bytes where
  parseJSON = parseJSON' "Bytes" parseBytes

instance FromJSON Expression where
  parseJSON = parseJSON' "Expression" parseExpression

instance FromJSON Binding where
  parseJSON = parseJSON' "Binding" parseBinding

instance FromJSON Number where
  parseJSON v = case v of
    Object o -> do
      validateYamlObject o ["index", "length", "domain"]
      asum
        [ Index <$> o .: "index"
        , Length <$> o .: "length"
        , Domain <$> o .: "domain"
        ]
    Number num -> pure (Literal (round num))
    _ ->
      fail "Expected a numerable expression (object or number)"

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
          validateYamlObject v ["and", "or", "not", "alpha", "nf", "xi-free", "eq", "in", "matches", "part-of", "primitive", "disjoint"]
          asum
            [ And <$> v .: "and"
            , Or <$> v .: "or"
            , Not <$> v .: "not"
            , Alpha <$> v .: "alpha"
            , NF <$> v .: "nf"
            , XiFree <$> v .: "xi-free"
            , Primitive <$> v .: "primitive"
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
  = Index Attribute
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
  | Alpha Attribute
  | Eq Comparable Comparable
  | NF Expression
  | XiFree Expression
  | Matches String Expression
  | PartOf Expression Binding
  | Primitive Expression
  | Disjoint [Attribute] [Binding]
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
        Right r -> r
        Left err -> error $ "YAML parse error in " ++ path ++ ": " ++ show err

yamlRule :: FilePath -> IO Rule
yamlRule = Yaml.decodeFileThrow

-- The right-hand side of a morphing reduction 𝕄(match) ⟿ then.
-- A mapping ('{ morph: arg }') keeps reducing under 𝕄; a bare expression
-- (including ⊥) is the terminal primitive result.
data MorphOutcome
  = MoMorph MorphArg
  | MoStop Expression
  deriving (Eq, Generic, Show)

-- The argument of a morphing continuation: either a plain expression ('𝕄(e)')
-- or the normalization of one ('𝕄(𝒩(e))', written '{ normalize: e }').
data MorphArg
  = MaExpr Expression
  | MaNormalize Expression
  deriving (Eq, Generic, Show)

-- The right-hand side of a dataization reduction 𝔻(match) ⟿ then.
-- A mapping ('{ dataize: arg }') keeps reducing under 𝔻; a bare bytes scalar
-- yields data; the 'nothing' keyword marks the function as undefined.
data DataizeOutcome
  = DoDataize DataizeArg
  | DoData Bytes
  | DoNothing
  deriving (Eq, Generic, Show)

-- The argument of a dataization continuation: either a plain expression
-- ('𝔻(e)') or the morphing of one ('𝔻(𝕄(e))', written '{ morph: e }').
data DataizeArg
  = DaExpr Expression
  | DaMorph Expression
  deriving (Eq, Generic, Show)

-- One ordered morphing rule: match the expression, build extra metas in
-- 'where', filter by 'when', then reduce per 'then'.
data MorphRule = MorphRule
  { name :: String
  , description :: Maybe String
  , match :: Expression
  , where_ :: Maybe [Extra]
  , when :: Maybe Condition
  , then_ :: MorphOutcome
  }
  deriving (Generic, Show)

-- One ordered dataization rule, structured like 'MorphRule' but reducing
-- under 𝔻 and able to terminate with bytes or 'nothing'.
data DataizeRule = DataizeRule
  { name :: String
  , description :: Maybe String
  , match :: Expression
  , where_ :: Maybe [Extra]
  , when :: Maybe Condition
  , then_ :: DataizeOutcome
  }
  deriving (Generic, Show)

instance FromJSON MorphOutcome where
  parseJSON (Object o) = do
    validateYamlObject o ["morph"]
    MoMorph <$> o .: "morph"
  parseJSON v = MoStop <$> parseJSON v

instance FromJSON MorphArg where
  parseJSON (Object o) = do
    validateYamlObject o ["normalize"]
    MaNormalize <$> o .: "normalize"
  parseJSON v = MaExpr <$> parseJSON v

instance FromJSON DataizeOutcome where
  parseJSON (Object o) = do
    validateYamlObject o ["dataize"]
    DoDataize <$> o .: "dataize"
  parseJSON (String "nothing") = pure DoNothing
  parseJSON v = DoData <$> parseJSON v

instance FromJSON DataizeArg where
  parseJSON (Object o) = do
    validateYamlObject o ["morph"]
    DaMorph <$> o .: "morph"
  parseJSON v = DaExpr <$> parseJSON v

instance FromJSON MorphRule where
  parseJSON =
    withObject
      "MorphRule"
      ( \o ->
          MorphRule
            <$> o .: "name"
            <*> o .:? "description"
            <*> o .: "match"
            <*> o .:? "where"
            <*> o .:? "when"
            <*> o .: "then"
      )

instance FromJSON DataizeRule where
  parseJSON =
    withObject
      "DataizeRule"
      ( \o ->
          DataizeRule
            <$> o .: "name"
            <*> o .:? "description"
            <*> o .: "match"
            <*> o .:? "where"
            <*> o .:? "when"
            <*> o .: "then"
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
