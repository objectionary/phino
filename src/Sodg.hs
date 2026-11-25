{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Sodg
  ( Command (..)
  , SodgException (..)
  , commands
  , sodg
  , printCommands
  , untested
  , vertex
  )
where

import AST
import Control.Exception (Exception, throwIO)
import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Printer (printExpression)

data Command
  = Formation Int [Text]
  | Dispatch Int Int Text
  | Application Int Int Text Int
  | Put Int Text Int
  | Delta Int Text
  | Lambda Int Text
  | Terminator Int
  deriving (Eq, Show)

data SodgException
  = Patterned Text Text
  | Unbound Text
  deriving (Exception)

instance Show SodgException where
  show (Patterned place what) =
    T.unpack ("The term at " <> place <> " holds " <> what <> ", which only a pattern of a rule may hold, not a program")
  show (Unbound place) =
    T.unpack ("The ξ at " <> place <> " has no formation around it")

type Walk = (Int, [Command])

commands :: Expression -> Either SodgException [Command]
commands expr = reverse . snd <$> top expr
  where
    top :: Expression -> Either SodgException Walk
    top formation@(ExFormation _) = snd <$> go Nothing "Φ" formation (0, [])
    top other = snd <$> go Nothing "Φ" other (1, [Formation 0 []])
    go :: Maybe Int -> Text -> Expression -> Walk -> Either SodgException (Int, Walk)
    go _ place (ExFormation bds) (next, done) = do
      names <- concat <$> traverse (listed place) bds
      walked <- foldM (binding next place) (next + 1, Formation next names : done) bds
      pure (next, walked)
    go (Just around) _ ExXi walk = Right (around, walk)
    go Nothing place ExXi _ = Left (Unbound place)
    go _ _ ExRoot walk = Right (0, walk)
    go _ _ ExTermination (next, done) = Right (next, (next + 1, Terminator next : done))
    go around place (ExDispatch base attr) walk = do
      name <- attribute place attr
      (from, (next, done)) <- go around place base walk
      pure (next, (next + 1, Dispatch next from name : done))
    go around place (ExApplication proto arg) walk = do
      (name, value) <- argument place arg
      (from, walked) <- go around place proto walk
      (kid, (next, done)) <- go around (place <> "." <> name) value walked
      pure (next, (next + 1, Application next from name kid : done))
    go _ place pattern _ = patterned place pattern
    listed :: Text -> Binding -> Either SodgException [Text]
    listed place = \case
      BiTau attr _ -> pure <$> attribute place attr
      BiVoid attr -> pure <$> attribute place attr
      BiDelta _ -> Right []
      BiLambda _ -> Right []
      other -> patterned place (ExFormation [other])
    binding :: Int -> Text -> Walk -> Binding -> Either SodgException Walk
    binding self place walk = \case
      BiTau attr value -> do
        name <- attribute place attr
        (kid, (next, done)) <- go (Just self) (place <> "." <> name) value walk
        pure (next, Put self name kid : done)
      BiVoid _ -> Right walk
      BiDelta bts -> (\data' -> fmap (Delta self data' :) walk) <$> bytes place bts
      BiLambda fn -> (\name -> fmap (Lambda self name :) walk) <$> function place fn
      _ -> Right walk
    attribute :: Text -> Attribute -> Either SodgException Text
    attribute place = \case
      AtLabel label -> Right label
      AtPhi -> Right "φ"
      AtRho -> Right "ρ"
      other -> patterned place (ExDispatch ExXi other)
    argument :: Text -> Argument -> Either SodgException (Text, Expression)
    argument place (ArTau attr value) = (,value) <$> attribute place attr
    bytes :: Text -> Bytes -> Either SodgException Text
    bytes place = \case
      BtEmpty -> Right "--"
      BtOne byte -> Right (T.pack byte <> "-")
      BtMany many -> Right (T.intercalate "-" (map T.pack many))
      other -> patterned place (ExFormation [BiDelta other])
    patterned :: Text -> Expression -> Either SodgException result
    patterned place pattern = Left (Patterned place (T.pack (printExpression pattern)))
    function :: Text -> Function -> Either SodgException Text
    function place = \case
      Function name -> Right name
      FnSymbol idx -> Right ("𝜎" <> T.pack (show idx))
      other -> patterned place (ExFormation [BiLambda other])

untested :: Expression -> Expression
untested = \case
  ExFormation bds -> ExFormation (map inner (filter kept bds))
  ExDispatch base attr -> ExDispatch (untested base) attr
  ExApplication proto (ArTau attr value) -> ExApplication (untested proto) (ArTau attr (untested value))
  other -> other
  where
    kept :: Binding -> Bool
    kept = \case
      BiTau (AtLabel label) _ -> not (test label)
      BiVoid (AtLabel label) -> not (test label)
      _ -> True
    test :: Text -> Bool
    test label = any (`T.isPrefixOf` label) ["p🌵", "n🌵", "+", "-"]
    inner :: Binding -> Binding
    inner = \case
      BiTau attr value -> BiTau attr (untested value)
      other -> other

sodg :: Expression -> IO [Command]
sodg = either throwIO pure . commands

vertex :: Int -> String
vertex idx = 'c' : show idx

printCommands :: [Command] -> String
printCommands = concatMap ((++ "\n") . unwords . spelled)
  where
    spelled :: Command -> [String]
    spelled = \case
      Formation self names -> "F" : vertex self : map T.unpack names
      Dispatch self base attr -> ["D", vertex self, vertex base, T.unpack attr]
      Application self proto attr arg -> ["A", vertex self, vertex proto, T.unpack attr, vertex arg]
      Put self attr kid -> ["P", vertex self, T.unpack attr, vertex kid]
      Delta self data' -> ["Δ", vertex self, T.unpack data']
      Lambda self name -> ["λ", vertex self, T.unpack name]
      Terminator self -> ["T", vertex self]
