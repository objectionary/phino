{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Lambdas
  ( Ending (..)
  , Lambda (..)
  , LambdaException (..)
  , Lambdas
  , Line (..)
  , Meta (..)
  , emptyLambdas
  , matched
  , minted
  , readLambdas
  , taken
  )
where

import AST
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON (parseJSON), Key, Object, withObject, (.!=), (.:?))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Char (isDigit)
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Yaml
import Language (Language, language, shared)
import Logger (logDebug)
import Metas (Metas (metas))
import Parser (parseBytes, parseExpression)
import Slots (Slots (slots))
import Text.Printf (printf)
import Text.Regex.PCRE (matchTest)
import Text.Regex.PCRE.ByteString (Regex, compUTF8, compile, execBlank)
import Yaml (referenceless)

data Meta = Meta
  { _spelling :: Text
  , _name :: Text
  }

data Line
  = Evaluated Meta Meta
  | Deferred Meta Meta

-- How an entry ends: answering with a term, or forking on a datum of
-- 'dataize' into the two sides a path picks between.
data Ending
  = Answering Expression
  | Forking Meta Expression Expression

data Lambda = Lambda
  { _key :: Text
  , _dataized :: [(Meta, Expression)]
  , _morphed :: [(Meta, Expression)]
  , _lines :: [Line]
  , _ending :: Ending
  }

newtype Lambdas = Lambdas [(Regex, Lambda)]

data LambdaException
  = BrokenLambdas FilePath String
  deriving anyclass (Exception)

instance Show LambdaException where
  show (BrokenLambdas file failure) =
    printf "The λ functions of '%s' cannot be read: %s" file failure

instance FromJSON Lambda where
  parseJSON = withObject "Lambda" $ \entry -> do
    key <- entry .:? "λ" >>= maybe (fail "The entry has no 'λ' key") pure
    unknown (T.unpack key) entry
    dataized <- operands key bytesMeta entry "dataize"
    ending <- concluded (T.unpack key) dataized entry
    morphed <- operands key expressionMeta entry "morph"
    lambda <-
      Lambda key dataized morphed
        <$> (written entry >>= ordered (T.unpack key) (map (_name . fst) morphed))
        <*> pure ending
    once (T.unpack key) lambda
    mapM_ (metaless (T.unpack key) "dataize") lambda._dataized
    mapM_ (metaless (T.unpack key) "morph") lambda._morphed
    answered (T.unpack key) lambda
    pure lambda
    where
      unknown :: String -> Object -> Yaml.Parser ()
      unknown key entry = case filter (`notElem` known) (map Key.toText (KeyMap.keys entry)) of
        [] -> pure ()
        name : _ -> fail (printf "The key '%s' of λ function '%s' is none of the keys an entry reads: %s" (T.unpack name) key (T.unpack (T.intercalate ", " known)))
        where
          known :: [Text]
          known = ["λ", "dataize", "morph", "evaluate", "defer", "fork", "𝑛"]
      concluded :: String -> [(Meta, Expression)] -> Object -> Yaml.Parser Ending
      concluded key dataized entry = do
        answer <- entry .:? "𝑛"
        block <- entry .:? "fork"
        case (answer, block) of
          (Nothing, Nothing) -> fail (printf "The entry of λ function '%s' has neither an '𝑛' key nor a 'fork' block" key)
          (Just _, Just _) -> fail (printf "The entry of λ function '%s' has both an '𝑛' key and a 'fork' block, while a fork answers the side it takes" key)
          (Just term, Nothing) -> Answering term <$ sigmas key term <* dataless key term
          (Nothing, Just fork) -> mapM_ alone ["morph", "evaluate", "defer"] >> forking key dataized fork
        where
          alone :: Key -> Yaml.Parser ()
          alone name
            | KeyMap.member name entry = fail (printf "The entry of λ function '%s' has a 'fork' block beside '%s', while a fork takes nothing but 'dataize'" key (Key.toString name))
            | otherwise = pure ()
      forking :: String -> [(Meta, Expression)] -> Object -> Yaml.Parser Ending
      forking key dataized block = do
        case filter (`notElem` ["on", "left", "right"]) (map Key.toText (KeyMap.keys block)) of
          [] -> pure ()
          name : _ -> fail (printf "The key '%s' of the 'fork' block of λ function '%s' is none of its keys: on, left, right" (T.unpack name) key)
        on <- required "on" >>= condition
        Forking on <$> (required "left" >>= side "left") <*> (required "right" >>= side "right")
        where
          required :: FromJSON value => Key -> Yaml.Parser value
          required name = block .:? name >>= maybe (fail (printf "The 'fork' block of λ function '%s' has no '%s' key" key (Key.toString name))) pure
          condition :: Text -> Yaml.Parser Meta
          condition spelling = case parseBytes (T.unpack spelling) of
            Right (BtMeta name) | name `elem` map (_name . fst) dataized -> pure (Meta spelling name)
            _ -> fail (printf "The 'on' of the 'fork' block of λ function '%s' is '%s', while it must name a bytes meta of 'dataize', such as '𝛿1'" key (T.unpack spelling))
          side :: String -> Expression -> Yaml.Parser Expression
          side name term = case metas term of
            [] -> pure term
            meta : _ -> fail (printf "The side '%s' of the 'fork' block of λ function '%s' reads the meta '%s', while only a path from '$' can be reduced there" name key (T.unpack meta))
      operands :: Text -> (Text -> Yaml.Parser Meta) -> Object -> Key -> Yaml.Parser [(Meta, Expression)]
      operands key kind entry name = do
        mapping <- entry .:? name .!= (Map.empty :: Map Text Expression)
        mapM bound (numbered mapping)
        where
          bound :: (Text, Expression) -> Yaml.Parser (Meta, Expression)
          bound (meta, term) = do
            referenceless (T.unpack key) (T.unpack meta) term
            kind meta >>= \named -> pure (named, term)
      expressionMeta :: Text -> Yaml.Parser Meta
      expressionMeta meta = case parseExpression (T.unpack meta) of
        Right (ExMeta name) -> pure (Meta meta name)
        _ -> fail (printf "The operand '%s' is not an expression meta, such as '𝑛1'" (T.unpack meta))
      bytesMeta :: Text -> Yaml.Parser Meta
      bytesMeta meta = case parseBytes (T.unpack meta) of
        Right (BtMeta name) -> pure (Meta meta name)
        _ -> fail (printf "The operand '%s' is not a bytes meta, such as '𝛿1'" (T.unpack meta))
      written :: Object -> Yaml.Parser [Line]
      written entry = (++) <$> links Evaluated entry "evaluate" <*> links Deferred entry "defer"
      links :: (Meta -> Meta -> Line) -> Object -> Key -> Yaml.Parser [Line]
      links kind entry name = do
        mapping <- entry .:? name .!= (Map.empty :: Map Text Text)
        mapM (\(meta, source) -> kind <$> expressionMeta meta <*> expressionMeta source) (numbered mapping)
      ordered :: String -> [Text] -> [Line] -> Yaml.Parser [Line]
      ordered key reduced waiting = mapM_ (bound . source) waiting >> go reduced waiting
        where
          bound :: Meta -> Yaml.Parser ()
          bound meta
            | meta._name `elem` reduced ++ map (_name . binding) waiting = pure ()
            | otherwise =
                fail
                  ( printf
                      "The operand '%s' of λ function '%s' names no meta bound by 'morph' or by any line"
                      (T.unpack meta._spelling)
                      key
                  )
          go :: [Text] -> [Line] -> Yaml.Parser [Line]
          go _ [] = pure []
          go known pending@(first : _) = case break ((`elem` known) . _name . source) pending of
            (before, line : after) -> (line :) <$> go ((binding line)._name : known) (before ++ after)
            (_, []) ->
              fail
                ( printf
                    "The line binding '%s' of λ function '%s' reads the meta it binds, directly or through other lines"
                    (T.unpack (goCircle [] (binding first))._spelling)
                    key
                )
            where
              goCircle :: [Text] -> Meta -> Meta
              goCircle seen meta
                | meta._name `elem` seen = meta
                | otherwise = goCircle (meta._name : seen) (fromMaybe meta (listToMaybe (feeding meta)))
              feeding :: Meta -> [Meta]
              feeding meta = [source line | line <- pending, meta._name == (binding line)._name, (source line)._name `notElem` known]
          source :: Line -> Meta
          source (Evaluated _ meta) = meta
          source (Deferred _ meta) = meta
      binding :: Line -> Meta
      binding (Evaluated meta _) = meta
      binding (Deferred meta _) = meta
      sigmas :: String -> Expression -> Yaml.Parser ()
      sigmas key answer = case ([kind | Slot kind _ <- slots answer, kind /= "S"], symbols answer) of
        ([], []) -> pure ()
        (kind : _, _) -> fail (printf "The anonymous meta '!%s' cannot be referenced in the '𝑛' of λ function '%s'" (T.unpack kind) key)
        (_, idx : _) -> fail (printf "The '𝑛' of λ function '%s' writes the numbered symbol '𝜎%d', while only a bare 𝜎 mints a fresh one" key idx)
      once :: String -> Lambda -> Yaml.Parser ()
      once key lambda = case twice [] bound of
        Nothing -> pure ()
        Just meta -> fail (printf "The meta '%s' of λ function '%s' is bound by more than one line, while each meta may be bound once" (T.unpack meta._spelling) key)
        where
          bound :: [Meta]
          bound = map fst lambda._morphed ++ map binding lambda._lines
          twice :: [Text] -> [Meta] -> Maybe Meta
          twice _ [] = Nothing
          twice seen (meta : rest)
            | meta._name `elem` seen = Just meta
            | otherwise = twice (meta._name : seen) rest
      metaless :: String -> String -> (Meta, Expression) -> Yaml.Parser ()
      metaless key block (meta, term) = case metas term of
        [] -> pure ()
        name : _ ->
          fail
            ( printf
                "The operand '%s' of '%s' of λ function '%s' reads the meta '%s', while only a path from '$' can be reduced there"
                (T.unpack meta._spelling)
                block
                key
                (T.unpack name)
            )
      answered :: String -> Lambda -> Yaml.Parser ()
      answered key lambda = case lambda._ending of
        Forking{} -> pure ()
        Answering term -> case filter (`notElem` ("S" : known)) (metas term) of
          [] -> pure ()
          name : _ -> fail (printf "The '𝑛' of λ function '%s' reads the meta '%s' that no block binds" key (T.unpack name))
        where
          known :: [Text]
          known =
            map (_name . fst) lambda._dataized
              ++ map (_name . fst) lambda._morphed
              ++ map (_name . binding) lambda._lines
      dataless :: String -> Expression -> Yaml.Parser ()
      dataless key answer
        | computes answer = fail (printf "The '𝑛' of λ function '%s' reads data, while a symbolic answer may mention nothing but 𝜎" key)
        | otherwise = pure ()

numbered :: Map Text a -> [(Text, a)]
numbered = sortOn (order . fst) . Map.toList
  where
    order :: Text -> (Text, Integer)
    order meta = case T.takeWhileEnd isDigit meta of
      digits | T.null digits -> (meta, 0)
      digits -> (T.dropWhileEnd isDigit meta, read (T.unpack digits))

computes :: Expression -> Bool
computes = goExpr
  where
    goExpr :: Expression -> Bool
    goExpr (ExFormation bds) = any goBinding bds
    goExpr (ExApplication expr arg) = goExpr expr || goArgument arg
    goExpr (ExDispatch expr _) = goExpr expr
    goExpr (ExPhiMeet _ _ expr) = goExpr expr
    goExpr (ExPhiAgain _ _ expr) = goExpr expr
    goExpr (ExBytes bts) = goBytes bts
    goExpr _ = False
    goBinding :: Binding -> Bool
    goBinding (BiTau _ expr) = goExpr expr
    goBinding (BiDelta bts) = goBytes bts
    goBinding _ = False
    goArgument :: Argument -> Bool
    goArgument (ArTau _ expr) = goExpr expr
    goArgument (ArAlpha _ expr) = goExpr expr
    goBytes :: Bytes -> Bool
    goBytes (BtMeta _) = True
    goBytes (BtAny _) = True
    goBytes _ = False

emptyLambdas :: Lambdas
emptyLambdas = Lambdas []

readLambdas :: FilePath -> IO Lambdas
readLambdas path = do
  entries <- Yaml.decodeFileEither path >>= either broken pure
  mapM_ (unique entries) entries
  registered <- mapM keyed entries
  overlaps path registered
  logDebug (printf "Loaded %d λ function(s) from '%s'" (length entries) path)
  pure (Lambdas registered)
  where
    broken :: Yaml.ParseException -> IO [Lambda]
    broken failure = throwIO (BrokenLambdas path (Yaml.prettyPrintParseException failure))
    unique :: [Lambda] -> Lambda -> IO ()
    unique entries entry
      | length (filter ((== entry._key) . (._key)) entries) == 1 = pure ()
      | otherwise = throwIO (BrokenLambdas path (printf "the key '%s' is used by more than one entry" (T.unpack entry._key)))
    keyed :: Lambda -> IO (Regex, Lambda)
    keyed entry = do
      compiled <- compile compUTF8 execBlank (encodeUtf8 ("^(?:" <> entry._key <> ")$"))
      either (unreadable entry._key) (\key -> pure (key, entry)) compiled
    unreadable :: Text -> (a, String) -> IO b
    unreadable key (_, failure) =
      throwIO (BrokenLambdas path (printf "the key '%s' is not a regular expression: %s" (T.unpack key) failure))
    overlaps :: FilePath -> [(Regex, Lambda)] -> IO ()
    overlaps _ [_] = pure ()
    overlaps file registered = mapM (spoken . snd) registered >>= check
      where
        spoken :: Lambda -> IO (Lambda, Language)
        spoken entry =
          either
            (throwIO . BrokenLambdas file . printf "the key '%s' cannot be compared with the other keys: %s" (T.unpack entry._key))
            (pure . (,) entry)
            (language entry._key)
        check :: [(Lambda, Language)] -> IO ()
        check [] = pure ()
        check (first : rest) = mapM_ (pair first) rest >> check rest
        pair :: (Lambda, Language) -> (Lambda, Language) -> IO ()
        pair (left, one) (right, other) =
          maybe
            (pure ())
            ( throwIO
                . BrokenLambdas file
                . printf "the keys '%s' and '%s' match some of the same lambda names, such as '%s'" (T.unpack left._key) (T.unpack right._key)
                . T.unpack
            )
            (shared one other)

matched :: Lambdas -> Text -> Maybe Lambda
matched (Lambdas entries) func = snd <$> find (\(key, _) -> matchTest key (encodeUtf8 func)) entries

minted :: Expression -> Int -> ([(Slot, Function)], Int)
minted answer spent = (zip fresh [FnSymbol idx | idx <- [spent + 1 ..]], spent + length fresh)
  where
    fresh :: [Slot]
    fresh = [slot | slot@(Slot kind _) <- slots answer, kind == "S"]

taken :: Expression -> Int
taken program = maximum (0 : symbols program)
