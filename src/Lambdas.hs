{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Lambdas
  ( Lambda (..)
  , LambdaException (..)
  , Lambdas
  , Meta (..)
  , emptyLambdas
  , joined
  , matched
  , minted
  , readLambdas
  , symbolized
  , taken
  )
where

import AST
import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), Key, Object, Value (Object), withObject, (.!=), (.:), (.:?))
import Data.Char (isDigit)
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import qualified Yaml as Y

data Meta = Meta
  { _spelling :: Text
  , _name :: Text
  }

data Lambda = Lambda
  { _key :: Text
  , _dataized :: [(Meta, Expression)]
  , _morphed :: [(Meta, Expression)]
  , _rewritten :: [(Meta, (Meta, [Y.Rule]))]
  , _symbolized :: [(Meta, Expression)]
  , _paired :: [(Meta, (Meta, Meta))]
  , _answer :: Expression
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
    answer <- entry .:? "𝑛" >>= maybe (fail "The entry has no '𝑛' key") pure
    lambda <-
      Lambda key
        <$> operands key bytesMeta entry "dataize"
        <*> operands key expressionMeta entry "morph"
        <*> rewrites (T.unpack key) entry
        <*> operands key expressionMeta entry "symbolize"
        <*> pairs (T.unpack key) entry
        <*> pure answer
    sigmas (T.unpack key) lambda._answer
    dataless (T.unpack key) lambda._answer
    earlier (T.unpack key) lambda
    once (T.unpack key) lambda
    mapM_ (metaless (T.unpack key) "dataize") lambda._dataized
    mapM_ (metaless (T.unpack key) "morph") lambda._morphed
    answered (T.unpack key) lambda
    pure lambda
    where
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
      pairs :: String -> Object -> Yaml.Parser [(Meta, (Meta, Meta))]
      pairs key entry = do
        mapping <- entry .:? "join" .!= (Map.empty :: Map Text [Text])
        mapM joins (numbered mapping)
        where
          joins :: (Text, [Text]) -> Yaml.Parser (Meta, (Meta, Meta))
          joins (meta, [left, right]) = do
            named <- expressionMeta meta
            branches <- (,) <$> expressionMeta left <*> expressionMeta right
            pure (named, branches)
          joins (meta, _) =
            fail
              ( printf
                  "The operand '%s' of λ function '%s' must join exactly two metas, such as '[𝑛1, 𝑛2]'"
                  (T.unpack meta)
                  key
              )
      rewrites :: String -> Object -> Yaml.Parser [(Meta, (Meta, [Y.Rule]))]
      rewrites key entry = do
        mapping <- entry .:? "rewrite" .!= (Map.empty :: Map Text Object)
        mapM line (numbered mapping)
        where
          line :: (Text, Object) -> Yaml.Parser (Meta, (Meta, [Y.Rule]))
          line (meta, body) = do
            named <- expressionMeta meta
            source <- body .: "of" >>= expressionMeta
            written <- body .: "rules"
            rules <- mapM rule written
            pure (named, (source, rules))
          rule :: Object -> Yaml.Parser Y.Rule
          rule body = do
            result <- body .: "result"
            symbolless result
            parsed <- parseJSON (Object body)
            bound parsed
            pure parsed
          symbolless :: Expression -> Yaml.Parser ()
          symbolless result
            | null (symbols result) && null [kind | Slot kind _ <- slots result, kind == "S"] = pure ()
            | otherwise = fail (printf "A rule of the 'rewrite' block of λ function '%s' writes a symbol 𝜎 into its result" key)
          bound :: Y.Rule -> Yaml.Parser ()
          bound parsed = case filter (`notElem` known) (metas parsed.result) of
            [] -> pure ()
            meta : _ ->
              fail
                ( printf
                    "The rule '%s' of the 'rewrite' block of λ function '%s' reads the meta '%s' it never binds"
                    parsed.name
                    key
                    (T.unpack meta)
                )
            where
              known :: [Text]
              known = metas parsed.pattern ++ concatMap (metas . (.meta)) (concat parsed.where_)
      earlier :: String -> Lambda -> Yaml.Parser ()
      earlier key lambda = do
        rewrote <- goRewrites (map (_name . fst) lambda._morphed) lambda._rewritten
        stood <- go rewrote lambda._symbolized
        void (goJoins stood lambda._paired)
        where
          goRewrites :: [Text] -> [(Meta, (Meta, [Y.Rule]))] -> Yaml.Parser [Text]
          goRewrites reduced [] = pure reduced
          goRewrites reduced ((meta, (source, _)) : rest)
            | source._name `elem` reduced = goRewrites (meta._name : reduced) rest
            | otherwise = unbound source
          go :: [Text] -> [(Meta, Expression)] -> Yaml.Parser [Text]
          go reduced [] = pure reduced
          go reduced ((meta, term) : rest) = case term of
            ExMeta name | name `elem` reduced -> go (meta._name : reduced) rest
            _ -> unbound meta
          goJoins :: [Text] -> [(Meta, (Meta, Meta))] -> Yaml.Parser [Text]
          goJoins reduced [] = pure reduced
          goJoins reduced ((meta, (left, right)) : rest)
            | all ((`elem` reduced) . _name) [left, right] = goJoins (meta._name : reduced) rest
            | otherwise = unbound (if left._name `elem` reduced then right else left)
          unbound :: Meta -> Yaml.Parser a
          unbound meta =
            fail
              ( printf
                  "The operand '%s' of λ function '%s' names no meta bound by 'morph' or by a line above it"
                  (T.unpack meta._spelling)
                  key
              )
      sigmas :: String -> Expression -> Yaml.Parser ()
      sigmas key answer = case ([kind | Slot kind _ <- slots answer, kind /= "S"], symbols answer) of
        ([], []) -> pure ()
        (kind : _, _) -> fail (printf "The anonymous meta '!%s' cannot be referenced in the '𝑛' of λ function '%s'" (T.unpack kind) key)
        (_, idx : _) -> fail (printf "The '𝑛' of λ function '%s' writes the numbered symbol '𝜎%d', while only a bare 𝜎 mints a fresh one" key idx)
      once :: String -> Lambda -> Yaml.Parser ()
      once key lambda = case twice [] bound of
        Nothing -> pure ()
        Just meta -> fail (printf "The meta '%s' of λ function '%s' is bound by more than one line, while each meta may be bound once" (T.unpack meta) key)
        where
          bound :: [Text]
          bound =
            map (_spelling . fst) lambda._morphed
              ++ map (_spelling . fst) lambda._rewritten
              ++ map (_spelling . fst) lambda._symbolized
              ++ map (_spelling . fst) lambda._paired
          twice :: [Text] -> [Text] -> Maybe Text
          twice _ [] = Nothing
          twice seen (meta : rest)
            | meta `elem` seen = Just meta
            | otherwise = twice (meta : seen) rest
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
      answered key lambda = case filter (`notElem` ("S" : known)) (metas lambda._answer) of
        [] -> pure ()
        name : _ -> fail (printf "The '𝑛' of λ function '%s' reads the meta '%s' that no block binds" key (T.unpack name))
        where
          known :: [Text]
          known =
            map (_name . fst) lambda._dataized
              ++ map (_name . fst) lambda._morphed
              ++ map (_name . fst) lambda._rewritten
              ++ map (_name . fst) lambda._symbolized
              ++ map (_name . fst) lambda._paired
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

type Minting = (Int, [(Int, Bytes)])

symbolized :: Expression -> Int -> (Expression, [(Int, Bytes)], Int)
symbolized term spent = case goExpr term (spent, []) of
  (masked, (spent', known)) -> (masked, reverse known, spent')
  where
    goExpr :: Expression -> Minting -> (Expression, Minting)
    goExpr (ExFormation bds) minting =
      let (bds', minting') = goBindings bds minting
       in (ExFormation bds', minting')
    goExpr (ExApplication expr arg) minting =
      let (expr', minting') = goExpr expr minting
          (arg', minting'') = goArgument arg minting'
       in (ExApplication expr' arg', minting'')
    goExpr (ExDispatch expr attr) minting =
      let (expr', minting') = goExpr expr minting
       in (ExDispatch expr' attr, minting')
    goExpr (ExPhiMeet prefix idx expr) minting =
      let (expr', minting') = goExpr expr minting
       in (ExPhiMeet prefix idx expr', minting')
    goExpr (ExPhiAgain prefix idx expr) minting =
      let (expr', minting') = goExpr expr minting
       in (ExPhiAgain prefix idx expr', minting')
    goExpr expr minting = (expr, minting)
    goBindings :: [Binding] -> Minting -> ([Binding], Minting)
    goBindings [] minting = ([], minting)
    goBindings (bd : rest) minting =
      let (bd', minting') = goBinding bd minting
          (rest', minting'') = goBindings rest minting'
       in (bd' : rest', minting'')
    goBinding :: Binding -> Minting -> (Binding, Minting)
    goBinding (BiDelta bts) (spent', known) =
      (BiLambda (FnSymbol fresh), (fresh, (fresh, bts) : known))
      where
        fresh :: Int
        fresh = spent' + 1
    goBinding (BiTau AtPhi expr) minting =
      let (expr', minting') = goExpr expr minting
       in (BiTau AtPhi expr', minting')
    goBinding bd minting = (bd, minting)
    goArgument :: Argument -> Minting -> (Argument, Minting)
    goArgument (ArTau attr expr) minting =
      let (expr', minting') = goExpr expr minting
       in (ArTau attr expr', minting')
    goArgument (ArAlpha alpha expr) minting =
      let (expr', minting') = goExpr expr minting
       in (ArAlpha alpha expr', minting')

type Joining = (Int, Map (Int, Int) Int, [(Int, (Int, Int))])

joined :: Expression -> Expression -> Int -> Maybe (Expression, [(Int, (Int, Int))], Int)
joined left right spent = taking <$> goExpr left right (spent, Map.empty, [])
  where
    taking :: (Expression, Joining) -> (Expression, [(Int, (Int, Int))], Int)
    taking (term, (spent', _, made)) = (term, reverse made, spent')
    goExpr :: Expression -> Expression -> Joining -> Maybe (Expression, Joining)
    goExpr (ExFormation one) (ExFormation two) joining = do
      (bds, joining') <- goBindings one two joining
      pure (ExFormation bds, joining')
    goExpr (ExApplication one arg) (ExApplication two arg') joining = do
      (expr, joining') <- goExpr one two joining
      (applied, joining'') <- goArgument arg arg' joining'
      pure (ExApplication expr applied, joining'')
    goExpr (ExDispatch one attr) (ExDispatch two attr') joining
      | attr == attr' = do
          (expr, joining') <- goExpr one two joining
          pure (ExDispatch expr attr, joining')
    goExpr (ExPhiMeet prefix idx one) (ExPhiMeet prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = do
          (expr, joining') <- goExpr one two joining
          pure (ExPhiMeet prefix idx expr, joining')
    goExpr (ExPhiAgain prefix idx one) (ExPhiAgain prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = do
          (expr, joining') <- goExpr one two joining
          pure (ExPhiAgain prefix idx expr, joining')
    goExpr one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    goBindings :: [Binding] -> [Binding] -> Joining -> Maybe ([Binding], Joining)
    goBindings [] [] joining = Just ([], joining)
    goBindings (one : rest) (two : rest') joining = do
      (bd, joining') <- goBinding one two joining
      (bds, joining'') <- goBindings rest rest' joining'
      pure (bd : bds, joining'')
    goBindings _ _ _ = Nothing
    goBinding :: Binding -> Binding -> Joining -> Maybe (Binding, Joining)
    goBinding (BiLambda (FnSymbol one)) (BiLambda (FnSymbol two)) joining
      | one /= two = case picked (one, two) joining of
          (fresh, joining') -> Just (BiLambda (FnSymbol fresh), joining')
    goBinding (BiTau AtPhi one) (BiTau AtPhi two) joining = do
      (expr, joining') <- goExpr one two joining
      pure (BiTau AtPhi expr, joining')
    goBinding bd@(BiTau attr _) (BiTau attr' _) joining
      | attr == attr' = Just (bd, joining)
    goBinding one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    goArgument :: Argument -> Argument -> Joining -> Maybe (Argument, Joining)
    goArgument (ArTau attr one) (ArTau attr' two) joining
      | attr == attr' = do
          (expr, joining') <- goExpr one two joining
          pure (ArTau attr expr, joining')
    goArgument (ArAlpha alpha one) (ArAlpha alpha' two) joining
      | alpha == alpha' = do
          (expr, joining') <- goExpr one two joining
          pure (ArAlpha alpha expr, joining')
    goArgument one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    picked :: (Int, Int) -> Joining -> (Int, Joining)
    picked pair joining@(spent', names, made)
      | Just name <- Map.lookup pair names = (name, joining)
      | otherwise = (fresh, (fresh, Map.insert pair fresh names, (fresh, pair) : made))
      where
        fresh :: Int
        fresh = spent' + 1

taken :: Expression -> Int
taken program = maximum (0 : symbols program)
