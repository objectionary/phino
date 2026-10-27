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
  , cast
  , emptyLambdas
  , matched
  , minted
  , readLambdas
  , rewritten
  , sized
  , stayed
  , symbolized
  , taken
  )
where

import AST
import Builder (buildExpressionThrows)
import Control.Exception (Exception, throwIO)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Aeson (FromJSON (parseJSON), Key, Object, withObject, (.!=), (.:?))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (JSONPathElement (Key), (<?>))
import Data.Char (isDigit)
import Data.Either (partitionEithers)
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
import Matcher (substEmpty)
import Metas (Metas (metas))
import Parser (parseBinding, parseBytes, parseExpression)
import Rule (RuleContext, matchExpressionWithRule')
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

data Line
  = Dataized Meta Meta
  | Morphed Meta Meta
  | Undressed Meta Meta
  | Evaluated Meta Meta
  | Deferred Meta Meta
  | Stayed Meta Meta
  | Rewritten Meta Meta [Y.Rule]
  | Symbolized Meta Meta
  | Cast Meta Meta Meta Meta

data Ending
  = Answering Expression
  | Joining (Either Meta Expression) Expression Expression (Maybe Function)

data Lambda = Lambda
  { _key :: Text
  , _dataized :: [(Meta, Expression)]
  , _morphed :: [(Meta, Expression)]
  , _undressed :: [(Meta, Expression)]
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
    (dataized, weighing) <- partitionEithers <$> reductions key bytesMeta Dataized entry "dataize"
    ending <- concluded (T.unpack key) dataized entry
    (morphed, reading) <- partitionEithers <$> reductions key expressionMeta Morphed entry "morph"
    (undressed, peeling) <- partitionEithers <$> reductions key expressionMeta Undressed entry "undress"
    lambda <-
      Lambda key dataized morphed undressed
        <$> (written (T.unpack key) entry >>= ordered (T.unpack key) (map (_name . fst) (morphed ++ undressed)) . ((weighing ++ reading ++ peeling) ++))
        <*> pure ending
    once (T.unpack key) lambda
    mapM_ (metaless (T.unpack key) "dataize" "a path from '$' or a meta alone") lambda._dataized
    mapM_ (metaless (T.unpack key) "morph" "a path from '$' or a meta alone") lambda._morphed
    mapM_ (metaless (T.unpack key) "undress" "a path from '$' or a meta alone") lambda._undressed
    answered (T.unpack key) lambda
    pure lambda
    where
      unknown :: String -> Object -> Yaml.Parser ()
      unknown key entry = case filter (`notElem` known) (map Key.toText (KeyMap.keys entry)) of
        [] -> pure ()
        name : _ -> fail (printf "The key '%s' of λ function '%s' is none of the keys an entry reads: %s" (T.unpack name) key (T.unpack (T.intercalate ", " known)))
        where
          known :: [Text]
          known = ["λ", "dataize", "morph", "undress", "evaluate", "defer", "stay", "rewrite", "symbolize", "cast", "join", "𝑛"]
      concluded :: String -> [(Meta, Expression)] -> Object -> Yaml.Parser Ending
      concluded key dataized entry = do
        answer <- entry .:? "𝑛"
        block <- entry .:? "join"
        case (answer, block) of
          (Nothing, Nothing) -> fail (printf "The entry of λ function '%s' has neither an '𝑛' key nor a 'join' block" key)
          (Just _, Just _) -> fail (printf "The entry of λ function '%s' has both an '𝑛' key and a 'join' block, while a join answers the term it joins" key)
          (Just term, Nothing) -> Answering term <$ sigmas key term <* dataless key term
          (Nothing, Just joint) -> joining key dataized joint
      joining :: String -> [(Meta, Expression)] -> Object -> Yaml.Parser Ending
      joining key dataized block = do
        case filter (`notElem` ["on", "left", "right", "otherwise"]) (map Key.toText (KeyMap.keys block)) of
          [] -> pure ()
          name : _ -> fail (printf "The key '%s' of the 'join' block of λ function '%s' is none of its keys: on, left, right, otherwise" (T.unpack name) key)
        on <- required "on" >>= condition
        Joining on <$> (required "left" >>= side "left") <*> (required "right" >>= side "right") <*> (block .:? "otherwise" >>= traverse fallback)
        where
          required :: FromJSON value => Key -> Yaml.Parser value
          required name = block .:? name >>= maybe (fail (printf "The 'join' block of λ function '%s' has no '%s' key" key (Key.toString name))) pure
          condition :: Text -> Yaml.Parser (Either Meta Expression)
          condition spelling = case (parseBytes (T.unpack spelling), parseExpression (T.unpack spelling)) of
            (Right (BtMeta name), _) | name `elem` map (_name . fst) dataized -> pure (Left (Meta spelling name))
            (_, Right path) | pathed path -> pure (Right path)
            _ -> fail (printf "The 'on' of the 'join' block of λ function '%s' is '%s', while it must name a bytes meta of 'dataize' reading a path from '$', such as '𝛿1', or be a path from '$', such as '$.on'" key (T.unpack spelling))
          pathed :: Expression -> Bool
          pathed ExXi = True
          pathed (ExDispatch inner _) = pathed inner
          pathed _ = False
          side :: String -> Expression -> Yaml.Parser Expression
          side _ term@(ExMeta _) = pure term
          side name term = case metas term of
            [] -> pure term
            meta : _ -> fail (printf "The side '%s' of the 'join' block of λ function '%s' reads the meta '%s' inside a term, while a side is a path from '$' or a meta alone" name key (T.unpack meta))
          fallback :: Text -> Yaml.Parser Function
          fallback spelling = case parseBinding (printf "λ ⤍ %s" (T.unpack spelling)) of
            Right (BiLambda func@(Function _)) -> pure func
            _ -> fail (printf "The 'otherwise' of the 'join' block of λ function '%s' is '%s', while it must name a λ function, such as 'L_join'" key (T.unpack spelling))
      reductions :: Text -> (Text -> Yaml.Parser Meta) -> (Meta -> Meta -> Line) -> Object -> Key -> Yaml.Parser [Either (Meta, Expression) Line]
      reductions key spelled kind entry block = do
        mapping <- entry .:? block .!= (Map.empty :: Map Text Text)
        mapM reduction (numbered mapping)
        where
          reduction :: (Text, Text) -> Yaml.Parser (Either (Meta, Expression) Line)
          reduction (meta, source) = do
            named <- spelled meta
            case parseExpression (T.unpack source) of
              Right (ExMeta name) -> pure (Right (kind named (Meta source name)))
              Right term -> Left (named, term) <$ referenceless (T.unpack key) (T.unpack meta) term
              Left failure -> fail failure <?> Key (Key.fromText meta) <?> Key block
      expressionMeta :: Text -> Yaml.Parser Meta
      expressionMeta meta = case parseExpression (T.unpack meta) of
        Right (ExMeta name) -> pure (Meta meta name)
        _ -> fail (printf "The operand '%s' is not an expression meta, such as '𝑛1'" (T.unpack meta))
      bytesMeta :: Text -> Yaml.Parser Meta
      bytesMeta meta = case parseBytes (T.unpack meta) of
        Right (BtMeta name) -> pure (Meta meta name)
        _ -> fail (printf "The operand '%s' is not a bytes meta, such as '𝛿1'" (T.unpack meta))
      written :: String -> Object -> Yaml.Parser [Line]
      written key entry = concat <$> sequence [links Evaluated entry "evaluate", links Deferred entry "defer", links Stayed entry "stay", rewrites key entry, links Symbolized entry "symbolize", casts key entry]
      links :: (Meta -> Meta -> Line) -> Object -> Key -> Yaml.Parser [Line]
      links kind entry name = do
        mapping <- entry .:? name .!= (Map.empty :: Map Text Text)
        mapM (\(meta, source) -> kind <$> expressionMeta meta <*> expressionMeta source) (numbered mapping)
      casts :: String -> Object -> Yaml.Parser [Line]
      casts key entry = do
        mapping <- entry .:? "cast" .!= (Map.empty :: Map Text Text)
        case numbered mapping of
          [] -> pure []
          [(one, first), (two, second)] -> (: []) <$> (Cast <$> expressionMeta one <*> expressionMeta first <*> expressionMeta two <*> expressionMeta second)
          pairs ->
            fail
              ( printf
                  "The 'cast' block of λ function '%s' holds %d %s, while it casts one pair and takes exactly two"
                  key
                  (length pairs)
                  (if length pairs == 1 then "line" else "lines" :: String)
              )
      rewrites :: String -> Object -> Yaml.Parser [Line]
      rewrites key entry = do
        mapping <- entry .:? "rewrite" .!= (Map.empty :: Map Text Object)
        mapM line (numbered mapping)
        where
          line :: (Text, Object) -> Yaml.Parser Line
          line (meta, body) = do
            case filter (`notElem` ["of", "rules"]) (map Key.toText (KeyMap.keys body)) of
              [] -> pure ()
              name : _ -> fail (printf "The key '%s' of the line '%s' of the 'rewrite' block of λ function '%s' is none of its keys: of, rules" (T.unpack name) (T.unpack meta) key)
            Rewritten <$> expressionMeta meta <*> (required "of" >>= expressionMeta) <*> (required "rules" >>= mapM symbolless)
            where
              required :: FromJSON value => Key -> Yaml.Parser value
              required name = body .:? name >>= maybe (fail (printf "The line '%s' of the 'rewrite' block of λ function '%s' has no '%s' key" (T.unpack meta) key (Key.toString name))) pure
          symbolless :: Y.Rule -> Yaml.Parser Y.Rule
          symbolless rule = case symbols rule.result of
            [] -> pure rule
            idx : _ -> fail (printf "The rule '%s' of the 'rewrite' block of λ function '%s' writes the symbol '𝜎%d' into its result, while only a firing mints a symbol" rule.name key idx)
      ordered :: String -> [Text] -> [Line] -> Yaml.Parser [Line]
      ordered key reduced waiting = mapM_ bound (concatMap sources waiting) >> go reduced waiting
        where
          bound :: Meta -> Yaml.Parser ()
          bound meta
            | meta._name `elem` reduced ++ map _name (concatMap bindings waiting) = pure ()
            | otherwise =
                fail
                  ( printf
                      "The operand '%s' of λ function '%s' names no meta bound by 'morph' or by any line"
                      (T.unpack meta._spelling)
                      key
                  )
          go :: [Text] -> [Line] -> Yaml.Parser [Line]
          go _ [] = pure []
          go known pending@(first : _) = case break (all ((`elem` known) . _name) . sources) pending of
            (before, line : after) -> (line :) <$> go (map _name (bindings line) ++ known) (before ++ after)
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
              feeding meta = [source | line <- pending, meta._name `elem` map _name (bindings line), source <- sources line, source._name `notElem` known]
          sources :: Line -> [Meta]
          sources (Dataized _ meta) = [meta]
          sources (Morphed _ meta) = [meta]
          sources (Undressed _ meta) = [meta]
          sources (Evaluated _ meta) = [meta]
          sources (Deferred _ meta) = [meta]
          sources (Stayed _ meta) = [meta]
          sources (Rewritten _ meta _) = [meta]
          sources (Symbolized _ meta) = [meta]
          sources (Cast _ first _ second) = [first, second]
      binding :: Line -> Meta
      binding (Dataized meta _) = meta
      binding (Morphed meta _) = meta
      binding (Undressed meta _) = meta
      binding (Evaluated meta _) = meta
      binding (Deferred meta _) = meta
      binding (Stayed meta _) = meta
      binding (Rewritten meta _ _) = meta
      binding (Symbolized meta _) = meta
      binding (Cast meta _ _ _) = meta
      bindings :: Line -> [Meta]
      bindings (Cast one _ two _) = [one, two]
      bindings line = [binding line]
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
          bound = map fst (lambda._morphed ++ lambda._undressed) ++ concatMap bindings lambda._lines
          twice :: [Text] -> [Meta] -> Maybe Meta
          twice _ [] = Nothing
          twice seen (meta : rest)
            | meta._name `elem` seen = Just meta
            | otherwise = twice (meta._name : seen) rest
      metaless :: String -> String -> String -> (Meta, Expression) -> Yaml.Parser ()
      metaless key block reducible (meta, term) = case metas term of
        [] -> pure ()
        name : _ ->
          fail
            ( printf
                "The operand '%s' of '%s' of λ function '%s' reads the meta '%s', while only %s can be reduced there"
                (T.unpack meta._spelling)
                block
                key
                (T.unpack name)
                reducible
            )
      answered :: String -> Lambda -> Yaml.Parser ()
      answered key lambda = case lambda._ending of
        Joining _ left right _ -> sided "left" left >> sided "right" right
        Answering term -> case filter (`notElem` ("S" : known)) (metas term) of
          [] -> pure ()
          name : _ -> fail (printf "The '𝑛' of λ function '%s' reads the meta '%s' that no block binds" key (T.unpack name))
        where
          known :: [Text]
          known = map (_name . fst) lambda._dataized ++ reduced
          reduced :: [Text]
          reduced = map (_name . fst) (lambda._morphed ++ lambda._undressed) ++ map _name (concatMap bindings lambda._lines)
          sided :: String -> Expression -> Yaml.Parser ()
          sided name side = case filter (`notElem` reduced) (metas side) of
            [] -> pure ()
            meta : _ -> fail (printf "The side '%s' of the 'join' block of λ function '%s' reads the meta '%s' that none of 'morph', 'undress', 'evaluate', 'stay', 'rewrite', 'symbolize', 'cast' and 'defer' binds" name key (T.unpack meta))
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

sized :: Lambdas -> Int
sized (Lambdas registered) = length registered

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
    broken failure = throwIO (BrokenLambdas path (Y.shiftYamlError (Yaml.prettyPrintParseException failure)))
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

stayed :: Expression -> Expression
stayed = goExpr
  where
    goExpr :: Expression -> Expression
    goExpr (ExFormation bds) = ExFormation (map goBinding bds)
    goExpr (ExApplication expr arg) = case (goExpr expr, arg) of
      (target, ArTau AtRho _) | dispatched target -> target
      (target, _) -> ExApplication target (goArgument arg)
    goExpr (ExDispatch expr attr) = ExDispatch (goExpr expr) attr
    goExpr (ExPhiMeet prefix idx expr) = ExPhiMeet prefix idx (goExpr expr)
    goExpr (ExPhiAgain prefix idx expr) = ExPhiAgain prefix idx (goExpr expr)
    goExpr expr = expr
    goBinding :: Binding -> Binding
    goBinding (BiTau attr expr) = BiTau attr (goExpr expr)
    goBinding bd = bd
    goArgument :: Argument -> Argument
    goArgument (ArTau attr expr) = ArTau attr (goExpr expr)
    goArgument (ArAlpha alpha expr) = ArAlpha alpha (goExpr expr)
    dispatched :: Expression -> Bool
    dispatched (ExDispatch _ _) = True
    dispatched (ExApplication expr _) = dispatched expr
    dispatched _ = False

cast :: Expression -> Expression -> Maybe (Int, Expression, [Attribute])
cast (ExFormation [BiLambda (FnSymbol bare)]) shape
  | length (symbols shape) == 1 = (\(made, path) -> (bare, made, path)) <$> goExpr shape
  where
    goExpr :: Expression -> Maybe (Expression, [Attribute])
    goExpr (ExFormation bds) = case [body | BiTau AtPhi body <- bds] of
      [ExFormation [BiLambda (FnSymbol _)]] -> Just (ExFormation (map (placed (ExFormation [BiLambda (FnSymbol bare)])) bds), [AtPhi])
      [body] -> (\(made, path) -> (ExFormation (map (placed made) bds), AtPhi : path)) <$> goExpr body
      _ -> Nothing
    goExpr _ = Nothing
    placed :: Expression -> Binding -> Binding
    placed made (BiTau AtPhi _) = BiTau AtPhi made
    placed _ bd = bd
cast _ _ = Nothing

rewritten :: RuleContext -> [Y.Rule] -> Expression -> IO Expression
rewritten ctx rules = goExpr
  where
    goExpr :: Expression -> IO Expression
    goExpr expr = goRules expr rules
    goRules :: Expression -> [Y.Rule] -> IO Expression
    goRules expr [] = goInside expr
    goRules expr (rule : rest) = do
      substs <- matchExpressionWithRule' [substEmpty] expr rule ctx
      case substs of
        subst : _ -> buildExpressionThrows rule.result subst
        [] -> goRules expr rest
    goInside :: Expression -> IO Expression
    goInside (ExFormation bds) = ExFormation <$> mapM goBinding bds
    goInside (ExApplication expr arg) = ExApplication <$> goExpr expr <*> goArgument arg
    goInside (ExDispatch expr attr) = (`ExDispatch` attr) <$> goExpr expr
    goInside (ExPhiMeet prefix idx expr) = ExPhiMeet prefix idx <$> goExpr expr
    goInside (ExPhiAgain prefix idx expr) = ExPhiAgain prefix idx <$> goExpr expr
    goInside expr = pure expr
    goBinding :: Binding -> IO Binding
    goBinding (BiTau attr expr) = BiTau attr <$> goExpr expr
    goBinding bd = pure bd
    goArgument :: Argument -> IO Argument
    goArgument (ArTau attr expr) = ArTau attr <$> goExpr expr
    goArgument (ArAlpha alpha expr) = ArAlpha alpha <$> goExpr expr

type Minting = (Int, [(Int, Bytes)])

symbolized :: Expression -> Int -> (Expression, [(Int, Bytes)], Int)
symbolized term spent = case runState (goExpr term) (spent, []) of
  (masked, (spent', known)) -> (masked, reverse known, spent')
  where
    goExpr :: Expression -> State Minting Expression
    goExpr (ExFormation bds) = ExFormation <$> mapM (goBinding (Just AtLambda `elem` map attributeFromBinding bds)) bds
    goExpr (ExApplication expr arg) = ExApplication <$> goExpr expr <*> goArgument arg
    goExpr (ExDispatch expr attr) = (`ExDispatch` attr) <$> goExpr expr
    goExpr (ExPhiMeet prefix idx expr) = ExPhiMeet prefix idx <$> goExpr expr
    goExpr (ExPhiAgain prefix idx expr) = ExPhiAgain prefix idx <$> goExpr expr
    goExpr expr = pure expr
    goBinding :: Bool -> Binding -> State Minting Binding
    goBinding _ (BiDelta bts) = state (\(count, known) -> (BiLambda (FnSymbol (succ count)), (succ count, (succ count, bts) : known)))
    goBinding atom (BiTau attr expr) | atom || attr == AtPhi = BiTau attr <$> goExpr expr
    goBinding _ bd = pure bd
    goArgument :: Argument -> State Minting Argument
    goArgument (ArTau attr expr) = ArTau attr <$> goExpr expr
    goArgument (ArAlpha alpha expr) = ArAlpha alpha <$> goExpr expr
