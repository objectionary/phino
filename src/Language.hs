-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Language (Language, language, shared) where

import Data.Char (isAlphaNum, isDigit)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Read (readMaybe)

newtype Span = Span [(Char, Char)]

data Pattern
  = Chars Span
  | Chain [Pattern]
  | Choice [Pattern]
  | Repeat Int (Maybe Int) Pattern

data Edge
  = Free Int
  | Step Span Int

newtype Language = Language (Map.Map Int [Edge])

language :: Text -> Either String Language
language key = do
  (pattern, rest) <- choice (T.unpack key)
  case rest of
    [] -> Right (automaton pattern)
    _ -> Left (printf "the character '%s' is not expected" (take 1 rest))
  where
    choice :: String -> Either String (Pattern, String)
    choice text = do
      (first, rest) <- chain text
      case rest of
        '|' : more -> do
          (other, left) <- choice more
          Right (Choice [first, other], left)
        _ -> Right (first, rest)
    chain :: String -> Either String (Pattern, String)
    chain = go []
      where
        go :: [Pattern] -> String -> Either String (Pattern, String)
        go done text@(char : _)
          | char `elem` ("|)" :: String) = Right (Chain (reverse done), text)
        go done [] = Right (Chain (reverse done), [])
        go done text = do
          (atom, rest) <- single text
          (quantified, left) <- quantifier atom rest
          go (quantified : done) left
    single :: String -> Either String (Pattern, String)
    single ('(' : '?' : ':' : rest) = group rest
    single ('(' : '?' : 'P' : '<' : rest) = group (drop 1 (dropWhile (/= '>') rest))
    single ('(' : '?' : '<' : rest@(char : _))
      | char `notElem` ("=!" :: String) = group (drop 1 (dropWhile (/= '>') rest))
    single ('(' : '?' : _) = Left "a group of that kind cannot be compared"
    single ('(' : rest) = group rest
    single ('[' : rest) = klass rest
    single ('.' : rest) = Right (Chars (complement (Span [('\n', '\n')])), rest)
    single ('\\' : rest) = escape rest >>= \(span', left) -> Right (Chars span', left)
    single (char : rest)
      | char `elem` ("^$" :: String) = Left "an anchor cannot be compared"
      | char `elem` ("*+?" :: String) = Left (printf "the quantifier '%s' quantifies nothing" [char])
      | otherwise = Right (Chars (Span [(char, char)]), rest)
    single [] = Left "the expression ends too early"
    group :: String -> Either String (Pattern, String)
    group text = do
      (inner, rest) <- choice text
      case rest of
        ')' : left -> Right (inner, left)
        _ -> Left "a group is not closed"
    quantifier :: Pattern -> String -> Either String (Pattern, String)
    quantifier atom ('*' : rest) = lazy (Repeat 0 Nothing atom) rest
    quantifier atom ('+' : rest) = lazy (Repeat 1 Nothing atom) rest
    quantifier atom ('?' : rest) = lazy (Repeat 0 (Just 1) atom) rest
    quantifier atom text@('{' : rest) =
      case bounded rest of
        Just (low, high, left) -> lazy (Repeat low high atom) left
        Nothing -> Right (atom, text)
    quantifier atom rest = Right (atom, rest)
    lazy :: Pattern -> String -> Either String (Pattern, String)
    lazy _ ('+' : _) = Left "a possessive quantifier cannot be compared"
    lazy pattern ('?' : rest) = Right (pattern, rest)
    lazy pattern rest = Right (pattern, rest)
    bounded :: String -> Maybe (Int, Maybe Int, String)
    bounded text = do
      let (low, rest) = span isDigit text
      from <- readMaybe low
      case rest of
        '}' : left -> Just (from, Just from, left)
        ',' : more -> do
          let (high, left) = span isDigit more
          case (high, left) of
            ([], '}' : after) -> Just (from, Nothing, after)
            (_, '}' : after) -> readMaybe high >>= \to -> if to < from then Nothing else Just (from, Just to, after)
            _ -> Nothing
        _ -> Nothing
    escape :: String -> Either String (Span, String)
    escape (char : rest)
      | Just span' <- lookup char sets = Right (span', rest)
      | Just literal <- lookup char controls = Right (Span [(literal, literal)], rest)
      | not (isAlphaNum char) = Right (Span [(char, char)], rest)
      | otherwise = Left (printf "the escape '\\%s' cannot be compared" [char])
    escape [] = Left "the expression ends with a backslash"
    sets :: [(Char, Span)]
    sets =
      [ ('d', digits)
      , ('D', complement digits)
      , ('w', word)
      , ('W', complement word)
      , ('s', spaces)
      , ('S', complement spaces)
      ]
    controls :: [(Char, Char)]
    controls = [('t', '\t'), ('n', '\n'), ('r', '\r'), ('f', '\f'), ('e', '\ESC')]
    digits :: Span
    digits = Span [('0', '9')]
    word :: Span
    word = union [Span [('0', '9')], Span [('A', 'Z')], Span [('_', '_')], Span [('a', 'z')]]
    spaces :: Span
    spaces = Span [('\t', '\r'), (' ', ' ')]
    klass :: String -> Either String (Pattern, String)
    klass ('^' : rest) = members rest >>= \(span', left) -> Right (Chars (complement span'), left)
    klass rest = members rest >>= \(span', left) -> Right (Chars span', left)
    members :: String -> Either String (Span, String)
    members (']' : rest) = collect [Span [(']', ']')]] rest
    members rest = collect [] rest
    collect :: [Span] -> String -> Either String (Span, String)
    collect done (']' : rest) = Right (union done, rest)
    collect _ ('[' : ':' : _) = Left "a POSIX class cannot be compared"
    collect done ('\\' : rest) = do
      (span', left) <- escape rest
      case (span', left) of
        (Span [(low, top)], '-' : high : after)
          | low == top && high /= ']' -> ranged done low (high : after)
        _ -> collect (span' : done) left
    collect done (low : '-' : high : rest)
      | high /= ']' = ranged done low (high : rest)
    collect done (char : rest) = collect (Span [(char, char)] : done) rest
    collect _ [] = Left "a class is not closed"
    ranged :: [Span] -> Char -> String -> Either String (Span, String)
    ranged done low ('\\' : rest) = do
      (span', left) <- escape rest
      case span' of
        Span [(high, high')] | high == high' -> bound done low high left
        _ -> Left "a range ends at a set of characters"
    ranged done low (high : rest) = bound done low high rest
    ranged _ _ [] = Left "a class is not closed"
    bound :: [Span] -> Char -> Char -> String -> Either String (Span, String)
    bound done low high rest
      | low <= high = collect (Span [(low, high)] : done) rest
      | otherwise = Left "a range runs backwards"

shared :: Language -> Language -> Maybe Text
shared (Language left) (Language right) = go (Set.singleton (0, 0)) [((0, 0), [])]
  where
    go :: Set.Set (Int, Int) -> [((Int, Int), String)] -> Maybe Text
    go _ [] = Nothing
    go seen (((here, there), name) : rest)
      | here == 1 && there == 1 = Just (T.pack (reverse name))
      | otherwise =
          let fresh = filter ((`Set.notMember` seen) . fst) (moves here there name)
           in go (foldr (Set.insert . fst) seen fresh) (rest ++ fresh)
    moves :: Int -> Int -> String -> [((Int, Int), String)]
    moves here there name =
      [((state, there), name) | Free state <- edges left here]
        ++ [((here, state), name) | Free state <- edges right there]
        ++ [ ((this, that), char : name)
           | Step first this <- edges left here
           , Step second that <- edges right there
           , Just char <- [sample (meet first second)]
           ]
    edges :: Map.Map Int [Edge] -> Int -> [Edge]
    edges table state = Map.findWithDefault [] state table

automaton :: Pattern -> Language
automaton pattern = Language (Map.fromListWith (flip (++)) [(from, [edge]) | (from, edge) <- snd (build pattern 0 1 2)])
  where
    build :: Pattern -> Int -> Int -> Int -> (Int, [(Int, Edge)])
    build (Chars span') from to next = (next, [(from, Step span' to)])
    build (Chain []) from to next = (next, [(from, Free to)])
    build (Chain [single]) from to next = build single from to next
    build (Chain (first : rest)) from to next =
      let (after, head') = build first from next (next + 1)
          (last', tail') = build (Chain rest) next to after
       in (last', head' ++ tail')
    build (Choice options) from to next =
      foldl
        (\(counter, done) option -> let (counter', made) = build option from to counter in (counter', done ++ made))
        (next, [])
        options
    build (Repeat 0 (Just 0) _) from to next = (next, [(from, Free to)])
    build (Repeat 0 Nothing inner) from to next =
      let (after, made) = build inner next next (next + 1)
       in (after, (from, Free next) : (next, Free to) : made)
    build (Repeat 0 (Just high) inner) from to next =
      build (Choice [Chain [], Chain [inner, Repeat 0 (Just (high - 1)) inner]]) from to next
    build (Repeat low high inner) from to next =
      build (Chain [inner, Repeat (low - 1) (subtract 1 <$> high) inner]) from to next

union :: [Span] -> Span
union spans = Span (merge (Set.toAscList (Set.fromList (concat [ranges | Span ranges <- spans]))))
  where
    merge :: [(Char, Char)] -> [(Char, Char)]
    merge ((low, high) : (low', high') : rest)
      | low' <= succ' high = merge ((low, max high high') : rest)
    merge (range : rest) = range : merge rest
    merge [] = []
    succ' :: Char -> Char
    succ' char
      | char == maxBound = char
      | otherwise = succ char

complement :: Span -> Span
complement (Span ranges) = Span (go minBound ranges)
  where
    go :: Char -> [(Char, Char)] -> [(Char, Char)]
    go from [] = [(from, maxBound)]
    go from ((low, high) : rest)
      | high == maxBound = [(from, pred low) | from < low]
      | otherwise = [(from, pred low) | from < low] ++ go (succ high) rest

meet :: Span -> Span -> Span
meet (Span first) (Span second) =
  Span [(max low low', min high high') | (low, high) <- first, (low', high') <- second, max low low' <= min high high']

sample :: Span -> Maybe Char
sample (Span ranges) =
  case [max low 'a' | (low, high) <- ranges, max low 'a' <= min high 'z'] ++ [low | (low, _) <- ranges] of
    char : _ -> Just char
    [] -> Nothing
