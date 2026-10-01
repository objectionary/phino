{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module AST
  ( Slot (..)
  , Expression (ExFormation, ExXi, ExRoot, ExTermination, ExApplication, ExDispatch, ExMeta, ExAny, ExPhiMeet, ExPhiAgain, ExBytes)
  , Argument (..)
  , Alpha (..)
  , Binding (..)
  , Bytes (..)
  , Attribute (..)
  , Function (..)
  , hashExpression
  , hashShape
  , hashSkeleton
  , inert
  , distinct
  , repeated
  , attributeFromBinding
  , alike
  , within
  , symbols
  , lifted
  , denoted
  , countNodes
  , matchBaseObject
  , pattern BaseObject
  , matchDataObject
  , pattern DataString
  , pattern DataNumber
  , pattern DataObject
  , dataBytes
  )
where

import Data.Bits (xor)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import GHC.Generics (Generic)

data Slot = Slot Text Int
  deriving (Eq, Ord, Show)

data Expression
  = Formed Facts [Binding]
  | ExXi
  | ExRoot
  | ExTermination
  | Applied Facts Expression Argument
  | Dispatched Facts Expression Attribute
  | ExMeta Text
  | ExAny Slot
  | ExPhiMeet (Maybe String) Int Expression
  | ExPhiAgain (Maybe String) Int Expression
  | ExBytes Bytes

data Facts = Facts !Int Int !Bool Bool

{-# COMPLETE ExFormation, ExXi, ExRoot, ExTermination, ExApplication, ExDispatch, ExMeta, ExAny, ExPhiMeet, ExPhiAgain, ExBytes #-}

pattern ExFormation :: [Binding] -> Expression
pattern ExFormation bds <- Formed _ bds
  where
    ExFormation bds = cached (`Formed` bds)

pattern ExApplication :: Expression -> Argument -> Expression
pattern ExApplication expr arg <- Applied _ expr arg
  where
    ExApplication expr arg = cached (\facts -> Applied facts expr arg)

pattern ExDispatch :: Expression -> Attribute -> Expression
pattern ExDispatch expr attr <- Dispatched _ expr attr
  where
    ExDispatch expr attr = cached (\facts -> Dispatched facts expr attr)

cached :: (Facts -> Expression) -> Expression
cached node = let term = node (established term) in term

known :: Expression -> Facts
known (Formed facts _) = facts
known (Applied facts _ _) = facts
known (Dispatched facts _ _) = facts
known term = established term

established :: Expression -> Facts
established term = Facts (layer id hashExpression term) (tally term) (calm term) (unrepeated term)

instance Eq Expression where
  left == right = isTrue# (reallyUnsafePtrEquality# left right) || congruent left right
    where
      congruent :: Expression -> Expression -> Bool
      congruent (Formed facts bds) (Formed facts' bds') = alongside facts facts' && bds == bds'
      congruent (Applied facts expr arg) (Applied facts' expr' arg') = alongside facts facts' && expr == expr' && arg == arg'
      congruent (Dispatched facts expr attr) (Dispatched facts' expr' attr') = alongside facts facts' && attr == attr' && expr == expr'
      congruent ExXi ExXi = True
      congruent ExRoot ExRoot = True
      congruent ExTermination ExTermination = True
      congruent (ExMeta meta) (ExMeta meta') = meta == meta'
      congruent (ExAny slot) (ExAny slot') = slot == slot'
      congruent (ExPhiMeet prefix idx expr) (ExPhiMeet prefix' idx' expr') = prefix == prefix' && idx == idx' && expr == expr'
      congruent (ExPhiAgain prefix idx expr) (ExPhiAgain prefix' idx' expr') = prefix == prefix' && idx == idx' && expr == expr'
      congruent (ExBytes bts) (ExBytes bts') = bts == bts'
      congruent _ _ = False
      alongside :: Facts -> Facts -> Bool
      alongside (Facts digest _ _ _) (Facts digest' _ _ _) = digest == digest'

instance Ord Expression where
  compare (ExFormation bds) (ExFormation bds') = compare bds bds'
  compare (ExApplication expr arg) (ExApplication expr' arg') = compare expr expr' <> compare arg arg'
  compare (ExDispatch expr attr) (ExDispatch expr' attr') = compare expr expr' <> compare attr attr'
  compare (ExMeta meta) (ExMeta meta') = compare meta meta'
  compare (ExAny slot) (ExAny slot') = compare slot slot'
  compare (ExPhiMeet prefix idx expr) (ExPhiMeet prefix' idx' expr') = compare prefix prefix' <> compare idx idx' <> compare expr expr'
  compare (ExPhiAgain prefix idx expr) (ExPhiAgain prefix' idx' expr') = compare prefix prefix' <> compare idx idx' <> compare expr expr'
  compare (ExBytes bts) (ExBytes bts') = compare bts bts'
  compare left right = compare (rank left) (rank right)
    where
      rank :: Expression -> Int
      rank = \case
        ExFormation _ -> 0
        ExXi -> 1
        ExRoot -> 2
        ExTermination -> 3
        ExApplication _ _ -> 4
        ExDispatch _ _ -> 5
        ExMeta _ -> 6
        ExAny _ -> 7
        ExPhiMeet{} -> 8
        ExPhiAgain{} -> 9
        ExBytes _ -> 10

instance Show Expression where
  showsPrec prec = \case
    ExFormation bds -> showParen (prec > 10) (showString "ExFormation " . showsPrec 11 bds)
    ExXi -> showString "ExXi"
    ExRoot -> showString "ExRoot"
    ExTermination -> showString "ExTermination"
    ExApplication expr arg -> showParen (prec > 10) (showString "ExApplication " . showsPrec 11 expr . showChar ' ' . showsPrec 11 arg)
    ExDispatch expr attr -> showParen (prec > 10) (showString "ExDispatch " . showsPrec 11 expr . showChar ' ' . showsPrec 11 attr)
    ExMeta meta -> showParen (prec > 10) (showString "ExMeta " . showsPrec 11 meta)
    ExAny slot -> showParen (prec > 10) (showString "ExAny " . showsPrec 11 slot)
    ExPhiMeet prefix idx expr -> showParen (prec > 10) (showString "ExPhiMeet " . showsPrec 11 prefix . showChar ' ' . showsPrec 11 idx . showChar ' ' . showsPrec 11 expr)
    ExPhiAgain prefix idx expr -> showParen (prec > 10) (showString "ExPhiAgain " . showsPrec 11 prefix . showChar ' ' . showsPrec 11 idx . showChar ' ' . showsPrec 11 expr)
    ExBytes bts -> showParen (prec > 10) (showString "ExBytes " . showsPrec 11 bts)

data Argument
  = ArTau Attribute Expression
  | ArAlpha Alpha Expression
  deriving (Eq, Ord, Show, Generic)

data Alpha
  = Alpha Int
  | AlMeta Text
  | AlAny Slot
  deriving (Eq, Ord, Generic)

data Binding
  = BiTau Attribute Expression
  | BiVoid Attribute
  | BiDelta Bytes
  | BiLambda Function
  | BiMeta Text
  | BiAny Slot
  deriving (Eq, Ord, Show, Generic)

data Bytes
  = BtEmpty
  | BtOne String
  | BtMany [String]
  | BtMeta Text
  | BtAny Slot
  deriving (Eq, Ord, Show, Generic)

data Attribute
  = AtLabel Text
  | AtPhi
  | AtRho
  | AtLambda
  | AtDelta
  | AtMeta Text
  | AtAny Slot
  deriving (Eq, Generic, Ord)

data Function
  = Function Text
  | FnMeta Text
  | FnAny Slot
  | FnSymbol Int
  | FnFresh Slot
  deriving (Eq, Generic, Show, Ord)

instance Show Attribute where
  show (AtLabel label) = T.unpack label
  show AtRho = "ρ"
  show AtPhi = "φ"
  show AtDelta = "Δ"
  show AtLambda = "λ"
  show (AtMeta meta) = '!' : T.unpack meta
  show (AtAny (Slot kind _)) = '!' : T.unpack kind

instance Show Alpha where
  show (Alpha idx) = 'α' : show idx
  show (AlMeta meta) = 'α' : '!' : T.unpack meta
  show (AlAny (Slot kind _)) = 'α' : '!' : T.unpack kind

hashExpression :: Expression -> Int
hashExpression term = case known term of
  Facts digest _ _ _ -> digest

hashShape :: Expression -> Int
hashShape = layer (const 0) hashShape

hashSkeleton :: Expression -> Int
hashSkeleton =
  hashShape . \case
    ExFormation bds -> ExFormation (map bare bds)
    ExApplication _ (ArTau attr _) -> ExApplication ExXi (ArTau attr ExXi)
    ExApplication _ (ArAlpha alpha _) -> ExApplication ExXi (ArAlpha alpha ExXi)
    ExDispatch _ attr -> ExDispatch ExXi attr
    ExPhiMeet prefix idx _ -> ExPhiMeet prefix idx ExXi
    ExPhiAgain prefix idx _ -> ExPhiAgain prefix idx ExXi
    term -> term
  where
    bare :: Binding -> Binding
    bare (BiTau attr _) = BiTau attr ExXi
    bare binding = binding

layer :: (Int -> Int) -> (Expression -> Int) -> Expression -> Int
layer symbol child = goExpr fnvOffset
  where
    fnvPrime, fnvOffset :: Int
    fnvPrime = 1099511628211
    fnvOffset = 14695981039
    step :: Int -> Int -> Int
    step h x = (h `xor` x) * fnvPrime
    hashText :: Int -> Text -> Int
    hashText = T.foldl' (\h c -> step h (fromEnum c))
    hashString :: Int -> String -> Int
    hashString = foldl' (\h c -> step h (fromEnum c))
    goSlot :: Int -> Slot -> Int
    goSlot h (Slot kind idx) = step (hashText h kind) idx
    hashMaybeString :: Int -> Maybe String -> Int
    hashMaybeString h Nothing = step h 0
    hashMaybeString h (Just s) = hashString (step h 1) s
    goExpr :: Int -> Expression -> Int
    goExpr h = \case
      ExFormation bds -> foldl' goBinding (step h 1) bds
      ExXi -> step h 2
      ExRoot -> step h 3
      ExTermination -> step h 4
      ExApplication ex arg -> goArgument (step (step h 5) (child ex)) arg
      ExDispatch ex at -> goAttribute (step (step h 6) (child ex)) at
      ExMeta t -> hashText (step h 7) t
      ExAny slot -> goSlot (step h 32) slot
      ExPhiMeet ms i ex -> step (hashMaybeString (step (step h 9) i) ms) (child ex)
      ExPhiAgain ms i ex -> step (hashMaybeString (step (step h 10) i) ms) (child ex)
      ExBytes bts -> goBytes (step h 8) bts
    goBinding :: Int -> Binding -> Int
    goBinding h = \case
      BiTau at ex -> step (goAttribute (step h 11) at) (child ex)
      BiDelta bts -> goBytes (step h 12) bts
      BiVoid at -> goAttribute (step h 13) at
      BiLambda fn -> goFunction (step h 14) fn
      BiMeta t -> hashText (step h 15) t
      BiAny slot -> goSlot (step h 33) slot
    goBytes :: Int -> Bytes -> Int
    goBytes h = \case
      BtEmpty -> step h 17
      BtOne s -> hashString (step h 18) s
      BtMany ss -> foldl' hashString (step h 19) ss
      BtMeta t -> hashText (step h 20) t
      BtAny slot -> goSlot (step h 34) slot
    goAttribute :: Int -> Attribute -> Int
    goAttribute h = \case
      AtLabel t -> hashText (step h 21) t
      AtPhi -> step h 23
      AtRho -> step h 24
      AtLambda -> step h 25
      AtDelta -> step h 26
      AtMeta t -> hashText (step h 27) t
      AtAny slot -> goSlot (step h 35) slot
    goArgument :: Int -> Argument -> Int
    goArgument h = \case
      ArTau at ex -> step (goAttribute (step h 22) at) (child ex)
      ArAlpha al ex -> step (goAlpha (step h 30) al) (child ex)
    goAlpha :: Int -> Alpha -> Int
    goAlpha h = \case
      Alpha idx -> step (step h 31) idx
      AlMeta t -> hashText (step h 28) t
      AlAny slot -> goSlot (step h 36) slot
    goFunction :: Int -> Function -> Int
    goFunction h = \case
      Function t -> hashText (step h 16) t
      FnMeta t -> hashText (step h 29) t
      FnAny slot -> goSlot (step h 37) slot
      FnSymbol idx -> step (step h 38) (symbol idx)
      FnFresh slot -> goSlot (step h 39) slot

alike :: Expression -> Expression -> Bool
alike one two = isJust (goExpr (Map.empty, Map.empty) one two)
  where
    goExpr :: (Map.Map Int Int, Map.Map Int Int) -> Expression -> Expression -> Maybe (Map.Map Int Int, Map.Map Int Int)
    goExpr pairing (ExFormation left) (ExFormation right) = goList goBinding pairing left right
    goExpr pairing (ExApplication left arg) (ExApplication right arg') = goExpr pairing left right >>= \next -> goArgument next arg arg'
    goExpr pairing (ExDispatch left attr) (ExDispatch right attr')
      | attr == attr' = goExpr pairing left right
    goExpr pairing (ExPhiMeet prefix idx left) (ExPhiMeet prefix' idx' right)
      | prefix == prefix' && idx == idx' = goExpr pairing left right
    goExpr pairing (ExPhiAgain prefix idx left) (ExPhiAgain prefix' idx' right)
      | prefix == prefix' && idx == idx' = goExpr pairing left right
    goExpr pairing left right = same pairing left right
    goBinding :: (Map.Map Int Int, Map.Map Int Int) -> Binding -> Binding -> Maybe (Map.Map Int Int, Map.Map Int Int)
    goBinding pairing (BiTau attr left) (BiTau attr' right)
      | attr == attr' = goExpr pairing left right
    goBinding pairing (BiLambda (FnSymbol left)) (BiLambda (FnSymbol right)) = paired pairing left right
    goBinding pairing left right = same pairing left right
    goArgument :: (Map.Map Int Int, Map.Map Int Int) -> Argument -> Argument -> Maybe (Map.Map Int Int, Map.Map Int Int)
    goArgument pairing (ArTau attr left) (ArTau attr' right)
      | attr == attr' = goExpr pairing left right
    goArgument pairing (ArAlpha alpha left) (ArAlpha alpha' right)
      | alpha == alpha' = goExpr pairing left right
    goArgument pairing left right = same pairing left right
    goList :: (pairing -> item -> item -> Maybe pairing) -> pairing -> [item] -> [item] -> Maybe pairing
    goList _ pairing [] [] = Just pairing
    goList walk pairing (left : lefts) (right : rights) = walk pairing left right >>= \next -> goList walk next lefts rights
    goList _ _ _ _ = Nothing
    same :: (Eq item) => pairing -> item -> item -> Maybe pairing
    same pairing left right
      | left == right = Just pairing
      | otherwise = Nothing
    paired :: (Map.Map Int Int, Map.Map Int Int) -> Int -> Int -> Maybe (Map.Map Int Int, Map.Map Int Int)
    paired (forward, backward) left right = case (Map.lookup left forward, Map.lookup right backward) of
      (Nothing, Nothing) -> Just (Map.insert left right forward, Map.insert right left backward)
      (Just right', Just _) | right' == right -> Just (forward, backward)
      _ -> Nothing

type Searched = Map.Map (Int, Int) [(Expression, Expression, Bool)]

type Search = Searched -> (Bool, Searched)

within :: Expression -> Expression -> Bool
within before after = fst (coupled before after Map.empty)
  where
    embedded :: Expression -> Expression -> Search
    embedded inner outer searched = case recalled inner outer searched of
      Just found -> (found, searched)
      Nothing -> remembered inner outer (some [answered (general inner outer), coupled inner outer, some (map (embedded inner) (children outer))] searched)
    recalled :: Expression -> Expression -> Searched -> Maybe Bool
    recalled inner outer searched = listToMaybe [found | (inner', outer', found) <- Map.findWithDefault [] (digests inner outer) searched, inner' == inner, outer' == outer]
    remembered :: Expression -> Expression -> (Bool, Searched) -> (Bool, Searched)
    remembered inner outer (found, searched) = (found, Map.insertWith (++) (digests inner outer) [(inner, outer, found)] searched)
    digests :: Expression -> Expression -> (Int, Int)
    digests inner outer = (hashExpression inner, hashExpression outer)
    general :: Expression -> Expression -> Bool
    general inner (ExFormation [BiLambda (FnSymbol _)]) = plain inner
    general _ _ = False
    plain :: Expression -> Bool
    plain (ExFormation bds) = all plainBinding bds
    plain (ExApplication expr (ArTau AtRho _)) = plain expr
    plain (ExApplication expr (ArTau _ arg)) = plain expr && plain arg
    plain (ExApplication expr (ArAlpha _ arg)) = plain expr && plain arg
    plain (ExDispatch expr _) = plain expr
    plain (ExPhiMeet _ _ expr) = plain expr
    plain (ExPhiAgain _ _ expr) = plain expr
    plain _ = True
    plainBinding :: Binding -> Bool
    plainBinding (BiTau AtRho _) = True
    plainBinding (BiTau _ expr) = plain expr
    plainBinding (BiLambda (FnSymbol _)) = False
    plainBinding _ = True
    coupled :: Expression -> Expression -> Search
    coupled (ExFormation left) (ExFormation right) = every (answered (length left == length right) : zipWith goBinding left right)
    coupled (ExApplication left arg) (ExApplication right arg') = every [embedded left right, goArgument arg arg']
    coupled (ExDispatch left attr) (ExDispatch right attr') = every [answered (attr == attr'), embedded left right]
    coupled (ExPhiMeet prefix idx left) (ExPhiMeet prefix' idx' right) = every [answered (prefix == prefix' && idx == idx'), embedded left right]
    coupled (ExPhiAgain prefix idx left) (ExPhiAgain prefix' idx' right) = every [answered (prefix == prefix' && idx == idx'), embedded left right]
    coupled left right = answered (left == right)
    goBinding :: Binding -> Binding -> Search
    goBinding (BiTau attr left) (BiTau attr' right) = every [answered (attr == attr'), embedded left right]
    goBinding (BiLambda (FnSymbol _)) (BiLambda (FnSymbol _)) = answered True
    goBinding left right = answered (left == right)
    goArgument :: Argument -> Argument -> Search
    goArgument (ArTau attr left) (ArTau attr' right) = every [answered (attr == attr'), embedded left right]
    goArgument (ArAlpha alpha left) (ArAlpha alpha' right) = every [answered (alpha == alpha'), embedded left right]
    goArgument _ _ = answered False
    answered :: Bool -> Search
    answered = (,)
    every :: [Search] -> Search
    every [] searched = (True, searched)
    every (search : rest) searched = case search searched of
      (True, searched') -> every rest searched'
      failed -> failed
    some :: [Search] -> Search
    some [] searched = (False, searched)
    some (search : rest) searched = case search searched of
      (False, searched') -> some rest searched'
      found -> found
    children :: Expression -> [Expression]
    children (ExFormation bds) = [expr | BiTau attr expr <- bds, attr /= AtRho]
    children (ExApplication expr (ArTau _ arg)) = [expr, arg]
    children (ExApplication expr (ArAlpha _ arg)) = [expr, arg]
    children (ExDispatch expr _) = [expr]
    children (ExPhiMeet _ _ expr) = [expr]
    children (ExPhiAgain _ _ expr) = [expr]
    children _ = []

symbols :: Expression -> [Int]
symbols = goExpr
  where
    goExpr :: Expression -> [Int]
    goExpr (ExFormation bds) = concatMap goBinding bds
    goExpr (ExApplication expr arg) = goExpr expr ++ goArgument arg
    goExpr (ExDispatch expr _) = goExpr expr
    goExpr (ExPhiMeet _ _ expr) = goExpr expr
    goExpr (ExPhiAgain _ _ expr) = goExpr expr
    goExpr _ = []
    goBinding :: Binding -> [Int]
    goBinding (BiTau _ expr) = goExpr expr
    goBinding (BiLambda (FnSymbol idx)) = [idx]
    goBinding _ = []
    goArgument :: Argument -> [Int]
    goArgument (ArTau _ expr) = goExpr expr
    goArgument (ArAlpha _ expr) = goExpr expr

lifted :: Int -> Int -> Expression -> Expression
lifted floor' offset = goExpr
  where
    goExpr :: Expression -> Expression
    goExpr (ExFormation bds) = ExFormation (map goBinding bds)
    goExpr (ExApplication expr arg) = ExApplication (goExpr expr) (goArgument arg)
    goExpr (ExDispatch expr attr) = ExDispatch (goExpr expr) attr
    goExpr (ExPhiMeet prefix idx expr) = ExPhiMeet prefix idx (goExpr expr)
    goExpr (ExPhiAgain prefix idx expr) = ExPhiAgain prefix idx (goExpr expr)
    goExpr expr = expr
    goBinding :: Binding -> Binding
    goBinding (BiTau attr expr) = BiTau attr (goExpr expr)
    goBinding (BiLambda (FnSymbol idx)) | idx > floor' = BiLambda (FnSymbol (idx + offset))
    goBinding bd = bd
    goArgument :: Argument -> Argument
    goArgument (ArTau attr expr) = ArTau attr (goExpr expr)
    goArgument (ArAlpha alpha expr) = ArAlpha alpha (goExpr expr)

denoted :: Expression -> Maybe Int
denoted = goExpr
  where
    goExpr :: Expression -> Maybe Int
    goExpr (ExFormation bds) = listToMaybe (concatMap goBinding bds)
    goExpr (ExApplication _ (ArTau AtPhi expr)) = goExpr expr
    goExpr (ExApplication expr _) = goExpr expr
    goExpr (ExPhiMeet _ _ expr) = goExpr expr
    goExpr (ExPhiAgain _ _ expr) = goExpr expr
    goExpr _ = Nothing
    goBinding :: Binding -> [Int]
    goBinding (BiLambda (FnSymbol idx)) = [idx]
    goBinding (BiTau AtPhi expr) = maybe [] pure (goExpr expr)
    goBinding _ = []

countNodes :: Expression -> Int
countNodes term = case known term of
  Facts _ size _ _ -> size

tally :: Expression -> Int
tally (ExFormation bds) = 1 + sum (map nodesInBinding bds) + length bds
  where
    nodesInBinding :: Binding -> Int
    nodesInBinding (BiTau _ expr) = countNodes expr + 2
    nodesInBinding (BiMeta _) = 1
    nodesInBinding (BiAny _) = 1
    nodesInBinding _ = 3
tally (ExApplication expr (ArTau _ expr')) = 4 + countNodes expr + countNodes expr'
tally (ExApplication expr (ArAlpha _ expr')) = 4 + countNodes expr + countNodes expr'
tally (ExDispatch expr' _) = 2 + countNodes expr'
tally (ExPhiMeet _ _ expr) = countNodes expr
tally (ExPhiAgain _ _ expr) = countNodes expr
tally _ = 1

inert :: Expression -> Bool
inert term = case known term of
  Facts _ _ still _ -> still

distinct :: Expression -> Bool
distinct term = case known term of
  Facts _ _ _ unique -> unique

unrepeated :: Expression -> Bool
unrepeated (ExFormation bds) = isNothing (repeated bds)
unrepeated _ = True

repeated :: [Binding] -> Maybe Attribute
repeated = go IntMap.empty
  where
    go :: IntMap.IntMap [Attribute] -> [Binding] -> Maybe Attribute
    go _ [] = Nothing
    go seen (bd : rest) = case attributeFromBinding bd of
      Just attr
        | attr `elem` IntMap.findWithDefault [] (key attr) seen -> Just attr
        | otherwise -> go (IntMap.insertWith (++) (key attr) [attr] seen) rest
      Nothing -> go seen rest
    key :: Attribute -> Int
    key (AtLabel label) = T.foldl' (\hash char -> (hash `xor` fromEnum char) * 1099511628211) 14695981039 label
    key (AtMeta meta) = T.length meta
    key _ = 0

attributeFromBinding :: Binding -> Maybe Attribute
attributeFromBinding (BiTau attr _) = Just attr
attributeFromBinding (BiVoid attr) = Just attr
attributeFromBinding (BiDelta _) = Just AtDelta
attributeFromBinding (BiLambda _) = Just AtLambda
attributeFromBinding (BiMeta _) = Nothing
attributeFromBinding (BiAny _) = Nothing

calm :: Expression -> Bool
calm = \case
  ExFormation bds -> settled False False bds
  ExDispatch expr attr -> inert expr && headless expr && plain attr
  ExApplication expr (ArTau attr arg) -> inert expr && headless expr && plain attr && inert arg
  ExApplication expr (ArAlpha (Alpha _) arg) -> inert expr && headless expr && inert arg
  ExXi -> True
  ExRoot -> True
  ExTermination -> True
  _ -> False
  where
    settled :: Bool -> Bool -> [Binding] -> Bool
    settled _ _ [] = True
    settled lambda delta (bd : rest) =
      quiet bd && case bd of
        BiLambda _ -> not delta && settled True delta rest
        BiDelta _ -> not lambda && settled lambda True rest
        _ -> settled lambda delta rest
    quiet :: Binding -> Bool
    quiet (BiTau attr expr) = plain attr && inert expr
    quiet (BiVoid attr) = plain attr
    quiet (BiDelta (BtMeta _)) = False
    quiet (BiDelta (BtAny _)) = False
    quiet (BiDelta _) = True
    quiet (BiLambda (Function _)) = True
    quiet (BiLambda (FnSymbol _)) = True
    quiet _ = False
    headless :: Expression -> Bool
    headless (ExFormation _) = False
    headless ExTermination = False
    headless _ = True
    plain :: Attribute -> Bool
    plain (AtMeta _) = False
    plain (AtAny _) = False
    plain _ = True

matchBaseObject :: Expression -> Maybe T.Text
matchBaseObject (ExDispatch ExRoot (AtLabel label)) = Just label
matchBaseObject _ = Nothing

pattern BaseObject :: T.Text -> Expression
pattern BaseObject label <- (matchBaseObject -> Just label)
  where
    BaseObject label = ExDispatch ExRoot (AtLabel label)

matchDataObject :: Expression -> Maybe (T.Text, Bytes)
matchDataObject (ExApplication outer arg)
  | Just inner <- asBytesArg arg = case (matchOuter outer, matchInner inner) of
      (Just label, Just bts) -> Just (label, bts)
      _ -> Nothing
  where
    asBytesArg :: Argument -> Maybe Expression
    asBytesArg (ArTau AtPhi inner) = Just inner
    asBytesArg (ArTau (AtLabel "as-bytes") inner) = Just inner
    asBytesArg (ArAlpha (Alpha 0) inner) = Just inner
    asBytesArg _ = Nothing
    dataArg :: Argument -> Maybe Expression
    dataArg (ArTau AtPhi formation) = Just formation
    dataArg (ArTau (AtLabel "data") formation) = Just formation
    dataArg (ArAlpha (Alpha 0) formation) = Just formation
    dataArg _ = Nothing
    matchOuter :: Expression -> Maybe T.Text
    matchOuter (BaseObject label) = Just label
    matchOuter (ExPhiAgain _ _ (BaseObject label)) = Just label
    matchOuter _ = Nothing
    matchInner :: Expression -> Maybe Bytes
    matchInner (ExPhiAgain _ _ inner') = matchInner inner'
    matchInner inner' = matchInner' inner'
    matchInner' :: Expression -> Maybe Bytes
    matchInner' (ExApplication bytes arg')
      | Just formation <- dataArg arg' = case (matchesBytes bytes, matchFormation formation) of
          (True, Just bts) -> Just bts
          _ -> Nothing
    matchInner' _ = Nothing
    matchesBytes :: Expression -> Bool
    matchesBytes (BaseObject "bytes") = True
    matchesBytes (ExPhiAgain _ _ (BaseObject "bytes")) = True
    matchesBytes _ = False
    matchFormation :: Expression -> Maybe Bytes
    matchFormation (ExFormation [BiDelta bts]) = Just bts
    matchFormation (ExPhiAgain _ _ (ExFormation [BiDelta bts])) = Just bts
    matchFormation _ = Nothing
matchDataObject _ = Nothing

pattern DataString :: Bytes -> Expression
pattern DataString bts = DataObject "string" bts

pattern DataNumber :: Bytes -> Expression
pattern DataNumber bts = DataObject "number" bts

pattern DataObject :: T.Text -> Bytes -> Expression
pattern DataObject label bts <- (matchDataObject -> Just (label, bts))
  where
    DataObject label bts =
      ExApplication (BaseObject label) (ArTau AtPhi (dataBytes bts))

dataBytes :: Bytes -> Expression
dataBytes bts =
  ExApplication
    (BaseObject "bytes")
    (ArTau AtPhi (ExFormation [BiDelta bts]))
