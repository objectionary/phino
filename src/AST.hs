{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents AST tree for parsed phi-calculus expression
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

-- An anonymous meta-variable, written bare — 𝜏, 𝐵, 𝑒, 𝑛, 𝑘, 𝛿, 𝑓, 𝜎 or 𝑖
-- with no index after it. It matches whatever term stands in its place and no rule
-- can name it afterwards, so it is known only by the kind it was written as
-- ('t', 'B', 'e', 'n', 'k', 'd', 'F', 'S', 'i') and by the offset it was written
-- at, which tells it apart from every other anonymous meta of the same term.
data Slot = Slot Text Int
  deriving (Eq, Ord, Show)

-- A formation, an application and a dispatch are the nodes a term is built of,
-- and each of them carries what has been worked out about the term it heads:
-- its digest, its size and whether it is inert. They are worked out once per
-- node, from what its children carry, the first time anybody asks, so a node
-- shared by many terms, as the objects of the world are, is walked once and
-- never again. The three nodes are reached through the patterns
-- 'ExFormation', 'ExApplication' and 'ExDispatch', which build and read them
-- like constructors and never show what they carry (#1453).
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
  | {- | Bare data 𝛿 — the raw bytes extracted by the 'delta' dataization rule.
    It is not a phi-calculus term but a rendering-only chain node, so a
    '--sequence' derivation can terminate at the data itself rather than at
    the data object it was pulled out of (see #980). It never flows into the
    matcher, builder or the dataization relation.
    -}
    ExBytes Bytes

-- What is worked out about the term a node heads: its digest (see
-- 'hashExpression'), the number of nodes it counts (see 'countNodes'),
-- whether it is inert (see 'inert') and whether its attributes are distinct
-- (see 'distinct'). The size and the distinctness are worked out only when
-- asked for, since only a few terms are ever asked for them.
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

-- The node made of the given constructor and of what is worked out about the
-- node itself, which is left to be worked out when first asked for.
cached :: (Facts -> Expression) -> Expression
cached node = let term = node (established term) in term

-- What is known about a term: what its top node carries, or what is worked
-- out on the spot for a term whose top node carries nothing.
known :: Expression -> Facts
known (Formed facts _) = facts
known (Applied facts _ _) = facts
known (Dispatched facts _ _) = facts
known term = established term

-- What is worked out about a term from what is known about its children,
-- without walking any deeper than them.
established :: Expression -> Facts
established term = Facts (layer id hashExpression term) (tally term) (calm term) (unrepeated term)

-- Two terms are equal when they are the very same node, or when they are
-- built alike and hold equal children. Two nodes carrying different digests
-- are told apart without walking either, and two terms sharing
-- their children compare the children by identity, so comparing a term with
-- what a rewriting step made of it costs the part the step rebuilt.
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

-- Terms are ordered by their constructors, in the order they are declared,
-- and then by what they hold, never by what is worked out about them.
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

-- A term is shown the way its constructors are written, without what is
-- worked out about it.
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
  | {- | A symbol 𝜎1 — a λ function nothing answers, which is what makes the
    value the term it stands in carries unknown. It is a name and not a
    meta-variable: no substitution ever binds it and the matcher never reads
    it, while 'FnMeta' 𝑓1 stands for any λ name at all, a symbol included.
    -}
    FnSymbol Int
  | {- | A symbol written bare, 𝜎, which is an answer asking for a fresh one.
    The slot it was written at tells two of them apart inside one answer, so
    each is minted its own name (see 'minted' in 'Lambdas').
    -}
    FnFresh Slot
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

-- A cheap, fixed-size digest of an expression, used for fast (dirty) equality
-- checks during loop detection. Equal expressions always produce the same
-- digest, but distinct expressions may collide, so a positive digest match
-- must always be confirmed with a full structural (==) comparison. A node
-- carries its digest, mixed of the digests its children carry, so asking for
-- it costs nothing once the node has been asked once (#1453).
hashExpression :: Expression -> Int
hashExpression term = case known term of
  Facts digest _ _ _ -> digest

-- The same digest, blind to which symbol stands where: every symbol is hashed
-- as the same one, so two terms that are 'alike' always produce the same
-- digest, while data, names and shape still tell terms apart. It is what keys
-- a store of terms compared up to a renaming of symbols, and like
-- 'hashExpression' a positive match must be confirmed, by 'alike' here.
hashShape :: Expression -> Int
hashShape = layer (const 0) hashShape

-- The same digest again, blind as well to every term the top of a term holds:
-- a formation is hashed by the names of its attributes, in order, with its data
-- and the λ function it names, and anything else by the constructor at its top
-- and the attribute or index it carries. Two terms one of which is 'within' the
-- other always produce the same digest, so it keys a store of terms compared by
-- embedding, and a positive match must be confirmed by 'within'.
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

-- The digest of the top node of a term, the one both 'hashExpression' and
-- 'hashShape' compute, with the index of every symbol passed through the first
-- function before it is mixed in, and every term the node holds mixed in as
-- the digest the second function gives it.
layer :: (Int -> Int) -> (Expression -> Int) -> Expression -> Int
layer symbol child = goExpr fnvOffset
  where
    fnvPrime, fnvOffset :: Int
    fnvPrime = 1099511628211
    fnvOffset = 14695981039
    -- FNV-1a style mixing step (Int multiplication wraps silently).
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

-- Whether two terms are the same up to a bijective renaming of their symbols:
-- structurally equal once some one-to-one pairing of the symbols of one with
-- the symbols of the other is applied, so 𝜎3 may stand in one where 𝜎5 stands
-- in the other, as long as it does so everywhere and no other symbol stands
-- there too. Everything else — data, attribute names, λ names — has to match
-- exactly. A symbol is an opaque unknown nobody worked out, so two terms that
-- differ by nothing but which unknowns they carry reduce the same way, while
-- two that differ by a datum may not. The pairing is built as the two terms
-- are walked in lockstep and is kept in both directions, which is what refuses
-- one symbol standing for two and two standing for one.
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

-- Whether the first term is embedded in the second: the two have the same
-- constructor, attributes, data and λ names at the top, and every term the
-- first holds there is embedded in the term the second holds at the same place,
-- either as it stands or somewhere below it. Deeper down a term may sit under
-- wrappers the other lacks, so a recursion whose argument gains one on every
-- round enters a formation the previous round is within, while a call nested
-- inside another is smaller and never holds it. Any symbol stands for any
-- other, since each is an opaque unknown. A ρ binding is never looked below,
-- since it holds the object a term was taken from and not a term it grew into.
within :: Expression -> Expression -> Bool
within = coupled
  where
    embedded :: Expression -> Expression -> Bool
    embedded inner outer = coupled inner outer || any (embedded inner) (children outer)
    coupled :: Expression -> Expression -> Bool
    coupled (ExFormation left) (ExFormation right) = length left == length right && and (zipWith goBinding left right)
    coupled (ExApplication left arg) (ExApplication right arg') = embedded left right && goArgument arg arg'
    coupled (ExDispatch left attr) (ExDispatch right attr') = attr == attr' && embedded left right
    coupled (ExPhiMeet prefix idx left) (ExPhiMeet prefix' idx' right) = prefix == prefix' && idx == idx' && embedded left right
    coupled (ExPhiAgain prefix idx left) (ExPhiAgain prefix' idx' right) = prefix == prefix' && idx == idx' && embedded left right
    coupled left right = left == right
    goBinding :: Binding -> Binding -> Bool
    goBinding (BiTau attr left) (BiTau attr' right) = attr == attr' && embedded left right
    goBinding (BiLambda (FnSymbol _)) (BiLambda (FnSymbol _)) = True
    goBinding left right = left == right
    goArgument :: Argument -> Argument -> Bool
    goArgument (ArTau attr left) (ArTau attr' right) = attr == attr' && embedded left right
    goArgument (ArAlpha alpha left) (ArAlpha alpha' right) = alpha == alpha' && embedded left right
    goArgument _ _ = False
    children :: Expression -> [Expression]
    children (ExFormation bds) = [expr | BiTau attr expr <- bds, attr /= AtRho]
    children (ExApplication expr (ArTau _ arg)) = [expr, arg]
    children (ExApplication expr (ArAlpha _ arg)) = [expr, arg]
    children (ExDispatch expr _) = [expr]
    children (ExPhiMeet _ _ expr) = [expr]
    children (ExPhiAgain _ _ expr) = [expr]
    children _ = []

-- Every symbol a term carries, in the order it was written. A symbol is what
-- makes the value a term stands for unknown, and the run reads the
-- dependencies between its firings off them: a term carrying 𝜎4 is the term
-- the firing that minted 𝜎4 answered with, whatever it has been rewritten
-- into since.
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

-- The symbol a term stands for, if its value is one at all. A term carries its
-- value where the φ chain ends, so that is the only place a symbol names this
-- term: one sitting under ρ, or inside an operand, belongs to the term it was
-- minted for and says nothing about this one. This is how a firing is read as
-- the answer of an earlier firing.
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

-- The number of nodes a term counts, which a node carries once asked.
countNodes :: Expression -> Int
countNodes term = case known term of
  Facts _ size _ _ -> size

-- The number of nodes a term counts, from the numbers its children count.
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

-- Whether no normalization rule can match anywhere in a term, judged by its
-- shape alone. Every such rule fires on one of four kinds of places: a
-- dispatch on a formation, an application of a formation, a dispatch or an
-- application of ⊥, and a formation holding both λ and Δ. A term is inert
-- when none of its places is of these kinds and it holds no meta-variable, so
-- ξ, Φ and ⊥ are inert, a formation is inert when its bodies are and it holds
-- not both λ and Δ, and a dispatch or an application is inert when its parts
-- are and its head is neither a formation nor ⊥. It says no more often than
-- it should, as '⟦ b ↦ ∅ ⟧( b ↦ ξ.x )' is normal but not inert, and that is
-- safe, since it only ever licenses skipping a term. A node carries the
-- answer once asked, so a term an earlier normalization produced is known to
-- be inert without being walked again (#1453).
inert :: Expression -> Bool
inert term = case known term of
  Facts _ _ still _ -> still

-- Whether no two bindings of a formation carry the same attribute, which a
-- node carries once asked, so an object carried from term to term is checked
-- once; any other term has no bindings to repeat one (#1453).
distinct :: Expression -> Bool
distinct term = case known term of
  Facts _ _ _ unique -> unique

-- Whether no two bindings of a formation carry the same attribute.
unrepeated :: Expression -> Bool
unrepeated (ExFormation bds) = isNothing (repeated bds)
unrepeated _ = True

-- The first attribute the bindings carry for the second time, if any. The
-- attributes seen so far are kept by a hash of their names and compared only
-- when two of them hash alike, which keeps checking a formation of hundreds of
-- bindings to one pass over them rather than one comparison of names after
-- another (#1453).
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

-- Extract attribute from binding
attributeFromBinding :: Binding -> Maybe Attribute
attributeFromBinding (BiTau attr _) = Just attr
attributeFromBinding (BiVoid attr) = Just attr
attributeFromBinding (BiDelta _) = Just AtDelta
attributeFromBinding (BiLambda _) = Just AtLambda
attributeFromBinding (BiMeta _) = Nothing
attributeFromBinding (BiAny _) = Nothing

-- Whether a term is inert, from whether its children are.
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

-- Minimal matcher function (required for view pattern)
--
-- Both bindings of a literal are named φ, the only void that the real
-- 'number', 'string' and 'bytes' declare ([@] > number, see #1155). The
-- legacy 'as-bytes', 'data' and positional α0 forms are still recognized
-- so XMIR produced by older jeo versions keeps sugaring back.
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

-- The bytes object Φ.bytes(φ ↦ ⟦ Δ ⤍ … ⟧) — what a 'bytes' atom
-- yields and what a 'DataObject' carries under its φ argument.
-- The payload is bound to 'φ', the void that the real 'bytes' object
-- declares ([@] > bytes), so that every dispatch on the literal can bind
-- (see #1142)
dataBytes :: Bytes -> Expression
dataBytes bts =
  ExApplication
    (BaseObject "bytes")
    (ArTau AtPhi (ExFormation [BiDelta bts]))
