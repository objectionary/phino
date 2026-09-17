-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to collect the meta-variables a term was written
-- with and to drop the index from the ones that stand alone in their kind.
-- Every name starts with the sigil of the kind it belongs to -- 'e', 'n', 'k',
-- 't', 'B', 'd', 'F' or 'i' -- and carries an index after it, which is there to
-- tell one meta of a kind from another. A term naming a kind just once has
-- nothing to tell apart, so the index counts nothing and the sigil may stand
-- alone, the way an anonymous meta stands.
module Metas (Metas (..), lonely) where

import AST
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

class Metas a where
  -- The names of the meta-variables the term was written with, an anonymous
  -- one named by the sigil of its kind alone
  metas :: a -> [Text]

  -- The term with each of the given names cut down to the sigil it starts with
  bare :: [Text] -> a -> a

-- The term with the index dropped from every name whose kind it mentions once,
-- since an index that tells a meta from no other only slows the reader down
lonely :: (Metas a) => a -> a
lonely term = bare (filter alone named) term
  where
    named :: [Text]
    named = nub (metas term)
    alone :: Text -> Bool
    alone name = indexed name && length (filter (kin name) named) == 1
    kin :: Text -> Text -> Bool
    kin name other = T.take 1 name == T.take 1 other
    indexed :: Text -> Bool
    indexed name = isJust (readMaybe (T.unpack (T.drop 1 name)) :: Maybe Int)

-- A name stands for the meta-variable it names, so it answers with itself and
-- sheds its index when asked to
instance Metas Text where
  metas name = [name]
  bare names name = if name `elem` names then T.take 1 name else name

-- An anonymous meta is known by the sigil of its kind, which is the name every
-- meta of that kind is cut down to, and there is nothing in it to shed
instance Metas Slot where
  metas (Slot kind _) = [kind]
  bare _ slot = slot

instance (Metas a) => Metas [a] where
  metas = concatMap metas
  bare names = map (bare names)

instance (Metas a) => Metas (Maybe a) where
  metas = maybe [] metas
  bare names = fmap (bare names)

instance Metas Expression where
  metas (ExMeta name) = metas name
  metas (ExAny slot) = metas slot
  metas (ExFormation bds) = metas bds
  metas (ExApplication expr arg) = metas expr ++ metas arg
  metas (ExDispatch expr attr) = metas expr ++ metas attr
  metas (ExPhiMeet _ _ expr) = metas expr
  metas (ExPhiAgain _ _ expr) = metas expr
  metas (ExBytes bts) = metas bts
  metas _ = []
  bare names (ExMeta name) = ExMeta (bare names name)
  bare names (ExFormation bds) = ExFormation (bare names bds)
  bare names (ExApplication expr arg) = ExApplication (bare names expr) (bare names arg)
  bare names (ExDispatch expr attr) = ExDispatch (bare names expr) (bare names attr)
  bare names (ExPhiMeet prefix idx expr) = ExPhiMeet prefix idx (bare names expr)
  bare names (ExPhiAgain prefix idx expr) = ExPhiAgain prefix idx (bare names expr)
  bare names (ExBytes bts) = ExBytes (bare names bts)
  bare _ expr = expr

instance Metas Argument where
  metas (ArTau attr expr) = metas attr ++ metas expr
  metas (ArAlpha alpha expr) = metas alpha ++ metas expr
  bare names (ArTau attr expr) = ArTau (bare names attr) (bare names expr)
  bare names (ArAlpha alpha expr) = ArAlpha (bare names alpha) (bare names expr)

instance Metas Binding where
  metas (BiTau attr expr) = metas attr ++ metas expr
  metas (BiVoid attr) = metas attr
  metas (BiDelta bts) = metas bts
  metas (BiLambda func) = metas func
  metas (BiMeta name) = metas name
  metas (BiAny slot) = metas slot
  bare names (BiTau attr expr) = BiTau (bare names attr) (bare names expr)
  bare names (BiVoid attr) = BiVoid (bare names attr)
  bare names (BiDelta bts) = BiDelta (bare names bts)
  bare names (BiLambda func) = BiLambda (bare names func)
  bare names (BiMeta name) = BiMeta (bare names name)
  bare _ bd = bd

instance Metas Attribute where
  metas (AtMeta name) = metas name
  metas (AtAny slot) = metas slot
  metas _ = []
  bare names (AtMeta name) = AtMeta (bare names name)
  bare _ attr = attr

instance Metas Alpha where
  metas (AlMeta name) = metas name
  metas (AlAny slot) = metas slot
  metas _ = []
  bare names (AlMeta name) = AlMeta (bare names name)
  bare _ alpha = alpha

instance Metas Bytes where
  metas (BtMeta name) = metas name
  metas (BtAny slot) = metas slot
  metas _ = []
  bare names (BtMeta name) = BtMeta (bare names name)
  bare _ bts = bts

-- A symbol 𝜎1 is a name and not a meta-variable, so nothing counts it among
-- the metas and nothing sheds the index that tells one symbol from another
instance Metas Function where
  metas (FnMeta name) = metas name
  metas (FnAny slot) = metas slot
  metas _ = []
  bare names (FnMeta name) = FnMeta (bare names name)
  bare _ func = func
