-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The normal forms a run already knows, so the rewriter does not look for a
-- redex where none can be. A term in normal form admits no normalization rule
-- at any of its places, and neither does any formation inside it, since whether
-- a rule matches at a place depends on the term at that place and nothing
-- around it. A run carries whole copies of big objects of its world from term
-- to term, and before this every rule searched every copy on every cycle of
-- every normalization, so a step cost the objects around the redex and not the
-- redex (#1457).
module Normals (Normals, Normality (..), learned, noNormals, normality, places) where

import AST
import qualified Data.Map.Strict as Map

-- The formations known to be normal forms, keyed by 'hashSkeleton', which
-- reads the attributes of a formation and none of its bodies, and confirmed by
-- a structural comparison, since two formations may share a skeleton.
newtype Normals = Normals (Map.Map Int [Expression])

-- What is known about the places of a term, in the order the deep matcher
-- visits them: 'Normal' for a term known to be a normal form, 'Unknown' for one
-- where nothing below is known, and 'Parts' for one where something below is,
-- holding one entry per place under it — the body of every τ binding of a
-- formation, the head of a dispatch, the head and the argument of an
-- application.
data Normality = Normal | Unknown | Parts [Normality]
  deriving (Eq, Show)

noNormals :: Normals
noNormals = Normals Map.empty

-- Learn a normal form, and with it every formation it holds. A formation
-- already known is skipped with everything inside it, since what is inside was
-- learned together with it.
learned :: Expression -> Normals -> Normals
learned expr normals@(Normals table) = case expr of
  ExFormation _
    | known normals expr -> normals
    | otherwise -> foldr learned (Normals (Map.insertWith (++) (hashSkeleton expr) [expr] table)) (places expr)
  _ -> foldr learned normals (places expr)

-- What is known about the places of the term.
normality :: Normals -> Expression -> Normality
normality normals@(Normals table)
  | Map.null table = const Unknown
  | otherwise = go
  where
    go :: Expression -> Normality
    go expr@(ExFormation _)
      | known normals expr = Normal
    go expr = parts (map go (places expr))
    parts :: [Normality] -> Normality
    parts nts
      | all unknown nts = Unknown
      | otherwise = Parts nts
    unknown :: Normality -> Bool
    unknown Unknown = True
    unknown _ = False

-- Whether the formation is a normal form this store knows.
known :: Normals -> Expression -> Bool
known (Normals table) expr = maybe False (elem expr) (Map.lookup (hashSkeleton expr) table)

-- The terms standing right under the term where the deep matcher looks.
places :: Expression -> [Expression]
places (ExFormation bds) = [body | BiTau _ body <- bds]
places (ExDispatch expr _) = [expr]
places (ExApplication expr (ArTau _ arg)) = [expr, arg]
places (ExApplication expr (ArAlpha _ arg)) = [expr, arg]
places _ = []
