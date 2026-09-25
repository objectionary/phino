{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to build phi expression based on
-- pattern expression and set of substitutions by replacing
-- meta variables with appropriate meta values
module Builder
  ( buildExpressionsThrows
  , buildExpression
  , buildExpressionThrows
  , buildAttribute
  , buildAttributeThrows
  , buildBinding
  , buildBindingThrows
  , buildBytes
  , buildBytesThrows
  , contextualize
  , objectOf
  , pathOf
  , BuildException (..)
  )
where

import AST
import Control.Exception (Exception)
import Control.Monad (zipWithM)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Matcher
import Misc (orThrow, uniqueBindings)
import Printer
import Text.Printf (printf)

data BuildException
  = CouldNotBuildExpression {_expr :: Expression, _msg :: String}
  | CouldNotBuildAttribute {_attr :: Attribute, _msg :: String}
  | CouldNotBuildBinding {_bd :: Binding, _msg :: String}
  | CouldNotBuildBytes {_bts :: Bytes, _msg :: String}
  deriving (Exception)

metaMsg :: Text -> String
metaMsg = printf "meta '%s' is either does not exist or refers to an inappropriate term" . T.unpack

-- An anonymous meta is bound only within the very pattern that matched it, so
-- a lookup that misses means the term being built is not that pattern
slotMsg :: Slot -> String
slotMsg (Slot kind _) = printf "anonymous meta '!%s' cannot be referenced" (T.unpack kind)

type Built a = Either String a

instance Show BuildException where
  show CouldNotBuildExpression{..} = printf "Couldn't build expression, %s\n--Expression: %s" _msg (printExpression _expr)
  show CouldNotBuildAttribute{..} = printf "Couldn't build attribute '%s', %s" (printAttribute _attr) _msg
  show CouldNotBuildBinding{..} = printf "Couldn't build binding, %s\n--Binding: %s" _msg (printBinding _bd)
  show CouldNotBuildBytes{..} = printf "Couldn't build bytes '%s', %s" (printBytes _bts) _msg

contextualize :: Expression -> Expression -> Expression
contextualize ExRoot _ = ExRoot
contextualize ExXi ex = ex
contextualize ExTermination _ = ExTermination
contextualize (ExFormation bds) _ = ExFormation bds
contextualize (ExDispatch ex at) context = ExDispatch (contextualize ex context) at
contextualize (ExApplication ex arg) context =
  ExApplication (contextualize ex context) (contextualizeArg arg)
  where
    contextualizeArg (ArTau at bexpr) = ArTau at (contextualize bexpr context)
    contextualizeArg (ArAlpha al bexpr) = ArAlpha al (contextualize bexpr context)
contextualize ex _ = ex

buildAttribute :: Attribute -> Subst -> Built Attribute
buildAttribute (AtMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvAttribute attr) -> Right attr
  _ -> Left (metaMsg meta)
buildAttribute (AtAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvAttribute attr) -> Right attr
  _ -> Left (slotMsg slot)
buildAttribute attr _ = Right attr

buildAlpha :: Alpha -> Subst -> Built Alpha
buildAlpha (AlMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvIndex idx) -> Right (Alpha idx)
  _ -> Left (metaMsg meta)
buildAlpha (AlAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvIndex idx) -> Right (Alpha idx)
  _ -> Left (slotMsg slot)
buildAlpha a _ = Right a

buildBytes :: Bytes -> Subst -> Built Bytes
buildBytes (BtMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvBytes bytes) -> Right bytes
  _ -> Left (metaMsg meta)
buildBytes (BtAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvBytes bytes) -> Right bytes
  _ -> Left (slotMsg slot)
buildBytes bts _ = Right bts

-- Build binding
-- The function returns [Binding] because the BiMeta is always attached
-- to the list of bindings
buildBinding :: Binding -> Subst -> Built [Binding]
buildBinding (BiTau attr expr) subst = do
  attribute <- buildAttribute attr subst
  expression <- buildExpression expr subst
  Right [BiTau attribute expression]
buildBinding (BiVoid attr) subst = do
  attribute <- buildAttribute attr subst
  Right [BiVoid attribute]
buildBinding (BiMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvBindings bds) -> uniqueBindings bds
  _ -> Left (metaMsg meta)
buildBinding (BiAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvBindings bds) -> uniqueBindings bds
  _ -> Left (slotMsg slot)
buildBinding (BiDelta bytes) subst = do
  bts <- buildBytes bytes subst
  Right [BiDelta bts]
buildBinding (BiLambda (FnMeta meta)) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvFunction func) -> Right [BiLambda func]
  _ -> Left (metaMsg meta)
buildBinding (BiLambda (FnAny slot)) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvFunction func) -> Right [BiLambda func]
  _ -> Left (slotMsg slot)
-- A bare 𝜎 asks for a symbol nothing has answered yet, and the one minted for
-- the slot it was written at is bound the way any other anonymous meta is.
buildBinding (BiLambda (FnFresh slot)) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvFunction func) -> Right [BiLambda func]
  _ -> Left (slotMsg slot)
buildBinding binding _ = Right [binding]

buildArgument :: Argument -> Subst -> Built Argument
buildArgument (ArTau attr expr) subst = do
  attribute <- buildAttribute attr subst
  expression <- buildExpression expr subst
  Right (ArTau attribute expression)
buildArgument (ArAlpha alpha expr) subst = do
  alpha' <- buildAlpha alpha subst
  expression <- buildExpression expr subst
  Right (ArAlpha alpha' expression)

-- Build bindings that may contain meta binding (BiMeta)
buildBindings :: [Binding] -> Subst -> Built [Binding]
buildBindings [] _ = Right []
buildBindings (bd : rest) subst = do
  first <- buildBinding bd subst
  bds <- buildBindings rest subst
  Right (first ++ bds)

-- The name a formation goes by in the world, where it has one: the path from Φ
-- it is reached by, applied to whatever its voids were filled with on the way.
-- The world is immutable, so an object of it copied into a term is a copy of a
-- constant, and a dispatch off 'Φ.number' that wrote 'number' out in full would
-- carry every method it declares — the whole of trigonometry to reach 'plus' —
-- into the term and into every term that one then dispatches (#1446). The name
-- is what 'dot' decorates a body with instead, and whoever reads the ρ resolves
-- it the way 'Φ' itself resolves, to the very formation that stood there.
--
-- Nothing is compared against the whole world to find it. The formation says
-- where it came from: a ρ holding 'Φ', or a path off 'Φ', names the object it
-- was dispatched off, and one declaring no ρ at all can only be a top-level
-- object, so only the objects that parent declares are candidates. A candidate
-- is the formation where the two agree binding by binding, save the voids of
-- the candidate the formation has filled: the ρ with exactly what the dispatch
-- off the parent hands it, and every other one with a closed term, which is an
-- argument of the application the name carries. A φ is no candidate: it is
-- what an object decorates rather than an object it declares, so a term that
-- merely equals it is not named after it. Anything else answers with the
-- formation itself, and so does a universe that is not a formation.
pathOf :: Expression -> Expression -> Expression
pathOf (ExFormation world) form@(ExFormation bds) = maybe form found (parent (find rho bds))
  where
    found :: (Expression, [Binding]) -> Expression
    found (path, siblings) = fromMaybe form (listToMaybe (mapMaybe (candidate path) siblings))
    -- The path of the object the formation was dispatched off, together with
    -- the bindings of that object as the world declares them.
    parent :: Maybe Binding -> Maybe (Expression, [Binding])
    parent Nothing = Just (ExRoot, world)
    parent (Just (BiTau AtRho path)) = (,) path <$> declared path
    parent _ = Nothing
    -- The bindings of the object a path off Φ leads to, applications skipped:
    -- they fill voids and leave every other binding as the world wrote it.
    declared :: Expression -> Maybe [Binding]
    declared ExRoot = Just world
    declared (ExApplication target _) = declared target
    declared (ExDispatch target attr) = do
      outer <- declared target
      BiTau _ (ExFormation inner) <- find (tau attr) outer
      Just inner
    declared _ = Nothing
    candidate :: Expression -> Binding -> Maybe Expression
    candidate path (BiTau attr (ExFormation origin))
      | attr /= AtRho && attr /= AtPhi && length origin == length bds = do
          args <- zipWithM (argument path) origin bds
          Just (foldl ExApplication (ExDispatch path attr) (concat args))
    candidate _ _ = Nothing
    -- What one binding of the formation adds to the application: nothing where
    -- it is the binding the world declares, the argument where it fills a void.
    argument :: Expression -> Binding -> Binding -> Maybe [Argument]
    argument path (BiVoid AtRho) (BiTau AtRho value)
      | value == path = Just []
    argument _ (BiVoid attr) (BiTau attr' value)
      | attr == attr' && attr /= AtRho && closed value = Just [ArTau attr value]
    argument _ origin binding
      | origin == binding = Just []
      | otherwise = Nothing
    rho :: Binding -> Bool
    rho (BiTau AtRho _) = True
    rho (BiVoid AtRho) = True
    rho _ = False
    tau :: Attribute -> Binding -> Bool
    tau attr (BiTau attr' _) = attr == attr'
    tau _ _ = False
pathOf _ form = form

-- The object a path off Φ stands for, built from the world directly: the
-- formation 'md' and 'ma' would reach by morphing the path one step after
-- another, each step a whole normalization of the object it reaches (#1453).
-- The world is already in normal form, so nothing of it needs another pass:
-- a dispatch takes the formation the object declares under the attribute, its
-- ρ filled the way 'dot' fills it, with the name 'named' gives the object it
-- was dispatched off, or with Φ where that object is the world itself, as
-- 'dotg' does; an application fills a void with a closed term, the way 'copy'
-- does, and an application of ρ leaves a ρ already bound as it was, the way
-- 'stay' does, and an object with no ρ too, the way 'skip' does. Nothing answers a path that leaves the world through a term
-- other than a formation, or that dispatches off an object holding both λ and
-- Δ, which neither 'dot' nor 'dotg' reduces, and Φ itself is no object here.
objectOf :: Expression -> Expression -> Maybe Expression
objectOf (ExFormation world) path
  | path /= ExRoot = ExFormation <$> built path
  where
    built :: Expression -> Maybe [Binding]
    built ExRoot = Just world
    built (ExApplication target (ArTau AtRho value))
      | closed value = map (decorated value) <$> built target
    built (ExApplication target (ArTau attr value))
      | closed value = do
          outer <- built target
          (before, _ : after) <- Just (break (== BiVoid attr) outer)
          Just (before ++ BiTau attr value : after)
    built (ExDispatch target attr) = do
      outer <- built target
      BiTau _ (ExFormation inner) <- find (tau attr) outer
      if any lambda outer && any delta outer
        then Nothing
        else Just (map (decorated (name target outer)) inner)
    built _ = Nothing
    name :: Expression -> [Binding] -> Expression
    name ExRoot _ = ExRoot
    name _ outer = pathOf (ExFormation world) (ExFormation outer)
    decorated :: Expression -> Binding -> Binding
    decorated parent (BiVoid AtRho) = BiTau AtRho parent
    decorated _ binding = binding
    tau :: Attribute -> Binding -> Bool
    tau attr (BiTau attr' _) = attr == attr'
    tau _ _ = False
    lambda :: Binding -> Bool
    lambda (BiLambda _) = True
    lambda _ = False
    delta :: Binding -> Bool
    delta (BiDelta _) = True
    delta _ = False
objectOf _ _ = Nothing

-- A term with no ξ of its own, the only kind 'copy' ever fills a void with.
closed :: Expression -> Bool
closed (ExFormation _) = True
closed ExRoot = True
closed ExTermination = True
closed (ExApplication target (ArTau _ value)) = closed target && closed value
closed (ExApplication target (ArAlpha _ value)) = closed target && closed value
closed (ExDispatch target _) = closed target
closed _ = False

-- The bindings of a formation a meta was bound to are checked once more here,
-- since a substitution may bring two of them together under one attribute.
unique :: Expression -> Built Expression
unique (ExFormation bds) = uniqueBindings bds >> Right (ExFormation bds)
unique expr = Right expr

-- Build meta expression with given substitution
buildExpression :: Expression -> Subst -> Built Expression
buildExpression (ExDispatch ex at) subst = do
  dispatched <- buildExpression ex subst
  at' <- buildAttribute at subst
  Right (ExDispatch dispatched at')
buildExpression (ExApplication ExRoot (ArTau AtRho expr)) subst = do
  _ <- buildExpression expr subst
  Right ExRoot
buildExpression (ExApplication expr arg) subst = do
  applied <- buildExpression expr subst
  arg' <- buildArgument arg subst
  Right (ExApplication applied arg')
buildExpression (ExFormation bds) subst = do
  bds' <- buildBindings bds subst >>= uniqueBindings
  Right (ExFormation bds')
buildExpression (ExMeta meta) (Subst mp) = case Map.lookup (Named meta) mp of
  Just (MvExpression expr) -> unique expr
  _ -> Left (metaMsg meta)
buildExpression (ExAny slot) (Subst mp) = case Map.lookup (Anon slot) mp of
  Just (MvExpression expr) -> unique expr
  _ -> Left (slotMsg slot)
buildExpression expr _ = Right expr

buildBytesThrows :: Bytes -> Subst -> IO Bytes
buildBytesThrows bytes subst = orThrow (CouldNotBuildBytes bytes) (buildBytes bytes subst)

buildBindingThrows :: Binding -> Subst -> IO [Binding]
buildBindingThrows bd subst = orThrow (CouldNotBuildBinding bd) (buildBinding bd subst)

buildAttributeThrows :: Attribute -> Subst -> IO Attribute
buildAttributeThrows attr subst = orThrow (CouldNotBuildAttribute attr) (buildAttribute attr subst)

buildExpressionThrows :: Expression -> Subst -> IO Expression
buildExpressionThrows expr subst = orThrow (CouldNotBuildExpression expr) (buildExpression expr subst)

-- Build a several expression from one expression and several substitutions
buildExpressionsThrows :: Expression -> [Subst] -> IO [Expression]
buildExpressionsThrows expr = traverse (buildExpressionThrows expr)
