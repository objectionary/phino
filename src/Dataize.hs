{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Dataize (morph, dataize, dataize', DataizeContext (..), execBuildTerm) where

import AST
import Builder (buildAttributeThrows, buildBytesThrows, buildExpressionThrows)
import Control.Exception (throwIO)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Deps (BuildTermFunc, BuildTermMethod, SaveStepFunc, Term (TeAttribute, TeExpression))
import Locator (locatedExpression, withLocatedExpression)
import Matcher (Subst, substEmpty)
import Misc
import Must (Must (..))
import Rewriter (RewriteContext (RewriteContext), Rewritten, rewrite)
import Rule (RuleContext (RuleContext), isNF, matchExpressionWithRule')
import Text.Printf (printf)
import Yaml (ExtraArgument (..), normalizationRules)
import qualified Yaml as Y

type Dataized = (Maybe Bytes, [Rewritten])

type Dataizable = (Expression, NonEmpty Rewritten)

type Morphed = Dataizable

data DataizeContext = DataizeContext
  { _locator :: Expression
  , _program :: Program
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _depthSensitive :: Bool
  , _buildTerm :: BuildTermFunc
  , _saveStep :: SaveStepFunc
  }

-- Resolve formation for LAMBDA Morphing rule.
-- If formation contains λ binding, the called atom
-- result is returned.
formation :: [Binding] -> DataizeContext -> IO (Maybe Expression)
formation bds ctx = do
  let (lambda, bds') = maybeLambda bds
  case lambda of
    Just (BiLambda (Function func)) -> Just <$> atom func (ExFormation bds') ctx
    _ -> pure Nothing
  where
    maybeLambda :: [Binding] -> (Maybe Binding, [Binding])
    maybeLambda = maybeBinding (\case BiLambda _ -> True; _ -> False)
    maybeBinding :: (Binding -> Bool) -> [Binding] -> (Maybe Binding, [Binding])
    maybeBinding _ [] = (Nothing, [])
    maybeBinding func bds =
      let (found, rest) = partition func bds
       in case found of
            [bd] -> (Just bd, rest)
            _ -> (Nothing, bds)

-- Resolve dispatch from global object (Q.tau) for ROOT Morphing rule.
-- Here tau is the name of the attribute which is taken from Q
-- and expr is expression which program refers to.
-- If Q refers to formation which contains binding with attribute == tau -
-- the expression from this binding is returned.
phiDispatch :: T.Text -> Expression -> Maybe Expression
phiDispatch tau expr = case expr of
  ExFormation bds -> boundExpr bds
  _ -> Nothing
  where
    boundExpr :: [Binding] -> Maybe Expression
    boundExpr [] = Nothing
    boundExpr (bd : bds) = case bd of
      BiTau (AtLabel attr) expr' -> if attr == tau then Just expr' else boundExpr bds
      _ -> boundExpr bds

-- The Morphing function 𝕄 maps a normal-form object to a primitive. The head
-- 'expr' is judged together with its tail 'wrap' (the dispatches and
-- applications to its right): a primitive whole stops ('prim'), a non-normal
-- whole is normalized and re-morphed ('nmz', splicing the rewriter's steps in).
-- On a normal-form whole the head fires — a λ-formation calls its atom
-- ('lambda') and a global dispatch Φ.τ is resolved ('root') — re-attaching the
-- tail before morphing on. A dispatch or application head instead pushes 𝕄 one
-- operation deeper (𝕄(e.τ) ⟿ 𝕄(e).τ, 𝕄(e(a)) ⟿ 𝕄(e)(a)), so an atom buried
-- under a tail fires only once that tail has filled its voids; anything else is
-- ⊥. Threading the tail as a function is what replaces the head/tail matcher.
morph :: Morphed -> DataizeContext -> IO Morphed
morph = morphWith id

morphWith :: (Expression -> Expression) -> Morphed -> DataizeContext -> IO Morphed
morphWith wrap (expr, seq) ctx@DataizeContext{..}
  | primitive whole = stop "prim" whole
  | not (isNF whole (RuleContext (execBuildTerm ctx))) = normalize
  | otherwise = fire expr
  where
    whole = wrap expr
    stop :: String -> Expression -> IO Morphed
    stop name built = do
      seq' <- leadsTo seq name built ctx
      pure (built, seq')
    -- 𝕄(𝒩(e)) delegates to the normalization rewriter and splices its
    -- individual steps (alpha, copy, dot, …) into the chain before morphing on.
    normalize :: IO Morphed
    normalize = do
      prog' <- withLocatedExpression _locator whole _program
      (rewrittens, _) <- rewrite prog' normalizationRules (switchContext ctx)
      let (rw :| rws) = NE.reverse rewrittens
          seq' = rw :| rws <> NE.tail seq
      expr' <- locatedExpression _locator (fst rw)
      morph (expr', seq') ctx
    -- Fire the head rule on a normal-form whole, re-attaching the tail; a
    -- dispatch or application head pushes 𝕄 one operation deeper.
    fire :: Expression -> IO Morphed
    fire (ExFormation bds) = do
      resolved <- formation bds ctx
      case resolved of
        Just obj -> morphOn "lambda" (wrap obj)
        Nothing -> pure (ExTermination, seq)
    fire (ExDispatch ExRoot (AtLabel label)) = case phiDispatch label root of
      Just obj -> morphOn "root" (wrap obj)
      Nothing -> pure (ExTermination, seq)
    fire (ExDispatch recv attr) = morphWith (wrap . (`ExDispatch` attr)) (recv, seq) ctx
    fire (ExApplication recv arg) = morphWith (wrap . (`ExApplication` arg)) (recv, seq) ctx
    fire _ = pure (ExTermination, seq)
    morphOn :: String -> Expression -> IO Morphed
    morphOn name built = do
      seq' <- leadsTo seq name built ctx
      morph (built, seq') ctx
    root :: Expression
    root = let Program e = _program in e
    switchContext :: DataizeContext -> RewriteContext
    switchContext DataizeContext{..} =
      RewriteContext
        _locator
        _maxDepth
        _maxCycles
        _depthSensitive
        _buildTerm
        MtDisabled
        Nothing
        _saveStep

dataize :: DataizeContext -> IO Dataized
dataize ctx@DataizeContext{..} = do
  expr <- locatedExpression _locator _program
  (maybeBytes, seq) <- dataize' (expr, (_program, Nothing) :| []) ctx
  pure (maybeBytes, reverse seq)

-- The Dataization function 𝔻 retrieves bytes from an expression. It is driven
-- by the ordered rules from 'dataization.yaml': 'delta' yields the asset bytes,
-- 'none' yields nothing, 'box' contextualizes the φ-body and keeps dataizing
-- (its step is labelled by the operation, 'contextualize'), and 'norm' reduces
-- through morphing, splicing the morphing steps into the chain.
dataize' :: Dataizable -> DataizeContext -> IO Dataized
dataize' (expr, seq) ctx = do
  matched <- firstMatch Y.dataizationRules
  case matched of
    Just (rule, subst) -> apply rule subst
    Nothing -> pure (Nothing, NE.toList seq)
  where
    firstMatch :: [Y.DataizeRule] -> IO (Maybe (Y.DataizeRule, Subst))
    firstMatch [] = pure Nothing
    firstMatch (rule : rest) = do
      substs <- matchExpressionWithRule' expr (asRule rule) (RuleContext (execBuildTerm ctx))
      case substs of
        (subst : _) -> pure (Just (rule, subst))
        [] -> firstMatch rest
    asRule :: Y.DataizeRule -> Y.Rule
    asRule rule = Y.Rule rule.name rule.description rule.match ExRoot Nothing rule.where_ rule.when
    apply :: Y.DataizeRule -> Subst -> IO Dataized
    apply rule subst = case rule.then_ of
      Y.DoData bytes -> do
        bts <- buildBytesThrows bytes subst
        pure (Just bts, NE.toList seq)
      Y.DoNothing -> pure (Nothing, NE.toList seq)
      Y.DoDataize (Y.DaExpr result) -> do
        built <- buildExpressionThrows result subst
        seq' <- leadsTo seq (operation rule) built ctx
        dataize' (built, seq') ctx
      -- 𝔻(𝕄(e)) delegates to the morphing relation, splicing its steps into
      -- the chain before dataizing on.
      Y.DoDataize (Y.DaMorph arg) -> do
        built <- buildExpressionThrows arg subst
        (morphed, seq') <- morph (built, seq) ctx
        dataize' (morphed, seq') ctx
    operation :: Y.DataizeRule -> String
    operation rule = case rule.where_ of
      Just (extra : _) -> Y.function extra
      _ -> ""

leadsTo :: NonEmpty Rewritten -> String -> Expression -> DataizeContext -> IO (NonEmpty Rewritten)
leadsTo ((prog, _) :| rest) rule expr DataizeContext{..} = do
  prog' <- withLocatedExpression _locator expr prog
  pure ((prog', Nothing) :| (prog, Just rule) : rest)

-- Synthetic dataize function for internal usage inside atoms
-- Here we modify original program from context by adding new binding
-- which refers to expression we want to dataize.
_dataize :: Expression -> DataizeContext -> IO (Maybe Bytes)
_dataize expr ctx@DataizeContext{_buildTerm = buildTerm, _program = Program (ExFormation bds)} = do
  (TeAttribute attr) <- buildTerm "random-tau" [] substEmpty
  let prog = Program (ExFormation (BiTau attr expr : bds))
  (bts, _) <- dataize' (expr, (prog, Nothing) :| []) ctx{_program = prog}
  pure bts
_dataize _ _ = throwIO (userError "Can't call _dataize from atoms with non-formation program")

atom :: T.Text -> Expression -> DataizeContext -> IO Expression
atom "L_number_plus" self ctx = do
  left <- _dataize (ExDispatch self (AtLabel "x")) ctx
  right <- _dataize (ExDispatch self AtRho) ctx
  case (left, right) of
    (Just left', Just right') -> do
      let first = either toDouble id (btsToNum left')
          second = either toDouble id (btsToNum right')
          sum = first + second
      pure (DataNumber (numToBts sum))
    _ -> pure ExTermination
atom "L_number_times" self ctx = do
  left <- _dataize (ExDispatch self (AtLabel "x")) ctx
  right <- _dataize (ExDispatch self AtRho) ctx
  case (left, right) of
    (Just left', Just right') -> do
      let first = either toDouble id (btsToNum left')
          second = either toDouble id (btsToNum right')
          sum = first * second
      pure (DataNumber (numToBts sum))
    _ -> pure ExTermination
atom "L_number_eq" self ctx = do
  x <- _dataize (ExDispatch self (AtLabel "x")) ctx
  rho <- _dataize (ExDispatch self AtRho) ctx
  case (x, rho) of
    (Just x', Just rho') -> do
      let self' = either toDouble id (btsToNum rho')
          first = either toDouble id (btsToNum x')
      if self' == first
        then pure (DataNumber (numToBts first))
        else pure (ExDispatch self (AtLabel "y"))
    _ -> pure ExTermination
atom func _ _ = throwIO (userError (printf "Atom '%s' does not exist" (T.unpack func)))

-- Augment the injected, context-free term builder with the dataization and
-- morphing operations that need the full evaluation context: 'lambda' applies
-- an atom and 'global' dispatches from the universe Q. Every other function is
-- delegated unchanged.
execBuildTerm :: DataizeContext -> BuildTermFunc
execBuildTerm ctx "lambda" = _lambda ctx
execBuildTerm ctx "global" = _global ctx
execBuildTerm ctx func = _buildTerm ctx func

_lambda :: DataizeContext -> BuildTermMethod
_lambda ctx [ArgExpression expr] subst = do
  form <- buildExpressionThrows expr subst
  case form of
    ExFormation bds -> do
      resolved <- formation bds ctx
      case resolved of
        Just obj -> pure (TeExpression obj)
        Nothing -> throwIO (userError "Function lambda() expects a formation with a λ binding")
    _ -> throwIO (userError "Function lambda() expects a formation")
_lambda _ _ _ = throwIO (userError "Function lambda() requires exactly 1 expression argument")

_global :: DataizeContext -> BuildTermMethod
_global DataizeContext{_program = Program prog} [ArgAttribute attr] subst = do
  attr' <- buildAttributeThrows attr subst
  case attr' of
    AtLabel label -> case phiDispatch label prog of
      Just expr -> pure (TeExpression expr)
      Nothing -> throwIO (userError (printf "Universe Q has no attribute '%s'" (show attr')))
    _ -> throwIO (userError "Function global() expects a labelled attribute")
_global _ _ _ = throwIO (userError "Function global() requires exactly 1 attribute argument")
