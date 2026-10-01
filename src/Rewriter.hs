{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter (Seen, direct, every, fast, interpreted, rewrite, RewriteContext (..), Rewritten, Rewrittens, Rewrittens', seenInsert, seenMember, stepHeaders) where

import AST
import Builder
import Control.Exception (Exception, throwIO)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Deps
import Locator (locatedExpression, withLocatedExpression)
import Logger (logDebug)
import Matcher (Subst, sites)
import Must (Must (..), exceedsUpperBound, inRange)
import Printer (printExpression)
import Replacer (ReplaceExpressionFunc, replaceExpression, replaceExpressionFast)
import Rule (RuleContext (RuleContext), Step (..))
import qualified Rule as R
import Text.Printf (printf)
import qualified Yaml as Y

type RewriteState = (NonEmpty Rewritten, Seen, Bool, Maybe (Set Int))

-- Loop-detection store. It maps a cheap fixed-size digest of an expression (see
-- 'hashExpression') to the full expressions that produced that digest. A
-- digest collision is resolved by a slow, exact structural (==) comparison,
-- so loops are still detected soundly while the common (no collision) case
-- stays O(1) on the digest instead of O(expressionSize) per lookup/insert.
type Seen = Map.Map Int [Expression]

-- Has this exact expression been seen before? The digest lookup is fast; the
-- (==) check runs only on a digest match, guarding against hash collisions.
seenMember :: Int -> Expression -> Seen -> Bool
seenMember digest expr seen = maybe False (elem expr) (Map.lookup digest seen)

-- Remember an expression under its digest, keeping any earlier collisions.
seenInsert :: Int -> Expression -> Seen -> Seen
seenInsert digest expr = Map.insertWith (++) digest [expr]

-- A step of a rewriting chain: the expression, and the rule that took it to
-- the next one, named and tagged with the judgment it belongs to, which is how
-- a chain of 𝕄 or 𝔻 mixing rules of several judgments tells them apart (#1536).
type Rewritten = (Expression, Maybe (Judgment, String))

type Rewrittens = (NonEmpty Rewritten, Bool)

type Rewrittens' = ([Rewritten], Bool)

-- Build a header line for every step of a rewriting chain. The chain is
-- '[(e0, Just r0), ..., (en, Nothing)]', where 'ri' is the rule applied to 'ei'
-- to produce 'e(i+1)' (see 'leadsTo'). A step's header names the rule that
-- produced its expression, together with the AST node counts before and after
-- that rule, e.g. "=== Step #4, Rule 'STOP', 32t -> 43t". The very first step is
-- the input, which no rule produced, so it carries only its number:
-- "=== Step #1". The 'N nodes -> M nodes' pair matches the debug log emitted
-- while rewriting.
stepHeaders :: [Rewritten] -> [String]
stepHeaders chain = zipWith3 header [1 ..] chain (Nothing : map Just chain)
  where
    header :: Int -> Rewritten -> Maybe Rewritten -> String
    header step _ Nothing = printf "=== Step #%d" step
    header step (current, _) (Just (before, rule)) =
      printf
        "=== Step #%d, Rule '%s', %dt -> %dt"
        step
        (maybe "?" snd rule)
        (countNodes before)
        (countNodes current)

type ToReplace = (Expression, Expression, Expression, [Subst])

data RewriteContext = RewriteContext
  { _locator :: Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _depthSensitive :: Bool
  , -- The world the rewritten term stands in, where one is known. The rules
    -- never see it: it reaches the 'named' function through 'RuleContext',
    -- which is how 'dot' tells the formation it dispatched from the whole
    -- program and writes 'ρ ↦ Φ' rather than the program itself (#1318,
    -- #1460). Normalization inside 𝕄 and 𝔻 knows the universe and names it
    -- here; the 'rewrite' command rewrites a term with no world around it and
    -- names nothing.
    _universe :: Maybe Expression
  , _buildTerm :: BuildTermFunc
  , -- Whether a term is a normal form, which a '𝑛' or '𝑘' meta of a rule asks
    -- (see '_normal' of 'RuleContext').
    _normal :: Expression -> Bool
  , -- The numbers of the steps matching somewhere in a term, in the order the
    -- rewriting is handed them, told the world the term stands in. A step it
    -- does not name is not tried, and it is asked again only once a step has
    -- changed the term, so a rule that matches nowhere costs no walk over the
    -- term. Normalization inside 𝕄 and 𝔻 asks the engine, which finds them
    -- all in one walk; the 'rewrite' command names every step it is handed
    -- (see 'every'), so each of its rules is tried as it always was (#1643).
    _matching :: Maybe Expression -> Expression -> Set Int
  , _must :: Must
  , _breakpoint :: Maybe String
  , _saveStep :: SaveStepFunc
  }

data RewriteException
  = MustBeGoing Must Int
  | MustStopBefore Must Int
  | StoppedOnLimit String Int
  | LoopingRewriting String String Int
  deriving (Exception)

instance Show RewriteException where
  show (MustBeGoing mst cnt) =
    printf
      "With option --must=%s it's expected rewriting cycles to be in range [%s], but rewriting stopped after %d cycles"
      (show mst)
      (show mst)
      cnt
  show (MustStopBefore mst cnt) =
    printf
      "With option --must=%s it's expected rewriting cycles to be in range [%s], but rewriting has already reached %d cycles and is still going"
      (show mst)
      (show mst)
      cnt
  show (StoppedOnLimit flg lim) =
    printf
      "With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --%s=%d"
      flg
      lim
  show (LoopingRewriting expr rul stp) =
    printf
      "On rewriting step '%d' of rule '%s' we got the same expression as we got at one of the previous step, it seems rewriting is looping\nExpression: %s"
      stp
      rul
      expr

-- Build pattern and result expression and replace patterns to results in given expression
buildAndReplace' :: ToReplace -> ReplaceExpressionFunc -> IO Expression
buildAndReplace' (expr, ptn, res, substs) func = do
  ptns <- buildExpressionsThrows ptn substs
  repls <- buildExpressionsThrows res substs
  pure (func (expr, ptns, map const repls))

-- If pattern and replacement are appropriate for fast replacing - does it.
-- Pattern and replacement expressions can be used in fast replacing only if
-- 1. they are both formations
-- 2. they start and end with the same meta bindings, e.g. [!B1, ..., !B2]
-- 3. the does not have meta bindings between first and last meta bindings
-- In such case we can just replace bindings one by one without building whole expression.
-- You can find more details in this ticket: https://github.com/objectionary/phino/issues/321
-- If we don't meet the conditions above - just do a regular replacing
tryBuildAndReplaceFast :: ToReplace -> IO Expression
tryBuildAndReplaceFast state@(expr, ptn@(ExFormation (_ : pbds)), res@(ExFormation (_ : rbds)), substs)
  | fast ptn res = do
      logDebug "Applying fast replacing since 'pattern' and 'result' are suitable for this..."
      buildAndReplace' (expr, ExFormation (init pbds), ExFormation (init rbds), substs) replaceExpressionFast
  | otherwise = do
      logDebug "Applying regular replacing..."
      buildAndReplace' state replaceExpression
tryBuildAndReplaceFast state = buildAndReplace' state replaceExpression

-- Whether a rule of the pattern and the result is replaced the fast way (see
-- 'tryBuildAndReplaceFast').
fast :: Expression -> Expression -> Bool
fast (ExFormation _pbds@(pbd : pbds)) (ExFormation _rbds@(rbd : rbds)) =
  startsAndEndsWithMeta _pbds
    && startsAndEndsWithMeta _rbds
    && pbd == rbd
    && last pbds == last rbds
    && not (hasMetaBindings (init pbds))
    && not (hasMetaBindings (init rbds))
  where
    startsAndEndsWithMeta :: [Binding] -> Bool
    startsAndEndsWithMeta [] = False
    startsAndEndsWithMeta bds@(bd : _) =
      length bds > 1
        && isMetaBinding bd
        && isMetaBinding (last bds)
    hasMetaBindings :: [Binding] -> Bool
    hasMetaBindings = foldl (\acc bd -> acc || isMetaBinding bd) False
    isMetaBinding :: Binding -> Bool
    isMetaBinding = \case
      BiMeta _ -> True
      BiAny _ -> True
      _ -> False
fast _ _ = False

-- The step a rule of YAML takes: the matcher finds every place the rule
-- matches at and the builder and the replacer rewrite them (see
-- 'tryBuildAndReplaceFast').
interpreted :: Y.Rule -> Step
interpreted rule = Step rule.name applied
  where
    applied :: RuleContext -> Expression -> IO (Maybe Expression)
    applied ctx expr =
      R.matchExpressionWithRule expr rule ctx >>= \case
        [] -> pure Nothing
        matched -> Just <$> tryBuildAndReplaceFast (expr, rule.pattern, rule.result, matched)

-- The step a rule 'phino compile' turned into Haskell takes: the rule is a
-- function telling what it rewrites a term to where the term matches it as a
-- whole, and the places it matches at are found in the order the matcher
-- finds them (see 'sites') and replaced in that order, exactly as the replacer
-- replaces those of a rule of YAML. A place inside what an earlier one was
-- rewritten to is therefore a step of its own, as it is for the matcher, and
-- the chain of steps does not depend on which of the two ran (#1617). The
-- flag says whether the rule matches only a redex (see 'R.redex').
-- The function is told the world the term stands in, where one is known,
-- which is what the 'named' function of a rule reads.
direct :: String -> Bool -> (Maybe Expression -> Expression -> [Expression]) -> Step
direct name redex rewritten = Step name applied
  where
    applied :: RuleContext -> Expression -> IO (Maybe Expression)
    applied (RuleContext _ universe _) expr = pure $ case sites redex (rewritten universe) expr of
      [] -> Nothing
      found -> Just (replaceExpression (expr, map fst found, map (const . snd) found))

-- The numbers of every one of the steps, whatever the term and its world:
-- what a rewriting that tries each of its steps hands as '_matching'.
every :: [Step] -> Maybe Expression -> Expression -> Set Int
every steps _ _ = Set.fromList (zipWith const [0 ..] steps)

-- The function returns tuple (X, Y, Z, W) where
-- - X is sequence of expressions;
-- - Y is Set of unique expressions after each rule application. It allows to stop the rewriting if we're getting
--   into loop and get back to an expression which we've already got before
-- - Z is boolean flag which tells us if we reach breakpoint. If unmatched rule is equal to breakpoint rule - entire
--   rewriting must be stopped and original expression must be returned
-- - W is Set of the numbers of the steps matching the last expression (see '_matching'), or Nothing if they were not
--   asked since it last changed
rewrite' :: RewriteState -> [(Int, Step)] -> Int -> RewriteContext -> IO RewriteState
rewrite' state [] _ _ = pure state
rewrite' (rewrittens, unique, stop, found) ((idx, rule) : rest) iteration ctx@RewriteContext{..} = do
  matched <- maybe (_matching _universe <$> locatedExpression _locator (fst (NE.head rewrittens))) pure found
  if Set.member idx matched || _breakpoint == Just (_name rule)
    then
      _rewrite (rewrittens, unique, stop, Just matched) 1 >>= \case
        state'@(_, _, True, _) -> pure state'
        state' -> rewrite' state' rest iteration ctx
    else rewrite' (rewrittens, unique, stop, Just matched) rest iteration ctx
  where
    _rewrite :: RewriteState -> Int -> IO RewriteState
    _rewrite (_rewrittens@((current, _) :| _), _unique, _, _found) _count =
      let ruleName = _name rule
       in if _count - 1 == _maxDepth
            then do
              logDebug (printf "Max amount of rewriting cycles (%d) for rule '%s' has been reached, rewriting is stopped" _maxDepth ruleName)
              if _depthSensitive
                then do
                  exhausted <- applicable current [rule] ctx
                  if exhausted
                    then throwIO (StoppedOnLimit "max-depth" _maxDepth)
                    else pure (_rewrittens, _unique, False, _found)
                else pure (_rewrittens, _unique, False, _found)
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName _count _maxDepth)
              expression <- locatedExpression _locator current
              _applied rule (RuleContext _buildTerm _universe _normal) expression >>= \case
                Nothing -> do
                  logDebug (printf "Rule '%s' does not match, rewriting is stopped" ruleName)
                  if _breakpoint == Just ruleName
                    then do
                      logDebug (printf "Rule '%s' is a breakpoint, dropping down all the previous rewritings..." ruleName)
                      pure (_rewrittens, _unique, True, _found)
                    else pure (_rewrittens, _unique, False, _found)
                Just expr -> do
                  logDebug (printf "Rule '%s' has been matched and applied" ruleName)
                  if expression == expr
                    then do
                      logDebug (printf "Applied '%s', no changes made" ruleName)
                      pure (_rewrittens, _unique, False, _found)
                    else
                      let digest = hashExpression expr
                       in if seenMember digest expr _unique
                            then throwIO (LoopingRewriting (printExpression expr) ruleName _count)
                            else do
                              logDebug
                                ( printf
                                    "Applied '%s' (%d nodes -> %d nodes)\n%s"
                                    ruleName
                                    (countNodes expression)
                                    (countNodes expr)
                                    (printExpression expr)
                                )
                              updated <- withLocatedExpression _locator expr current
                              _saveStep updated
                              _rewrite (leadsTo updated, seenInsert digest expr _unique, False, Nothing) (_count + 1)
      where
        leadsTo :: Expression -> NonEmpty Rewritten
        leadsTo next =
          let (head', _) :| rest = _rewrittens
           in (next, Nothing) :| (head', Just (Normalization, _name rule)) : rest

-- Tells whether any of the rules still matches the located expression. A run
-- with nothing left to rewrite after its last allowed step has finished, not
-- run out of its limit, so --depth-sensitive lets it pass (#1439)
applicable :: Expression -> [Step] -> RewriteContext -> IO Bool
applicable current rules RewriteContext{..} = do
  expression <- locatedExpression _locator current
  go expression rules
  where
    go :: Expression -> [Step] -> IO Bool
    go _ [] = pure False
    go expression (rule : rest) =
      _applied rule (RuleContext _buildTerm _universe _normal) expression >>= \case
        Nothing -> go expression rest
        Just _ -> pure True

-- Rewrite the expression by provided locator from RewriteContext
rewrite :: Expression -> [Step] -> RewriteContext -> IO Rewrittens
rewrite expr rules ctx@RewriteContext{..} = do
  (rewrittens, exceeded) <- _rewrite ((expr, Nothing) :| [], Map.empty, False, Nothing) 0
  pure (NE.reverse rewrittens, exceeded)
  where
    _rewrite :: RewriteState -> Int -> IO Rewrittens
    _rewrite state@(rewrittens@((current, _) :| _), _, _, _) count
      | not (inRange _must count) && count > 0 && exceedsUpperBound _must count = throwIO (MustStopBefore _must count)
      | count == _maxCycles && not (inRange _must count) = throwIO (MustBeGoing _must count)
      | count == _maxCycles = do
          logDebug (printf "Max amount of rewriting cycles for all rules (%d) has been reached, rewriting is stopped" _maxCycles)
          if _depthSensitive
            then do
              exhausted <- applicable current rules ctx
              if exhausted
                then throwIO (StoppedOnLimit "max-cycles" _maxCycles)
                else pure (rewrittens, False)
            else pure (rewrittens, True)
      | otherwise = do
          logDebug (printf "Starting rewriting cycle for all rules: %d out of %d" count _maxCycles)
          rewrite' state (zip [0 ..] rules) count ctx >>= \case
            (_, _, True, _) -> pure ((expr, Nothing) :| [], False)
            state'@(rewrittens'@((current', _) :| _), _, False, _) ->
              if length rewrittens' == length rewrittens || current' == current
                then do
                  logDebug "Rewriting is stopped since it has no effect"
                  if not (inRange _must count)
                    then throwIO (MustBeGoing _must count)
                    else pure (rewrittens', False)
                else _rewrite state' (count + 1)
