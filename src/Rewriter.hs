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
import Data.Maybe (fromMaybe, isNothing)
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

type RewriteState = (NonEmpty Rewritten, Expression, Seen, Bool, Maybe (Set Int))

type Seen = Map.Map Int [Expression]

seenMember :: Int -> Expression -> Seen -> Bool
seenMember digest expr seen = maybe False (elem expr) (Map.lookup digest seen)

seenInsert :: Int -> Expression -> Seen -> Seen
seenInsert digest expr = Map.insertWith (++) digest [expr]

type Rewritten = (Expression, Maybe (Judgment, String))

type Rewrittens = (NonEmpty Rewritten, Bool)

type Rewrittens' = ([Rewritten], Bool)

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
  , _universe :: Maybe Expression
  , _buildTerm :: BuildTermFunc
  , _normal :: Expression -> Bool
  , _matching :: Maybe Expression -> Expression -> Set Int
  , _must :: Must
  , _breakpoint :: Maybe String
  , _saveStep :: SaveStepFunc
  , _saveMade :: SaveMadeFunc
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

buildAndReplace' :: ToReplace -> ReplaceExpressionFunc -> IO (Expression, [(Expression, Expression)])
buildAndReplace' (expr, ptn, res, substs) func = do
  ptns <- buildExpressionsThrows ptn substs
  repls <- buildExpressionsThrows res substs
  pure (func (expr, ptns, map const repls), zip ptns repls)

tryBuildAndReplaceFast :: ToReplace -> IO (Expression, [(Expression, Expression)])
tryBuildAndReplaceFast state@(expr, ptn@(ExFormation (_ : pbds)), res@(ExFormation (_ : rbds)), substs)
  | fast ptn res = do
      logDebug "Applying fast replacing since 'pattern' and 'result' are suitable for this..."
      buildAndReplace' (expr, ExFormation (init pbds), ExFormation (init rbds), substs) replaceExpressionFast
  | otherwise = do
      logDebug "Applying regular replacing..."
      buildAndReplace' state replaceExpression
tryBuildAndReplaceFast state = buildAndReplace' state replaceExpression

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

interpreted :: Y.Rule -> Step
interpreted rule = Step rule.name applied
  where
    applied :: RuleContext -> Expression -> IO (Maybe (Expression, [(Expression, Expression)]))
    applied ctx expr =
      R.matchExpressionWithRule expr rule ctx >>= \case
        [] -> pure Nothing
        matched
          | isNothing rule.when && isNothing rule.having -> Just <$> tryBuildAndReplaceFast (expr, rule.pattern, rule.result, matched)
          | otherwise -> Just <$> buildAndReplace' (expr, rule.pattern, rule.result, matched) replaceExpression

direct :: String -> Bool -> (Maybe Expression -> Expression -> [Expression]) -> Step
direct name redex rewritten = Step name applied
  where
    applied :: RuleContext -> Expression -> IO (Maybe (Expression, [(Expression, Expression)]))
    applied (RuleContext _ universe _) expr = pure $ case sites redex (rewritten universe) expr of
      [] -> Nothing
      found -> Just (replaceExpression (expr, map fst found, map (const . snd) found), found)

every :: [Step] -> Maybe Expression -> Expression -> Set Int
every steps _ _ = Set.fromList (zipWith const [0 ..] steps)

rewrite' :: RewriteState -> [(Int, Step)] -> Int -> RewriteContext -> IO RewriteState
rewrite' state [] _ _ = pure state
rewrite' (rewrittens, located, unique, stop, found) ((idx, rule) : rest) iteration ctx@RewriteContext{..}
  | Set.member idx matched || _breakpoint == Just (_name rule) =
      _rewrite (rewrittens, located, unique, stop, Just matched) 1 >>= \case
        state'@(_, _, _, True, _) -> pure state'
        state' -> rewrite' state' rest iteration ctx
  | otherwise = rewrite' (rewrittens, located, unique, stop, Just matched) rest iteration ctx
  where
    matched :: Set Int
    matched = fromMaybe (_matching _universe located) found
    _rewrite :: RewriteState -> Int -> IO RewriteState
    _rewrite (_rewrittens@((current, _) :| _), expression, _unique, _, _found) _count =
      let ruleName = _name rule
       in if _count - 1 == _maxDepth
            then do
              logDebug (printf "Max amount of rewriting cycles (%d) for rule '%s' has been reached, rewriting is stopped" _maxDepth ruleName)
              if _depthSensitive
                then do
                  exhausted <- applicable expression [rule] ctx
                  if exhausted
                    then throwIO (StoppedOnLimit "max-depth" _maxDepth)
                    else pure (_rewrittens, expression, _unique, False, _found)
                else pure (_rewrittens, expression, _unique, False, _found)
            else do
              logDebug (printf "Starting rewriting cycle for rule '%s': %d out of %d" ruleName _count _maxDepth)
              _applied rule (RuleContext _buildTerm _universe _normal) expression >>= \case
                Nothing -> do
                  logDebug (printf "Rule '%s' does not match, rewriting is stopped" ruleName)
                  if _breakpoint == Just ruleName && ruleName `notElem` [fired | (_, Just (_, fired)) <- NE.toList _rewrittens]
                    then do
                      logDebug (printf "Rule '%s' is a breakpoint, dropping down all the previous rewritings..." ruleName)
                      pure (_rewrittens, expression, _unique, True, _found)
                    else pure (_rewrittens, expression, _unique, False, _found)
                Just (expr, rewritten) -> do
                  logDebug (printf "Rule '%s' has been matched and applied" ruleName)
                  if expression == expr
                    then do
                      logDebug (printf "Applied '%s', no changes made" ruleName)
                      pure (_rewrittens, expression, _unique, False, _found)
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
                              mapM_ (uncurry _saveMade) [(redex, object) | (redex@(ExApplication head' (ArTau attr _)), object@(ExFormation _)) <- rewritten, attr /= AtRho, object /= head']
                              _rewrite (leadsTo updated, expr, seenInsert digest expr _unique, False, Nothing) (_count + 1)
      where
        leadsTo :: Expression -> NonEmpty Rewritten
        leadsTo next =
          let (head', _) :| rest = _rewrittens
           in (next, Nothing) :| (head', Just (Normalization, _name rule)) : rest

applicable :: Expression -> [Step] -> RewriteContext -> IO Bool
applicable _ [] _ = pure False
applicable expression (rule : rest) ctx@RewriteContext{..} =
  _applied rule (RuleContext _buildTerm _universe _normal) expression >>= \case
    Just (changed, _) | changed /= expression -> pure True
    _ -> applicable expression rest ctx

rewrite :: Expression -> [Step] -> RewriteContext -> IO Rewrittens
rewrite expr rules ctx@RewriteContext{..} = do
  located <- locatedExpression _locator expr
  (rewrittens, exceeded) <- _rewrite ((expr, Nothing) :| [], located, Map.empty, False, Nothing) 0
  pure (NE.reverse rewrittens, exceeded)
  where
    _rewrite :: RewriteState -> Int -> IO Rewrittens
    _rewrite state@(rewrittens@((current, _) :| _), expression, _, _, _) count
      | not (inRange _must count) && count > 0 && exceedsUpperBound _must count = throwIO (MustStopBefore _must count)
      | count == _maxCycles && not (inRange _must count) = throwIO (MustBeGoing _must count)
      | count == _maxCycles = do
          logDebug (printf "Max amount of rewriting cycles for all rules (%d) has been reached, rewriting is stopped" _maxCycles)
          if _depthSensitive
            then do
              exhausted <- applicable expression rules ctx
              if exhausted
                then throwIO (StoppedOnLimit "max-cycles" _maxCycles)
                else pure (rewrittens, False)
            else pure (rewrittens, True)
      | otherwise = do
          logDebug (printf "Starting rewriting cycle for all rules: %d out of %d" count _maxCycles)
          rewrite' state (zip [0 ..] rules) count ctx >>= \case
            (_, _, _, True, _) -> pure ((expr, Nothing) :| [], False)
            state'@(rewrittens'@((current', _) :| _), _, _, False, _) ->
              if length rewrittens' == length rewrittens || current' == current
                then do
                  logDebug "Rewriting is stopped since it has no effect"
                  if not (inRange _must count)
                    then throwIO (MustBeGoing _must count)
                    else pure (rewrittens', False)
                else _rewrite state' (count + 1)
