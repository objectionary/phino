{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- | Module for pattern matching functionality in φ-programs
module Match (performMatch, MatchResult(..)) where

import Ast
import Builder (buildExpression, buildBinding)
import Control.Exception (throwIO)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Functions (buildTerm)
import Logger (logDebug)
import Matcher
import Parser (parseExpression, parseBinding)
import Pretty (prettyExpression, prettyExpression', PrintMode(..))
import Rule (RuleContext(..), meetCondition)
import Text.Printf (printf)
import Yaml (Condition)
import qualified Yaml as Y

-- | Result of a pattern match
data MatchResult = MatchResult
  { matchedExpression :: Expression,
    matchedScope :: Expression,
    substitution :: Subst
  }
  deriving (Show, Eq)

-- | Pattern type - can be either a binding or expression
data PatternType 
  = BindingPattern Binding
  | ExpressionPattern Expression

-- | Find all locations in a program where a pattern matches
findMatches :: PatternType -> Program -> [MatchResult]
findMatches (BindingPattern ptnBinding) prog@(Program rootExpr) = 
  findBindingMatches ptnBinding rootExpr
  where
    -- Find all bindings that match the pattern
    findBindingMatches :: Binding -> Expression -> [MatchResult]
    findBindingMatches pattern expr = case expr of
      ExFormation bds -> 
        let directMatches = concatMap (matchSingleBinding pattern) bds
            deepMatches = concatMap (findInBinding pattern) bds
        in directMatches ++ deepMatches
      ExDispatch e _ -> findBindingMatches pattern e
      ExApplication e bd -> 
        findBindingMatches pattern e ++ findInBinding pattern bd
      _ -> []
    
    -- Try to match a single binding
    matchSingleBinding :: Binding -> Binding -> [MatchResult]
    matchSingleBinding pattern target =
      case matchBinding pattern target defaultScope of
        [] -> []
        substs -> map (\s -> MatchResult (bindingToExpression target) defaultScope s) substs
    
    -- Find matches within a binding
    findInBinding :: Binding -> Binding -> [MatchResult]
    findInBinding pattern (BiTau _ e) = findBindingMatches pattern e
    findInBinding _ _ = []
    
    -- Convert binding to expression for display
    bindingToExpression :: Binding -> Expression
    bindingToExpression bd = ExFormation [bd]

findMatches (ExpressionPattern ptnExpr) prog@(Program rootExpr) = 
  let substs = matchProgram ptnExpr prog
  in concatMap (findExprLocations ptnExpr rootExpr) substs
  where
    -- Find specific expression locations that match
    findExprLocations :: Expression -> Expression -> Subst -> [MatchResult]
    findExprLocations pattern expr subst = 
      let directMatch = case matchExpression pattern expr defaultScope of
            [] -> []
            matches -> if subst `elem` matches
                      then [MatchResult expr defaultScope subst]
                      else []
          deepMatches = case expr of
            ExFormation bds -> 
              concatMap (\bd -> findInBinding pattern bd (ExFormation bds) subst) bds
            ExDispatch e _ -> 
              findExprLocations pattern e subst
            ExApplication e bd -> 
              findExprLocations pattern e subst ++ 
              findInBinding pattern bd defaultScope subst
            _ -> []
      in directMatch ++ deepMatches
    
    -- Find matches within a binding
    findInBinding :: Expression -> Binding -> Expression -> Subst -> [MatchResult]
    findInBinding pattern (BiTau _ e) scope subst = 
      findExprLocations pattern e subst
    findInBinding _ _ _ _ = []

-- | Filter matches based on a condition
filterByCondition :: Maybe Condition -> [MatchResult] -> IO [MatchResult]
filterByCondition Nothing results = pure results
filterByCondition (Just cond) results = do
  logDebug (printf "Filtering %d matches with condition" (length results))
  filtered <- mapM checkResult results
  pure (concat filtered)
  where
    checkResult :: MatchResult -> IO [MatchResult]
    checkResult result@MatchResult{..} = do
      let ctx = RuleContext (Program matchedScope) buildTerm
      met <- meetCondition cond [substitution] ctx
      if null met
        then pure []
        else pure [result]

-- | Format a match result for display
formatMatchResult :: MatchResult -> PrintMode -> String
formatMatchResult MatchResult{..} mode = 
  case mode of
    SWEET -> prettyExpression' matchedExpression
    SALTY -> prettyExpression matchedExpression

-- | Format match results showing unique matches
formatUniqueMatches :: [MatchResult] -> PrintMode -> String
formatUniqueMatches results mode =
  let uniqueExprs = nub (map matchedExpression results)
      formatted = map (\e -> case mode of
                              SWEET -> prettyExpression' e
                              SALTY -> prettyExpression e) uniqueExprs
  in unlines formatted

-- | Main function to perform pattern matching
performMatch :: String -> Maybe Condition -> Program -> PrintMode -> IO String
performMatch patternStr maybeCond prog mode = do
  -- Parse the pattern - try as binding first, then as expression
  pattern <- case parseBinding patternStr of
    Left _ -> case parseExpression patternStr of
      Left err -> throwIO $ userError $ "Invalid pattern: " ++ err
      Right expr -> pure $ ExpressionPattern expr
    Right binding -> pure $ BindingPattern binding
  
  logDebug (printf "Searching for pattern: %s" patternStr)
  
  -- Find all matches
  let matches = findMatches pattern prog
  logDebug (printf "Found %d raw matches" (length matches))
  
  -- Filter by condition if provided
  filtered <- filterByCondition maybeCond matches
  logDebug (printf "After filtering: %d matches" (length filtered))
  
  -- Format and return results
  if null filtered
    then pure ""
    else pure $ formatUniqueMatches filtered mode