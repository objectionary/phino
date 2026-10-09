{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Evaluate (evaluation, fired) where

import AST
import Builder (buildExpressionThrows)
import Control.Exception (catch, throwIO, try)
import Control.Monad (foldM, unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import qualified Data.Text as T
import Deps (Evaluation (..), Judgment (Morphing), State (..), resited)
import Engine (Engine (..))
import Lambdas (Lambda (..), Meta (..), joined, matched, minted, symbolized)
import Matcher (MetaValue (..), Subst, combine, substEmpty, substSingle, substSlot)
import Morph (Answer, Firing (..), Kept (..), ReduceContext (..), ReduceException (..), Refused (..), Steps (..), admitted, charged, counted, deeper, isLambda, lambda, morphing, normalized, opening, recalled, refused, remember, remembered, retained, settled, starved, unfired, unparked)
import Printer (printFunction)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Text.Printf (printf)
import qualified Yaml as Y

evaluation :: ReduceContext -> State -> Expression -> Expression -> IO (Expression, State)
evaluation ctx state form univ = case form of
  ExFormation bds
    | not (any isLambda bds) -> pure (ExTermination, state)
    | otherwise -> case lambda bds of
        Just (func, args) -> do
          (raw, state') <- symbol func form args univ state ctx
          (normal, _) <- normalized raw ((univ, Nothing) :| []) ctx
          pure (normal, state')
        Nothing -> case unknown bds of
          Just idx -> stuck idx form
          Nothing -> throwIO (userError "Function evaluate() expects a formation with a single λ binding naming a function")
  _ -> throwIO (userError "Function evaluate() expects a formation")
  where
    unknown :: [Binding] -> Maybe Int
    unknown bindings = case partition isLambda bindings of
      ([BiLambda (FnSymbol idx)], _) -> Just idx
      _ -> Nothing
    stuck :: Int -> Expression -> IO (Expression, State)
    stuck idx form = do
      unless (name `elem` ctx._parked) (ctx._saveEval (EvStuck ctx._nesting name ctx._judgment form))
      throwIO (Stuck name)
      where
        name :: T.Text
        name = T.pack (printFunction (FnSymbol idx))

symbol :: T.Text -> Expression -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
symbol func form self univ state caller = case matched caller._symbolic func of
  Nothing -> do
    unless (func `elem` caller._parked) (caller._saveEval (EvStuck caller._nesting func caller._judgment form))
    throwIO (Stuck func)
  Just entry -> do
    known <- recalled caller._memo form caller._steps._spent
    maybe (made entry) told known
  where
    made :: Lambda -> IO (Expression, State)
    made entry = do
      charged caller
      stamp <- counted caller._memo
      exhausted <- starved caller._memo
      caller._saveEval (EvFiring caller._nesting func caller._judgment caller._site)
      let ctx = caller{_nesting = caller._nesting + 1}
      outcome <- try $ do
        (bound, dataized, conditions) <- foldM (down ctx) (substEmpty, state, []) entry._dataized
        (bound', morphed, normals) <- foldM (through ctx) (bound, dataized, []) entry._morphed
        let firing = Firing func (reverse conditions) (reverse normals)
        known <- remembered caller._memo firing
        maybe (worked ctx entry firing bound' morphed) (\answer -> (answer, morphed) <$ shown ctx._nesting answer) known
      case outcome of
        Right (answer, state') -> do
          retained caller._memo form stamp (Answered answer)
          pure (snd answer, state')
        Left failure -> do
          exhausted' <- starved caller._memo
          mapM_ (retained caller._memo form stamp) (kept (exhausted' /= exhausted) failure)
          mapM_ (caller._saveEval . EvStuckOn (caller._nesting + 1)) (stranded failure)
          throwIO failure
    worked :: ReduceContext -> Lambda -> Firing -> Subst -> State -> IO (Answer, State)
    worked ctx entry firing@(Firing _ operands _) bound state' = do
      rewrote <- foldM (reshaped ctx) bound entry._rewritten
      stood <- foldM (masked ctx) rewrote entry._symbolized
      forked <- foldM (paired ctx entry._lenient (listToMaybe operands)) stood entry._paired
      (answer, state'') <- answered ctx entry operands forked state'
      remember caller._memo firing answer
      pure (answer, state'')
    shown :: Int -> Answer -> IO ()
    shown depth (built, normal) = do
      caller._saveEval (EvBuilt depth built)
      caller._saveEval (EvAnswer depth normal)
    kept :: Bool -> ReduceException -> Maybe Kept
    kept _ (Looping term) = Just (Looped term)
    kept _ (LoopingAt term _ _) = Just (Looped term)
    kept starving (Stuck name) = Just (Stalled name (least starving))
    kept starving (StuckAt name _ _) = Just (Stalled name (least starving))
    kept _ _ = Nothing
    least :: Bool -> Int
    least True = caller._steps._spent
    least False = 0
    stranded :: ReduceException -> Maybe T.Text
    stranded (Stuck name) = Just name
    stranded (StuckAt name _ _) = Just name
    stranded _ = Nothing
    told :: Kept -> IO (Expression, State)
    told (Answered answer) = do
      caller._saveEval (EvFiring caller._nesting func caller._judgment caller._site)
      shown (caller._nesting + 1) answer
      pure (snd answer, state)
    told (Looped term) = do
      caller._saveEval (EvFiring caller._nesting func caller._judgment caller._site)
      mapM_ (\mode -> caller._saveEval (EvLooped (caller._nesting + 1) caller._judgment mode term caller._site Nothing)) caller._acyclic
      throwIO (Looping term)
    told (Stalled name _) = do
      caller._saveEval (EvFiring caller._nesting func caller._judgment caller._site)
      caller._saveEval (EvStall (caller._nesting + 1) name)
      throwIO (Stuck name)
    down :: ReduceContext -> (Subst, State, [Either Int Bytes]) -> (Meta, Expression) -> IO (Subst, State, [Either Int Bytes])
    down ctx (bound, state', conditions) (meta, term) = do
      placed <- operand term
      (value, state'') <- unparked (ctx._reduce univ ctx placed state'{_manufactured = Nothing, _stuck = Nothing})
      case value of
        Nothing -> throwIO (Stuck (fromMaybe func state''._stuck))
        Just bytes -> do
          let datum = maybe (Right bytes) Left state''._manufactured
          ctx._saveEval (EvData ctx._nesting meta._spelling term datum)
          bound' <- bind meta (MvBytes bytes) bound
          pure (bound', state'', datum : conditions)
    through :: ReduceContext -> (Subst, State, [Expression]) -> (Meta, Expression) -> IO (Subst, State, [Expression])
    through ctx (bound, state', normals) (meta, term) = do
      placed <- operand term
      (normal, state'') <- morphing univ ctx placed state'
      ctx._saveEval (EvTerm ctx._nesting meta._spelling term normal)
      bound' <- bind meta (MvExpression normal) bound
      pure (bound', state'', normal : normals)
    reshaped :: ReduceContext -> Subst -> (Meta, (Meta, [Y.Rule])) -> IO Subst
    reshaped ctx bound (meta, (source, rules)) = do
      term <- buildExpressionThrows (ExMeta source._name) bound
      shaped <- rewritten rules (RuleContext ctx._buildTerm Nothing ctx._engine._normal) term
      ctx._saveEval (EvSymbolize ctx._nesting meta._spelling (ExMeta source._name) shaped)
      bind meta (MvExpression shaped) bound
    masked :: ReduceContext -> Subst -> (Meta, Expression) -> IO Subst
    masked ctx bound (meta, term) = do
      reduced <- buildExpressionThrows term bound
      (stood, known, spent) <- symbolized reduced <$> readIORef ctx._minted
      writeIORef ctx._minted spent
      mapM_ (ctx._saveEval . fact) known
      ctx._saveEval (EvSymbolize ctx._nesting meta._spelling term stood)
      bind meta (MvExpression stood) bound
      where
        fact :: (Int, Bytes) -> Evaluation
        fact (fresh, bytes) = EvKnown ctx._nesting fresh bytes
    paired :: ReduceContext -> Bool -> Maybe (Either Int Bytes) -> Subst -> (Meta, (Meta, Meta)) -> IO Subst
    paired ctx lenient condition bound (meta, (left, right)) = do
      one <- branch left
      two <- branch right
      case (one, two) of
        (ExTermination, ExTermination) -> both one two
        (ExTermination, _) -> terminating "left" left two
        (_, ExTermination) -> terminating "right" right one
        _ -> both one two
      where
        both :: Expression -> Expression -> IO Subst
        both one two = do
          hops <- newIORef ctx
          outcome <- joined (unwrapped hops) lenient one two =<< readIORef ctx._minted
          case outcome of
            Nothing -> throwIO (Stuck func)
            Just (term, made, spent) -> do
              writeIORef ctx._minted spent
              mapM_ (ctx._saveEval . fact) made
              ctx._saveEval (EvJoin ctx._nesting meta._spelling (left._spelling, right._spelling) term)
              bind meta (MvExpression term) bound
        unwrapped :: IORef ReduceContext -> Expression -> IO (Maybe Expression)
        unwrapped hops term = do
          hop <- deeper =<< readIORef hops
          writeIORef hops hop
          unfired (ExDispatch term AtPhi) univ state hop
        terminating :: T.Text -> Meta -> Expression -> IO Subst
        terminating side raised term = do
          ctx._saveEval (EvTerminate ctx._nesting condition side raised._spelling)
          ctx._saveEval (EvJoin ctx._nesting meta._spelling (left._spelling, right._spelling) term)
          bind meta (MvExpression term) bound
        branch :: Meta -> IO Expression
        branch named = buildExpressionThrows (ExMeta named._name) bound
        fact :: (Int, (Int, Int)) -> Evaluation
        fact (fresh, pair) = EvJoined ctx._nesting fresh pair
    answered :: ReduceContext -> Lambda -> [Either Int Bytes] -> Subst -> State -> IO (Answer, State)
    answered ctx entry operands bound state' = do
      (fresh, spent) <- minted entry._answer <$> readIORef ctx._minted
      writeIORef ctx._minted spent
      mapM_ (\idx -> ctx._saveEval (EvMinted ctx._nesting idx operands)) [idx | (_, FnSymbol idx) <- fresh]
      symbolic <- foldM mint bound fresh
      built <- buildExpressionThrows entry._answer symbolic
      ctx._saveEval (EvBuilt ctx._nesting built)
      (normal, state'') <- settled built univ state' =<< opening Morphing ctx{_saveEval = ctx._saveEval . resited ctx._site built}
      ctx._saveEval (EvAnswer ctx._nesting normal)
      pure ((built, normal), state'')
    mint :: Subst -> (Slot, Function) -> IO Subst
    mint bound (slot, fresh) = case combine (substSlot slot (MvFunction fresh)) bound of
      Just bound' -> pure bound'
      Nothing -> throwIO (userError (printf "A fresh symbol of λ function '%s' clashes with an existing binding" (T.unpack func)))
    operand :: Expression -> IO Expression
    operand term = caller._engine._contextualize term self
    bind :: Meta -> MetaValue -> Subst -> IO Subst
    bind meta value bound = case combine (substSingle meta._name value) bound of
      Just bound' -> pure bound'
      Nothing ->
        throwIO
          (userError (printf "The meta '%s' of λ function '%s' clashes with an existing binding" (T.unpack meta._spelling) (T.unpack func)))

rewritten :: [Y.Rule] -> RuleContext -> Expression -> IO Expression
rewritten rules ctx = goExpr
  where
    goExpr :: Expression -> IO Expression
    goExpr expr = goRules rules
      where
        goRules :: [Y.Rule] -> IO Expression
        goRules [] = inside expr
        goRules (rule : rest) = do
          substs <- matchExpressionWithRule' [substEmpty] expr rule ctx
          case substs of
            subst : _ -> buildExpressionThrows rule.result subst
            [] -> goRules rest
    inside :: Expression -> IO Expression
    inside (ExFormation bds) = ExFormation <$> mapM goBinding bds
    inside (ExApplication expr arg) = ExApplication <$> goExpr expr <*> goArgument arg
    inside (ExDispatch expr attr) = (`ExDispatch` attr) <$> goExpr expr
    inside (ExPhiMeet prefix idx expr) = ExPhiMeet prefix idx <$> goExpr expr
    inside (ExPhiAgain prefix idx expr) = ExPhiAgain prefix idx <$> goExpr expr
    inside expr = pure expr
    goBinding :: Binding -> IO Binding
    goBinding (BiTau attr expr) = BiTau attr <$> goExpr expr
    goBinding bd = pure bd
    goArgument :: Argument -> IO Argument
    goArgument (ArTau attr expr) = ArTau attr <$> goExpr expr
    goArgument (ArAlpha alpha expr) = ArAlpha alpha <$> goExpr expr

fired :: Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Maybe Expression, State)
fired dispatched term univ state caller = do
  ctx <- deeper caller
  morphed <- try (reduced ctx)
  case morphed of
    Right (ExFormation bds, state')
      | demanded bds -> maybe (pure (Nothing, state')) (evaluated ctx state' (ExFormation bds)) (saturated term bds)
    Right (_, state') -> pure (Nothing, state')
    Left failure -> parked state failure
  where
    demanded :: [Binding] -> Bool
    demanded bds = not (any bound bds)
      where
        bound :: Binding -> Bool
        bound (BiTau attr _) = Just attr == dispatched
        bound _ = False
    reduced :: ReduceContext -> IO (Expression, State)
    reduced = settled term univ state
    evaluated :: ReduceContext -> State -> Expression -> (T.Text, Expression) -> IO (Maybe Expression, State)
    evaluated ctx state' form (func, self)
      | isNothing (matched ctx._symbolic func) = pure (Nothing, state')
      | otherwise = do
          made <- try (admitted form ctx >>= either (\(mode, before) -> throwIO (Refused mode before state')) (symbol func form self univ state'))
          case made of
            Right (answer, answered) -> do
              (again, reached) <- fired dispatched answer univ answered ctx `catch` refused ctx
              pure (Just (fromMaybe answer again), reached)
            Left failure -> parked state' failure
    parked :: State -> ReduceException -> IO (Maybe Expression, State)
    parked _ (StuckAt _ _ reached) | caller._partial = pure (Nothing, reached)
    parked _ (OutOfStepsAt _ _ reached) | caller._partial = pure (Nothing, reached)
    parked reached (Stuck _) | caller._partial = pure (Nothing, reached)
    parked reached (OutOfSteps _) | caller._partial = pure (Nothing, reached)
    parked _ (LoopingAt _ _ reached) = pure (Nothing, reached)
    parked reached (Looping _) = pure (Nothing, reached)
    parked _ (StuckAt func _ _) = throwIO (Stuck func)
    parked _ (OutOfStepsAt budget _ _) = throwIO (OutOfSteps budget)
    parked _ failure = throwIO failure

saturated :: Expression -> [Binding] -> Maybe (T.Text, Expression)
saturated term bds = case lambda bds of
  Just (func, ExFormation rest)
    | all filled rest && (not (any raising rest) || given rest) -> Just (func, ExFormation rest)
  _ -> Nothing
  where
    named :: [Attribute]
    positional :: Int
    valued :: Bool
    (named, positional, valued) = written term
    given :: [Binding] -> Bool
    given rest = valued && length (filter unwritten rest) <= positional
    raising :: Binding -> Bool
    raising (BiTau _ ExTermination) = True
    raising _ = False
    unwritten :: Binding -> Bool
    unwritten (BiTau attr ExTermination) = attr `notElem` named
    unwritten _ = False
    written :: Expression -> ([Attribute], Int, Bool)
    written (ExApplication expr (ArTau attr ExTermination)) =
      let (attrs, count, other) = written expr in (attr : attrs, count, other)
    written (ExApplication expr (ArAlpha _ ExTermination)) =
      let (attrs, count, other) = written expr in (attrs, count + 1, other)
    written (ExApplication expr _) =
      let (attrs, count, _) = written expr in (attrs, count, True)
    written _ = ([], 0, False)

filled :: Binding -> Bool
filled (BiVoid _) = False
filled _ = True
