{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Evaluate (evaluation) where

import AST
import Builder (buildExpressionThrows)
import Control.Exception (throwIO, try)
import Control.Monad (foldM, unless)
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Deps (Evaluation (..), Judgment (Evaluation, Morphing), State (..), dontSaveEval, dontSaveStep, resited)
import Engine (Engine (..))
import Forks (joined)
import Lambdas (Ending (..), Lambda (..), Line (..), Meta (..), emptyLambdas, matched, minted, stayed, symbolized)
import Matcher (MetaValue (..), Subst, combine, substEmpty, substSingle, substSlot)
import Morph (Answer (..), Firing (..), Kept (..), ReduceContext (..), ReduceException (..), Steps (..), boxed, charged, counted, deeper, ended, enter, expired, insideUniverse, isLambda, lambda, morph, morphing, normalized, opening, recalled, remember, remembered, retained, settled, starved, unparked)
import Printer (printFunction)
import Text.Printf (printf)

evaluation :: ReduceContext -> State -> Expression -> Expression -> IO (Expression, State)
evaluation ctx state form univ = case form of
  ExFormation bds
    | not (any isLambda bds) -> pure (form, state)
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
        firing <- Firing func (reverse conditions) . (reverse normals ++) <$> sides entry._ending
        known <- remembered caller._memo firing
        maybe (worked ctx entry firing bound' morphed) (\answer -> (answer, morphed) <$ shown ctx._nesting answer) known
      case outcome of
        Right (answer, state') -> do
          retained caller._memo form stamp (Answered answer)
          pure (ended answer, state')
        Left failure -> do
          exhausted' <- starved caller._memo
          mapM_ (retained caller._memo form stamp) (kept (exhausted' /= exhausted) (escaped failure))
          mapM_ (caller._saveEval . EvStuckOn (caller._nesting + 1)) (stranded failure)
          throwIO (escaped failure)
    worked :: ReduceContext -> Lambda -> Firing -> Subst -> State -> IO (Answer, State)
    worked ctx entry firing@(Firing _ operands _) bound state' = do
      (lined, state'') <- foldM (line ctx) (bound, state') entry._lines
      (answer, state''') <- concluded ctx entry operands lined state''
      remember caller._memo firing answer
      pure (answer, state''')
    sides :: Ending -> IO [Expression]
    sides (Answering _) = pure []
    sides (Joining _ left right) = traverse keyed [left, right]
    keyed :: Expression -> IO Expression
    keyed side@(ExMeta _) = pure side
    keyed side = operand side
    line :: ReduceContext -> (Subst, State) -> Line -> IO (Subst, State)
    line ctx held (Evaluated meta source) = evaluated ctx held meta source
    line ctx (bound, state') (Deferred meta source) = (,state') <$> deferred ctx state' meta source bound
    line ctx (bound, state') (Stayed meta source) = (,state') <$> reshaped ctx "stay" (pure . stayed) meta source bound
    line ctx (bound, state') (Symbolized meta source) = (,state') <$> reshaped ctx "symbolize" (masked ctx) meta source bound
    shown :: Int -> Answer -> IO ()
    shown depth (Answer built normal) = do
      caller._saveEval (EvBuilt depth built)
      caller._saveEval (EvAnswer depth normal)
    shown depth (Joined condition made (left, right) normal) = do
      mapM_ (\(fresh, pair) -> caller._saveEval (EvJoined depth condition fresh pair)) made
      caller._saveEval (EvTaken depth [left, right] normal)
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
    escaped :: ReduceException -> ReduceException
    escaped (Diverged name) = Stuck name
    escaped failure = failure
    told :: Kept -> IO (Expression, State)
    told (Answered answer) = do
      caller._saveEval (EvFiring caller._nesting func caller._judgment caller._site)
      shown (caller._nesting + 1) answer
      pure (ended answer, state)
    told (Looped term) = do
      caller._saveEval (EvFiring caller._nesting func caller._judgment caller._site)
      mapM_ (\mode -> caller._saveEval (EvLooped (caller._nesting + 1) caller._judgment mode term caller._site)) caller._acyclic
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
      ctx._saveEval (EvTerm ctx._nesting meta._spelling Morphing term normal)
      bound' <- bind meta (MvExpression normal) bound
      pure (bound', state'', normal : normals)
    reshaped :: ReduceContext -> T.Text -> (Expression -> IO Expression) -> Meta -> Meta -> Subst -> IO Subst
    reshaped ctx block change meta source bound = do
      kept <- buildExpressionThrows (ExMeta source._name) bound
      term <- change kept
      ctx._saveEval
        ( if term == kept
            then EvBound ctx._nesting meta._spelling (ExMeta source._name) term
            else EvReshaped ctx._nesting meta._spelling block (ExMeta source._name) term
        )
      bind meta (MvExpression term) bound
    masked :: ReduceContext -> Expression -> IO Expression
    masked ctx term = do
      (stood, known, spent) <- symbolized term <$> readIORef ctx._minted
      writeIORef ctx._minted spent
      mapM_ (\(fresh, bytes) -> ctx._saveEval (EvSymbolized ctx._nesting fresh bytes)) known
      pure stood
    evaluated :: ReduceContext -> (Subst, State) -> Meta -> Meta -> IO (Subst, State)
    evaluated ctx (bound, state') meta source = do
      term <- buildExpressionThrows (ExMeta source._name) bound
      (answer, state'') <- fire ctx (ExMeta source._name) state' term
      ctx._saveEval (EvTerm ctx._nesting meta._spelling Evaluation (ExMeta source._name) answer)
      bound' <- bind meta (MvExpression answer) bound
      pure (bound', state'')
    fire :: ReduceContext -> Expression -> State -> Expression -> IO (Expression, State)
    fire _ _ state' ExTermination = pure (ExTermination, state')
    fire ctx site state' term@(ExFormation _) = do
      entered <- deeper =<< enter term ctx{_judgment = Evaluation, _site = site}
      evaluation entered state' term univ
    fire _ _ _ _ = throwIO (Stuck func)
    figured :: ReduceContext -> State -> Expression -> Expression -> IO (Maybe Expression)
    figured ctx state' site placed = do
      outcome <- try $ do
        (normal, morphed) <- settled placed univ state' =<< opening Morphing ctx{_site = site}
        fst <$> fire ctx site morphed normal
      case outcome of
        Right answer -> pure (Just answer)
        Left (OutOfTime cap) -> throwIO (OutOfTime cap)
        Left _ -> pure Nothing
    named :: Attribute -> Binding -> Bool
    named attr (BiTau attr' _) = attr == attr'
    named _ _ = False
    called :: [Binding] -> Maybe Expression
    called known = uncurry (applied known) <$> origin known
    applied :: [Binding] -> Expression -> [Binding] -> Expression
    applied known path declared = foldl ExApplication path [ArTau attr value | BiTau attr value <- known, attr /= AtRho, BiVoid attr `elem` declared]
    origin :: [Binding] -> Maybe (Expression, [Binding])
    origin known = do
      parent <- case filter ((== Just AtRho) . attributeFromBinding) known of
        [] -> Just ExRoot
        [BiTau AtRho (ExFormation outer)] -> fst <$> origin outer
        [BiTau AtRho path] -> Just (erased path)
        _ -> Nothing
      ExFormation siblings <- located parent (fromMaybe univ caller._universe)
      chosen [(ExDispatch parent attr, declared) | BiTau attr (ExFormation declared) <- siblings, attr /= AtRho, fits declared]
      where
        fits :: [Binding] -> Bool
        fits declared = all (`elem` map attributeFromBinding declared) [attributeFromBinding bd | bd <- known, attributeFromBinding bd /= Just AtRho]
        chosen :: [(Expression, [Binding])] -> Maybe (Expression, [Binding])
        chosen [] = Nothing
        chosen candidates = case [candidate | candidate <- candidates, agreed candidate == maximum (map agreed candidates)] of
          [one] -> Just one
          _ -> Nothing
        agreed :: (Expression, [Binding]) -> (Int, Int)
        agreed (_, declared) = (length [attr | BiTau attr _ <- known, BiVoid attr `elem` declared], length (filter (`elem` declared) known))
    located :: Expression -> Expression -> Maybe Expression
    located ExRoot whole = Just whole
    located (ExDispatch path attr) whole = case located path whole of
      Just (ExFormation siblings) -> listToMaybe [body | BiTau attr' body <- siblings, attr' == attr]
      _ -> Nothing
    located _ _ = Nothing
    erased :: Expression -> Expression
    erased (ExApplication target _) = erased target
    erased (ExDispatch target attr) = ExDispatch (erased target) attr
    erased other = other
    deferred :: ReduceContext -> State -> Meta -> Meta -> Subst -> IO Subst
    deferred ctx state' meta source bound = do
      term <- buildExpressionThrows (ExMeta source._name) bound
      answer <- postponing ctx state' (ExMeta source._name) term >>= maybe (kept term) pure
      bind meta (MvExpression answer) bound
      where
        kept :: Expression -> IO Expression
        kept term = term <$ ctx._saveEval (EvBound ctx._nesting meta._spelling (ExMeta source._name) term)
    postponing :: ReduceContext -> State -> Expression -> Expression -> IO (Maybe Expression)
    postponing ctx state' site (ExFormation bds)
      | copied bds = do
          known <- mapM resolved bds
          if any bare known then Just <$> postponed known else pure Nothing
      where
        copied :: [Binding] -> Bool
        copied bds = boxed bds && not (any void bds) && or [not (formed body) | BiTau AtPhi body <- bds]
          where
            void :: Binding -> Bool
            void (BiVoid _) = True
            void _ = False
            formed :: Expression -> Bool
            formed (ExFormation _) = True
            formed _ = False
        bare :: Binding -> Bool
        bare (BiTau attr (ExFormation [BiLambda (FnSymbol _)])) = attr /= AtPhi && attr /= AtRho
        bare _ = False
        resolved :: Binding -> IO Binding
        resolved bd@(BiTau attr body)
          | attr /= AtPhi && attr /= AtRho && reducible body = do
              placed <- ctx._engine._contextualize body (ExFormation (filter (not . named attr) bds))
              outcome <- try (settled placed univ state' reading)
              case outcome of
                Right (sole@(ExFormation [BiLambda (FnSymbol _)]), _) -> pure (BiTau attr sole)
                Left (OutOfTime cap) -> expired ctx cap
                _ -> pure bd
        resolved bd = pure bd
        reducible :: Expression -> Bool
        reducible (ExDispatch _ _) = True
        reducible (ExApplication _ _) = True
        reducible _ = False
        reading :: ReduceContext
        reading = ctx{_symbolic = emptyLambdas, _memo = Nothing, _tally = Nothing, _acyclic = Nothing, _saveStep = dontSaveStep, _saveEval = dontSaveEval}
        postponed :: [Binding] -> IO Expression
        postponed known = do
          call <- argued ctx state' site known
          fresh <- coined
          ctx._saveEval (EvDeferred ctx._nesting fresh Morphing (ExFormation known) call (ExDispatch site AtPhi))
          pure (ExFormation [BiLambda (FnSymbol fresh)])
    postponing _ _ _ _ = pure Nothing
    argued :: ReduceContext -> State -> Expression -> [Binding] -> IO (Maybe Expression)
    argued ctx state' site bds = traverse go (called bds)
      where
        go :: Expression -> IO Expression
        go (ExApplication target (ArTau attr body)) = do
          target' <- go target
          ExApplication target' . ArTau attr <$> reckoned attr body
        go call = pure call
        reckoned :: Attribute -> Expression -> IO Expression
        reckoned _ body@(ExFormation [BiLambda (FnSymbol _)]) = pure body
        reckoned attr body = do
          placed <- ctx._engine._contextualize body (ExFormation (filter (not . named attr) bds))
          fromMaybe body <$> figured ctx state' (ExDispatch site attr) placed
    coined :: IO Int
    coined = atomicModifyIORef' caller._minted (\count -> (succ count, succ count))
    concluded :: ReduceContext -> Lambda -> [Either Int Bytes] -> Subst -> State -> IO (Answer, State)
    concluded ctx entry operands bound state' = case entry._ending of
      Answering template -> answered ctx template operands bound state'
      Joining on left right -> merged ctx bound (lookup on._name (zip (map (_name . fst) entry._dataized) operands)) left right state'
    answered :: ReduceContext -> Expression -> [Either Int Bytes] -> Subst -> State -> IO (Answer, State)
    answered ctx template operands bound state' = do
      (fresh, spent) <- minted template <$> readIORef ctx._minted
      writeIORef ctx._minted spent
      mapM_ (\idx -> ctx._saveEval (EvMinted ctx._nesting idx operands)) [idx | (_, FnSymbol idx) <- fresh]
      symbolic <- foldM mint bound fresh
      built <- buildExpressionThrows template symbolic
      ctx._saveEval (EvBuilt ctx._nesting built)
      (normal, state'') <- settled built univ state' =<< opening Morphing ctx{_saveEval = ctx._saveEval . resited ctx._site built}
      ctx._saveEval (EvAnswer ctx._nesting normal)
      pure (Answer built normal, state'')
    merged :: ReduceContext -> Subst -> Maybe (Either Int Bytes) -> Expression -> Expression -> State -> IO (Answer, State)
    merged _ _ Nothing _ _ _ = throwIO (userError (printf "The join of λ function '%s' is on a meta no operand of 'dataize' bound" (T.unpack func)))
    merged ctx bound (Just condition) left right state' = do
      (one, state'') <- reached ctx bound left state'
      (two, state''') <- reached ctx bound right state''
      spent <- readIORef ctx._minted
      case joined one two spent of
        Just (term, made, spent') -> do
          writeIORef ctx._minted spent'
          mapM_ (\(fresh, pair) -> ctx._saveEval (EvJoined ctx._nesting condition fresh pair)) made
          ctx._saveEval (EvTaken ctx._nesting [left, right] term)
          pure (Joined condition made (left, right) term, state''')
        Nothing -> do
          ctx._saveEval (EvDiverged ctx._nesting (left, one) (right, two))
          throwIO (Diverged func)
    reached :: ReduceContext -> Subst -> Expression -> State -> IO (Expression, State)
    reached _ bound (ExMeta name) state' = (,state') <$> buildExpressionThrows (ExMeta name) bound
    reached ctx _ side state' = do
      (universe, aiming) <- (\placed -> insideUniverse placed univ ctx) =<< operand side
      (normal, _, state'') <- morph universe state' aiming
      pure (normal, state'')
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
