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
import Data.List (nub, partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import qualified Data.Text as T
import Deps (Acyclic, BuildTermFunc, BuildTermMethod, Evaluation (..), Judgment (Evaluation, Morphing), State (..), Term (TeExpression), dontSaveEval, dontSaveStep, resited)
import Engine (Engine (..))
import Lambdas (Lambda (..), Line (..), Meta (..), Option (..), emptyLambdas, joined, matched, message, minted, stayed, symbolized, thrown)
import Matcher (MetaValue (..), Subst, combine, substEmpty, substSingle, substSlot)
import Morph (Answer, Firing (..), Kept (..), ReduceContext (..), ReduceException (..), Steps (..), admitting, boxed, charged, counted, deeper, expired, isLambda, lambda, morphing, normalized, opening, recalled, refused, remember, remembered, retained, settled, starved, unparked)
import Printer (printFunction)
import Rule (RuleContext (RuleContext), matchExpressionWithRule', xiFree)
import Text.Printf (printf)
import qualified Yaml as Y

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
        let firing = Firing func (reverse conditions) (reverse normals)
        known <- remembered caller._memo firing
        maybe (worked ctx entry firing bound' morphed) (\answer -> (answer, morphed) <$ shown ctx._nesting answer) known
      case outcome of
        Right (answer, state') -> do
          retained caller._memo form stamp (Answered answer)
          pure (snd answer, state')
        Left failure -> do
          exhausted' <- starved caller._memo
          mapM_ (retained caller._memo form stamp) (kept (exhausted' /= exhausted) (escaped failure))
          mapM_ (caller._saveEval . EvStuckOn (caller._nesting + 1)) (stranded failure)
          throwIO (escaped failure)
    worked :: ReduceContext -> Lambda -> Firing -> Subst -> State -> IO (Answer, State)
    worked ctx entry firing@(Firing _ operands _) bound state' = do
      (lined, state'') <- foldM (line ctx entry (listToMaybe operands)) (bound, state') entry._lines
      (answer, state''') <- answered ctx entry operands lined state''
      remember caller._memo firing answer
      pure (answer, state''')
    line :: ReduceContext -> Lambda -> Maybe (Either Int Bytes) -> (Subst, State) -> Line -> IO (Subst, State)
    line ctx entry _ held (Evaluated meta source) = evaluated ctx entry held meta source
    line ctx _ _ (bound, state') (Rewritten meta source rules) = (,state') <$> reshaped ctx bound meta source rules
    line ctx _ _ (bound, state') (Stayed meta source) = (,state') <$> trimmed ctx bound meta source
    line ctx _ _ (bound, state') (Symbolized meta source) = (,state') <$> masked ctx bound meta source
    line ctx entry condition held (Paired meta left right) = paired ctx entry condition held meta left right
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
    escaped :: ReduceException -> ReduceException
    escaped (Unjoined name) = Stuck name
    escaped failure = failure
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
      ctx._saveEval (EvTerm ctx._nesting meta._spelling Morphing term normal)
      bound' <- bind meta (MvExpression normal) bound
      pure (bound', state'', normal : normals)
    evaluated :: ReduceContext -> Lambda -> (Subst, State) -> Meta -> Meta -> IO (Subst, State)
    evaluated ctx entry (bound, state') meta source = do
      term <- buildExpressionThrows (ExMeta source._name) bound
      (answer, state'') <- fire refused ctx entry (ExMeta source._name) state' term
      ctx._saveEval (EvTerm ctx._nesting meta._spelling Evaluation (ExMeta source._name) answer)
      bound' <- bind meta (MvExpression answer) bound
      pure (bound', state'')
    fire :: (Expression -> ReduceContext -> (Acyclic, Expression) -> IO Expression) -> ReduceContext -> Lambda -> Expression -> State -> Expression -> IO (Expression, State)
    fire _ _ entry _ state' term
      | raising entry term = pure (term, state')
    fire cut ctx _ site state' term@(ExFormation _) = do
      let firing = ctx{_judgment = Evaluation, _site = site}
      admission <- admitting term firing
      case admission of
        Left refusal -> do
          answer <- cut term firing refusal
          pure (answer, state')
        Right admitted -> do
          entered <- deeper admitted
          evaluation entered state' term univ
    fire _ _ _ _ _ _ = throwIO (Stuck func)
    reshaped :: ReduceContext -> Subst -> Meta -> Meta -> [Y.Rule] -> IO Subst
    reshaped ctx bound meta source rules = do
      term <- buildExpressionThrows (ExMeta source._name) bound
      shaped <- rewritten rules (RuleContext reminting Nothing ctx._engine._normal) term
      ctx._saveEval (EvSymbolize ctx._nesting meta._spelling (ExMeta source._name) shaped)
      bind meta (MvExpression shaped) bound
      where
        reminting :: BuildTermFunc
        reminting "remint" = reminted
        reminting func = ctx._buildTerm func
        reminted :: BuildTermMethod
        reminted [Y.ArgExpression expr] subst = do
          term <- buildExpressionThrows expr subst
          fresh <- coined
          ctx._saveEval (EvReminted ctx._nesting fresh (nub (symbols term)) term)
          pure (TeExpression (ExFormation [BiLambda (FnSymbol fresh)]))
        reminted _ _ = throwIO (userError "Function remint() requires exactly 1 argument as expression")
    trimmed :: ReduceContext -> Subst -> Meta -> Meta -> IO Subst
    trimmed ctx bound meta source = do
      term <- stayed <$> buildExpressionThrows (ExMeta source._name) bound
      ctx._saveEval (EvSymbolize ctx._nesting meta._spelling (ExMeta source._name) term)
      bind meta (MvExpression term) bound
    masked :: ReduceContext -> Subst -> Meta -> Meta -> IO Subst
    masked ctx bound meta source = do
      reduced <- buildExpressionThrows (ExMeta source._name) bound
      (stood, known, spent) <- symbolized reduced <$> readIORef ctx._minted
      writeIORef ctx._minted spent
      mapM_ (ctx._saveEval . fact) known
      ctx._saveEval (EvSymbolize ctx._nesting meta._spelling (ExMeta source._name) stood)
      bind meta (MvExpression stood) bound
      where
        fact :: (Int, Bytes) -> Evaluation
        fact (fresh, bytes) = EvKnown ctx._nesting fresh bytes
    paired :: ReduceContext -> Lambda -> Maybe (Either Int Bytes) -> (Subst, State) -> Meta -> Meta -> Meta -> IO (Subst, State)
    paired ctx entry condition (bound, state') meta left right = do
      one <- branch left
      two <- branch right
      case (raising entry one, raising entry two) of
        (True, False) -> terminating "left" left one two
        (False, True) -> terminating "right" right two one
        _ -> both one two
      where
        both :: Expression -> Expression -> IO (Subst, State)
        both one two = do
          outcome <- joined entry._lenient one two <$> readIORef ctx._minted
          case outcome of
            Just made -> written state' made
            Nothing
              | isJust entry._lenient && (hanging one || hanging two) -> do
                  (one', reducing) <- reduced left one state'
                  (two', state'') <- reduced right two reducing
                  outcome' <- joined entry._lenient one' two' <$> readIORef ctx._minted
                  maybe (unjoined one' two') (written state'') outcome'
              | otherwise -> unjoined one two
        unjoined :: Expression -> Expression -> IO a
        unjoined one two = do
          ctx._saveEval (EvUnjoined ctx._nesting func one two)
          throwIO (Unjoined func)
        written :: State -> (Expression, [(Int, Expression, [(Int, [Attribute])])], [(Int, (Int, Int))], Int) -> IO (Subst, State)
        written state'' (term, casts, made, spent) = do
          writeIORef ctx._minted spent
          mapM_ (\(bare, cast, paths) -> ctx._saveEval (EvUpcast ctx._nesting bare cast paths)) casts
          mapM_ (ctx._saveEval . fact) made
          ctx._saveEval (EvJoin ctx._nesting meta._spelling (left._spelling, right._spelling) term)
          bound' <- bind meta (MvExpression term) bound
          pure (bound', state'')
        reduced :: Meta -> Expression -> State -> IO (Expression, State)
        reduced side term@(ExFormation bds) state''
          | hanging term && any (elem Defer) entry._lenient && copied bds = deferred side bds state'' >>= maybe (entered (severed side bds state'') side term state'') (\answer -> pure (answer, state''))
        reduced side term state''
          | hanging term = entered refused side term state''
          | otherwise = pure (term, state'')
        entered :: (Expression -> ReduceContext -> (Acyclic, Expression) -> IO Expression) -> Meta -> Expression -> State -> IO (Expression, State)
        entered cut side term state'' = do
          (normal, morphed) <- morphing univ ctx (ExDispatch term AtPhi) state''
          ctx._saveEval (EvTerm ctx._nesting side._spelling Morphing (ExDispatch (ExMeta side._name) AtPhi) normal)
          (answer, state''') <- fire cut ctx entry (ExMeta side._name) morphed normal
          ctx._saveEval (EvTerm ctx._nesting side._spelling Evaluation (ExMeta side._name) answer)
          pure (answer, state''')
        hanging :: Expression -> Bool
        hanging (ExFormation bds) = not (any isLambda bds) && or [not (xiFree body) | BiTau AtPhi body <- bds]
        hanging _ = False
        copied :: [Binding] -> Bool
        copied bds = boxed bds && not (any void bds) && or [not (formed body) | BiTau AtPhi body <- bds]
          where
            void :: Binding -> Bool
            void (BiVoid _) = True
            void _ = False
            formed :: Expression -> Bool
            formed (ExFormation _) = True
            formed _ = False
        deferred :: Meta -> [Binding] -> State -> IO (Maybe Expression)
        deferred side bds state'' = do
          known <- mapM resolved bds
          if any bare known then Just <$> postponed known else pure Nothing
          where
            bare :: Binding -> Bool
            bare (BiTau attr (ExFormation [BiLambda (FnSymbol _)])) = attr /= AtPhi && attr /= AtRho
            bare _ = False
            resolved :: Binding -> IO Binding
            resolved bd@(BiTau attr body)
              | attr /= AtPhi && attr /= AtRho && reducible body = do
                  placed <- ctx._engine._contextualize body (ExFormation (filter (not . named attr) bds))
                  outcome <- try (settled placed univ state'' reading)
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
              call <- traverse (argued side known state'') (called known)
              fresh <- coined
              ctx._saveEval (EvDeferred ctx._nesting fresh Morphing (ExFormation known) call (ExDispatch (ExMeta side._name) AtPhi))
              pure (ExFormation [BiLambda (FnSymbol fresh)])
        severed :: Meta -> [Binding] -> State -> Expression -> ReduceContext -> (Acyclic, Expression) -> IO Expression
        severed side bds state'' _ firing (mode, before) = do
          call <- traverse (argued side bds state'') (called bds)
          fresh <- coined
          firing._saveEval (EvLooped firing._nesting firing._judgment mode before firing._site (Just (fresh, call)))
          pure (ExFormation [BiLambda (FnSymbol fresh)])
        argued :: Meta -> [Binding] -> State -> Expression -> IO Expression
        argued side bds state'' (ExApplication target (ArTau attr body)) = do
          target' <- argued side bds state'' target
          ExApplication target' . ArTau attr <$> reckoned side bds state'' attr body
        argued _ _ _ call = pure call
        reckoned :: Meta -> [Binding] -> State -> Attribute -> Expression -> IO Expression
        reckoned _ _ _ _ body@(ExFormation [BiLambda (FnSymbol _)]) = pure body
        reckoned side bds state'' attr body = do
          let site = ExDispatch (ExMeta side._name) attr
          placed <- ctx._engine._contextualize body (ExFormation (filter (not . named attr) bds))
          outcome <- try $ do
            (normal, morphed) <- settled placed univ state'' =<< opening Morphing ctx{_site = site}
            fst <$> fire refused ctx entry site morphed normal
          case outcome of
            Right answer -> pure answer
            Left (OutOfTime cap) -> throwIO (OutOfTime cap)
            Left _ -> pure body
        named :: Attribute -> Binding -> Bool
        named attr (BiTau attr' _) = attr == attr'
        named _ _ = False
        called :: [Binding] -> Maybe Expression
        called known = do
          (path, declared) <- origin known
          pure (foldl ExApplication path [ArTau attr value | BiTau attr value <- known, attr /= AtRho, BiVoid attr `elem` declared])
        origin :: [Binding] -> Maybe (Expression, [Binding])
        origin known = do
          parent <- case filter ((== Just AtRho) . attributeFromBinding) known of
            [] -> Just ExRoot
            [BiTau AtRho (ExFormation outer)] -> fst <$> origin outer
            [BiTau AtRho path] -> Just (erased path)
            _ -> Nothing
          ExFormation siblings <- located parent (fromMaybe univ ctx._universe)
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
        terminating :: T.Text -> Meta -> Expression -> Expression -> IO (Subst, State)
        terminating side raised fault term = do
          ctx._saveEval (EvTerminate ctx._nesting condition side raised._spelling (message fault))
          ctx._saveEval (EvJoin ctx._nesting meta._spelling (left._spelling, right._spelling) term)
          bound' <- bind meta (MvExpression term) bound
          pure (bound', state')
        branch :: Meta -> IO Expression
        branch named = buildExpressionThrows (ExMeta named._name) bound
        fact :: (Int, (Int, Int)) -> Evaluation
        fact (fresh, pair) = EvJoined ctx._nesting fresh pair
    coined :: IO Int
    coined = atomicModifyIORef' caller._minted (\count -> (succ count, succ count))
    raising :: Lambda -> Expression -> Bool
    raising _ ExTermination = True
    raising entry term = thrown entry._terminators term
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
