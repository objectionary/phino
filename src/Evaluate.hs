{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The Evaluation function 𝔼 and everything a λ function of the '--symbolic'
-- file needs to fire: finding the λ of a formation, bringing the operands of
-- its entry down, standing the data of a term it reduced into unknowns,
-- minting the symbols its answer carries and writing the firing into the
-- protocol. 𝕄 and 𝔻 live in 'Morph' and 'Dataize', and what
-- all three share — the context, the budget, the signals — lives in 'Morph',
-- which this module imports. The edges pointing back the other way, 𝕄 asking
-- 𝔼 to fire, are injected as '_evaluate' and '_fire' rather than imported, the
-- way 'Dataize' hands 'Morph' its '_reduce' (see 'EvaluationFunc').
module Evaluate (evaluation, fired, lambda) where

import AST
import Builder (buildExpressionThrows, contextualize)
import Control.Exception (throwIO, try)
import Control.Monad (foldM)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Deps (BuildTermMethodS, Evaluation (..), State (..), Term (..))
import Lambdas (Lambda (..), Meta (..), joined, matched, minted, symbolized)
import Matcher (MetaValue (..), Subst, combine, substEmpty, substSingle, substSlot)
import Morph (ReduceContext (..), ReduceException (..), deeper, morph', morphing, normalized, unparked)
import Text.Printf (printf)
import Yaml (ExtraArgument (..))

-- The Evaluation function 𝔼(b, e, s): it fires the λ function of a formation
-- 'b' against the global universe 'e', under the incoming state 𝑠, normalizes
-- its raw result 𝒩(e₁) = n, and returns that normal form together with the new
-- state. Normalizing here makes 𝔼's codomain 𝓝 (as its type demands), so
-- callers ('fire', 'ml') need no follow-up 'normalize' premise. The universe is
-- passed explicitly as the second argument (rather than threaded behind the
-- scenes), matching how the morphing 𝕄 and dataization 𝔻 functions carry it.
-- Every firing writes itself into the protocol the '--protocol' option keeps
-- (see 'symbol'). Firings are written in the order they start, so the λ
-- function of a head reduced by 'ml' stands above the one dispatched on its
-- result; that order carries nothing, since what depends on what is read off
-- the symbols.
--
-- A formation carrying no λ binding has nothing to fire, and that is a question
-- the calculus answers rather than a malformed one: 𝔼 hands back ⊥, the way 𝕄
-- does for a term nobody reduces further. Only a λ 𝔼 cannot make sense of fails
-- — several of them, or one standing for a meta, a slot or a symbol rather than
-- a plain name — since a rule naming such a binding meant something phino
-- cannot work out (see 'lambda').
evaluation :: ReduceContext -> State -> BuildTermMethodS
evaluation ctx state [ArgExpression expr, ArgExpression universe] subst = do
  form <- buildExpressionThrows expr subst
  univ <- buildExpressionThrows universe subst
  case form of
    ExFormation bds
      | not (any isLambda bds) -> pure (TeExpression ExTermination, state)
      | otherwise -> case lambda bds of
          Just (func, args) -> do
            (raw, state') <- symbol func args univ state ctx
            (normal, _) <- normalized raw ((univ, Nothing) :| []) ctx
            pure (TeExpression normal, state')
          Nothing -> throwIO (userError "Function evaluate() expects a formation with a single λ binding naming a function")
    _ -> throwIO (userError "Function evaluate() expects a formation")
evaluation _ _ _ _ = throwIO (userError "Function evaluate() requires exactly 2 expression arguments")

-- phino implements no λ function of its own. Which ones exist is a property of
-- the object model being reduced, not of the calculus, so they come from the
-- '--symbolic' file, where each is an entry phino answers the firing with
-- itself (see 'Lambdas'). The entry is looked up by the λ name and there is at
-- most one, since the keys are unique; a name no entry answers has no λ
-- function to fire at all, and 𝔼 gets stuck on it — the one behaviour left
-- here. The formation 'self' is the one 𝔼 fired against, its λ binding already
-- removed, so the entry may name the attributes of it; the universe 'univ' is
-- what every operand of it is reduced inside. What comes back is the term the
-- entry answers with, morphed (see 'answered').
--
-- The firing writes itself into the protocol as it goes: the entry that
-- answered first, then each operand as it is reduced, then the answer. Whatever
-- fires inside an operand writes itself between those lines, one level deeper,
-- which is what makes the protocol a tree of firings rather than a list of
-- them. A name no entry answers writes itself too, before 𝔼 gets stuck on it,
-- so the protocol says what was asked for whether or not '_partial' goes on to
-- park the run.
symbol :: T.Text -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
symbol func self univ state caller = case matched caller._symbolic func of
  Nothing -> do
    caller._saveEval (EvStuck caller._nesting func)
    throwIO (Stuck func)
  Just entry -> do
    caller._saveEval (EvFiring caller._nesting func)
    let ctx = caller{_nesting = caller._nesting + 1}
    (bound, dataized) <- foldM (down ctx) (substEmpty, state) entry._dataized
    (bound', morphed) <- foldM (through ctx) (bound, dataized) entry._morphed
    (bound'', stood) <- foldM (masked ctx) (bound', morphed) entry._symbolized
    (bound''', forked) <- foldM (paired ctx) (bound'', stood) entry._paired
    answered ctx entry bound''' forked
  where
    -- Bring one 'dataize' operand down through 𝔻 and bind the bytes meta that
    -- names it. An operand 𝔻 could not bring down to data — a site '_partial'
    -- parked — leaves the firing with nothing to bind, so it gets stuck like a
    -- λ function no entry answers at all. Every symbol dataizes to the very
    -- same datum, so the protocol is told which unknown that datum was
    -- manufactured for rather than the datum itself (see 'State').
    --
    -- The reduction runs on a universe of its own, so a signal escaping it
    -- carries that universe's derivation and not the spine's; 'unparked' drops
    -- it and lets the spine frame around this firing attach its own, which is
    -- what keeps '--sequence' free of the synthetic attribute the operand was
    -- reduced under.
    down :: ReduceContext -> (Subst, State) -> (Meta, Expression) -> IO (Subst, State)
    down ctx (bound, state') (meta, term) = do
      (value, state'') <- unparked (ctx._reduce univ ctx (operand term) state'{_manufactured = Nothing})
      case value of
        Nothing -> throwIO (Stuck func)
        Just bytes -> do
          ctx._saveEval (EvData ctx._nesting meta._spelling term (maybe (Right bytes) Left state''._manufactured))
          bound' <- bind meta (MvBytes bytes) bound
          pure (bound', state'')
    -- Reduce one 'morph' operand through 𝕄 and bind the expression meta that
    -- names it. Unlike a dataized one it may stay an unknown: a term carrying a
    -- symbol is a perfectly good normal form, and standing it into the answer
    -- is how a firing hands its own unknowns on.
    through :: ReduceContext -> (Subst, State) -> (Meta, Expression) -> IO (Subst, State)
    through ctx (bound, state') (meta, term) = do
      (normal, state'') <- morphing univ ctx (operand term) state'
      ctx._saveEval (EvTerm ctx._nesting meta._spelling term normal)
      bound' <- bind meta (MvExpression normal) bound
      pure (bound', state'')
    -- Stand the data of a term another line of the entry has bound into
    -- unknowns and bind the expression meta naming what it becomes. Nothing is
    -- reduced here: what changes is that every datum of the term becomes a
    -- symbol nobody worked out, so a normal form reached from a literal
    -- compares with one reached from an unknown, which is what a later join of
    -- two branches of a fork needs. What is known about each fresh symbol goes
    -- into the protocol ahead of the line binding the term, since the term is
    -- written with the symbols and the facts are what tells a constant among
    -- them from an unknown.
    masked :: ReduceContext -> (Subst, State) -> (Meta, Expression) -> IO (Subst, State)
    masked ctx (bound, state') (meta, term) = do
      reduced <- buildExpressionThrows term bound
      let (stood, known, spent) = symbolized reduced state'._minted
      mapM_ (ctx._saveEval . fact) known
      ctx._saveEval (EvTerm ctx._nesting meta._spelling term stood)
      bound' <- bind meta (MvExpression stood) bound
      pure (bound', state'{_minted = spent})
      where
        fact :: (Int, Bytes) -> Evaluation
        fact (fresh, bytes) = EvKnown ctx._nesting fresh bytes
    -- Join two terms other lines of the entry have bound into one and bind the
    -- expression meta naming it. Nothing is reduced here either: the two are
    -- required to match verbatim and every pair of symbols they differ by
    -- becomes one fresh symbol (see 'joined'), which is the one term standing
    -- for either of them and so the one thing a fork can answer with. Two
    -- terms differing anywhere else are no join at all and the firing gets
    -- stuck the way a λ function no entry answers does, so '_partial' parks
    -- the site rather than failing the whole run (#1246).
    --
    -- What is known about each fresh symbol goes into the protocol ahead of
    -- the line binding the term, the way a 'symbolize' line writes what it
    -- knows, since a reader ties the join to the two values it was made from
    -- by that fact alone and never by diffing the terms.
    paired :: ReduceContext -> (Subst, State) -> (Meta, (Meta, Meta)) -> IO (Subst, State)
    paired ctx (bound, state') (meta, (left, right)) = do
      one <- branch left
      two <- branch right
      case joined one two state'._minted of
        Nothing -> throwIO (Stuck func)
        Just (term, made, spent) -> do
          mapM_ (ctx._saveEval . fact) made
          ctx._saveEval (EvJoin ctx._nesting meta._spelling (left._spelling, right._spelling) term)
          bound' <- bind meta (MvExpression term) bound
          pure (bound', state'{_minted = spent})
      where
        -- The term one side of the join is bound to, which is what a meta of
        -- the entry reads out of the substitution the firing has made (see
        -- 'earlier' in 'Lambdas': a 'join' line names metas bound above it and
        -- nothing else, so there is always one to read).
        branch :: Meta -> IO Expression
        branch named = buildExpressionThrows (ExMeta named._name) bound
        fact :: (Int, (Int, Int)) -> Evaluation
        fact (fresh, pair) = EvJoined ctx._nesting fresh pair
    -- Mint the fresh symbols the answer asks for, build it and reduce it
    -- through 𝕄. A bare 𝜎 stands for an unknown nobody has named yet, so each
    -- one is bound to the next symbol the run has not minted, and the state
    -- counts them, which is what keeps two firings from spelling two unknowns
    -- alike. Each one goes into the protocol as it is handed out, ahead of the
    -- answer carrying it, so a reader ties an unknown back to the firing that
    -- made it without reading the term it stands in (#1280).
    --
    -- The answer is morphed rather than handed back as the entry wrote it,
    -- because a firing is one of the things a term can come from and every
    -- other one answers a normal form: 'Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )' written in
    -- the program morphs to the formation of the object, so the same term
    -- answered by an entry has to morph to it too. Two terms of one forma that
    -- do not look alike cannot be compared leaf by leaf, and comparing them is
    -- what a fork of two branches is (#1268). The residual and the answer lines
    -- of the protocol grow by the size of that formation, which is the price of
    -- saying the same thing one way.
    --
    -- Both terms go to the protocol, the built one before 'settled' is asked
    -- about it and the normal one after, so the morphing is a step of the
    -- protocol and no silent change of shape: whatever 𝕄 fires on the way opens
    -- its block between the two, where every other firing of an operand opens
    -- its own, and the formation standing on the second line is read as what
    -- the three tokens on the first came to (#1298).
    answered :: ReduceContext -> Lambda -> Subst -> State -> IO (Expression, State)
    answered ctx entry bound state' = do
      let (fresh, spent) = minted entry._answer state'._minted
      mapM_ (ctx._saveEval . EvMinted ctx._nesting) [idx | (_, FnSymbol idx) <- fresh]
      symbolic <- foldM mint bound fresh
      built <- buildExpressionThrows entry._answer symbolic
      ctx._saveEval (EvBuilt ctx._nesting built)
      (normal, state'') <- settled built univ state'{_minted = spent} ctx
      ctx._saveEval (EvAnswer ctx._nesting normal)
      pure (normal, state'')
    mint :: Subst -> (Slot, Function) -> IO Subst
    mint bound (slot, fresh) = case combine (substSlot slot (MvFunction fresh)) bound of
      Just bound' -> pure bound'
      Nothing -> throwIO (userError (printf "A fresh symbol of λ function '%s' clashes with an existing binding" (T.unpack func)))
    -- The operand an entry wrote, in the scope it is reduced in: ξ stands for
    -- the formation being fired, so '$.x' is the x of it, and the calculus does
    -- the reaching.
    operand :: Expression -> Expression
    operand = (`contextualize` self)
    bind :: Meta -> MetaValue -> Subst -> IO Subst
    bind meta value bound = case combine (substSingle meta._name value) bound of
      Just bound' -> pure bound'
      Nothing ->
        throwIO
          (userError (printf "The meta '%s' of λ function '%s' clashes with an existing binding" (T.unpack meta._spelling) (T.unpack func)))

-- Ask 𝕄 about a term and fire the λ of the formation it reaches, as long as an
-- entry of the '--symbolic' file answers it, asking 𝕄 about every answer again:
-- what comes back is the answer of the last firing, or nothing at all where
-- nothing fired. This is the firing 'ml' makes without the dispatch that makes
-- 'ml' make it — the one 𝕄 leaves to 𝔻. A λ no entry answers is left alone
-- rather than fired and got stuck on, so what phino cannot compute stays as it
-- was written with or without '_partial'; a λ function that cannot fire deeper
-- on the spine still fails the run, exactly as it does under 𝕄 alone, and
-- '_partial' parks it. A formation still waiting for its arguments is left
-- alone too (see 'saturated'). A term standing as the
-- target of a dispatch is where 'ml' has its say: the λ is fired only where the
-- dispatched attribute is none of the formation's own (see 'demanded').
fired :: Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Maybe Expression, State)
fired dispatched term univ state caller = do
  ctx <- deeper caller
  morphed <- try (reduced ctx)
  case morphed of
    Right (ExFormation bds, state')
      | demanded bds -> maybe (pure (Nothing, state')) (evaluated ctx state') (saturated bds)
    Right (_, state') -> pure (Nothing, state')
    Left failure -> parked failure
  where
    -- Whether the dispatch the term stands under demands the λ of the formation
    -- 𝕄 reached. 'ml' fires that λ only where the dispatched attribute is none
    -- of the formation's own, since 'dot' resolves the dispatch before 'ml' is
    -- ever reached, and a walk firing it first answers a formation the dispatch
    -- no longer fits (#1187). A term standing anywhere else is demanded by
    -- nothing and the walk fires what 'mf' left bare, as it always has.
    demanded :: [Binding] -> Bool
    demanded bds = not (any bound bds)
      where
        bound :: Binding -> Bool
        bound (BiTau attr _) = Just attr == dispatched
        bound _ = False
    -- The term the walk was handed, as 𝕄 leaves it. The chains it drops are
    -- the walk's and not the spine's, which reports one step of its own (see
    -- 'morph'), so a stuck λ function leaves without a derivation.
    reduced :: ReduceContext -> IO (Expression, State)
    reduced = settled term univ state
    -- Fire the λ of the formation 𝕄 reached and go on from its answer, keeping
    -- the answer of the last firing. A λ no entry of the '--symbolic' file
    -- answers is not fired at all, which is what keeps the walk as total as 𝕄
    -- itself. The firing reports itself to '_saveEval', so the protocol and the
    -- program agree on what was answered.
    evaluated :: ReduceContext -> State -> (T.Text, Expression) -> IO (Maybe Expression, State)
    evaluated ctx state' (func, self)
      | isNothing (matched ctx._symbolic func) = pure (Nothing, state')
      | otherwise = do
          (answer, answered) <- symbol func self univ state' ctx
          (again, reached) <- fired dispatched answer univ answered ctx
          pure (Just (fromMaybe answer again), reached)
    -- A site the walk cannot reduce — a λ function whose operands never came
    -- down to data, or one the step budget ran out on — is left as it was
    -- written and the walk goes on, which is what a partial morphing is: phino
    -- stops where it cannot decide rather than failing the whole run. The state
    -- the parked site had reached travels back, so the symbols it minted before
    -- it stopped are never minted again; the chain it parked on is dropped,
    -- since that chain is the walk's and not the spine's.
    parked :: ReduceException -> IO (Maybe Expression, State)
    parked (StuckAt _ _ reached) | caller._partial = pure (Nothing, reached)
    parked (OutOfStepsAt _ _ reached) | caller._partial = pure (Nothing, reached)
    parked (Stuck _) | caller._partial = pure (Nothing, state)
    parked (OutOfSteps _) | caller._partial = pure (Nothing, state)
    parked (LoopingAt _ _ reached) = pure (Nothing, reached)
    parked (Looping _) = pure (Nothing, state)
    parked (StuckAt func _ _) = throwIO (Stuck func)
    parked (OutOfStepsAt limit _ _) = throwIO (OutOfSteps limit)
    parked failure = throwIO failure

-- A term of the '--symbolic' file as 𝕄 leaves it: an entry's answer on its way
-- out of a firing, and the term the deep walk was handed on its way in. 𝕄 takes
-- normal forms only and a term written in an entry, or taken from the program
-- as it was written, is not necessarily one, so it is normalized against the
-- universe first, exactly as '--inside' normalizes what it is handed. It is
-- reduced against the universe itself rather than inside it: a term bound under
-- an attribute of the universe reaches a ρ naming that attribute too, and
-- neither of these two is a part of the program the way an operand of a firing
-- is. Both chains are dropped, since what happens here is the protocol's
-- business and not the spine's.
settled :: Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
settled term univ state ctx = do
  (normal, _) <- normalized term ((univ, Nothing) :| []) ctx
  ((morphed, _), state') <- morph' (normal, (univ, Nothing) :| []) univ state ctx
  pure (morphed, state')

-- Split the λ binding off a formation for the LAMBDA morphing rule: the name of
-- the λ function to fire and the formation it fires against, the λ binding
-- removed. A formation with no λ binding, or with more than one, has nothing to
-- fire; neither has one carrying a symbol, which is a λ name nothing answers.
-- The three are one answer here but not to 𝔼, which tells the first of them
-- from the other two (see 'evaluation').
lambda :: [Binding] -> Maybe (T.Text, Expression)
lambda bds = case partition isLambda bds of
  ([BiLambda (Function func)], rest) -> Just (func, ExFormation rest)
  _ -> Nothing

-- Whether a binding names a λ function, whatever that name turns out to be.
-- 𝔼 asks this before 'lambda' does its splitting, since a formation carrying no
-- λ at all is answered with ⊥ rather than refused (see 'evaluation').
isLambda :: Binding -> Bool
isLambda (BiLambda _) = True
isLambda _ = False

-- The same as 'lambda', but only for a formation that is saturated: one with
-- every binding of it filled (see 'filled'). A void is an argument the program
-- has not given yet, so such a formation is a method waiting to be applied
-- rather than an application waiting to be computed, and firing it would hand
-- the λ function a ∅ where it expects a value. 𝔻 needs no such guard, since it
-- fires only what dataization demands and nothing demands a method; the deep
-- walk meets every one a program declares — the method table of the object
-- model above all — so it asks first (see 'deepened').
saturated :: [Binding] -> Maybe (T.Text, Expression)
saturated bds = case lambda bds of
  Just (func, ExFormation rest) | all filled rest -> Just (func, ExFormation rest)
  _ -> Nothing

-- Whether a binding hands the formation something to work with. A void does
-- not: it names an argument the program has still to supply. Neither does ⊥:
-- the deep walk reduces a body in the scope of the formation around it, and a
-- formation standing unapplied still holds ρ ↦ ∅, so a ξ.ρ in that body comes
-- back as ⊥ rather than as the object the next dispatch supplies (#1196).
filled :: Binding -> Bool
filled (BiVoid _) = False
filled (BiTau _ ExTermination) = False
filled _ = True
