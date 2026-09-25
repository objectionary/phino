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
module Evaluate (evaluation, fired) where

import AST
import Builder (buildExpressionThrows, contextualize, pathOf)
import Control.Exception (throwIO, try)
import Control.Monad (foldM, unless)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import qualified Data.Text as T
import Deps (BuildTermMethodS, Evaluation (..), State (..), Term (..))
import Lambdas (Lambda (..), Meta (..), joined, matched, minted, symbolized)
import Matcher (MetaValue (..), Subst, combine, substEmpty, substSingle, substSlot)
import Morph (ReduceContext (..), ReduceException (..), deeper, enter, isLambda, lambda, morph', morphing, normalized, unparked)
import Printer (printFunction)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Text.Printf (printf)
import Yaml (ExtraArgument (..))
import qualified Yaml as Y

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
-- does for a term nobody reduces further. Neither is a λ naming a symbol: a
-- symbol is a value nobody worked out, so no entry of the '--symbolic' file
-- answers it and 𝔼 gets stuck on it exactly as it does on a λ name nothing
-- answers — the site is written to the protocol and '_partial' parks it, rather
-- than the run ending on a term the program was entitled to hold (#1287). Only
-- a λ 𝔼 cannot make sense of fails — several of them, or one standing for a
-- meta or a slot — since a rule naming such a binding meant something phino
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
            (raw, state') <- symbol func form args univ state ctx
            (normal, _) <- normalized raw ((univ, Nothing) :| []) ctx
            pure (TeExpression normal, state')
          Nothing -> case unknown bds of
            Just idx -> stuck idx form
            Nothing -> throwIO (userError "Function evaluate() expects a formation with a single λ binding naming a function")
    _ -> throwIO (userError "Function evaluate() expects a formation")
  where
    -- The symbol the one λ binding of a formation names, where that is what it
    -- names. It is the one λ 'lambda' refuses that 𝔼 still has an answer for,
    -- so it is told apart here and nowhere else: a formation carrying several
    -- λ bindings, or one standing for a meta or a slot, is still a term phino
    -- cannot work out.
    unknown :: [Binding] -> Maybe Int
    unknown bindings = case partition isLambda bindings of
      ([BiLambda (FnSymbol idx)], _) -> Just idx
      _ -> Nothing
    -- Get stuck on a symbol the way 'symbol' gets stuck on a λ name no entry
    -- answers, and for the same reason: nothing answers either, so there is no
    -- firing to make. The site is written to the protocol under the name the
    -- symbol is spelled with everywhere else, so a reader joining the record to
    -- the term it came from compares two strings that look alike, and it is
    -- written once however many times the walk comes back to it (see '_parked').
    stuck :: Int -> Expression -> IO (Term, State)
    stuck idx form = do
      unless (name `elem` ctx._parked) (ctx._saveEval (EvStuck ctx._nesting name ctx._judgment form))
      throwIO (Stuck name)
      where
        name :: T.Text
        name = T.pack (printFunction (FnSymbol idx))
evaluation _ _ _ _ = throwIO (userError "Function evaluate() requires exactly 2 expression arguments")

-- phino implements no λ function of its own. Which ones exist is a property of
-- the object model being reduced, not of the calculus, so they come from the
-- '--symbolic' file, where each is an entry phino answers the firing with
-- itself (see 'Lambdas'). The entry is looked up by the λ name and there is at
-- most one, since the keys are unique; a name no entry answers has no λ
-- function to fire at all, and 𝔼 gets stuck on it — the one behaviour left
-- here. The formation 'self' is the one 𝔼 fired against, its λ binding already
-- removed, so the entry may name the attributes of it; 'form' is that same
-- formation as 𝔼 was handed it, λ binding and all, which is what the protocol
-- says a firing nothing answered was about, since a formation with its λ split
-- off is no longer the term anybody asked about; the universe 'univ' is what
-- every operand of it is reduced inside. What comes back is the term the entry
-- answers with, morphed (see 'answered').
--
-- The firing writes itself into the protocol as it goes: the entry that
-- answered first, then each operand as it is reduced, then the answer. Whatever
-- fires inside an operand writes itself between those lines, one level deeper,
-- which is what makes the protocol a tree of firings rather than a list of
-- them. A name no entry answers writes itself too, before 𝔼 gets stuck on it,
-- so the protocol says what was asked for whether or not '_partial' goes on to
-- park the run — once, and not once per attempt: a site '_partial' has parked
-- is still standing in the residue the '_deep' walk goes over, so 𝔼 is fired on
-- it again and again answers nothing, and a reader counting the '?(…)' lines
-- counts the sites 𝔼 got stuck on rather than the passes the walk made over
-- them (see '_parked', #1300).
symbol :: T.Text -> Expression -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
symbol func form self univ state caller = case matched caller._symbolic func of
  Nothing -> do
    unless (func `elem` caller._parked) (caller._saveEval (EvStuck caller._nesting func caller._judgment form))
    throwIO (Stuck func)
  Just entry -> do
    caller._saveEval (EvFiring caller._nesting func caller._judgment caller._site)
    let ctx = caller{_nesting = caller._nesting + 1}
    (bound, dataized, conditions) <- foldM (down ctx) (substEmpty, state, []) entry._dataized
    (bound', morphed) <- foldM (through ctx) (bound, dataized) entry._morphed
    rewrote <- foldM (reshaped ctx) bound' entry._rewritten
    (bound'', stood) <- foldM (masked ctx) (rewrote, morphed) entry._symbolized
    (bound''', forked) <- foldM (paired ctx (listToMaybe (reverse conditions))) (bound'', stood) entry._paired
    answered ctx entry (reverse conditions) bound''' forked
  where
    -- Bring one 'dataize' operand down through 𝔻 and bind the bytes meta that
    -- names it. An operand 𝔻 could not bring down to data — a site '_partial'
    -- parked — leaves the firing with nothing to bind, so it gets stuck like a
    -- λ function no entry answers at all, and gets stuck on the very name that
    -- parked the operand rather than on the λ function of this firing: an entry
    -- answers this one, so blaming it would name a λ function the '--symbolic'
    -- file carries where the one nothing answers stands one reduction deeper
    -- (#1288). The name travels back in the state the parked run hands over,
    -- since the signal it was made of stayed inside that run (see '_stuck').
    -- Every symbol dataizes to the very same datum, so the protocol is told
    -- which unknown that datum was manufactured for rather than the datum
    -- itself (see 'State').
    --
    -- The reduction runs on a universe of its own, so a signal escaping it
    -- carries that universe's derivation and not the spine's; 'unparked' drops
    -- it and lets the spine frame around this firing attach its own, which is
    -- what keeps '--sequence' free of the synthetic attribute the operand was
    -- reduced under.
    --
    -- What the operand came down to goes on beside the substitution, the last
    -- operand first, since the first of them is the condition a fork branches
    -- on and a 'join' line one side of which reaches ⊥ names it (see 'paired').
    down :: ReduceContext -> (Subst, State, [Either Int Bytes]) -> (Meta, Expression) -> IO (Subst, State, [Either Int Bytes])
    down ctx (bound, state', conditions) (meta, term) = do
      (value, state'') <- unparked (ctx._reduce univ ctx (operand term) state'{_manufactured = Nothing, _stuck = Nothing})
      case value of
        Nothing -> throwIO (Stuck (fromMaybe func state''._stuck))
        Just bytes -> do
          let datum = maybe (Right bytes) Left state''._manufactured
          ctx._saveEval (EvData ctx._nesting meta._spelling term datum)
          bound' <- bind meta (MvBytes bytes) bound
          pure (bound', state'', datum : conditions)
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
    -- Rewrite a term another line of the entry has bound with the rules of the
    -- line and bind the expression meta naming what it becomes (see
    -- 'rewritten'). Nothing is reduced and nothing is minted: a rewrite is a
    -- substitution the entry vouches for, exactly as an answer is, so the term
    -- it makes is written to the protocol and bound as it is, which is what
    -- lets a program bring the branches of a fork to one shape before they
    -- are compared (#1409). The line is commented with the meta it rewrote,
    -- the way a 'symbolize' line is, since no judgment of the calculus made it.
    reshaped :: ReduceContext -> Subst -> (Meta, (Meta, [Y.Rule])) -> IO Subst
    reshaped ctx bound (meta, (source, rules)) = do
      term <- buildExpressionThrows (ExMeta source._name) bound
      shaped <- rewritten rules (RuleContext ctx._buildTerm) term
      ctx._saveEval (EvSymbolize ctx._nesting meta._spelling (ExMeta source._name) shaped)
      bind meta (MvExpression shaped) bound
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
      ctx._saveEval (EvSymbolize ctx._nesting meta._spelling term stood)
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
    --
    -- One side reaching ⊥ is a join too, the one 'if. cond value ⊥' spells
    -- "raise unless cond" with: the program raises on that side of the
    -- condition and has a perfectly good value on the other. The protocol is
    -- told on which side it raises, naming the condition by what the first
    -- operand the entry dataized came down to, and the meta is bound to the
    -- other side as it stands, which is the one value the fork can still
    -- answer with. Both sides reaching ⊥ is no such case: they are one term
    -- and join into ⊥ verbatim (#1405).
    paired :: ReduceContext -> Maybe (Either Int Bytes) -> (Subst, State) -> (Meta, (Meta, Meta)) -> IO (Subst, State)
    paired ctx condition (bound, state') (meta, (left, right)) = do
      one <- branch left
      two <- branch right
      case (one, two) of
        (ExTermination, ExTermination) -> both one two
        (ExTermination, _) -> terminating "left" left two
        (_, ExTermination) -> terminating "right" right one
        _ -> both one two
      where
        -- The two sides joined symbol by symbol (see 'joined').
        both :: Expression -> Expression -> IO (Subst, State)
        both one two = case joined one two state'._minted of
          Nothing -> throwIO (Stuck func)
          Just (term, made, spent) -> do
            mapM_ (ctx._saveEval . fact) made
            ctx._saveEval (EvJoin ctx._nesting meta._spelling (left._spelling, right._spelling) term)
            bound' <- bind meta (MvExpression term) bound
            pure (bound', state'{_minted = spent})
        -- The side that raises written down, named by the meta holding its ⊥,
        -- and the other side bound as the join; nothing is minted, since one
        -- value is left and a symbol would stand for nothing but it.
        terminating :: T.Text -> Meta -> Expression -> IO (Subst, State)
        terminating side raised term = do
          ctx._saveEval (EvTerminate ctx._nesting condition side raised._spelling)
          ctx._saveEval (EvJoin ctx._nesting meta._spelling (left._spelling, right._spelling) term)
          bound' <- bind meta (MvExpression term) bound
          pure (bound', state')
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
    -- made it without reading the term it stands in (#1280). Each record also
    -- carries what the 'dataize' operands of the entry came down to, in the
    -- order the entry declares them, since the symbol stands for what the λ
    -- function makes of them (#1421).
    --
    -- The answer is morphed rather than handed back as the entry wrote it,
    -- because a firing is one of the things a term can come from and every
    -- other one answers a normal form: 'Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )' written in
    -- the program morphs to the formation of the object, so the same term
    -- answered by an entry has to morph to it too. Two terms of one forma that
    -- do not look alike cannot be compared leaf by leaf, and comparing them is
    -- what a fork of two branches is (#1268). What is handed back, though, is
    -- the name that formation goes by in the world where it has one (see
    -- 'pathOf'): 'Φ.number( φ ↦ 𝑘 )' rather than a copy of every method
    -- 'number' declares, which the term would otherwise carry, and every step
    -- after this one walk again, for as long as it survives (#1453). The name
    -- is one way of saying the formation, since the world never changes, so
    -- two answers still compare leaf by leaf; the answer line of the protocol
    -- keeps the formation, which is what 𝕄 said.
    --
    -- Both terms go to the protocol, the built one before 'settled' is asked
    -- about it and the normal one after, so the morphing is a step of the
    -- protocol and no silent change of shape: whatever 𝕄 fires on the way opens
    -- its block between the two, where every other firing of an operand opens
    -- its own, and the formation standing on the second line is read as what
    -- the three tokens on the first came to (#1298).
    answered :: ReduceContext -> Lambda -> [Either Int Bytes] -> Subst -> State -> IO (Expression, State)
    answered ctx entry operands bound state' = do
      let (fresh, spent) = minted entry._answer state'._minted
      mapM_ (\idx -> ctx._saveEval (EvMinted ctx._nesting idx operands)) [idx | (_, FnSymbol idx) <- fresh]
      symbolic <- foldM mint bound fresh
      built <- buildExpressionThrows entry._answer symbolic
      ctx._saveEval (EvBuilt ctx._nesting built)
      (normal, state'') <- settled built univ state'{_minted = spent} ctx
      ctx._saveEval (EvAnswer ctx._nesting normal)
      pure (maybe normal (`pathOf` normal) ctx._universe, state'')
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

-- The term with the rules of a 'rewrite' line applied to it. Every position of
-- the term is tried, the outermost first, and the first rule whose pattern
-- matches a position as a whole rewrites it with the first match it made; the
-- position rewritten is not walked into again, so a rule whose result carries
-- its own pattern rewrites it once and never loops. A position no rule matches
-- is walked into, every binding and every argument of it, ρ among them, since
-- the shape of a branch is the program's to say and phino has no say in where
-- that shape is written. Nothing is normalized afterwards: the rules are the
-- program's word on what one term stands for, and reducing their outcome would
-- have phino second-guess it (#1409).
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
      | demanded bds -> maybe (pure (Nothing, state')) (evaluated ctx state' (ExFormation bds)) (saturated term bds)
    Right (_, state') -> pure (Nothing, state')
    Left failure -> parked state failure
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
    --
    -- A firing that cannot be made — an operand of the entry that never came
    -- down to data, above all — is parked exactly as a term 𝕄 could not reduce
    -- is, and for the same reason: the walk meets every λ function a program
    -- declares and one of them answering nothing is no failure of the run but a
    -- part of it phino cannot decide. Without this the signal left the walk
    -- altogether and the binding after the one it stopped on was never entered,
    -- so a single entry nothing could answer ended a run over a whole object
    -- model (#1288). The state it had reached goes back rather than the one the
    -- walk came in with, since the firings before it are done and the symbols
    -- they minted are spent.
    --
    -- The firing enters the formation it fires, the way 'fire' of 𝔻 does, so
    -- '--acyclic' sees a recursion the walk alone drives: an entry reducing an
    -- operand whose walk fires the same formation again is cut there and parked
    -- like any other loop (#1451).
    evaluated :: ReduceContext -> State -> Expression -> (T.Text, Expression) -> IO (Maybe Expression, State)
    evaluated ctx state' form (func, self)
      | isNothing (matched ctx._symbolic func) = pure (Nothing, state')
      | otherwise = do
          made <- try (enter form ctx >>= symbol func form self univ state')
          case made of
            Right (answer, answered) -> do
              (again, reached) <- fired dispatched answer univ answered ctx
              pure (Just (fromMaybe answer again), reached)
            Left failure -> parked state' failure
    -- A site the walk cannot reduce — a λ function whose operands never came
    -- down to data, or one the step budget ran out on — is left as it was
    -- written and the walk goes on, which is what a partial morphing is: phino
    -- stops where it cannot decide rather than failing the whole run. The state
    -- the parked site had reached travels back, so the symbols it minted before
    -- it stopped are never minted again; a signal carrying none of its own is
    -- answered with the state the caller reached before it was raised, which is
    -- the walk's state where 𝕄 was asked and the firing's where a firing was
    -- made. The chain it parked on is dropped, since that chain is the walk's
    -- and not the spine's.
    parked :: State -> ReduceException -> IO (Maybe Expression, State)
    parked _ (StuckAt _ _ reached) | caller._partial = pure (Nothing, reached)
    parked _ (OutOfStepsAt _ _ reached) | caller._partial = pure (Nothing, reached)
    parked reached (Stuck _) | caller._partial = pure (Nothing, reached)
    parked reached (OutOfSteps _) | caller._partial = pure (Nothing, reached)
    parked _ (LoopingAt _ _ reached) = pure (Nothing, reached)
    parked reached (Looping _) = pure (Nothing, reached)
    parked _ (StuckAt func _ _) = throwIO (Stuck func)
    parked _ (OutOfStepsAt limit _ _) = throwIO (OutOfSteps limit)
    parked _ failure = throwIO failure

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

-- The same as 'lambda', but only for a formation that is saturated: one with
-- every binding of it filled (see 'filled'). A void is an argument the program
-- has not given yet, so such a formation is a method waiting to be applied
-- rather than an application waiting to be computed, and firing it would hand
-- the λ function a ∅ where it expects a value. 𝔻 needs no such guard, since it
-- fires only what dataization demands and nothing demands a method; the deep
-- walk meets every one a program declares — the method table of the object
-- model above all — so it asks first (see 'deepened').
--
-- A binding holding ⊥ counts as filled only where the term the walk was handed
-- wrote that ⊥ as an argument itself. A ⊥ the reduction made is no argument:
-- the deep walk reduces a body in the scope of the formation around it, and a
-- formation declaring ρ and standing unapplied still holds ρ ↦ ∅, so a ξ.ρ in
-- that body comes back as ⊥ rather than as the object the next dispatch
-- supplies (#1196). A ⊥
-- written as an argument is what the program meant, and 'if. cond value ⊥' is
-- how it spells "raise unless cond", so a fork like that fires and its join
-- says on which side it raises (#1405). An argument given by name covers the
-- binding of that name; one given by position covers some binding, so there
-- have to be as many of them as ⊥ bindings no name covers. A term handing
-- nothing but ⊥ is still left alone, since there is no value for a firing to
-- work with and all it could do is get stuck on one of them.
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
    -- Whether every ⊥ of the bindings is one the term wrote, beside some
    -- argument that is not ⊥.
    given :: [Binding] -> Bool
    given rest = valued && length (filter unwritten rest) <= positional
    raising :: Binding -> Bool
    raising (BiTau _ ExTermination) = True
    raising _ = False
    -- Whether a binding holds a ⊥ no argument given by name wrote.
    unwritten :: Binding -> Bool
    unwritten (BiTau attr ExTermination) = attr `notElem` named
    unwritten _ = False
    -- The attributes the application chain of a term hands a literal ⊥ by
    -- name, how many literal ⊥ it hands by position, and whether it hands
    -- anything but ⊥ at all.
    written :: Expression -> ([Attribute], Int, Bool)
    written (ExApplication expr (ArTau attr ExTermination)) =
      let (attrs, count, other) = written expr in (attr : attrs, count, other)
    written (ExApplication expr (ArAlpha _ ExTermination)) =
      let (attrs, count, other) = written expr in (attrs, count + 1, other)
    written (ExApplication expr _) =
      let (attrs, count, _) = written expr in (attrs, count, True)
    written _ = ([], 0, False)

-- Whether a binding hands the formation something to work with. A void does
-- not: it names an argument the program has still to supply. A ⊥ is told
-- apart by 'saturated', which knows the term it was written in.
filled :: Binding -> Bool
filled (BiVoid _) = False
filled _ = True
