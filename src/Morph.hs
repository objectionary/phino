{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The Morphing function 𝕄 and the machinery every reduction of the calculus is
-- threaded with: the context, the step budget, the signals a stuck run raises
-- and the plumbing that reads a rule's premises. 𝔻 lives in 'Dataize' and 𝔼 in
-- 'Evaluate', both of which import this module; the edges pointing back — the
-- 'dataize' operand of a λ function, which is a dataization, and the firing of
-- a λ function itself, which is an evaluation — are injected as '_reduce',
-- '_evaluate' and '_fire' rather than imported (see 'ReductionFunc' and
-- 'EvaluationFunc').
module Morph (ReduceContext (..), ReduceException (..), EvaluationFunc, FiringFunc, ReductionFunc, Morphed, Steps (..), Tally (..), boxed, charged, deeper, emptyState, enter, entering, excluding, execBuildTerm, insideUniverse, isLambda, lambda, leadsTo, morph, morph', morphing, normalized, parking, producer, sidePremise, tallied, universed, unparked, verb) where

import AST
import Builder (buildExpressionThrows, contextualize)
import Control.Exception (Exception, catch, throwIO, try)
import Control.Monad (foldM, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find, partition)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Deps (Acyclic (..), BuildTermFunc, BuildTermMethodS, Evaluation (..), Judgment (..), SaveEvalFunc, SaveStepFunc, State (..), Term (..), dontSaveStep)
import Lambdas (Lambdas)
import Locator (locatedExpression, withLocatedExpression)
import Matcher (MetaValue (..), Subst (..), combine, matchExpression', substEmpty, substSingle)
import Must (Must (..))
import Printer (printExpression)
import Random (shuffle)
import Rewriter (RewriteContext (RewriteContext), Rewritten, Seen, rewrite, seenInsert)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Text.Printf (printf)
import Yaml (ExtraArgument (..), normalizationRules)
import qualified Yaml as Y

-- A term together with the derivation that reached it: what one frame of a
-- judgment's spine is handed and hands on.
type Morphed = (Expression, NonEmpty Rewritten)

-- How the morphing side reaches back to the dataization one. A λ function
-- brings its 'dataize' operands down through 𝔻, and that is a whole run of a
-- judgment 𝕄 has no business knowing about, since 'Dataize' imports 'Morph'
-- and not the other way round. The reduction is therefore injected into the
-- context, the way 'Deps' injects '_buildTerm', and 'Dataize' supplies its own
-- 'reduction' for it. What comes back is data or nothing at all, since an
-- operand 𝔻 could not bring down to bytes leaves the firing that asked for it
-- with nothing to bind. The state 𝑠 goes in and comes back out, so the symbols
-- a nested run mints are counted in the same sequence as the ones around it.
type ReductionFunc = Expression -> ReduceContext -> Expression -> State -> IO (Maybe Bytes, State)

-- How 𝕄 reaches the Evaluation function 𝔼, which lives in 'Evaluate' and
-- imports this module for the machinery every judgment shares. 𝔼 is what the
-- 'ml' and 'fire' rules ask for through an 'evaluate' premise, and it answers
-- with a normal form, so the rule that asked needs no 'normalize' after it. The
-- edge is injected rather than imported, exactly as 'ReductionFunc' injects the
-- 𝔻 one, and 'Evaluate' supplies its own 'evaluation' for it.
type EvaluationFunc = ReduceContext -> State -> BuildTermMethodS

-- How the deep walk reaches 𝔼. Like 'EvaluationFunc' it answers a normal form,
-- or nothing at all where nothing fired: the walk stands that answer back into
-- the program, and what a firing stands there has to look like what the program
-- itself would have morphed to, or the two cannot be compared (#1268). The
-- first argument is the attribute the term stands dispatched under, which is
-- what tells a λ the dispatch demands from one it does not.
type FiringFunc = Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Maybe Expression, State)

-- The initial, empty state a run of 𝕄 or 𝔻 starts from: nothing minted and
-- nothing manufactured yet. The 'State' type itself lives in 'Deps' next to
-- 'BuildTermMethod'.
emptyState :: State
emptyState = State 0 Nothing Nothing

-- How many steps of the 𝕄/𝔻 recursion one branch of a derivation may take
-- ('_limit', the '--max-steps' option) and how many the branch reaching this
-- point has already taken ('_spent'). 𝕄 and 𝔻 recurse into each other, into the
-- premises of their own rules and into the λ functions they fire, so a budget local to
-- one of those chains is reset by the next nested call and bounds nothing (see
-- #1052). This one rides in the context that every such path — the spine, the
-- side-premises, '_dataize' and '_morph' — already carries, so a nested call
-- inherits the count of the call that made it. It bounds depth, not total work:
-- a premise passes its count down but not back, so siblings each descend from
-- the same '_spent'. Bounding every branch is enough to terminate, since a rule
-- has finitely many premises.
data Steps = Steps
  { _limit :: Int
  , _spent :: Int
  }

-- How many λ functions the whole run may fire ('_ceiling', the '--max-firings'
-- option) and how many it has fired so far ('_count'). Unlike 'Steps' it bounds
-- total work and not one branch: a firing whose answer is wider than the term
-- it replaced makes the next descent more siblings than the last one, each of
-- them shallow, so a recursion that widens the term instead of nesting it fires
-- forever inside the depth '--max-steps' gives it (#1472). The count is one
-- cell every frame of the run shares rather than a field of 'State', since a
-- parked frame hands back the state it started from and so would refund every
-- firing made inside it.
data Tally = Tally
  { _ceiling :: Int
  , _count :: IORef Int
  }

-- The context every reduction of the calculus is threaded with — 𝕄 here and 𝔻 in
-- 'Dataize' — carrying the configuration plus the step budget spent so far. Nothing global is fixed here: the universe (the second argument 'e' of
-- 𝕄(n, e, s) and 𝔻(n, e, s)) is a plain expression threaded as an argument to
-- 'dataize'', 'morph'' and on to the λ functions, and the state 's' is threaded the same
-- way (see 'State'). The working expression needed for normalization is taken
-- from the head of the step chain, so no separate wrapper type is threaded
-- around.
data ReduceContext = ReduceContext
  { _locator :: Expression
  , -- Where in the universe the term being reduced stands, which is what a
    -- firing of 𝔼 is written under: the protocol names the entry that answered
    -- and this names the part of the program the answer belongs to, since one
    -- entry answers the same way wherever it is fired and only the site tells
    -- two firings of it apart (#1302). It starts as the aim of the run itself
    -- ('_locator', the '--locator' option, or the binding '--inside' mints) and
    -- the '--deep' walk refines it as it enters a binding, so a λ fired inside
    -- an object is written under the locator of that object. It is refined no
    -- further than a locator reaches: the head of a dispatch and the argument
    -- of an application stand under no attribute of any formation, so a firing
    -- there is written under the nearest binding the walk entered, which is
    -- where the term it fired against stands. It is kept apart from '_locator'
    -- because that one is where a derivation is spliced back into the working
    -- expression ('leadsTo', 'normalized'), and the walk reduces terms no
    -- locator of the universe aims at.
    _site :: Expression
  , -- The world this run reduces in, as Φ denotes it: the program in normal
    -- form. Normalization is handed it so that 'dot', dispatching off a
    -- formation, can tell the whole program from a part of it and decorate the
    -- body with the name Φ rather than with the program itself; writing the
    -- program out would copy it into the term, and into every term that term
    -- then dispatches, until the copies weigh hundreds of times what the
    -- program does (#1318). Nothing until a run works it out ('universed'),
    -- once, and every frame below inherits what the first one named.
    _universe :: Maybe Expression
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _steps :: Steps
  , -- How many λ functions the whole run may fire and how many it has fired
    -- (see 'Tally'), or nothing where '--max-firings' asks for no such limit.
    _tally :: Maybe Tally
  , _nesting :: Int
  , _depthSensitive :: Bool
  , _shuffle :: Bool
  , _partial :: Bool
  , _deep :: Bool
  , _acyclic :: Maybe Acyclic
  , -- The judgment whose rule is asking 𝔼 to fire, which is what a stuck site
    -- is written under: 𝔼 is reached from the 'ml' rule of morphing and from
    -- the 'fire' rule of dataization, and a reader of the protocol is told
    -- which of the two asked the question nothing answered. Every frame of 𝕄
    -- names itself here and every frame of 𝔻 does the same, so what a firing
    -- reads is the judgment of the frame it was fired from and never of one
    -- above it (#1300).
    _judgment :: Judgment
  , -- The λ functions this run has already got stuck on and written a '?(…)'
    -- to the protocol for. A parked site stays in the residue exactly as it was
    -- written, so the '_deep' walk over that residue reaches it again and 𝕄
    -- fires 𝔼 on it once more, only to find out what the spine already found
    -- out; the site is one and the protocol records it once, so the firings
    -- after the first write nothing (see 'symbol' in 'Evaluate', #1300).
    _parked :: [T.Text]
  , -- The formations the frames above this one have entered, which is what
    -- '_acyclic' answers "have I been here before" with (see 'entering'). A
    -- frame enters a formation where it fires the λ of one — 𝔻 through 'fire',
    -- 𝕄 through 'ml', the '--deep' walk through 'fired' of 'Evaluate' — or gets
    -- into the φ body of one — 𝔻 through 'box' — and nowhere else, so 𝕄 and 𝔻 handing each other the very term they were
    -- asked about enter nothing twice and one store serves both of them. The
    -- store is keyed by 'hashShape' under 'Proven' and by 'hashSkeleton' under
    -- 'Plausible', so a formation is found again the way the mode compares it.
    _entered :: Seen
  , _symbolic :: Lambdas
  , _buildTerm :: BuildTermFunc
  , _reduce :: ReductionFunc
  , _evaluate :: EvaluationFunc
  , _fire :: FiringFunc
  , _saveStep :: SaveStepFunc
  , _saveEval :: SaveEvalFunc
  }

-- Which of the two budgets a run spent, with the limit it was given: the depth
-- one branch may descend ('--max-steps', see 'Steps') or the firings the whole
-- run may make ('--max-firings', see 'Tally'). Both are the same signal to
-- '_partial', which parks either as a site that never finishes, and differ only
-- in what the message names.
data Budget
  = Depth Int
  | Firings Int

data ReduceException
  = OutOfSteps Budget
  | -- A λ function could not fire: the '--symbolic' file carries no entry
    -- answering that name, or an operand of the entry it does carry never came
    -- down to data, or the two branches it joins differ by more than a symbol,
    -- so there is nothing to answer with. The name is that of the function 𝔼
    -- actually failed on, which for a chain of dispatches is the innermost one,
    -- since 'ml' reduces a head before the function above it fires.
    Stuck T.Text
  | -- A 'Stuck' caught by a frame of the 𝕄/𝔻 spine, together with the
    -- derivation and the state that frame had reached (see 'parking'). The head
    -- of the chain is the working expression with the stuck application left
    -- intact and everything reduced before it already in place: the residual
    -- program that '_partial' turns into the 'Residual' outcome. The state
    -- travels with it, so the symbols a parked run minted are never minted
    -- again.
    StuckAt T.Text (NonEmpty Rewritten) State
  | -- An 'OutOfSteps' caught by a spine frame, carrying that frame's derivation
    -- and state just like 'StuckAt': a term that never reduces is a stuck site
    -- too, so '_partial' parks it and hands back the residual instead of
    -- failing hard (#1078)
    OutOfStepsAt Budget (NonEmpty Rewritten) State
  | -- A frame was about to enter a formation a frame above it has already
    -- entered, up to a renaming of symbols, which it can only ever answer by
    -- entering it again. 𝕄 and 𝔻 both raise it, over the one store of the
    -- formations their branch has entered (see 'entering'), and neither names
    -- itself in the message, since a run that meets the signal meets it
    -- through whichever of the two came back. It carries the formation.
    -- Raised under '_acyclic' alone, so the signal itself is the permission to
    -- park on it: a run that never asked for the guard never sees it.
    Looping Expression
  | -- A 'Looping' caught by a frame of the 𝕄 or 𝔻 spine, carrying that frame's
    -- derivation and state the way 'StuckAt' does. The guard runs as a frame
    -- opens, before that frame parks anything, so the frame attaching the chain
    -- is the one the repeat was reached from and the head of the chain is its
    -- working expression — the term that would have entered the formation
    -- again left exactly where it stood, the way an exhausted budget stops on
    -- the last step it could afford.
    LoopingAt Expression (NonEmpty Rewritten) State
  | -- 𝔻 was handed a term outside its domain: the terminator ⊥, which signals
    -- an error (see #955), or a term no dataization rule matches, such as a
    -- formation whose φ is a void nothing filled. It carries the term and the
    -- state the frame that met it had reached. A run of 𝔻 fails on it, with or
    -- without '_partial', but an operand of a firing that meets it parks that
    -- firing under '_partial' the way an unanswered λ function does, since the
    -- dead end is a property of the program rather than of phino (#1401).
    Undataizable Expression State
  deriving anyclass (Exception)

instance Show ReduceException where
  show (OutOfSteps (Depth limit)) =
    printf "Dataization did not finish before reaching the limit of steps: --max-steps=%d" limit
  show (OutOfSteps (Firings limit)) =
    printf "Evaluation did not finish before reaching the limit of firings: --max-firings=%d" limit
  show (OutOfStepsAt budget _ _) = show (OutOfSteps budget)
  show (Stuck func) = printf "No entry of --symbolic answers the λ function '%s'" (T.unpack func)
  show (StuckAt func _ _) = show (Stuck func)
  show (Looping term) = printf "Reduction entered a formation it is already inside: %s" (printExpression term)
  show (LoopingAt term _ _) = show (Looping term)
  show (Undataizable ExTermination _) = "dataization reached the terminator ⊥, which signals an error and cannot be dataized"
  show (Undataizable _ _) = "no dataization rule matched"

-- Charge one step of the 𝕄/𝔻 recursion to the budget, refusing to descend once
-- it is gone. '--max-cycles' and '--max-depth' bound only the normalization run
-- inside a single step, so before this the recursion itself was unbounded and a
-- term that never reduces to bytes kept 𝕄 and 𝔻 calling each other forever
-- (#1052). Rewriting hands back whatever it has reached when it runs out of
-- cycles; 𝔻 has no partial answer to give, so an exhausted budget always throws,
-- with or without '--depth-sensitive'.
deeper :: ReduceContext -> IO ReduceContext
deeper ctx@ReduceContext{_steps = Steps limit spent}
  | spent >= limit = throwIO (OutOfSteps (Depth limit))
  | otherwise = pure ctx{_steps = Steps limit (spent + 1)}

-- The tally a run starts from where '--max-firings' gives a ceiling: nothing
-- fired yet.
tallied :: Maybe Int -> IO (Maybe Tally)
tallied = traverse (\cap -> Tally cap <$> newIORef 0)

-- Charge one firing of a λ function to the budget of the whole run, refusing
-- to fire once it is gone (see 'Tally'). 'deeper' bounds how far one branch
-- descends, which stops a recursion that nests but not one that widens.
charged :: ReduceContext -> IO ()
charged ReduceContext{_tally = Nothing} = pure ()
charged ReduceContext{_tally = Just (Tally cap count)} = do
  fired <- readIORef count
  when (fired >= cap) (throwIO (OutOfSteps (Firings cap)))
  writeIORef count (fired + 1)

-- Run one frame of the 𝕄/𝔻 spine, attaching its derivation and its state to a
-- stuck λ function or an exhausted budget escaping it. 'Stuck' is raised deep
-- inside a firing, which knows nothing about the chain, so the innermost spine
-- frame it reaches is the one to record where the derivation stopped: the head
-- of that frame's chain is the working expression with the stuck application
-- intact and everything reduced before it already in place. The same holds for
-- 'OutOfSteps': a term cycling through the universe is no more a failure of the
-- chain than a missing λ function is, and under '_partial' it deserves the same
-- parked residual (#1078). Outer frames see the '…At' signals and let them
-- pass, since their chains are prefixes of that one; a side-computation running
-- on a chain of its own strips the chain off again (see 'unparked') before the
-- signal reaches the spine.
parking :: NonEmpty Rewritten -> State -> IO a -> IO a
parking seq state action = action `catch` rethrow
  where
    rethrow :: ReduceException -> IO a
    rethrow (Stuck func) = throwIO (StuckAt func seq state)
    rethrow (OutOfSteps budget) = throwIO (OutOfStepsAt budget seq state)
    rethrow (Looping term) = throwIO (LoopingAt term seq state)
    rethrow failure = throwIO failure

-- Strip the derivation off a stuck λ function escaping a side-computation that
-- ran on a chain of its own — a firing reducing an operand of its own, or a
-- 'morph' premise through '_morph'. That chain is not the spine's, so it is
-- dropped and the spine frame around the side-computation attaches its own
-- (see 'parking').
unparked :: IO a -> IO a
unparked action = action `catch` rethrow
  where
    rethrow :: ReduceException -> IO a
    rethrow (StuckAt func _ _) = throwIO (Stuck func)
    rethrow (OutOfStepsAt budget _ _) = throwIO (OutOfSteps budget)
    rethrow (LoopingAt term _ _) = throwIO (Looping term)
    rethrow failure = throwIO failure

-- The formations the frames above this one have entered, which is what
-- '_acyclic' answers "have I been here before" with: where the frame opening on
-- this term enters a formation (see 'entrance') and a frame above it has
-- already entered the same one, the run is going round and 'Looping' says so;
-- otherwise the formation is remembered for the frames below. The context
-- travels down the recursion and never back up, exactly as the step budget
-- does, so what it carries is the branch from the run to this frame and not
-- everything the run has ever touched: two siblings entering one formation
-- enter it twice, while a formation entered from inside itself is a loop.
--
-- Under 'Proven' the same means 'alike', equal up to a bijective renaming of
-- symbols, and not equal: every round of a recursion over an unknown mints fresh symbols, so
-- the formation it enters on the second round is the first one with 𝜎5 where
-- 𝜎3 stood, and an exact comparison never finds it (#1420). That is sound,
-- since a symbol is an opaque unknown — each dataizes to the same manufactured
-- datum and no entry of '--symbolic' answers one — so a formation entered again
-- with nothing but its symbols renamed replays the round forever; data still
-- tells rounds apart, so a recursion over a literal is not cut. The store is a
-- digest map keyed by 'hashShape', which is blind to symbols, and a digest
-- match is confirmed by 'alike', the way 'Seen' confirms one by (==). The cut
-- is written to the protocol where the formation would have opened, as a
-- 'looped' line carrying the site, the mode and the formation the frame above entered,
-- spelled as that frame's own 'formation' line spelled it, so the two lines
-- read as a pair without renaming symbols by eye (#1434).
--
-- Under 'Plausible' the same means that the formation a frame above entered is
-- 'within' the one about to be entered: an accumulator gains a wrapper every
-- round, so no two rounds are ever 'alike', while each still holds the one
-- before it (#1451). A formation entered from inside a smaller one is never cut,
-- since a smaller term never holds a larger one, which is what keeps a call
-- nested in its own operand, such as a sum of sums, reducing as it did. It is
-- not sound: a recursion whose argument grows on its way to stopping is cut as
-- well. The store is keyed by 'hashSkeleton', which sees the attributes and
-- not the terms bound to them, and a digest match is confirmed by 'within'.
entering :: Expression -> ReduceContext -> IO ReduceContext
entering term ctx = maybe (pure ctx) (`enter` ctx) (entrance ctx._judgment term)

-- The same guard asked about a formation the frame is about to enter, for a
-- frame that knows it enters one without being a rule of 𝕄 or 𝔻: the '--deep'
-- walk, which fires the λ of every formation 𝕄 leaves bare, so a recursion
-- driven by the walk alone goes through no rule 'entrance' knows of (#1451).
enter :: Expression -> ReduceContext -> IO ReduceContext
enter form ctx = maybe (pure ctx) remembered ctx._acyclic
  where
    remembered :: Acyclic -> IO ReduceContext
    remembered mode = case find (repeated mode form) (Map.findWithDefault [] (digest mode form) ctx._entered) of
      Just before -> do
        ctx._saveEval (EvLooped ctx._nesting ctx._judgment mode before ctx._site)
        throwIO (Looping form)
      Nothing -> pure ctx{_entered = seenInsert (digest mode form) form ctx._entered}
    digest :: Acyclic -> Expression -> Int
    digest Proven = hashShape
    digest Plausible = hashSkeleton
    repeated :: Acyclic -> Expression -> Expression -> Bool
    repeated Proven form before = alike form before
    repeated Plausible form before = within before form

-- The formation a frame of the judgment enters as it opens on the term, if it
-- enters one at all. Only three rules get into a formation, besides the firing
-- of the '--deep' walk, which asks 'enter' itself: 'box' of 𝔻, into
-- the φ body of a formation carrying no λ and no Δ; 'fire' of 𝔻, into the λ
-- function of a formation carrying one naming a function; and 'ml' of 𝕄, into
-- the λ function of the head of a dispatch, which is the formation entered and
-- not the dispatch off it. Every other term the two judgments are handed is
-- one they only pass through on their way to such a formation, and 𝕄 stops at
-- a formation without getting into it.
entrance :: Judgment -> Expression -> Maybe Expression
entrance Dataization term@(ExFormation bds)
  | boxed bds || isJust (lambda bds) = Just term
entrance Morphing (ExDispatch form@(ExFormation bds) _)
  | isJust (lambda bds) = Just form
entrance _ _ = Nothing

-- Whether the 'box' rule of 𝔻 gets into a formation with these bindings: one
-- binding φ to a term, and none binding Δ or a λ (see 'box.yaml').
boxed :: [Binding] -> Bool
boxed bds = any phi bds && not (any isLambda bds) && not (any delta bds)
  where
    phi :: Binding -> Bool
    phi (BiTau AtPhi _) = True
    phi _ = False
    delta :: Binding -> Bool
    delta (BiDelta _) = True
    delta _ = False

-- Split the λ binding off a formation for the LAMBDA morphing rule: the name of
-- the λ function to fire and the formation it fires against, the λ binding
-- removed. A formation with no λ binding, or with more than one, has nothing to
-- fire; neither has one carrying a symbol, which is a λ name nothing answers.
-- The three are one answer here but not to 𝔼, which tells all three apart: no λ
-- at all is answered with ⊥, a symbol gets stuck the way an unanswered name
-- does, and only the rest is a term it cannot work out (see 'evaluation' in
-- 'Evaluate'). It lives here and not beside 𝔼 because the guard of '_acyclic'
-- asks it too (see 'entrance').
lambda :: [Binding] -> Maybe (T.Text, Expression)
lambda bds = case partition isLambda bds of
  ([BiLambda (Function func)], rest) -> Just (func, ExFormation rest)
  _ -> Nothing

-- Whether a binding names a λ function, whatever that name turns out to be.
-- 𝔼 asks this before 'lambda' does its splitting, since a formation carrying no
-- λ at all is answered with ⊥ rather than refused (see 'evaluation' in
-- 'Evaluate').
isLambda :: Binding -> Bool
isLambda (BiLambda _) = True
isLambda _ = False

-- The Morphing function 𝕄 maps normal forms to formations. It is ternary,
-- 𝕄(n, e, s): besides the term 'n' it takes the universe 'e' ('univ') — a plain
-- expression — and the mutable state 's', returning the morphed term together
-- with the new state. The universe is matched against the rule's 'universe'
-- pattern (usually the '𝑒' meta, which binds 'e' so the 'universe' rule substitutes
-- it, but a rule may pin it to a literal such as 'mg' matching Φ). Its rules
-- come from 'resources/morphing': the first matching rule's premises are evaluated and
-- its conclusion 'nresult' is built, always forwarding the same universe. The
-- clauses are disjoint (see #856, #860), so their declaration order must not be
-- load-bearing; when '_shuffle' is on (the '--shuffle' flag) the rules are
-- shuffled before the 'firstMatch' walk to exercise that invariant — mirroring
-- normalization's "apply until they stop matching". A genuinely order-independent
-- step stays deterministic; a hidden overlap surfaces as a nondeterministic
-- failure rather than staying silently green.
-- The 'morph' premise that produces the conclusion is the spine: when
-- its argument comes from a 'normalize' premise, the rewriter runs over that
-- argument and its individual steps (alpha, copy, dot, …) are spliced into the
-- chain before morphing continues. Every other premise is a side-computation
-- evaluated in isolation by 'sidePremise', its own steps discarded.
morph' :: Morphed -> Expression -> State -> ReduceContext -> IO (Morphed, State)
morph' (expr, seq) univ state caller = do
  ctx <- deeper =<< entering expr =<< universed univ caller{_judgment = Morphing}
  parking seq state $ do
    rules <- if ctx._shuffle then shuffle Y.morphingRules else pure Y.morphingRules
    matched <- firstMatch ctx rules
    case matched of
      Just (rule, subst) -> reduce ctx rule subst
      Nothing -> throwIO (userError "no morphing rule matched")
  where
    firstMatch :: ReduceContext -> [Y.MorphRule] -> IO (Maybe (Y.MorphRule, Subst))
    firstMatch _ [] = pure Nothing
    firstMatch ctx (rule : rest) = do
      substs <- matchExpressionWithRule' (matchExpression' rule.ematch univ) expr (asRule rule) (RuleContext (execBuildTerm univ ctx) (Just univ))
      case substs of
        (subst : _) -> pure (Just (rule, subst))
        [] -> firstMatch ctx rest
    -- Match the conclusion term and check the guard; premises are no longer the
    -- matcher's business, so 'where'/'having' stay empty and the guard lives in
    -- 'when'. Every morphing guard reads only meta-variables bound by 'match'
    -- and 'universe', so it holds before any premise runs.
    asRule :: Y.MorphRule -> Y.Rule
    asRule rule = Y.Rule rule.name Nothing Nothing rule.match ExRoot rule.when Nothing Nothing
    -- Evaluate the rule's premises and build its conclusion. A literal
    -- conclusion is terminal. Otherwise the conclusion meta is produced by a
    -- trailing 'morph' premise (the spine); if that premise's argument is itself
    -- bound by a 'normalize' premise, the normalization joins the spine and its
    -- steps splice in before morphing continues.
    reduce :: ReduceContext -> Y.MorphRule -> Subst -> IO (Morphed, State)
    reduce ctx rule subst = case producer rule.nresult rule.premises of
      Nothing -> do
        (final, state') <- sides ctx rule.premises subst
        built <- buildExpressionThrows rule.nresult final
        seq' <- leadsTo seq rule.name built ctx
        pure ((built, seq'), state')
      Just concl@(Y.Premise _ (Y.OpMorph arg)) -> case producer arg rule.premises of
        Just normal@(Y.Premise _ (Y.OpNormalize inner)) -> do
          (final, state') <- sides ctx (rule.premises `excluding` [concl, normal]) subst
          built <- buildExpressionThrows inner final
          (normal', seq') <- settle ctx rule inner built
          morph' (normal', seq') univ state' ctx
        _ -> do
          (final, state') <- sides ctx (rule.premises `excluding` [concl]) subst
          built <- buildExpressionThrows arg final
          seq' <- leadsTo seq rule.name built ctx
          morph' (built, seq') univ state' ctx
      Just _ -> throwIO (userError (printf "morphing rule '%s' must conclude with a 'morph' premise" rule.name))
    sides :: ReduceContext -> [Y.Premise] -> Subst -> IO (Subst, State)
    sides ctx premises subst = foldM (sidePremise univ ctx) (subst, state) premises
    -- Bring the term a 'normalize' premise built to its normal form and splice
    -- the steps into the chain. A premise normalizing the universe itself, the
    -- meta the rule's 'universe' bound, is answered with the world the run has
    -- already named (see '_universe'), since that is the normal form of the
    -- very same program: the 'universe' rule asks for it every time 𝕄 resolves
    -- Φ, and normalizing the whole program again for each of them made every
    -- step cost the size of the world (#1453).
    settle :: ReduceContext -> Y.MorphRule -> Expression -> Expression -> IO Morphed
    settle ctx rule inner built = case ctx._universe of
      Just world | inner == rule.ematch -> do
        seq' <- leadsTo seq rule.name world ctx
        pure (world, seq')
      _ -> do
        labelled <- leadsTo seq rule.name built ctx
        normalized built labelled ctx

-- Morph the expression located at '_locator' — 𝕄 asked on its own, the way
-- 'dataize' asks 𝔻. The whole input expression is itself the universe Φ (the 'e'
-- argument) threaded through 𝕄, so it is passed both as the located target and
-- as the universe; the default locator Q therefore morphs the top formation,
-- which 'mf' hands back unchanged, and '_locator' is how one aims 𝕄 at a
-- subterm. Unlike 𝔻, 𝕄 is total: it stops at the first formation it reaches
-- ('mf') and never demands bytes, and where no formation is reachable it answers
-- with the terminator ⊥ ('dead', 'xi', 'mg', 'mad', 'maad') rather than failing.
-- Only the λ functions 'ml' fires can still get stuck, and '_partial' parks
-- them just as it does under 𝔻: the answer is then the residual subterm the
-- spine had reached, taken from '_locator' of its working expression. Stopping
-- at the first formation leaves everything that formation holds as it was
-- written, which is what '_deep' walks into before the answer is handed back
-- (see 'deepened'). The state 𝑠 goes in and comes back out, so a 𝕄 asked
-- inside another judgment goes on minting symbols where that judgment left off.
morph :: Expression -> State -> ReduceContext -> IO (Expression, [Rewritten], State)
morph universe state caller@ReduceContext{..} = do
  ctx <- universed universe caller
  expr <- locatedExpression _locator universe
  result <- try (morph' (expr, (universe, Nothing) :| []) universe state ctx)
  case result of
    Right ((morphed, seq), state') -> walked (walking ctx) morphed seq state'
    Left (StuckAt func seq parked) | _partial -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked (marked ctx func) residue seq parked{_stuck = Just func}
    Left (OutOfStepsAt _ seq parked) | _partial -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked (walking ctx) residue seq parked
    -- Unlike the two above, this one takes no '_partial' guard: a 'LoopingAt'
    -- exists only where '_acyclic' put it, so asking for the guard is already
    -- asking to be parked on what it finds.
    Left (LoopingAt _ seq parked) -> do
      residue <- locatedExpression _locator (fst (NE.head seq))
      walked (walking ctx) residue seq parked
    Left failure -> throwIO (failure :: ReduceException)
  where
    -- The context the walk runs with: the one this run was given, named after
    -- 𝕄, since the walk is 𝕄's own and a λ function it fires is fired by no
    -- other judgment, whichever one asked for this run (see '_judgment'). It
    -- carries the world the spine named, so no firing of the walk normalizes
    -- the whole program again to name it once more (#1453).
    walking :: ReduceContext -> ReduceContext
    walking ctx = ctx{_judgment = Morphing}
    -- The same, plus the λ function the spine got stuck on. The site is still
    -- standing in the residue, so the walk asks 𝕄 about it again and 𝔼 gets
    -- stuck on it again; the protocol has the site already and the second
    -- firing writes nothing (see '_parked', #1300).
    marked :: ReduceContext -> T.Text -> ReduceContext
    marked ctx func = (walking ctx){_parked = func : _parked}
    -- The answer 𝕄 reached, walked by '_deep' before it is handed back (see
    -- 'deepened'), and the chain that led to both. The walk joins the chain as
    -- one step named 'deep', so '--sequence' ends on the term the command
    -- prints.
    walked :: ReduceContext -> Expression -> NonEmpty Rewritten -> State -> IO (Expression, [Rewritten], State)
    walked walker morphed seq state'
      | not _deep = pure (morphed, reverse (NE.toList seq), state')
      | otherwise = do
          (deep, state'') <- deepened morphed universe state' walker
          seq' <- leadsTo seq "deep" deep walker
          pure (deep, reverse (NE.toList seq'), state'')

-- Walk what 𝕄 answered with, entering everything it left as it was written —
-- the mechanism behind '--deep' ('_deep'). 𝕄 navigates a term to the first
-- formation it reaches and 'mf' hands that formation back with its bindings
-- untouched, since firing a bare λ is 𝔻's business; 𝔻 in turn follows the one
-- path dataization demands and ends in bytes. A part of a program that nothing
-- demands — the argument of a λ function that cannot fire, for one — is
-- therefore reduced by neither, and the object structure is lost to the one
-- that does reduce it (#1124). This walk demands nothing either. It asks 𝕄
-- about every sub-expression and, where 𝕄 lands on a formation whose λ the
-- '--symbolic' file answers, fires it and asks 𝕄 about the answer again (see
-- '_fire', which 'Evaluate' answers with its own 'fired'). A sub-expression on
-- whose way a λ function fired is replaced by the
-- answer of the last firing; where none fired it stays as it was written and
-- only its own parts are walked, so the calls no entry answers keep their names
-- and what comes back is still the same program, reduced as far as the file
-- allows. Every entry is charged to the '--max-steps' budget, which is what
-- bounds the walk.
deepened :: Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
deepened expr univ state ctx = go (Just ctx._site) Nothing ExXi expr state ctx
  where
    -- A term as it was written, together with the locator naming it where one
    -- does and with what its free ξ stands for: the formation the walk entered
    -- it from, without the binding it came from, exactly the context the 'dot'
    -- rule hands a dispatched body. At the top there is no such formation, so ξ
    -- stands for itself and contextualization leaves the term alone, and the
    -- locator is the one the whole run was aimed at.
    go :: Maybe Expression -> Maybe Attribute -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
    go standing dispatched context term state' caller = do
      let here = sited standing caller
      ctx' <- deeper here
      (walked, walkedState) <- parts standing context term state' here
      (answer, answered) <- ctx'._fire dispatched (contextualize walked context) univ walkedState ctx'
      pure (fromMaybe walked answer, answered)
    -- The context a term is walked in, aimed at the term itself where a locator
    -- names it. Where none does, the aim stays where it was: a firing standing
    -- deeper in a term than a locator reaches belongs to the last binding the
    -- walk entered, and saying that is saying where it is (see '_site').
    sited :: Maybe Expression -> ReduceContext -> ReduceContext
    sited Nothing caller = caller
    sited (Just loc) caller = caller{_site = loc}
    -- The parts of a term nothing fired on, walked one by one and put back
    -- where they were, so the term keeps the shape it was written in. Only a
    -- binding of a formation carries the locator further: the head of a
    -- dispatch and both sides of an application stand under no attribute, so
    -- what they hold is entered with no locator of its own. An abstract
    -- formation, one holding a void, is a method nobody applied: its body is
    -- parametric, walking it can only end in ⊥ or a stuck term, and a λ there
    -- dataizing a parameter would end the whole run, so it is handed back as it
    -- was written (#1393). A void ρ counts like any other: a formation holds
    -- one only where the program declared it, so it is a method waiting for
    -- the receiver a dispatch hands it, and its ξ.ρ can only collapse to ⊥
    -- wherever it stands, in a copy that kept its ρ or in one 'skip' dropped
    -- it from (#1397, #1414).
    parts :: Maybe Expression -> Expression -> Expression -> State -> ReduceContext -> IO (Expression, State)
    parts _ _ term@(ExFormation bds) state' _
      | any abstract bds = pure (term, state')
      where
        abstract :: Binding -> Bool
        abstract (BiVoid _) = True
        abstract _ = False
    parts standing _ (ExFormation bds) state' caller = do
      (entered, state'') <- bindings standing bds bds state' caller
      pure (ExFormation entered, state'')
    parts _ context (ExDispatch target attr) state' caller = do
      (entered, state'') <- go Nothing (Just attr) context target state' caller
      pure (ExDispatch entered attr, state'')
    parts _ context (ExApplication target arg) state' caller = do
      (entered, state'') <- go Nothing Nothing context target state' caller
      (applied, state''') <- argument context arg state'' caller
      pure (ExApplication entered applied, state''')
    parts _ _ term state' _ = pure (term, state')
    -- Walk the bindings of a formation left to right, threading the state
    -- through them. Only what the formation itself holds is entered: ρ names
    -- the object around it rather than one inside it, and a void, Δ or λ
    -- binding carries no term to walk at all. A body of a formation the walk
    -- can name is named by that locator and the attribute it is bound to, which
    -- is the very locator '--locator' would aim a run of its own at.
    bindings :: Maybe Expression -> [Binding] -> [Binding] -> State -> ReduceContext -> IO ([Binding], State)
    bindings _ _ [] state' _ = pure ([], state')
    bindings standing whole (BiTau attr body : rest) state' caller
      | attr /= AtRho = do
          (entered, state'') <- go (fmap (`ExDispatch` attr) standing) Nothing (scope attr whole) body state' caller
          (others, state''') <- bindings standing whole rest state'' caller
          pure (BiTau attr entered : others, state''')
    bindings standing whole (bd : rest) state' caller = do
      (others, state'') <- bindings standing whole rest state' caller
      pure (bd : others, state'')
    -- The context a binding's body is entered in: the formation without that
    -- binding, the very context 'dot' contextualizes a dispatched body in, so
    -- a body reaching back at itself through ξ collapses instead of looping.
    scope :: Attribute -> [Binding] -> Expression
    scope attr bds = ExFormation (filter (not . named) bds)
      where
        named :: Binding -> Bool
        named (BiTau attr' _) = attr' == attr
        named _ = False
    -- Both sides of an application stand in the same context: the term it
    -- applies is walked by the caller and the argument it binds is walked here.
    argument :: Expression -> Argument -> State -> ReduceContext -> IO (Argument, State)
    argument context (ArTau attr arg) state' caller = do
      (entered, state'') <- go Nothing Nothing context arg state' caller
      pure (ArTau attr entered, state'')
    argument context (ArAlpha alpha arg) state' caller = do
      (entered, state'') <- go Nothing Nothing context arg state' caller
      pure (ArAlpha alpha entered, state'')

-- The premise binding the given expression meta, if any. The conclusion of a
-- morphing rule and the argument of a continuation premise are looked up here to
-- find the premise that produces them.
producer :: Expression -> [Y.Premise] -> Maybe Y.Premise
producer (ExMeta name) = find (\premise -> premise.result == name)
producer _ = const Nothing

-- The premises whose result meta is not bound by any of the given ones — the
-- side-computations left once the spine premises are removed.
excluding :: [Y.Premise] -> [Y.Premise] -> [Y.Premise]
excluding premises removed = filter (\premise -> premise.result `notElem` map (.result) removed) premises

-- Evaluate one side-computation premise — a 'morph', 'evaluate' or 'contextualize'
-- of an earlier term — in isolation, binding its result meta. These never splice
-- steps into the trace: 'morph' and 'evaluate' reduce on a fresh chain and discard
-- it, 'contextualize' is pure. The state is threaded through: 'evaluate' (the
-- 𝔼 of the 'ml' and 'fire' rules) takes the incoming state 𝑠1 and yields a
-- new one 𝑠2, 'morph' propagates whatever its sub-reduction produced, and every
-- other operation leaves the state untouched.
sidePremise :: Expression -> ReduceContext -> (Subst, State) -> Y.Premise -> IO (Subst, State)
sidePremise univ ctx (subst, state) premise = do
  (term, state') <- runOperation
  case combine (substSingle premise.result (metaValue term)) subst of
    Just subst' -> pure (subst', state')
    Nothing -> throwIO (userError (printf "premise meta '%s' clashes with an existing binding" (T.unpack premise.result)))
  where
    -- The 𝔼 ('evaluate') and 𝕄 ('morph') operations can change the state, so they
    -- go through their state-aware builders; every other operation is stateless
    -- and the incoming state is returned unchanged.
    runOperation :: IO (Term, State)
    runOperation = case premise.operation of
      Y.OpEvaluate expr universe -> ctx._evaluate ctx state [ArgExpression expr, ArgExpression universe] subst
      Y.OpMorph expr -> _morph univ ctx state [ArgExpression expr] subst
      operation -> do
        term <- execBuildTerm univ ctx (verb operation) (verbArgs operation) subst
        pure (term, state)
    metaValue :: Term -> MetaValue
    metaValue (TeExpression value) = MvExpression value
    metaValue (TeAttribute value) = MvAttribute value
    metaValue (TeBytes value) = MvBytes value
    metaValue (TeBindings value) = MvBindings value

-- The build-term function name backing a premise operation.
verb :: Y.Operation -> String
verb (Y.OpMorph _) = "morph"
verb (Y.OpNormalize _) = "normalize"
verb (Y.OpEvaluate _ _) = "evaluate"
verb (Y.OpContextualize _ _) = "contextualize"
verb (Y.OpDataize _) = "dataize"

-- The build-term arguments backing a premise operation.
verbArgs :: Y.Operation -> [ExtraArgument]
verbArgs (Y.OpMorph expr) = [ArgExpression expr]
verbArgs (Y.OpNormalize expr) = [ArgExpression expr]
verbArgs (Y.OpEvaluate expr universe) = [ArgExpression expr, ArgExpression universe]
verbArgs (Y.OpContextualize expr context) = [ArgExpression expr, ArgExpression context]
verbArgs (Y.OpDataize expr) = [ArgExpression expr]

leadsTo :: NonEmpty Rewritten -> String -> Expression -> ReduceContext -> IO (NonEmpty Rewritten)
leadsTo ((current, _) :| rest) rule expr ReduceContext{..} = do
  updated <- withLocatedExpression _locator expr current
  pure ((updated, Nothing) :| (current, Just rule) : rest)

-- Reduce 'expr' to its normal form through the normalization rewriter, embedding
-- it at '_locator' into the working expression taken from the head of the step
-- chain so the rewriter sees the surrounding context. Splices the individual
-- steps (alpha, copy, dot, …) into the chain and returns the normalized
-- expression together with the extended sequence.
normalized :: Expression -> NonEmpty Rewritten -> ReduceContext -> IO (Expression, NonEmpty Rewritten)
normalized expr seq ctx@ReduceContext{..} = do
  whole <- withLocatedExpression _locator expr (fst (NE.head seq))
  (rewrittens, _) <- rewrite whole normalizationRules (rewriteContext ctx)
  let (rw :| rws) = NE.reverse rewrittens
      seq' = rw :| rws <> NE.tail seq
  expr' <- locatedExpression _locator (fst rw)
  pure (expr', seq')
  where
    -- Switch the reduction context to a rewriting context for normalization,
    -- disabling the must-checker and breakpoints.
    rewriteContext :: ReduceContext -> RewriteContext
    rewriteContext ReduceContext{..} =
      RewriteContext _locator _maxDepth _maxCycles _depthSensitive _universe _buildTerm MtDisabled Nothing _saveStep

-- Name the world a run reduces in, where nothing has named it yet: the program
-- in normal form, which is what Φ denotes and what 'dot' compares a dispatched
-- formation against before it writes 'ρ ↦ Φ' (see '_universe'). Every frame of
-- 𝕄 and of 𝔻 asks, and only the first one of a run answers, since the context
-- travels down the recursion and what it names travels with it. The walk that
-- works it out is itself given no world, so it folds nothing while it is
-- deciding what the world is.
universed :: Expression -> ReduceContext -> IO ReduceContext
universed _ ctx@ReduceContext{_universe = Just _} = pure ctx
universed univ ctx = do
  (normal, _) <- normalized univ ((univ, Nothing) :| []) ctx{_locator = ExRoot, _saveStep = dontSaveStep}
  pure ctx{_universe = Just normal}

-- Bind 'expr' to a synthetic attribute of the universe and reduce it to a
-- normal form there, handing back the extended universe together with the
-- locator that aims at the binding. This is the trick phino has always played
-- to reduce a sub-expression that is not part of the program — the operand a
-- λ function names under 'dataize' or 'morph', above all — and it is also
-- the contract of the '--inside' option, so a caller asking phino to reduce a
-- part of the formation it was given does not have to splice it into the text
-- of the universe by hand. 𝔻 and 𝕄 accept normal forms only and an expression
-- handed in from outside is not necessarily one (a dispatch off a formation,
-- '⟦ x ↦ 6, ρ ↦ 5 ⟧.x', is not), so it is normalized against the extended
-- universe before either judgment sees it. The context comes back aimed at that
-- binding, so the caller hands the extended universe and the context it got
-- straight to 'dataize' or 'morph'. The site every firing is written under
-- moves with the aim, so a λ function fired while such a term is being reduced
-- is written under the synthetic binding it was bound to and not under whatever
-- the run around it was aimed at (see '_site').
insideUniverse :: Expression -> Expression -> ReduceContext -> IO (Expression, ReduceContext)
insideUniverse expr univ ctx@ReduceContext{_buildTerm = buildTerm} = case univ of
  ExFormation bds -> do
    (TeAttribute attr) <- buildTerm "random-tau" [] substEmpty
    let aiming = ctx{_locator = ExDispatch ExRoot attr, _site = ExDispatch ExRoot attr}
        synthetic = ExFormation (BiTau attr expr : bds)
    (normal, _) <- normalized expr ((synthetic, Nothing) :| []) aiming
    pure (ExFormation (BiTau attr normal : bds), aiming{_universe = extended attr normal})
  _ -> throwIO (userError "Can't reduce an expression inside a universe which is not a formation")
  where
    -- What Φ denotes inside the extended universe. Normalization works binding
    -- by binding, so the normal form of the extension is the normal form of
    -- the universe with the already-normalized term bound in front of it, and
    -- no second walk of the world is needed to name it (see '_universe'). A
    -- run that has not named its world yet leaves it unnamed here too, and the
    -- frame below works it out.
    extended :: Attribute -> Expression -> Maybe Expression
    extended attr normal = case ctx._universe of
      Just (ExFormation bds) -> Just (ExFormation (BiTau attr normal : bds))
      _ -> Nothing

-- Morph a term that is not part of the program, the way 'reduction' in
-- 'Dataize' dataizes one: bound to a synthetic attribute of the universe and
-- reduced there (see 'insideUniverse'), since 𝕄 takes normal forms only and an
-- operand taken out of a formation as it was written is not necessarily one.
-- This is what a 'morph' operand of a λ function is reduced with, and the
-- dataizing sibling of it reaches 'Dataize' through '_reduce'.
morphing :: Expression -> ReduceContext -> Expression -> State -> IO (Expression, State)
morphing univ ctx expr state = do
  (universe, aiming) <- insideUniverse expr univ ctx
  (morphed, _, state') <- morph universe state aiming
  pure (morphed, state')

-- Augment the injected, context-free term builder with the dataization and
-- morphing operations that need the universe: 'evaluate' fires a λ function and
-- 'morph' morphs a sub-expression. 𝔼 ('evaluate') takes the universe as an
-- explicit second expression argument, while 𝕄 ('morph') is handed the threaded
-- 'univ'. Every other function is delegated unchanged. This is the matcher's
-- condition path (guards in 'when'/'having'), which has no state to thread, so 𝔼
-- and 𝕄 run here on a fresh, empty state whose result is discarded; the
-- state-threading callers in 'sidePremise' use '_evaluate' and '_morph' directly.
execBuildTerm :: Expression -> ReduceContext -> BuildTermFunc
execBuildTerm _ ctx "evaluate" = \args subst -> fst <$> ctx._evaluate ctx emptyState args subst
execBuildTerm univ ctx "morph" = \args subst -> fst <$> _morph univ ctx emptyState args subst
execBuildTerm _ ctx func = _buildTerm ctx func

-- The Morphing function 𝕄 exposed as a build-term function so a rule can morph
-- a sub-expression in its 'where' (the 'md' and 'ma' rules morph
-- the head before re-attaching it). The step chain is discarded: the producing
-- rule splices the surrounding normalization steps itself, and a stuck λ met
-- on the way leaves without it (see 'unparked'). The state is threaded through
-- and the new state returned alongside the morphed term.
_morph :: Expression -> ReduceContext -> State -> BuildTermMethodS
_morph univ ctx state [ArgExpression expr] subst = unparked $ do
  built <- buildExpressionThrows expr subst
  ((morphed, _), state') <- morph' (built, (univ, Nothing) :| []) univ state ctx
  pure (TeExpression morphed, state')
_morph _ _ _ _ _ = throwIO (userError "Function morph() requires exactly 1 expression argument")
