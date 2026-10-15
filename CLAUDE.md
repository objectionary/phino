<!--
SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
SPDX-License-Identifier: MIT
-->

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Repository

A single `origin` remote points at `objectionary/phino`.
Never commit directly to `master`. Always branch from an up-to-date `master`.

## Commands

```bash
make test          # cabal test --ghc-options=-Werror
make compiled      # phino compile + the whole suite on the compiled rules
make hlint         # hlint src app test
make fourmolu      # --mode check src app test (2-space indent, leading commas)
make ruff          # ruff check + ruff format --check over the whole tree
make coverage      # cabal test --enable-coverage + hpc-codecov (threshold 65%)
make bench         # prepare resources + cabal bench --enable-benchmarks
make binary        # build stripped release binary into dist-release/phino[.exe]
make all           # coverage + hlint + fourmolu
```

Run a single test by matching against describe/it text or the module name
prefix:

```bash
cabal v2-run spec -- --match "parse bytes"
cabal v2-run spec -- --match "Rewriter"
```

Benchmarks run 3 warmup iterations, auto-calibrate batch size to hit a
~20ms measurement window per batch, then run 10 batches and print total,
avg, min, max, and std dev per operation. Batch size scales automatically
so the same benchmark works for both tiny and large inputs; a case whose
single run outlasts the wall-clock budget of `benchmark/Main.hs` gets fewer
warmups and fewer batches instead, never fewer than one batch, so the whole
suite still fits the job the workflow runs it in. Resource files
(`benchmark/tmp/`) are generated on first run and cached by Make; removed
by `make clean`. Requires Java and curl; Maven is fetched automatically
via `benchmark/mvnw`.

Besides parsing, printing and rewriting, the suite morphs symbolically:
`benchmark/demo.phi` is a small world whose entries name the λ functions of
`benchmark/atoms.yaml`, one case per entry, and the smallest of them is timed
twice — over the demo world alone and over the same world merged into
`native.phi` — so the cost of the world around an entry is a number too
(#1291). Both files are checked in, unlike the generated ones under
`benchmark/tmp/`.

The loop of #1453 is timed too: `benchmark/accum.phi` over the table
`benchmark/accum.yaml`, cut by `--acyclic=plausible`, once as written and once
with 400 methods the loop never calls added to `number` at runtime, as
`morph/symbolic/accum/0` and `morph/symbolic/accum/400`. A step should cost the
redex and not the objects around it, so the two should stay close; the
regression check reports every case ending in a number as a ratio to its `/0`
sibling, on both sides, under "Scaling".

## Architecture

`phino` is a CLI tool for manipulating phi-calculus (𝜑-calculus) expressions
— the formal foundation of the EO programming language. Four Cabal
components: `library` (`src/`), executable `phino` (`app/`), test suite
`spec` (`test/`), benchmark suite `bench` (`benchmark/`).

### Seven CLI commands

`rewrite` | `dataize` | `morph` | `explain` | `merge` | `match` | `compile` —
all wired in `src/CLI/Runners.hs`, parsed in `src/CLI/Parsers.hs`.

### Two-phase rendering pipeline

`AST -> CST -> Text`. `AST.hs` holds the semantic tree (`Expression`,
`Binding`, `Attribute`, `Bytes`). `CST.hs` holds the formatting
tree (every token is a typed constructor). The `Render` typeclass converts
CST to `Text`. This keeps layout concerns out of the semantic layer.

### Sugar/Salty duality

`SWEET` (default) uses compact syntax sugar; `SALTY` expands to canonical
phi-calculus. The `ToSalty` typeclass in `Sugar.hs` transforms CST nodes
between these representations before rendering.

### Pattern-based rewriting

YAML rule files (`resources/<judgment>/<rule>.yaml`, plus user-supplied via
`--rule`) each define a `pattern`, `result`, optional `when`/`having`
conditions, and `where` extensions. Every judgment keeps its rules in its own
directory — `normalize/`, `morphing/`, `dataization/`, `contextualization/` —
one rule per file, named after the rule, and the whole directory is compiled in
via `embedDir` of `file-embed`.
`Contextualize.hs` runs the rules of `contextualization/` as 𝒞, the way the
engines run the other three sets, and fails on a term no rule or more than one
rule matches (#1618).
Matching (`Matcher.hs`) produces `[Subst]` — a list of
`Map Text MetaValue` — and conditions filter that list. `Builder.hs` then
applies a substitution to a result template.

### Compiled rules

`phino compile` (#1617) writes `compiled/generated/Compiled.hs`, which git
ignores: `Emit.hs` turns every rule of normalization, the built-in ones and
those of `--rule`, into a list comprehension over the term it may match as a
whole, 𝒞 into one function with an equation per rule, built on `concluded` of
`Contextualize.hs`, and every rule of 𝕄 and 𝔻 into a list comprehension over
the term and the universe (#1628). What runs the rules is an `Engine`
(`Engine.hs`): the steps of normalization, the test of which of them match a
term, a step per compiled rule keyed by the `show` text of the rule, the
normal-form test, 𝒞, and the rules of 𝕄 and 𝔻. `yaml` interprets the rules;
the module `Compiled` holds the other one, and the Cabal flag `compiled` picks
its source folder, `compiled/stub` (where `compiled = Nothing`) or
`compiled/generated`. Only the CLI (`engine` in `CLI/Helpers.hs`) and the
specs reach for `Compiled`; the library gets the engine through `_engine` of
`ReduceContext`, `_normal` and `_matching` of `RewriteContext`, `_normal` of
`RuleContext`, and `building`, which routes `contextualize` to the engine.

A rewriting step is a `Step` of `Rule.hs`, a name and a function. `interpreted`
of `Rewriter.hs` wraps the matcher and the replacer; `direct` wraps a compiled
function, finds the places it matches at with `sites` of `Matcher.hs` in the
order the deep matcher finds them, and hands them to the same
`replaceExpression`, so the chain of steps is the same with either engine.
`rewrite'` tries only the steps `_matching` names, asked again only once a step
changed the term. The compiled engine finds them all in one walk over the term
(`hits` of `Matcher.hs`) and builds its normal-form test on the same walk; the
interpreted engine and the `rewrite` command name every step (`every` of
`Rewriter.hs`), so each rule is tried as before (#1643). A rule the generated
code could not run that way is refused with the reason: `having`, a `where`
function other than `contextualize` and `named`, `matches`, `part-of`, a rule
of the fast shape (`fast` of `Rewriter.hs`), and a pattern applying Φ to a ρ. A
compiled engine carries the texts of the built-in rules it was made from and
the CLI refuses it once they changed (`fresh`). `CompiledSpec.hs` runs both
engines over random terms and compares the chains, the normal-form test and 𝒞,
and the chains 𝕄 and 𝔻 make of an object of random programs; it also checks
that the compiled test of which rules match never misses one.

A rule of 𝕄 or 𝔻 is an `Inference` of `Inference.hs`: matched against a term
and a universe it answers the `Premises` it runs beside its spine, in order —
`Morphs`, `Evaluates` or `Contextualizes`, each a function of the answer it is
handed — ending in a `Conclusion`: `Answered`, or `Onward` in a `Way`
(`Taken`, `Normalized`, `Named`, `Staged`) the judgment is asked again in.
`morphingSpine` and `dataizationSpine` read the spine off a rule for both
engines; `morphingOf` and `dataizationOf` interpret a rule, `direct` wraps a
compiled one, and `inferred` and `onward` of `Morph.hs` run either, so the
chain is the same with either engine. A rule running anything else beside its
spine, or concluding with no `morph` or `dataize` premise, is refused.

### λ functions live outside the binary

`phino` implements no λ function. `Lambdas.hs` reads them from the YAML file
the `--symbolic` option names, one entry per function: a `λ` key, a regular
expression over λ names; the operands brought down to data through 𝔻 under
`dataize`, each binding a bytes meta `𝛿1`; the operands reduced to a normal
form through 𝕄 under `morph`, each binding an expression meta `𝑛1`; the lines
under `evaluate`, each firing through 𝔼 the term a `morph` meta or an earlier
line is bound to and binding a meta of its own to the answer (`evaluated` of
`Evaluate.hs`, #1874); and the answer under `𝑛`. An `evaluate` line leaves a
formation with no λ and ⊥ as they are, never fires a terminator of its entry,
and never fires its answer again, so another call takes another line. The
lines of `evaluate`, `rewrite`, `stay`, `symbolize` and `join` are one list,
`_lines` of `Lambda`, which `ordered` of `Lambdas.hs` sorts by the metas each
line reads, keeping the order of the blocks where no meta decides, and refuses
a line reading what nothing binds or what it binds itself (#1903). No two
keys may match one λ name, whether or not either
spells it: `Language.hs` reads each key as an automaton and looks for a name
both accept, refusing a key beyond a regular language, such as a back
reference (#1440). Firing an entry is 𝔼's business and lives in `Evaluate.hs`,
which reaches the judgments an operand is reduced with through `Morph.hs`,
using the same `insideUniverse` trick the `--inside` option exposes.

An entry may name `terminators`, λ functions no key of the file matches, such
as the `L_throw` EO raises with. A `join` line takes a branch that is a
formation with one of them, or an application of one, for a branch that
raises, as it takes ⊥ (`thrown` of `Lambdas.hs` and `paired` of
`Evaluate.hs`, #1865). The `terminate` record names the `message` the branch
raises with, as written (`message` of `Lambdas.hs`), so the error path can be
lowered too (#1867).

A rule of a `rewrite` line may not write a `𝜎` into its result, but its
`where` may call `remint`, which only `reshaped` of `Evaluate.hs` knows: it
mints a fresh symbol for the term it is given, answers the bare
`⟦ λ ⤍ 𝜎k ⟧`, and writes `reminted(…)` (`EvReminted`) with the symbols the
term holds and the term itself, so a large term becomes a small one made of
symbols, each of which the protocol explains (#1909).

A bare symbol, `⟦ λ ⤍ 𝜎k ⟧`, joins too: the `join` of `Lambdas.hs` pairs it
with the symbol the φ chain of the other branch ends in, and answers a bare
fresh symbol, dropping the methods. An entry naming a `lenient` list, empty or
not, is lenient: `_lenient` of `Lambdas.hs` holds the options of the list and
refuses a name it does not know (#1913). Under it a chain ending in no
symbol, such as one stopping at a call of an object of the world, still joins
if the other branch holds exactly one symbol (`goSole` of `joined`, #1817):
the answer is a copy of that branch with the symbol replaced by a fresh one,
and nothing outside the two branches is read. When that fails too, a lenient
entry morphs the φ of a branch that is a formation with no λ whose φ reads ξ,
fires what it comes to through 𝔼 the way an `evaluate` line does, and joins
once more (`reduced` of `paired` in `Evaluate.hs`, #1885). The step is taken
once, and it fires through `fire`, as an `evaluate` line does, so the step
budget and `--acyclic` bound it. Under `lenient: [defer]` (`Defer` of
`Lambdas.hs`) the step never enters a copy, a formation with no λ, no void
and code in φ, one of whose arguments other than φ and ρ reads to a bare
symbol: `deferred` of `Evaluate.hs` reduces every argument with no entry of the
file, and once one comes to `⟦ λ ⤍ 𝜎k ⟧` it answers a fresh symbol instead and
writes `deferred(…)` (`EvDeferred`), naming the call the copy stands for by the
object of the world it was made of, the way the record of 0.0.153 did (#1892).
A copy the step does enter gets a fresh symbol too once `--acyclic` refuses
the firing its φ comes to: `fire` asks `admitting` of `Morph.hs` and hands a
refusal to the function it is given, `refused` for an `evaluate` line and for
a step that does not defer, `severed` for one that does, which writes the
symbol and the call on the `looped(…)` line (`EvLooped`, #1895).

An entry answers, it never computes: the answer carries a symbol `𝜎` standing
for a value nobody worked out, minted fresh per firing and counted by `_minted`
of `ReduceContext`. The count is an `IORef`, as the tally of `--max-firings`
is, since a parked frame hands back the state it started from, and a count kept
there would hand out again the symbols its protocol records already spell
(#1738). `started` of `CLI/Helpers.hs` sets it past the symbols the program
carries. Dataizing a symbol answers a fixed 42, so a `𝛿` always
holds data. A λ name no entry answers gets stuck, and so does a λ naming a
symbol, since nothing answers that either; that is what `--partial`
parks on, and the protocol records it as `unanswered(name)` either way. What
fired is written as a tree by `--protocol`
(`Evaluation` and `Protocol` in `Deps.hs`), which spells the three judgments
the way the calculus does: `𝕄(…)` or `𝔻(…)` opens the run, `𝔼(…)` names a
firing and `𝔻(𝜎1)` is the datum manufactured for a symbol. Every comment of a
line is a judgment applied to a term — the intent the value beside it came
from: the judgment that asked for a firing and the site it was fired at
(`_site` of `ReduceContext`), and the one that reduced an operand and the term
it was reduced from. A line no judgment made — a `symbolize` or a `join` one —
names a meta of the entry instead. The extension of the file picks the format:
`.xml` gets markup (`Nesting` and `saveEvalXml`), anything else the indented
text. Both carry the same facts and both stream, so
a run that fails still leaves a complete file; the markup references a value by
the symbol it denotes and spells it `𝜎1` as everywhere else, where the text
names a line by counting.

An application writes a line too, `𝑛·1·1 := …` (`EvApplied`, #1745). A `Step`
answers the term it made and the pairs of redex and result it replaced, and
`rewrite'` hands every pair where an application became a formation other than
its head, which of the built-in rules only `copy` does, to `_saveMade` of
`RewriteContext`, except one filling ρ, which a dispatch makes. `normalized` of
`Morph.hs` writes it with the judgment and the site, spelling the call as the
head, named by `nameIn` of `Builder.hs`, applied to its argument; the `rewrite`
command saves nothing (`dontSaveMade`). The name of the object is counted with
`built` and `answer` of the innermost open firing, which both writers keep as
a stack every record prunes by its depth (`tier`), and from then on
`abbreviated` writes the object, and the call too, since `copy` makes the same
object of the same call, by that name wherever a later line holds it whole.
The line itself goes through `abbreviatedInside`, which names the head and
each argument and never the call as a whole, so an application made again is
spelled out again (#1756). The
markup writes the head in `of` and the argument in an `<attr>`: its name, the
symbol where it is a bare one, or the term. Under `--inside` the universe is
normalized before the heading, so `aimed` of `CLI/Helpers.hs` holds those
records back and writes them after it.

### Dependency inversion for circular imports

`Deps.hs` exists solely to break the cycle
`Dataize -> Functions -> Rewriter -> Dataize` via the `BuildTermFunc`
type alias.

### Morphing, dataization and evaluation

One module per judgment: `Morph.hs` implements the formal Morphing (M)
function, `Dataize.hs` the Dataization (D) one and `Evaluate.hs` the
Evaluation (E) one, with named rules: PRIM, NMZ, LAMBDA, PHI (morphing) and
DELTA, BOX, NORM (dataization). `Morph.hs` also holds what all three judgments
share — the context, the step budget, the signals and the premise plumbing —
so `Dataize.hs` and `Evaluate.hs` import it and nothing points back. Nothing
but two edges, each injected into the context rather than imported, the way
`Deps.hs` injects `_buildTerm`: an atom asking phino to reduce an operand of
its own is a whole run of D, so it goes in as `_reduce` (a `ReductionFunc`),
and E is reached as `_evaluate` (an `EvaluationFunc`, what an `evaluate`
premise fires and what answers a normal form).
All configuration is threaded through
`ReduceContext` and `RewriteContext` records — no global state. Each
function has a top-level wrapper that locates the subterm and starts the
chain (`morph`, `dataize`) and a recursive worker the rules drive
(`morph'`, `dataize'`); the `morph` and `dataize` commands enter through
the wrappers.

The step budget `--max-steps` is what ends that recursion, and the only thing
that does unless `morph` or `dataize` is given `--acyclic=<mode>`: the option
has every 𝕄 and every 𝔻 frame remember the formations its ancestors have
entered (`entering` and `enter` in `Morph.hs`, one store for both judgments,
since only `box` and `fire` of 𝔻, `ml` of 𝕄 and an `evaluate` line, `fire` of
`evaluated` in `Evaluate.hs`, enter one, and the line spends a step of
`deeper` as a frame does, #1882) and park the site the moment a frame is about
to enter one of them again, the way `--partial`
parks a λ that cannot
fire. The mode `Acyclic` of `Deps.hs` says what "again" means, and there is no
default. Under `proven` two formations are the same up to a renaming of their
symbols (`alike` in `AST.hs`), since a recursion over a symbol mints fresh ones
every round and never repeats a term (#1420). Under `plausible` the earlier one
is embedded in the later one (`within` in `AST.hs`), so a recursion whose
accumulator gains a wrapper every round is cut too, at the price of cutting
now and then one that would have stopped (#1451). `--max-loops=<n>`
(`_maxLoops` of `ReduceContext`) softens either mode: `enter` cuts a frame only
once more than `n` of the formations above it are the same as its own, so a
recursion stopping within `n` rounds, such as the `or` inside `abs` of EO
reaching `or` once more, gets through (#1889). A judgment started at a site
opens a block of the protocol, `𝔻(…):` or `𝕄(…):` with the site (`EvStarted`,
written by `opening` of `Morph.hs`), and what it does stands under it, unless
the block it stands in is the same judgment at the same site (`_opened` of
`ReduceContext`). Both writers print a block only once a line stands in it,
merge two sibling blocks of one heading, and drop the comment of a line that
repeats the innermost block; a firing is a block with no heading, so a line in
it keeps its comment. The morphing of an answer is named after the answer
(`resited`), as `𝕄(𝑛·1·5):`. The datum `delta` finds closes a `𝔻` block as
`𝛿·1·1 := …` (`EvDelta`), commented with the double eight bytes hold (`ieee`,
the `number` of `<delta>` in the markup), and the operand line right after
names it. A cut is a line too, `looped(…)`
(`EvLooped`), written by `enter` where the refused frame would have opened,
carrying the formation its ancestor entered (#1434) and naming the mode that
cut it.
Four records say why a firing gave no answer (#1524): `stuck(…)` closes a
firing that got stuck, `unjoined(…)` closes in its place one whose `join` line
could not pair its branches, carrying both, `stall(…)` stands under one the
memo answered with a kept stall, and `starved(…)` is written by `deeper` where
the step budget runs out, with the site it stood at rather than its term
(#1531). `both` of `paired` in `Evaluate.hs` writes `unjoined(…)` and throws
`Unjoined`, which the firing it stands in turns back into `Stuck` once it has
kept the stall, so `--partial` parks it as before (#1900).

`--max-steps` bounds the depth of one branch, so a recursion that widens the
term instead of nesting it fires forever inside it. `--max-firings` bounds the
firings of the whole run (`Tally` and `charged` in `Morph.hs`, charged by
`symbol` in `Evaluate.hs`), off unless given. Its count is an `IORef` in the
context rather than a field of `State`, since a parked frame hands back the
state it started from and would refund the firings made inside it (#1472).
`--max-seconds` bounds the time of the whole run the same way (`Deadline` and
`clocked` in `Morph.hs`, off unless given), read by `deeper` at every step and
by `charged` at every firing, since a run may spend its time rewriting terms
that fire nothing (#1619), and by `enter` while it compares formations, since
under `plausible` one comparison by `within` may outlast the whole run, so a
timer cuts that comparison where it stands (#1622). The first step, firing or
comparison after the deadline writes `timeout(…)` (`EvTimeout`) and throws
`OutOfTime`, which no frame parks, so the run fails with or without
`--partial` and a caller that cannot wait gets a closed protocol ending in the
timeout instead of killing the process (#1607).

Every use of a binding copies the term bound to it, so one formation is fired
as many times as the program reads it, and every firing mints symbols of its
own. A run under `plausible` keeps what every firing answered, by the
formation it fired (`Memo`, `recalled` and `retained` in `Morph.hs`, made by
`memoized` for that mode alone and asked and told by `symbol` in
`Evaluate.hs`), and a later firing of the same formation, ρ and all, takes
the answer with the symbols the first one minted: it is charged nothing and
reduces nothing, and it is written to the protocol as a firing at its own
site carrying that answer and no operand line (#1476). The memo keeps the
answer by the firing too, the λ name with the data, the symbols and the normal
forms its operands came down to (`Firing`, `remembered` and `remember` in
`Morph.hs`), since an entry answers from its operands alone: a firing of
another formation, such as one whose operand reads another object holding the
same datum, brings its operands down, finds the firing answered and takes
that answer, minting nothing, so one value never gets two names (#1661). A
symbol is kept as the symbol it is, not as the datum every symbol
manufactures, so two firings over two symbols stay two. Such a firing is
charged to `--max-firings` like one made, since the tally charges a firing as
it starts bringing its operands down, the only place a recursion widening
inside its operands can be cut, and a recalled answer counts as nothing new
to a stall. The store is an
`IORef` in the context for the reason the tally is. A firing a cut stopped on
its way to an answer keeps the cut (`Looped` of `Kept`), and a later firing of
the same formation is cut at its own site without reducing anything (#1480). A
firing that got stuck keeps the λ function it got stuck on (`Stalled` of
`Kept`), and a later firing of the same formation gets stuck at its own site
the same way (#1493), but only while the memo has answered nothing since the
stuck firing began, as the count of answers kept beside the stall tells: an
operand that could not be brought down may come down once something new was
answered, inside the stuck firing or after it (#1495, #1507). A firing inside
which the step budget ran out keeps its stall beside the steps it had spent
(`starved` in `Morph.hs`), and the memo tells it only to a firing that has
spent at least as many, since the same formation fired at a shallower site may
bring the operand down (#1514, #1521).

### Test pattern: YAML packs

Most spec files load test cases from `test-resources/*-packs/*.yaml` at
runtime via `runIO`. Each pack defines `input`/`output`/`rules`/`skip`.
Test files map 1:1 with source modules (`RewriterSpec.hs` ↔ `Rewriter.hs`).

### Style

When introducing new function, put it under 'where' scope if it's possible.
Always declare an explicit type signature for every function, including
helpers under 'where'.
Don't use single char variables like 'a', 'b'; use more meaningful names.
If two functions does the same but have different amount of arguments - name
them with apostrophe: foo, foo':
Name an internal, 'where'-bound recursive worker 'go' (or 'goThing' when
several coexist in one scope); reserve the apostrophe variant for genuine
sibling functions, not hidden loops.
