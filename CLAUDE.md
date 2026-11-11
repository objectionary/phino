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
make coverage      # cabal test --enable-coverage + hpc-codecov (threshold 80%)
make bench         # prepare resources + cabal bench --enable-benchmarks
make binary        # build stripped release binary into dist-release/phino[.exe]
make all           # test + hlint + fourmolu + ruff
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
formation with no λ, a bare symbol `⟦ λ ⤍ 𝜎1 ⟧`, which no entry answers
(#2036), and ⊥ as they are, and never fires its answer again, so
another call takes another line. It leaves as it is a formation binding an
attribute other than ρ, a name ending with 🌵 or a void of the first object
the facts lead it back to (`voided` of `Morph.hs`), such as the `tag` of a
copy of `box(v) ↦ ⟦ tag ↦ ξ.v, λ ⤍ L_box ⟧`, since its λ stands for its φ
alone and a later dispatch on `tag` must find it (`owned` of
`Evaluate.hs`, #2265). A formation no fact holds, such as one written in place as
`⟦ v ↦ ∅, λ ⤍ L_box ⟧( v ↦ 𝜎2 )`, holds no attribute of its own, so the
line fires it (#2267). Any
other term, such as one `--partial` parked, gets the line stuck, unless the
entry joins, where `kindled` of `Evaluate.hs` passes it on as it is, since a
join compares any two terms (#1999). A `stay` line binds its meta to the term
another meta is bound to with every `( ρ ↦ … )` applied to a dispatch dropped
(`stayed` of `Lambdas.hs`, #1967), which `stay` or `skip` of normalization
drops anyway. A `symbolize` line binds its meta to the term another meta is
bound to with every datum it holds in a φ, an argument, a dispatch or any
binding of an atom turned into a fresh symbol, leaving alone the ρ and the
methods of a formation holding no λ, since they hold the code of a library
object, not its value (`symbolized` of `Lambdas.hs`, #1968, #2008, #2131), and
writes `symbolized 𝜎4 := …` (`EvSymbolized`) per symbol it minted with the
datum it stands for. A line of either that changed its term writes
`# stay(𝑛1)` or `# symbolize(𝑛1)` to the text protocol (`EvReshaped`), and the
markup binds it as any other. A
`rewrite` line holds `of`, the meta it reads, and `rules`, spelled as in a
rule file and read as `Rule` of `Yaml.hs`, and binds its meta to that term
with the rules applied (`rewritten` of `Lambdas.hs`, #2005): every position
is tried, the outermost first, the first rule matching it rewrites it, what
a rule made is not walked into again, and nothing is normalized. A position
no rule changed is handed back as the very term it was, so a term sharing its
parts, as the formation a join fabricates does, stays shared (#2196). A rule
writing a symbol such as `𝜎4` into its result is refused, and a line that
changed its term writes `# rewrite(𝑛1)` (`EvReshaped`) as `stay` does. An
operand of `morph` naming a meta alone, such as `𝑛4: 𝑛3`, is a line too
(`Morphed` of `Line`, `reduced` of `Evaluate.hs`, #2016): it morphs the term
the meta is bound to through `morphing`, as it stands and not read against the
formation fired, and writes `# 𝕄(𝑛3)`, while `_morphed` keeps the operands
reading `$` alone, which run before every line. An operand of `dataize`
naming a meta alone, such as `𝛿2: 𝑛1`, is a line the same way (`Dataized` of
`Line`, `lowered` of `Evaluate.hs`, #2139): it brings the term down through
`_reduce`, as it stands, and writes `# 𝔻(𝑛1)`, while `_dataized` keeps the
operands reading `$`, which alone make the `on` of a join and the data of
`<minted>`. An operand of `undress`, a path from `$` or a meta alone
(`_undressed` of `Lambda`, `Undressed` of `Line`, `peeled` of `Evaluate.hs`),
is morphed as one of `morph` is, and then, while the term is a formation
binding φ alone beside its ρ, gets φ dispatched off it and morphed again,
never its body taken out, so ξ keeps its meaning (#2174). Every pass spends a
step of `deeper` and writes `undressed 𝑛·1·5 := 𝑛·1·4.φ` (`EvUndressed`),
counted and named as `boxed` is, so the 𝕄 after it is named after that meta,
and an operand a pass changed writes `# undress(…)` (`EvReshaped`). The
operands reading `$` join the key of the memo as those of `morph` do. A
`cast` block holds exactly two lines, read as one line binding both metas
(`Cast` of `Line`, #2187). Where one of its two terms is a bare
`⟦ λ ⤍ 𝜎1 ⟧` and the other is a formation binding φ, whatever else it
holds, `cast` of `Lambdas.hs` copies the other with `𝜎1` in place of the
term its φ chain ends in, the first one down the chain that is no formation
binding φ, such as another fork (#2223), and `casting` of `Evaluate.hs`
binds the meta of the bare symbol to that copy and the other meta to its
term as it is, so a join sees a branch that reached its data and one a
decorator short of it differ only where that chain ends, and joins them or,
under `otherwise`, fabricates a formation of them. The cast mints no
symbol: it writes the meta it bound
as `cast 𝑛20·1 := …  # 𝑛14·1 up to 𝑛13·1` (`EvCast`), naming the meta it
read and the meta whose shape it took, and the markup as `<cast>`. A line
of `evaluate`, `defer`, `stay`, `rewrite`, `symbolize`, `cast` or `undress`
reading a meta that leaves its term as it was writes nothing (`EvBound`), in
the text and the markup alike, and a later record naming that meta, in a site,
a comment, a cast or a side of a join, names the term the meta holds instead
(`_unwritten` of `Protocol`, `_unmarked` of `Nesting`, #2192). The
lines of `dataize`, `morph`, `undress`,
`evaluate`, `defer`, `stay`, `rewrite`, `symbolize` and `cast` are one list,
`_lines` of `Lambda`, which `ordered` of `Lambdas.hs` sorts by the metas each
line reads, keeping the order of the blocks where no meta decides, and refuses
a line reading what nothing binds or what it binds itself (#1903). No
two keys may match one λ name, whether or not either
spells it: `Language.hs` reads each key as an automaton and looks for a name
both accept, refusing a key beyond a regular language, such as a back
reference (#1440). Firing an entry is 𝔼's business and lives in `Evaluate.hs`,
which reaches the judgments an operand is reduced with through `Morph.hs`,
using the same `insideUniverse` trick the `--inside` option exposes.

An entry may list under `terminators` the λ functions that only raise, such
as the `L_throw` of EO, which no key of the file may match (#2263). A line
reading a meta bound to a formation holding one of them, or to an application
of one (`thrown` of `Lambdas.hs`), binds its own meta to that term as it is,
never firing or walking it, and `merged` of `Evaluate.hs` hands such a side
to `joined` as ⊥ when the other side does not raise, so the `joined` record
names it under `raised`. A firing hands its terminators to every 𝕄 it runs
in (`_raising` of `ReduceContext`), which answers a dispatch off a formation
holding one of them with that formation, never firing it, so a side such as
`Φ.throw( 𝜎2 ).q` raises the way `Φ.throw( 𝜎2 )` does (#2308). An operand
of `dataize`, a path from `$` or a meta alone, that holds one of them makes
the firing answer ⊥ (`raising` of `Evaluate.hs`, `Raised` of `Answer`),
never firing the terminator, since data that comes from a raise is the same
raise, so a join takes such a side for one that raises (#2330).

An entry ends in an `𝑛` or in a `join` block (`Ending` of `Lambdas.hs`),
never both (#1938), and a joining entry may hold every other block, `evaluate`
among them (#1999). The block names the condition under `on`, a bytes meta of
`dataize` or a path from `$`, and the two sides under `left` and `right`, each
a path from `$` or a meta `morph`, `undress`, `evaluate`, `stay`, `rewrite`,
`symbolize`, `cast` or `defer` binds.
`merged` of `Evaluate.hs` first takes the condition (`weighed`), its bytes or
the symbol they were manufactured for: a meta as its `dataize` operand came
down, and a path off the formation fired, walking its bindings with no 𝔻, so
a firing of the `L_join` below brings no condition down again; a path leading
to neither a datum nor a symbol gets the firing stuck. It then brings
both sides down (`reached`): it morphs
a path where it stands in the universe through `insideUniverse`, which moves
the locator alone, so the protocol sites it as the side, such as `ξ.left`
(#2145), and takes a meta as it is bound. The join
defers nothing by itself: an entry that wants a side deferred says so with a
`defer` line, which works as in an entry that computes (#1982). Two sides
that are the same, or differ only in the symbols at their leaves, are one
term: `joined` of `Forks.hs` mints a fresh symbol per pair of symbols, the
firing writes `joined …` (`EvJoined`) per fresh symbol, naming a condition read
off a path by that path, as `ξ.on🌵`, and answers the joined term (`Joined`
of `Answer`, written by `EvTaken`) with no decision (#1954).
Where one side is ⊥ and the other holds a symbol, `joined` keeps the shape of
the other side, mints one symbol to stand for ⊥ and a fresh symbol per symbol
the other side holds, paired with it, so the firing writes
`joined 𝜎4 := 𝛿1·1 ? 𝜎2 : 𝜎3  # where 𝜎3 is ⊥` and the markup
`right="𝜎3" terminator="𝜎3"`, naming the side that raises (#2195); a side holding
no symbol has nothing to stand for ⊥ and does not join.
Two formations of other shapes, neither holding a datum nor a λ (#2041), are
joined attribute by attribute when the block names a λ function under
`otherwise` (#2024):
`forked` of `Forks.hs` builds a formation leaving void every attribute either
side leaves void and holding under `left🌵` and `right🌵` each side applied to
its own voids, as `𝑛1( x ↦ ξ.x )`, so an argument handed to the formation
reaches the side while the side keeps the ρ it binds (#2029). It binds every
other attribute of both sides but ρ, λ and a name ending with 🌵 to a copy of
that function, whose `on🌵` holds `⟦ Δ ⤍ 𝛿1 ⟧`, the bytes of the condition, or
`⟦ λ ⤍ 𝜎k ⟧`, the symbol it came down to (#2026), and whose `left🌵` and
`right🌵` ask the copies of the sides for the attribute through its void ρ,
as `ρ.left🌵.a`. The 🌵 mark keeps
these names apart from any attribute of a side, such as the `left` and
`right` voids of the `if` of a bool in EO, since no program spells it
(#2027). A copy leaves void every void of the formations the sides bind its
attribute to, the left side's first, and asks a side binding the attribute to
a formation for it applied to the voids of that formation alone, as
`ρ.left🌵.a( x ↦ ξ.x )`, and any other side for it applied to every void, so
a method of the fabricated formation takes its arguments (#2031). An
attribute both sides bind to the same `ξ.f`, where `f` is an attribute of
either side, stays that alias, so `g ↦ ξ.f` reaches the copy of `f` and its
voids (`aliased`, #2260). The firing writes
`fabricated 𝑛·2·8 := 𝑛1·1 and 𝑛2·1  # ⟦ … ⟧` (`EvFabricated`), naming the
sides as `diverged` does and spelling the formation out in the comment, and
answers that formation as `Joined` of `Answer`. The entry of `L_join` reads
`on: $.on🌵`, so the copy it fires decides by that condition.
Any other two sides make the firing write `diverged … vs …` (`EvDiverged`),
naming each side by the line that bound it. Its comment tells the two steps
apart (#2043, #2204): `cannot join:` says what each side binds where they
differ, as in `at .λ the left side has λ ⤍ 𝜎2 and the right side has no λ`,
or what each side is where they differ at the top, and `cannot fabricate:`
says why nothing was fabricated of them, as in `the left side is an atom`.
`lodged` of `Deps.hs` walks a side down the path to what it binds there, and
the markup writes that inside `<diverged>` as `<left λ="𝜎2"/><right/>`, the
way `<formed>` writes a binding, or each whole side as `<left>Φ.k</left>`
where the path is empty. `joined` answers the
path of attributes through the formations both sides share down to the first
one they bind differently, cut short at an application or a dispatch, and
`forked` answers a `Refusal`, `Unformed` or `Holding` a Δ or a λ, the left
side first, while `merged` of `Evaluate.hs` hands over `Unnamed` for a block
with no `otherwise`. `forked` answers `Unripe` too, the left side first, where
one side leaves void an attribute the other one binds, since that side is
applied further and a formation leaving the attribute void would hand a void
to whoever takes it there; the firing writes it as
`unripe 𝑛1·2 vs 𝑛2·2  # k is void on the left and attached on the right`
(`EvUnripe`) in place of `diverged`, and the markup as
`<unripe left="𝑛·2·1" right="𝑛·2·2" at="k"/>` (#2202). The firing then
throws `Diverged`, which the
firing rethrows as `Stuck`, so `--partial` parks it and the firings around
it write `stuck` as for a λ function no entry answers. A side or an operand
whose morph throws fails the firing with what it threw, so a run never
splits and prints one result (#1996). It does so under `--partial` too:
`morphing` of `Morph.hs` runs the operand through `morph'` and parks nothing,
so no later block runs on a term that was never morphed, and `--partial`
parks the firing as a whole (#2014).

A `defer` line, `𝑛3: 𝑛1` (`Deferred` of `Lambdas.hs`, `deferred` of
`Evaluate.hs`, #1933), looks at a copy, a formation with no λ, no void and
code in φ, whatever its φ reads: it reduces every argument other than φ and ρ
with no entry of the file, and once one comes to `⟦ λ ⤍ 𝜎k ⟧` it binds a
fresh symbol and writes `deferred …` (`EvDeferred`), naming the call the copy
stands for by the path of the world the facts lead it back to (`declared` and
`called` of `Morph.hs`), the way the record of 0.0.153 did (#1892); any other
term is bound as it is. Before the line is
written, `argued` of `Morph.hs` works out every argument of the call that
is no bare symbol: it morphs the argument in the copy, at a site such as
`𝑛2.index`, fires what that comes to through 𝔼 the way an `evaluate` line
does, and writes every step to the protocol, so the markup names the argument
by the object an application made of it, such as `𝑛·77·3`. An argument whose
morph or firing gets stuck, is cut or runs out of steps gets the firing stuck
at once, as an operand does (#2014).

An entry answers, it never computes: the answer carries a symbol `𝜎` standing
for a value nobody worked out, minted fresh per firing and counted by `_minted`
of `ReduceContext`. The count is an `IORef`, as the tally of `--max-firings`
is, since a parked frame hands back the state it started from, and a count kept
there would hand out again the symbols its protocol records already spell
(#1738). `started` of `CLI/Helpers.hs` sets it past the symbols the program
carries. Dataizing a symbol answers a fixed 42, so a `𝛿` always
holds data. A λ name no entry answers gets stuck, and so does a λ naming a
symbol, since nothing answers that either; that is what `--partial`
parks on, and the protocol records it as `unanswered name` either way. What
fired is written as a tree by `--protocol` and `--minutes`
(`Evaluation` and `Protocol` in `Deps.hs`), which spells the three judgments
the way the calculus does: `𝕄(…)` or `𝔻(…)` opens the run, `𝔼(…)` names a
firing and `𝔻(𝜎1)` is the datum manufactured for a symbol. Every comment of a
line is a judgment applied to a term — the intent the value beside it came
from: the judgment that asked for a firing and the site it was fired at
(`_site` of `ReduceContext`), and the one that reduced an operand and the term
it was reduced from. The option picks the format, whatever the file is
named: `--protocol` gets markup (`Nesting` and `saveEvalXml`) and `--minutes`
the indented text, and one run given both writes both files (`withEvalFunc'`
of `CLI/Helpers.hs`, #2335). Both carry the same facts and both stream, so
a run that fails still leaves a complete file; the markup references a value by
the symbol it denotes and spells it `𝜎1` as everywhere else, where the text
names a line by counting.

An application writes a line too, `𝑛·1·1 := …` (`EvApplied`, #1745). A `Step`
answers the term it made and the pairs of redex and result it replaced, and
`rewrite'` hands every pair to `_saveMade` of `RewriteContext`. `normalized` of
`Morph.hs` writes a line for an application that became a formation other than
its head, which of the built-in rules only `copy` does, other than one filling
ρ, which a dispatch makes, with the judgment and the site, spelling the call as
the head, named by `nameIn` of `Builder.hs`, applied to its argument; the
`rewrite` command saves nothing (`dontSaveMade`). One normalization copies one
call once: `normalized` keeps every copy it made by the call, and a place
holding the same call again writes `recalled 𝑛·0·1 ⇒ 𝑛·0·2` (`EvRecalled`), the
call and its copy, instead of copying it again (#2343). The line names the call,
and the line right after it, `𝑛·1·2 := ⟦ … ⟧`, names the object the call made,
abridged as any formation is, each by a name of its own counted with `built` and
`answer` of the innermost open firing, which both writers keep as a stack every
record prunes by its depth (`tier`), and from then on `abbreviated` writes each
by its name wherever a later line holds it whole (#2165). The markup writes the
object as a `<formed>` record with that name right after the `<applied>` one
(`made` of `saveEvalXml`), so both formats count the same names.
Either line goes through `abbreviatedInside`, which names the head and each
argument of a call, or each binding of an object, and never the term as a
whole, so an application made again is spelled out again, and so is the object
it made (#1756). Both writers name the formation an answer binds
the same way (`namedFormation`), even when a line named it before, unless it
holds λ or Δ, so the box step after a firing writes `boxed 𝑛·0·2 := 𝑛·1·2.φ`
with the meta of the answer rather than the whole answer again (#2151, #2158).
The markup writes the head in `of` and the argument by name in an element named
after the kind of its value, `<symbol>`, `<datum>`, `<meta>`, `<ref>` for a
path or an empty `<bottom>` for ⊥ (`attributed` of `saveEvalXml`), and so does
every binding of `<formed>` and `<with>` (#1961). Under `--inside` the
universe is normalized before the heading, so `aimed` of `CLI/Helpers.hs`
holds those records back and writes them after it. Before the heading, `aimed`
writes `EvHeader`, which `headerOf` builds: the phino that wrote the protocol,
the time, the command, the size of the world, the λ functions, the locator and
the term it points at. The text writes it as `# …` lines and an empty line, and
the markup as elements of `<protocol>`, leaving the term out (#2163).

The markup is read by programs and never writes a term (#1924): every value is
a symbol, a datum, a meta, a path off `Φ`, `ξ`, `ρ` or a meta, a name, or `⊥`.
`leaf` of `saveEvalXml` names any other term before the record that holds it
and by that name from then on: an application by an `<applied>` record with no
`by` and `at`, a formation by a `<formed>` one, and anything else by an
expression meta, `𝑒1`, which nothing defines. The first two are named
`𝑒·1·1` (`recorded`), counted per firing apart from the `𝑛` of the steps, so
the markup and the text of one run give every step the same `𝑛` (#2152). A
`<formed>` record names in `of` the object of the world the formation copies
and holds only the attributes it binds anew (`copied`); the world is the
normalized universe, which `universed` of `Morph.hs` hands to the writers as
`EvWorld` and the text writer ignores.

### Dependency inversion for circular imports

`Deps.hs` exists solely to break the cycle
`Dataize -> Functions -> Rewriter -> Dataize` via the `BuildTermFunc`
type alias.

### Morphing, dataization and evaluation

IMPORTANT: 𝕄 and 𝔻 work only with normal forms. Any other term handed to
them is broken input and must be refused, never normalized first: `vetted` of
`Morph.hs` throws `Abnormal` from the wrappers `morph` and `dataize` when the
term at the locator fails the normal-form test of the engine, such as
`⟦ a ↦ ⟦ x ↦ ∅ ⟧( x ↦ ⟦⟧ ) ⟧`, whose call can still be copied (#2343).
In the same way, a meta `𝑛` of an entry of `--symbolic`, such as `𝑛2`, may
hold only a normal form: `bind` of `Evaluate.hs` refuses to bind one to any
other term, such as what a `rewrite` line made that `dot` would still take.

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
entered (`severing` and `enter` in `Morph.hs`, one store for both judgments,
since only `box` and `fire` of 𝔻, `ml` of 𝕄 and an `evaluate` line, `fired`
of `Morph.hs`, enter one, and the line spends a step of `deeper` as a frame
does, #1882) and park the site the moment a frame is about to enter one of
them again, the way `--partial` parks a λ that cannot fire. Where `box` of 𝔻
or an `evaluate` line is about to enter a formation binding φ and holding no λ,
`sever` answers it with a fresh symbol, and the run goes on (#2226). Where 𝔻 or
an `evaluate` line is about to enter a formation holding a λ, such as a copy of
an atom whose operand dataizes the call again, or the φ of a formation a join
fabricated, `sever` answers it the same way (#2272, #2275, #2278). Where 𝕄 is
about to dispatch off a formation holding a λ that repeats one above, `sever`
answers the whole dispatch with a fresh symbol and names it after the call the
facts trace with the attribute on top, as `looped 𝜎1 := 𝑛·2·1.q`, leaving the
arguments as they are, since working them out runs into the same loop again
(#2307). The cut is
named only by the facts the run wrote down (#2287): in every run, each
dispatch normalization turns into a copy of an object of the world, every
application of a copy it knows, and every firing of one, writes `remembered
𝑛·0·1 ≡ Φ.p` (`EvRemembered`, `noted` of `Morph.hs`), keeping the pair in
`_facts` of `ReduceContext`, a mapping of a term to the term it came from.
The answer a firing came to is a fact too, `remembered 𝑛·1·5 ≡ 𝑛·0·3`, the
answer and the formation fired. `traced` of
`Morph.hs` looks for a fact whose term holds the formation cut, or the one it
repeats, follows the facts back to a path or a call of `Φ`, and writes one
`recalled 𝑛·0·3 ⇒ 𝑛·0·1( x ↦ ⟦⟧ )` (`EvRecalled`, `recited` of `Morph.hs`)
per fact it followed before the cut, so the cut is named after that
call, as `looped 𝜎1 := Φ.cyc( x ↦ 𝑛·0·9 )`. A formation no fact leads to the
world from, such as a fork written in place, is cut by its symbol alone, as
`looped 𝜎7  # …`, since nothing guesses which object it stands for.
A dispatch off a copy the facts hold leaves that copy without the attribute
it took, and `normalized` keeps that copy as an alias of the one it came
from (`peeled`), so a copy made of it later is traced back all the same.
Every `recalled` line repeats both sides of a `remembered` or `memoized`
line above it:
both writers keep the text they wrote for every fact (`_stated` of
`Protocol`, `_attested` of `Nesting`) and recall a fact by that text, though
a later line names the term anew, and `EvaluateSpec.hs` checks it on every
pack.
The morphing of an operand of an entry (`morphing` of `Morph.hs`) answers
such a copy the same way where the operand comes to one that repeats the
formations above it more often than `--max-loops` lets through, since a
recursion through a fork reaches the copy of every round only as a side of
the fork and never boxes it (#2228). That side is morphed in a
context of its own, gone once the firing answers, so `morphing` hands a copy
it lets through while it repeats one above back in `_copies` of `State`, and
the next frame of the 𝔻 chain that fired the entry adds it to the formations
it remembers (`carried` of `Morph.hs`), which `--max-loops` counts.
The mode `Acyclic` of `Deps.hs` says what "again" means, and there is no
default. Under `proven` two formations are the same up to a renaming of their
symbols (`alike` in `AST.hs`), since a recursion over a symbol mints fresh ones
every round and never repeats a term (#1420). Under `plausible` the earlier one
is embedded in the later one (`within` in `AST.hs`), so a recursion whose
accumulator gains a wrapper every round is cut too, at the price of cutting
now and then one that would have stopped (#1451). A copy of an atom, a
formation holding a λ, is the exception: under `plausible` too it repeats
only a formation `alike` it, since copies of one atom called from different
places resemble each other once a symbol stands in one of them, and a loop
through copies of atoms alone repeats them exactly (`repeats` of `Morph.hs`,
issue #2347). `--max-loops=<n>`
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
it keeps its comment. The `box` rule of 𝔻 binds the term it goes on with to a
meta of its own, `boxed 𝑛·1·2 := 𝑛·1·1.φ` (`EvBoxed`, #2134), counted with the
metas of the firing, and the rest of that 𝔻 is sited at that term (`resited` in
`dataize'`, #2144): the morphing that follows is named after the meta, as
`𝕄(𝑛·1·2):`, and opens a block of its own, and a firing or a cut the 𝔻 makes
there names the meta in its comment, as `# 𝔻(𝑛·1·2)`. The text writer names
the world `EvWorld` hands it `Φ`, as the markup does. The morphing of an answer
is named after the answer (`resited`), as `𝕄(𝑛·1·5):`. The datum `delta` finds
closes a `𝔻` block as `𝛿·1·1 := …` (`EvDelta`), commented with the double
eight bytes hold (`ieee`, the `number` of `<dataized>` in the markup), and the
operand line right after names it. The symbol a `𝔻` comes down to closes the
block the same way, as `𝛿·1·2 := 𝔻(𝜎1:λ)` (`EvManufactured`, #2150), and as
`<manufactured>` in the markup (#1962), counted with the data. The term a `𝕄`
came to closes its block too, as `morphed 𝑛·1·2 := 𝑛·1·1`
(`EvMorphed`, #2153), and as `<morphed>` in the markup (#1984), counted with
the metas of the firing, and both writers name that term by the meta from then
on (`namedFormation`), even when a line named it before, unless it holds λ or
Δ, so the box step after it writes `boxed 𝑛·1·3 := 𝑛·1·2.φ` (#2158). Only the
chain that opened the block writes it: `detached` runs a premise of a rule of 𝕄
through `morph''` told it owns nothing, since it stands in the block of the
rule. A cut is a line too,
`looped …` (`EvLooped`), written by `enter` where the refused frame would have
opened, carrying the formation its ancestor entered (#1434) and naming the mode
that cut it. A copy `severed` answered writes the symbol and the call the copy
stands for, `looped 𝜎1 := Φ.down( i ↦ 𝑛·1·6 )  # …, repeats 𝑛·0·3`, with
every argument worked out by `argued` as for `defer`, and the markup writes
`<looped symbol="𝜎1" … object="Φ.down">` holding a `<with>` (#2226).
An argument `argued` works out may come to a copy that repeats one above
too, which is cut in turn; that inner cut, told by `_reckoning` of
`ReduceContext`, writes the call the facts trace as it is, with no argument
worked out, since working it out runs into the same loop again (#2313).
An argument that comes to a copy of a terminator of the entry, as
`Φ.throw( 𝜎1 )`, is written as that copy and never fired (#2316).
A call ending with attributes over its last application, such as
`Φ.fork( … )( … ).φ`, keeps them on top: `argued` and the markup step over
them (`dispatched` of `Deps.hs`), so the object stays a path of the world,
and the markup writes them as `dispatch="φ"` beside `object` (#2305).
Three records say why a firing gave no answer (#1524): `stuck …` closes a
firing that got stuck, other than a join that could not join, which
`diverged …` or `unripe …` closes, `stall …` stands under one the memo
answered with a kept stall, and `starved …` is written by `deeper` where the
step budget runs out, with the site it stood at rather than its term (#1531).
`exceeded …` (`EvExceeded`) is written by `normalized` of `Morph.hs` where
normalization reaches `--max-cycles`, with the site the same way (#2309).
`oversized …` (`EvOversized`) is written by `measured` of `Morph.hs` where the
term `normalized` starts from, or any step of it, holds more nodes than
`--max-nodes` (`_maxNodes` of `ReduceContext`, off unless given), since a term
may grow until it takes all memory long before any other limit runs out
(#2320). It counts only the term at the locator, never the program around it,
and never the world `universed` normalizes, so attributes the run never
touches cannot push it over the limit (#2324). The count spells a part shared
in memory again every time it appears, so `blown …` (`EvBlown`) is written by
`measured` at the same places where the live bytes of the last collection of
the garbage collector pass `--max-memory` (`_maxMemory` of `ReduceContext`),
read by `getRTSStats`, which both the binary and the specs enable with
`-with-rtsopts=-T` (#2323).

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
timer cuts that comparison where it stands (#1622). The same timer
(`awaited` of `Morph.hs`) cuts the walk of a `rewrite` line, which takes no
step either (#2196), and the search of the facts `severed` makes to name a
cut, which may outlast the run on a wide copy (#2349). It wraps no work that
steps or writes a record, so the clock of the steps keeps the place of the
timeout. The first step, firing, comparison or walk after the deadline writes
`timeout …` (`EvTimeout`) and throws `OutOfTime`, which no frame parks, so the
run fails with or without `--partial` and a caller that cannot wait gets a
closed protocol ending in the timeout instead of killing the process (#1607).

Every use of a binding copies the term bound to it, so one formation is fired as
many times as the program reads it, and every firing mints symbols of its own. A
run under `plausible` keeps what every firing answered, by the formation it
fired (`Memo`, `recalled` and `retained` in `Morph.hs`, made by `memoized` for
that mode alone and asked and told by `symbol` in `Evaluate.hs`), and a later
firing of the same formation, ρ and all, takes the answer with the symbols the
first one minted: it reduces nothing, but is charged to `--max-firings` and
reads the clock of `--max-seconds` as one made, so a run of memo hits still
stops on its limits (#2328). A firing that
answers writes `memoized 𝑛·1·3 ⇛ 𝜎1:λ` (`EvMemoized`), the formation fired
and its answer, and a later one taking the answer is written to the protocol
as a firing at its own site carrying `recalled 𝑛·1·3 ⇒ 𝜎1:λ`, and no operand
line (#1476, #2287). The memo keeps the answer by the firing too, the
λ name with the data, the symbols and the normal forms its operands reading `$`
came down to (`Firing`, `remembered` and `remember` in `Morph.hs`, asked before
any line, a `morph` one among them, runs), since an entry answers from its
operands alone: a firing of another formation, such as one whose operand reads
another object holding the same datum, brings its operands down, finds the
firing answered and takes that answer, minting nothing, so one value never gets
two names (#1661); it recalls the entry of the formation the memo first
fired, `recalled 𝑛·1·3 ⇒ 𝜎1:λ`, and then writes `memoized 𝑛·1·6 ⇛ 𝜎1:λ`
for its own. A join answers from its sides too, so its key carries both
sides as its formation places them (`sides` of `Evaluate.hs`), and a second join
on a condition the first one read answers its own sides, not the first
one's (#1940). A symbol is kept as the symbol it is, not as the datum every symbol
manufactures, so two firings over two symbols stay two. Such a firing is charged
to `--max-firings` like one made, since the tally charges a firing as it starts
bringing its operands down, the only place a recursion widening inside its
operands can be cut, and a recalled answer counts as nothing new to a stall. The
store is an `IORef` in the context for the reason the tally is. A firing a cut
stopped on its way to an answer keeps the cut (`Looped` of `Kept`), and a later
firing of the same formation is cut at its own site without reducing
anything (#1480). A firing that got stuck keeps the λ function it got stuck on (`Stalled`
of `Kept`), and a later firing of the same formation gets stuck at its own site
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

`PrinterSpec.hs` also states the round trip of the printer and the parser as
a QuickCheck property over `Grown` of `test/Grown.hs`, a term grown at random
and shrunk to the smallest one still failing (#2002). The generator grows only
what the sweet syntax spells back: it applies nothing to ξ or Φ, closes a
formation with its λ and Δ, since neither takes a position (#1411), and
applies no ρ straight to a number or string literal, since the sweet syntax
drops such a ρ (#451).

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
