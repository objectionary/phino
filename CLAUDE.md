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
make hlint         # hlint src app test
make fourmolu      # --mode check src app test (2-space indent, leading commas)
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

## Architecture

`phino` is a CLI tool for manipulating phi-calculus (𝜑-calculus) expressions
— the formal foundation of the EO programming language. Four Cabal
components: `library` (`src/`), executable `phino` (`app/`), test suite
`spec` (`test/`), benchmark suite `bench` (`benchmark/`).

### Six CLI commands

`rewrite` | `dataize` | `morph` | `explain` | `merge` | `match` — all wired in
`src/CLI/Runners.hs`, parsed in `src/CLI/Parsers.hs`.

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
Matching (`Matcher.hs`) produces `[Subst]` — a list of
`Map Text MetaValue` — and conditions filter that list. `Builder.hs` then
applies a substitution to a result template.

### λ functions live outside the binary

`phino` implements no λ function. `Lambdas.hs` reads them from the YAML file
the `--symbolic` option names, one entry per function: a `λ` key, a regular
expression over λ names; the operands brought down to data through 𝔻 under
`dataize`, each binding a bytes meta `𝛿1`; the operands reduced to a normal
form through 𝕄 under `morph`, each binding an expression meta `𝑛1`; and the
answer under `𝑛`. Firing an entry is 𝔼's business and lives in `Evaluate.hs`,
which reaches the judgments an operand is reduced with through `Morph.hs`,
using the same `insideUniverse` trick the `--inside` option exposes.

An entry answers, it never computes: the answer carries a symbol `𝜎` standing
for a value nobody worked out, minted fresh per firing and counted in the state
`State` of `Deps.hs`. Dataizing a symbol answers a fixed 42, so a `𝛿` always
holds data. A λ name no entry answers gets stuck, and so does a λ naming a
symbol, since nothing answers that either; that is what `--partial`
parks on, and the protocol records it as `?(name)` either way. What fired is
written as a tree by `--protocol`
(`Evaluation` and `Protocol` in `Deps.hs`), which spells the three judgments
the way the calculus does: `𝕄(…)` or `𝔻(…)` opens the run, `𝔼(…)` names a
firing and `𝔻(𝜎1)` is the datum manufactured for a symbol. Every comment of a
line is a judgment applied to a term — the intent the value beside it came
from: the judgment that asked for a firing and the site it was fired at
(`_site` of `ReduceContext`, which the `--deep` walk refines as it enters a
binding), and the one that reduced an operand and the term it was reduced
from. A line no judgment made — a `symbolize` or a `join` one — names a meta
of the entry instead. The extension of
the file picks the format: `.xml` gets markup (`Nesting` and `saveEvalXml`),
anything else the indented text. Both carry the same facts and both stream, so
a run that fails still leaves a complete file; the markup references a value by
the symbol it denotes and spells it `𝜎1` as everywhere else, where the text
names a line by counting.

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
but three edges, each injected into the context rather than imported, the way
`Deps.hs` injects `_buildTerm`: an atom asking phino to reduce an operand of
its own is a whole run of D, so it goes in as `_reduce` (a `ReductionFunc`),
and E is reached as `_evaluate` (an `EvaluationFunc`, what an `evaluate`
premise fires and what answers a normal form) and `_fire` (a `FiringFunc`,
what the `--deep` walk fires and what answers the raw term the entry wrote).
All configuration is threaded through
`ReduceContext` and `RewriteContext` records — no global state. Each
function has a top-level wrapper that locates the subterm and starts the
chain (`morph`, `dataize`) and a recursive worker the rules drive
(`morph'`, `dataize'`); the `morph` and `dataize` commands enter through
the wrappers.

The step budget `--max-steps` is what ends that recursion, and the only thing
that does unless `morph` or `dataize` is given `--acyclic`: the flag has every
𝕄 and every 𝔻 frame remember the formations its ancestors have entered
(`entering` in `Morph.hs`, one store for both judgments, since only `box` and
`fire` of 𝔻 and `ml` of 𝕄 enter one) and park the site the moment a frame is
about to enter one of them again, the way `--partial` parks a λ that cannot
fire. Two formations are the same up to a renaming of their symbols (`alike`
in `AST.hs`), since a recursion over a symbol mints fresh ones every round and
never repeats a term (#1420). A formation `box` gets into is also a line of the
protocol, `formation(…)`, and what its φ body fires stands under it.

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
