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
so the same benchmark works for both tiny and large inputs. Resource files
(`benchmark/tmp/`) are generated on first run and cached by Make; removed
by `make clean`. Requires Java and curl; Maven is fetched automatically
via `benchmark/mvnw`.

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

`phino` implements no λ function. `Lambdas.hs` reads the YAML file of them (the
`--functions` option), where each entry is a rule 𝔼 answers the firing with: a
`λ` key that is a regular expression over λ names, `dataize`/`morph` blocks
reducing operands of the fired formation through 𝔻 and 𝕄, a `symbols` block
minting fresh λ names counted in the state 𝑠, the `when` and `where` a
rewriting rule takes, and the answer under `𝑛`. An operand is a term, not a
name: ξ stands for the formation being fired, so `$.x` is its `x`, and
`contextualize` is all `Morph.symbol` does to it before handing it to a
judgment — the calculus does the reaching, so a dispatch attaches ρ there the
way it does anywhere else. The two premise blocks bind the metas their
judgments answer with, exactly as every rule in `resources/` does: `dataize`
binds a bytes meta `δ1`, since 𝔻 answers data, and `morph` an expression meta
`𝑛1`. An operand 𝔻 cannot bring down to data discharges no premise, so that
entry does not hold and the next one is tried — which is how a λ function
branches on data and how it answers when no data comes. Several entries may
answer one name and the guards tell them apart. A λ
name no entry answers gets stuck, which is what `--partial` parks on. Firing an
entry needs the judgments, so it lives in `Morph.hs` (`symbol`) while
`Lambdas.hs` holds only the entries and the lookup — otherwise
`Morph → Lambdas → Dataize → Morph` would be a cycle. Every firing reports
itself to `--evaluations` as one JSON object per line (`Evaluation` in
`Deps.hs`).

### Dependency inversion for circular imports

`Deps.hs` exists solely to break the cycle
`Dataize -> Functions -> Rewriter -> Dataize` via the `BuildTermFunc`
type alias.

### Morphing and dataization

`Morph.hs` implements the formal Morphing (M) function and `Dataize.hs` the
Dataization (D) one, with named rules: PRIM, NMZ, LAMBDA, PHI (morphing) and
DELTA, BOX, NORM (dataization). `Morph.hs` also holds what both judgments
share — the context, the step budget, the signals and the premise plumbing —
so `Dataize.hs` imports it and nothing points back. Nothing but one edge: the
`dataize` block of a λ function is a whole run of D, so it is injected into the
context as `_reduce` (a `ReductionFunc`), the way `Deps.hs` injects
`_buildTerm`. All configuration is threaded through
`ReduceContext` and `RewriteContext` records — no global state. Each
function has a top-level wrapper that locates the subterm and starts the
chain (`morph`, `dataize`) and a recursive worker the rules drive
(`morph'`, `dataize'`); the `morph` and `dataize` commands enter through
the wrappers.

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
