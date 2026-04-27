# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
make test          # cabal test --ghc-options=-Werror
make hlint         # hlint src app test
make fourmolu      # fourmolu --mode check src app test (2-space indent, leading commas)
make coverage      # cabal test --enable-coverage + hpc-codecov (threshold 65%)
make bench         # prepare resources + cabal bench --enable-benchmarks
make all           # coverage + hlint + fourmolu
```

Run a single test by matching against describe/it text or the module name prefix:
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

`phino` is a CLI tool for manipulating phi-calculus (𝜑-calculus) expressions — the formal foundation of the EO programming language. Four Cabal components: `library` (`src/`), executable `phino` (`app/`), test suite `spec` (`test/`), benchmark suite `bench` (`benchmark/`).

### Five CLI commands
`rewrite` | `dataize` | `explain` | `merge` | `match` — all wired in `src/CLI/Runners.hs`, parsed in `src/CLI/Parsers.hs`.

### Two-phase rendering pipeline
`AST -> CST -> String`. `AST.hs` holds the semantic tree (`Expression`, `Binding`, `Attribute`, `Bytes`, `Program`). `CST.hs` holds the formatting tree (every token is a typed constructor). The `Render` typeclass converts CST to `String`. This keeps layout concerns out of the semantic layer.

### Sugar/Salty duality
`SWEET` (default) uses compact syntax sugar; `SALTY` expands to canonical phi-calculus. The `ToSalty` typeclass in `Sugar.hs` transforms CST nodes between these representations before rendering.

### Pattern-based rewriting
YAML rule files (`resources/*.yaml`, plus user-supplied via `--rule`) each define a `pattern`, `result`, optional `when`/`having` conditions, and `where` extensions. Built-in rules are compiled in via `file-embed`. Matching (`Matcher.hs`) produces `[Subst]` — a list of `Map String MetaValue` — and conditions filter that list. `Builder.hs` then applies a substitution to a result template.

### Dependency inversion for circular imports
`Deps.hs` exists solely to break the cycle `Dataize -> Functions -> Rewriter -> Dataize` via the `BuildTermFunc` type alias.

### Dataization
`Dataize.hs` implements the formal Morphing (M) and Dataization (D) functions with named rules: PRIM, NMZ, LAMBDA, PHI (morphing) and DELTA, BOX, NORM (dataization). All configuration is threaded through `DataizeContext` and `RewriteContext` records — no global state.

### Test pattern: YAML packs
Most spec files load test cases from `test-resources/*-packs/*.yaml` at runtime via `runIO`. Each pack defines `input`/`output`/`rules`/`skip`. Test files map 1:1 with source modules (`RewriterSpec.hs` ↔ `Rewriter.hs`).
