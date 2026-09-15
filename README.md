<!--
SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
SPDX-License-Identifier: MIT
-->

# Command-Line Manipulator of 𝜑-Calculus Expressions

[![DevOps By Rultor.com](https://www.rultor.com/b/objectionary/phino)](https://www.rultor.com/p/objectionary/phino)

[![`phino` on Hackage](https://img.shields.io/hackage/v/phino)](http://hackage.haskell.org/package/phino)
[![cabal-linux](https://github.com/objectionary/phino/actions/workflows/cabal.yml/badge.svg)](https://github.com/objectionary/phino/actions/workflows/cabal.yml)
[![stack-linux](https://github.com/objectionary/phino/actions/workflows/stack.yml/badge.svg)](https://github.com/objectionary/phino/actions/workflows/stack.yml)
[![codecov](https://codecov.io/gh/objectionary/phino/branch/master/graph/badge.svg)](https://app.codecov.io/gh/objectionary/phino)
[![Haddock](https://img.shields.io/badge/docs-Haddock-blue.svg)](https://objectionary.github.io/phino/)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSES/MIT.txt)
[![Hits-of-Code](https://hitsofcode.com/github/objectionary/phino?branch=master&label=Hits-of-Code)](https://hitsofcode.com/github/objectionary/phino/view?branch=master&label=Hits-of-Code)
[![PDD status](https://www.0pdd.com/svg?name=objectionary/phino)](https://www.0pdd.com/p?name=objectionary/phino)

This is a command-line normalizer, rewriter, and dataizer
of [𝜑-calculus](https://www.eolang.org) expressions.

First, you write a simple [𝜑-calculus](https://www.eolang.org) expression
in the `hello.phi` file:

```text
⟦ φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧, t ↦ ξ.k, k ↦ ⟦⟧ ⟧
```

## Installation

Then you can install `phino` in two ways:

Install [Cabal][cabal] first and then:

```bash
cabal update
cabal install --overwrite-policy=always phino-0.0.132
phino --version
```

Or download binary from the internet using [curl](https://curl.se/) or
[wget](https://en.wikipedia.org/wiki/Wget):

```bash
sudo curl -o /usr/local/bin/phino http://phino.objectionary.com/releases/macos-15/phino-latest
sudo chmod +x /usr/local/bin/phino
phino --version
```

Download paths are:

* Ubuntu 22.04: <http://phino.objectionary.com/releases/ubuntu-22.04/phino-latest>
* Ubuntu 24.04: <http://phino.objectionary.com/releases/ubuntu-24.04/phino-latest>
* MacOS (ARM): <http://phino.objectionary.com/releases/macos-15/phino-latest>
* MacOS (Intel): <http://phino.objectionary.com/releases/macos-14-large/phino-latest>
* Windows: <http://phino.objectionary.com/releases/windows-2022/phino-latest.exe>

## Build

To build `phino` from source, clone this repository:

```bash
git clone git@github.com:objectionary/phino.git
cd phino
```

Then, run the following command (ensure you have [Cabal][cabal] installed):

```bash
cabal build all
```

Next, run this command to install `phino` system-wide:

```bash
sudo cp "$(cabal list-bin phino)" /usr/local/bin/phino
```

Verify that `phino` is installed correctly:

```bash
$ phino --version
0.0.0
```

You can ensure scripts are run with a specific version of `phino` using
the `--pin` global option. It exits with an error when the version supplied
doesn't match the installed one:

```bash
phino --pin=0.0.0.67 dataize hello.phi
```

## Dataize

Then, you dataize the expression:

```bash
$ phino dataize hello.phi
68-65-6C-6C-6F
```

### Functions

Which λ functions exist is a property of the object model being dataized, not
of the calculus, so `phino` implements none of them. They come from a YAML file
given with `--functions`, where each one is a rule 𝔼 answers the firing with,
written in the very language `phino`'s own judgments are written in:

```yaml
- λ: L_number_plus
  dataize:
    𝑛1: ρ
    𝑛2: x
  where:
    - meta: 𝑛3
      function: sum
      args:
        - 𝑛1
        - 𝑛2
  𝑛: 𝑛3
```

The `λ` of an entry names the λ function it answers, the blocks under it reduce
the operands of the firing, and `𝑛` is the term the firing answers with,
normalized before it is handed back. Nothing here leaves the process: there is
no script, no interpreter and no channel, so a run is as fast and as
reproducible as a rewriting run:

```bash
$ cat sum.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  φ ↦ 5.plus( 6 )
⟧
$ phino dataize --functions=functions.yaml sum.phi
40-26-00-00-00-00-00-00
```

An entry takes six keys, of which only `λ` and `𝑛` are required:

* `dataize` reduces the named operands through 𝔻 and binds each to its meta,
  so `𝑛1: ρ` says "dataize the ρ of the formation being fired and call the
  result 𝑛1". The result is the formation carrying the bytes, `⟦ Δ ⤍ … ⟧`.
* `morph` does the same through 𝕄, so what it binds is a term rather than a
  datum: `𝑛1: φ` is how a box hands its content back without insisting that the
  content be data.
* `symbols` mints a fresh λ name for each of its metas, which is how a λ
  function answers that it cannot decide (see below).
* `when` guards the entry, with the same conditions a rewriting rule's `when`
  takes.
* `where` calls the build-term functions a rewriting rule's `where` calls —
  `sum`, `concat`, `bytes` and the rest (see [Rule structure](#rule-structure))
  — so the arithmetic an object model needs is spelled in the file.
* `𝑛` is the answer.

Both `dataize` and `morph` name an attribute by a path down the formation being
fired, read left to right and split on the dot, which no attribute of
𝜑-calculus carries in its own name. Every segment but the last has to name a
formation or an application to go on into, so `ρ.length` goes two deep, and an
argument of an application binds an attribute the way a τ binding does, so
`x.if.guard` reaches the `guard` of `x ↦ Φ.bool( if ↦ ⟦ guard ↦ … ⟧ )`. The
metas are reduced in the order of their names, which is why they are called
𝑛1, 𝑛2, … : a YAML mapping keeps no order of its own.

Each `λ` is a regular expression, and it must match the whole name, so a plain
name such as `L_number_plus` means that one λ function and nothing else, while
`L_box_[0-9]+_number` stands for a whole family of them:

```yaml
- λ: L_box_[0-9]+_number
  morph:
    𝑛1: φ
  𝑛: 𝑛1
```

Several entries may answer one name, and `when` is what tells them apart: the
entries that match are tried in the order the file lists them, their operands
are reduced once for all of them, and the first one whose guard holds is the
one that answers. So a comparison is a guard and not a function:

```yaml
- λ: L_bytes_eq
  dataize:
    𝑛1: ρ
    𝑛2: x
  when:
    eq:
      - 𝑛1
      - 𝑛2
  𝑛: Φ.true
- λ: L_bytes_eq
  dataize:
    𝑛1: ρ
    𝑛2: x
  𝑛: Φ.false
```

A name no key matches, and a name every guard declines, both leave 𝔼 with
nothing to answer, so it gets stuck on it — which is what `--partial` parks.
Without `--functions` no λ function is registered at all and every one of them
gets stuck. A key that is not a regular expression, an entry with no `𝑛` and a
file that is no list of entries are all refused where the file is read, before
the input is even parsed.

#### Answering with an unknown

A λ function that cannot decide need not fail: `symbols` mints a fresh λ name
for it, and the answer stands for whatever that name turns out to be. The state
𝑠 threaded through 𝕄, 𝔻 and 𝔼 counts the names a run has minted, so no two
unknowns of one run are ever spelled alike, and the counting is sequential
rather than random, which keeps a symbolic run reproducible:

```yaml
- λ: L_bytes_eq
  symbols: [𝑓0]
  𝑛: ⟦ λ ⤍ 𝑓0 ⟧
- λ: L_fork
  dataize:
    𝑛1: guard
  morph:
    𝑛2: left
    𝑛3: right
  symbols: [𝑓1]
  𝑛: ⟦ λ ⤍ 𝑓1 ⟧
```

Here `L_bytes_eq` declines to decide the comparison, so the fork it guards
cannot pick a branch either: `L_fork` morphs both branches — neither is
dataized, since neither is demanded — and answers an unknown of its own,
standing for whichever branch the comparison turns out to take:

```bash
$ cat fork.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅, eq(x) ↦ ⟦ λ ⤍ L_bytes_eq ⟧ ⟧,
  bool ↦ ⟦ φ ↦ ∅, if(left, right) ↦ ⟦ guard ↦ ξ.ρ, λ ⤍ L_fork ⟧ ⟧,
  φ ↦ Φ.bool(
    Φ.bytes( φ ↦ ⟦ Δ ⤍ 2A- ⟧ ).eq( Φ.bytes( φ ↦ ⟦ Δ ⤍ 2B- ⟧ ) )
  ).if( ⟦ Δ ⤍ 01- ⟧, ⟦ Δ ⤍ 02- ⟧ )
⟧
$ phino morph --deep --partial --functions=fork.yaml --locator=Q.φ \
    --sweet --hide-rho fork.phi
⟦ λ ⤍ S_2 ⟧
```

Recursion is nothing `phino` prevents: whether a λ function answers with a
firing of itself is the object model's business, not the calculus's. What ends
such a run is `--max-steps`, and under `--partial` the site it ran out on is
left as it was written while the rest of the program goes on, so a morphing
that cannot finish is still a morphing that answers.

A term nothing in the input carries is still worth reducing inside the object
model the input declares, and `--inside` is how one aims a run at it: the
expression it names is bound to a fresh synthetic attribute of the input
expression, which the run takes as the universe, normalized there, and then
reduced.

```bash
$ phino dataize --functions=functions.yaml --inside='5.plus( 6 )' universe.phi
40-26-00-00-00-00-00-00
```

The `--inside` option cannot be combined with `--locator`, since it aims the
run at the binding it mints itself. Both `dataize` and `morph` take
`--functions` and `--inside`.

### Recording what fired

Every λ function fired on the way to the bytes may be recorded in a
machine-readable protocol, with the `--evaluations` option. One firing is one
JSON object on a line of its own: the name of the function under `λ` and, next
to it, every meta the entry of it bound, each under the name the file spells it
with:

```bash
$ phino dataize --functions=functions.yaml --evaluations=fired.json --quiet \
    sum.phi
$ cat fired.json
{"λ":"L_number_plus","𝑛1":"40-14-00-00-00-00-00-00","𝑛2":"40-18-00-00-00-00-00-00"}
```

A byte array is spelled in hex and a λ name as text, so a reader of the file
never parses 𝜑. An operand of a `morph` block is a whole term, which has no
such spelling, so it is bracketed by an opening and a closing record instead
and whatever fires inside it stands between them:

```json
{"λ":"L_bytes_eq","𝑓0":"S_1"}
{"λ":"S_1","stuck":true}
{"λ":"L_fork","morph":"𝑛2","at":"begin"}
{"λ":"L_fork","morph":"𝑛2","at":"end"}
{"λ":"L_fork","morph":"𝑛3","at":"begin"}
{"λ":"L_fork","morph":"𝑛3","at":"end"}
{"λ":"L_fork","𝑛1":"S_1","𝑓1":"S_2"}
```

A firing that got stuck and survived in the residual program of a partial
evaluation names the function alone, under `stuck`. The file is truncated at
the beginning of every run, and the records of a run that fails are kept.

### Partial evaluation

A λ function that cannot fire fails the run: no entry of the file given with
`--functions` answers its name. This is what happens when an operation is
deliberately left unimplemented — a data input replaced by a placeholder
formation such as `⟦ λ ⤍ Sym_arg_0 ⟧`, or an operation whose answer is not
known yet. With `--partial`, dataization becomes partial evaluation instead:
what the known inputs decide is computed, the rest survives as the residual
program, which is printed in place of the bytes, and the run ends successfully:

```bash
$ cat partial.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦
    φ ↦ ∅,
    plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧,
    as-bool ↦ ⟦ λ ⤍ L_number_as_bool ⟧
  ⟧,
  φ ↦ 2.plus( 3 ).plus( 4 ).as-bool
⟧
$ phino dataize --functions=functions.yaml --partial --sweet --hide-rho \
    partial.phi
⟦ λ ⤍ L_number_as_bool ⟧
```

Here `2.plus( 3 ).plus( 4 )` was decided by the entries the file carries, so it
was computed (its result, `9`, sits in the hidden `ρ` of the residual program),
while `as-bool` names a λ function no entry answers, so it stays in place as a
normal-form subterm. Each such stuck site also lands in the `--evaluations`
file, as a record naming the function alone, since there is nothing it bound:

```bash
$ phino dataize --functions=functions.yaml --partial --evaluations=fired.json \
    --quiet --sweet --hide-rho partial.phi
$ cat fired.json
{"λ":"L_number_plus","𝑛1":"40-00-00-00-00-00-00-00","𝑛2":"40-08-00-00-00-00-00-00"}
{"λ":"L_number_plus","𝑛1":"40-14-00-00-00-00-00-00","𝑛2":"40-10-00-00-00-00-00-00"}
{"λ":"L_number_as_bool","stuck":true}
```

Evaluation stays demand-driven, as the calculus prescribes: an argument
that nothing asked for before the run got stuck is left as it is in the
residual program, for the next iteration.

The nested morphing and dataization recursion is bounded by the
`--max-steps` option (default `1000`): when the budget is exhausted, the run
fails with `Dataization did not finish before reaching the limit of steps`.
This guards against non-terminating terms, which used to loop forever before
the bound was introduced:

```bash
$ phino dataize --max-steps=50 problem.phi
[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=50
```

## Morph

Dataization insists on bytes. Morphing 𝕄 asks a different question: evaluate
as far as the object model allows, without demanding data. It resolves Φ
against the universe, peels dispatches and applications through
normalization, fires whichever λ functions sit under a dispatch, and stops at
the first formation it reaches, handing that formation back untouched. The
`morph` command runs 𝕄 on its own:

```bash
$ cat two.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  φ ↦ 5.plus( 6 ).plus( 7 )
⟧
$ phino dataize --functions=functions.yaml --sweet --hide-rho two.phi
40-32-00-00-00-00-00-00
$ phino morph --functions=functions.yaml --locator=Q.φ --sweet --hide-rho two.phi
⟦ x ↦ 7, λ ⤍ L_number_plus ⟧
```

The inner `5.plus( 6 )` fires, because `.plus` is dispatched on its result,
and `11` lands in the `ρ` hidden by `--hide-rho`. The outer application is
saturated but bare, so 𝕄 returns it and is finished; firing it is
dataization's job and takes `dataize` on to `18`.

The default locator `Q` morphs the whole top formation, which 𝕄 returns
unchanged, so `--locator` is how one aims 𝕄 at a subterm, exactly as in
`dataize`. Unlike 𝔻, 𝕄 is total: where no formation is reachable the answer
is the terminator `⊥`, printed rather than reported as a failed run:

```bash
$ phino morph --locator=Q.x <<< '⟦ x ↦ ξ ⟧'
⊥
```

The whole `dataize` option surface applies unchanged — `--functions`,
`--inside`,
`--sequence`, `--headers`, `--steps-dir`, `--evaluations`, `--partial`,
`--max-steps`, `--shuffle`/`--seed`, `--output`, `--focus` and the rest.

### Deep morphing

𝕄 stops at the first formation it reaches and hands its bindings back as they
were written, since firing a bare λ is dataization's job, and `dataize`
follows the one path dataization demands and ends in bytes. What a program
holds but nothing demands — the argument of a λ function no entry answers, for
one — is therefore reduced by neither. The `--deep` flag enters it:

```bash
$ cat gap.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  bar(x) ↦ ⟦ λ ⤍ L_bar ⟧,
  demo ↦ ⟦ foo ↦ ⟦ n ↦ 3, φ ↦ Φ.bar( ξ.n.plus( 5 ).plus( 7 ) ) ⟧ ⟧
⟧
$ phino morph --functions=functions.yaml --inside='Q.demo.foo' \
    --sweet --hide-rho gap.phi
⟦ n ↦ 3, φ ↦ Φ.bar( n.plus( 5 ).plus( 7 ) ) ⟧
$ phino morph --deep --functions=functions.yaml --inside='Q.demo.foo' \
    --sweet --hide-rho gap.phi
⟦ n ↦ 3, φ ↦ Φ.bar( 15 ) ⟧
```

Every binding of the formation is entered, recursively. 𝕄 is asked about the
term standing there and, where it lands on a saturated formation whose λ an
entry answers, that λ is fired and 𝕄 is asked about the answer again. A term on
whose way a λ function fired is replaced by the answer of the last firing,
which is the 𝜑-program the entry wrote rather than the normal form of it, so
`15` stands where the arithmetic stood. A term no λ function touched stays
exactly as it was written and only its own parts are walked, so `Φ.bar` keeps
its name and what comes back is still the same program, reduced as far as the
file allows. The step joins the chain under the name `deep`, so `--sequence`
shows it, and `--max-steps` bounds the walk.

Two things are left alone. A λ no entry answers is not fired at all, so
`--deep` stays as total as 𝕄 itself and needs no `--partial`; a λ function that
gets stuck deeper on a spine still fails the run, and `--partial` parks it,
leaving that term as it was written. A formation still holding a void binding
is not fired either: the void is an argument the program has not given yet, so
`plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧` is a method waiting to be applied, not an
application waiting to be computed. Walking the whole program therefore folds
what it can and leaves the object model as it was declared:

```bash
$ phino morph --deep --functions=functions.yaml --sweet --hide-rho gap.phi
⟦
  bytes(φ) ↦ ⟦⟧,
  number(φ) ↦ ⟦ plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  bar(x) ↦ ⟦ λ ⤍ L_bar ⟧,
  demo ↦ ⟦ foo ↦ ⟦ n ↦ 3, φ ↦ Φ.bar( 15 ) ⟧ ⟧
⟧
```

## Rewrite

You can rewrite this expression with the help of [rules](#rule-structure)
defined in the `my-rule.yml` YAML file (here, the `!d` is a capturing group,
similar to regular expressions):

```yaml
name: My custom rule
pattern: Δ ⤍ !d
result: Δ ⤍ 62-79-65
```

Then, rewrite:

```bash
$ phino rewrite --rule=my-rule.yml hello.phi
⟦ φ ↦ ⟦ Δ ⤍ 62-79-65 ⟧, t ↦ ξ.k, k ↦ ⟦⟧ ⟧
```

If you want to use many rules, just use `--rule` as many times as you need:

```bash
phino rewrite --rule=rule1.yaml --rule=rule2.yaml ...
```

You can also use [built-in rules](resources), which are designed
to normalize expressions:

```bash
phino rewrite --normalize hello.phi
```

Both flags may be combined, so that your own rules are applied
alongside the built-in ones, in a single rewriting session:

```bash
phino rewrite --normalize --rule=my-rule.yaml hello.phi
```

Some rules mint fresh synthetic names via the `random-string` built-in. To
keep the output reproducible across runs, `phino` seeds the random generator
deterministically with `0` by default. Use `--seed` to pick a different seed:

```bash
phino rewrite --seed=42 --rule=my-rule.yml hello.phi
```

If no input file is provided, the 𝜑-expression is taken from `stdin`:

```bash
$ echo '⟦ φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧ ⟧' | phino rewrite --rule=my-rule.yml
⟦ φ ↦ ⟦ Δ ⤍ 62-79-65 ⟧ ⟧
```

You're able to pass [`XMIR`][xmir] as input. Use `--input=xmir` and `phino`
will parse given `XMIR` from file or `stdin` and convert it to `phi` AST.

```bash
phino rewrite --rule=my-rule.yaml --input=xmir file.xmir
```

Also `phino` supports 𝜑-expressions in
[ASCII](https://en.wikipedia.org/wiki/ASCII) format and with
syntax sugar. The `rewrite` command also allows you to desugar the expression
and print it in canonical syntax:

```bash
$ echo '[[ @ -> Q.io.stdout("hello") ]]' | phino rewrite
⟦
  φ ↦ Φ.io.stdout(
    α0 ↦ Φ.string( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F, ρ ↦ ∅ ⟧ ) )
  ),
  ρ ↦ ∅
⟧
```

## Merge

You can merge several 𝜑-expressions into a single one by merging their
top level formations:

```bash
$ cat bytes.phi
⟦ bytes ↦ ⟦ φ ↦ ∅ ⟧ ⟧
$ cat number.phi
⟦
  number ↦ ⟦
    φ ↦ ∅,
    plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧
  ⟧
⟧
$ cat minus.phi
⟦ number ↦ ⟦ minus(x) ↦ ⟦ λ ⤍ L_number_minus ⟧ ⟧ ⟧
$ phino merge bytes.phi number.phi minus.phi --sweet
⟦
  bytes(φ) ↦ ⟦⟧,
  number(φ) ↦ ⟦
    plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧,
    minus(x) ↦ ⟦ λ ⤍ L_number_minus ⟧
  ⟧
⟧
```

## Match

You can test the 𝜑-expression matches against the [rule](#rule-structure)
pattern. The result output contains matched substitutions:

```bash
$ phino match --pattern='⟦ Δ ⤍ !d, !B ⟧' hello.phi
B >> ⟦ ρ ↦ ∅ ⟧
d >> 68-65-6C-6C-6F
```

## Explain

You can _explain_ the built-in rules by printing them in [LaTeX][latex]
format. Pass exactly one of `--normalize`, `--morph`, `--dataize` or
`--contextualize` for the rewriting, morphing (𝕄), dataization (𝔻) or
contextualization (𝒞) rules (or `--rule` for a custom rule file):

```bash
$ phino explain --normalize
\begin{tabular}{rl}
\phinoNormalizationRule{alpha}
  { [[ B_1, \tau -> ?, B_2 ]] ( \phiTerminal{\alpha_{i}} -> e ) }
  { [[ B_1, \tau -> ?, B_2 ]] ( \tau -> e ) }
  { $ i = \vert \overline{ B_1 } \vert $ }
  { }
\phinoNormalizationRule{dc}
  { T ( \tau -> e ) }
  { T }
  { }
  { }
...
\phinoNormalizationRule{stop}
  { [[ B ]] . \tau }
  { T }
  { $ \tau \notin B \;\text{and}\; @ \notin B \;\text{and}\; L \notin B $ }
  { }
\end{tabular}
```

The morphing and dataization rules are printed the same way:

```bash
$ phino explain --morph
\begin{tabular}{rl}
\phinoMorphingRule{mf}
  { \mathbb{M}( [[ B ]], e ) }
  { [[ B ]] }
  { }
  { }
...
\phinoMorphingRule{universe}
  { \mathbb{M}( Q, e ) }
  { \mathbb{M}( \phinoNormalize{ e }, e ) }
  { $ e \not= Q $ }
  { }
\end{tabular}
```

```bash
$ phino explain --dataize
\begin{tabular}{rl}
\phinoDataizationRule{delta}
  { \phinoDataize{ [[ B_1, D> δ, B_2 ]] } }
  { δ }
  { }
  { }
...
\phinoDataizationRule{norm}
  { \phinoDataize{ n } }
  { \phinoDataize{ \mathbb{M}( n, e ) } }
  { }
  { }
\end{tabular}
```

```bash
$ phino explain --contextualize
\begin{phinoContextualizationInference}
  \phinoName{cxi}
  \phinoConclusion{ \phinoContextualize{ \phiTerminal{\xi} }{ k }{ k } }
\end{phinoContextualizationInference}
...
\begin{phinoContextualizationInference}
  \phinoName{cd}
  \phinoPremise{ \phinoContextualize{ n }{ k }{ n_1 } }
  \phinoConclusion{ \phinoContextualize{ n . \tau }{ k }{ n_1 . \tau } }
\end{phinoContextualizationInference}
```

For more details, use `phino [COMMAND] --help` option.

## Rule structure

This is BNF-like yaml rule structure. Here types ended with
apostrophe, like `Attribute'` are built types from 𝜑-expression [AST](src/AST.hs)

```bnfc
Rule:
  name: String
  pattern: String
  result: String
  when: Condition?       # predicate, works with substitutions before extension
  where: [Extension]?    # substitution extensions
  having: Condition?     # predicate, works with substitutions after extension

Condition:
  = and: [Condition]     # logical AND
  | or:  [Condition]     # logical OR
  | not: Condition       # logical NOT
  | eq:                  # compare two comparable objects
      - Comparable
      - Comparable
  | in:                  # check if attributes exist in bindings
      - Attribute'
      - Binding'
  | nf: Expression'      # returns True if given expression in normal form
                         # which means that no more other normalization rules
                         # can be applied
  | absolute: Expression' # returns True if given expression is xi-free, i.e.
                         # there is no ξ outside of a formation: it is Φ, a
                         # formation, a dispatch with a xi-free subject, or an
                         # application with a xi-free subject and argument.
                         # Combined with a normal-form check by the '𝑘'/'!k'
                         # meta variable, which ranges over the absolute
                         # expressions 𝒦 ⊆ 𝒩, used by the Rcopy rule.
  | matches:             # returns True if given expression after dataization
      - String           # matches to given regex
      - Expression
  | part-of:             # returns True if given expression is attached to any
      - Expression'      # attribute in ginve bindings
      - BiMeta'
  | formation:           # returns True if given expression is a formation
      Expression'        # (an abstraction ⟦…⟧); used by morphing 'md'
                         # as 'not (formation 𝑛)', so a non-formation head is
                         # morphed and a formation head is left to 'ml'
  | gt:                  # returns True if the first comparable object is
      - Comparable       # greater than the second one
      - Comparable
  | disjoint:            # returns True if none of the given attributes exists
      - [Attribute']     # in the given bindings
      - Binding'

Comparable:              # comparable object that may be used in 'eq' condition
  = Attribute'
  | Number
  | Expression'

Number:                  # comparable number
  = Integer              # just regular integer
  | IndexMeta'           # 𝑖 (or !i), the index captured by an α𝑖 argument
  | length: BiMeta'      # calculate length of bindings by given meta binding
  | domain: BiMeta'      # calculate number of unique attributes in given
                         # meta binding (excluding 'assets')

Extension:               # substitutions extension used to introduce new meta variables
  meta: [ExtArgument]    # new introduced meta variable
  function: String       # name of the function
  args: [ExtArgument]    # arguments of the function

ExtArgument
  = Bytes'               # !d
  | Binding'             # !B
  | Expression'          # !e
  | Attribute'           # !t
```

Here's list of functions that are supported for extensions:

* `contextualize` - function of two arguments, that rewrites given expression
  depending on provided context according to the contextualization
  [rules](assets/contextualize.jpg)
* `random-tau` - creates attribute with random unique name. Accepts bindings,
  and attributes. Ensures that created attribute is not present in list of
  provided attributes and does not exist as attribute in provided bindings.
* `dataize` - dataizes given expression and returns bytes.
* `concat` - accepts bytes or dataizable expressions as arguments,
  concatenates them into single sequence and convert it to expression
  that can be pretty printed as human readable string:
  `Φ.string(Φ.bytes⟦ Δ ⤍ !d ⟧)`.
* `sed` - pattern replacer, works like unix `sed` function.
  Accepts two arguments: target expression and pattern.
  Pattern must start with `s/`, consists of three parts
  separated by `/`, for example, this pattern `s/\\s+//g`
  replaces all the spaces with empty string. To escape braces and slashes
  in pattern and replacement parts - use them with `\\`,
  e.g. `s/\\(.+\\)//g`.
* `random-string` - accepts dataizable expression or bytes as pattern.
  Replaces `%x` and `%d` formatters with random hex numbers and
  decimals accordingly. Uniqueness is guaranteed during one
  execution of `phino`.
* `size` - accepts exactly one meta binding and returns size of it and
  `Φ.number`.
* `tau` - accepts `Φ.string`, dataizes it and converts it to attribute.
  If dataized string can't be converted to attribute - an error is thrown.
* `string` - accepts `Φ.string` or `Φ.number` or attribute and converts it
  to `Φ.string`.
* `number` - accepts `Φ.string` and converts it `Φ.number`
* `sum` - accepts list of `Φ.number` or `Φ.bytes` and returns sum of them as `Φ.number`
* `join` - accepts list of bindings and returns list of joined bindings. Duplicated
  `ρ`, `Δ` and `λ` attributes are ignored, all other duplicated attributes are replaced
  with unique attributes using `random-tau` function.

## Meta variables

The `phino` supports meta variables to write 𝜑-expression patterns for
capturing attributes, bindings, etc.

This is the list of supported meta variables:

* `!t` || `𝜏` - attribute
* `!i` || `𝑖` - the index of a positional (α) application argument,
                captured by writing `α𝑖` (or `~!i`)
* `!e` || `𝑒` - any expression
* `!n` || `𝑛` - any expression that is already in normal form (behaves like
                `!e`/`𝑒`, but only binds a sub-expression in NF, so no explicit
                `nf:` guard is needed)
* `!k` || `𝑘` - any expression that is absolute, i.e. xi-free and in normal
                form (ranges over `𝒦 ⊆ 𝒩`); behaves like `!e`/`𝑒` but only
                binds an absolute sub-expression, so no explicit `absolute:`
                or `nf:` guard is needed
* `!B` || `𝐵` - list of bindings
* `!d` || `δ` - bytes in meta delta binding
* `!F` || `𝑓` - function name in meta lambda binding

A meta variable carries a suffix, like `!B1` or `𝜏0`, to name what it
captured, so that the `result`, `when`, `where` and `having` of a rule can
read it back.

Written bare, with no suffix at all, a meta variable is anonymous: it matches
whatever term stands in its place, every occurrence on its own, and binds no
name. Two anonymous metas of one kind are therefore two different captures,
which is what lets a pattern ask for any two attributes without inventing
names for them:

```yaml
name: two-attributes
pattern: '⟦ 𝜏 ↦ 𝑒, 𝜏 ↦ 𝑒 ⟧'
result: '⟦ x ↦ ⟦ Δ ⤍ 2A- ⟧ ⟧'
```

Spelled with suffixes, that pattern would read `⟦ 𝜏1 ↦ 𝑒1, 𝜏2 ↦ 𝑒2 ⟧` and
name four captures the result never mentions, while `⟦ 𝜏1 ↦ 𝑒1, 𝜏1 ↦ 𝑒1 ⟧`
would be rejected as a duplicated attribute.

Nothing can refer to an anonymous meta, since it has no name to be referred to
by. Writing one outside a `pattern` (or the `match`, `e-match` and `c-match` of
an inference rule) is a mistake in the rule and is reported as the rule loads.

A positional (α) application argument is written as `α0`, `~0` (ASCII), or
`α𝑖`/`~!i` when its index is captured by an `!i`/`𝑖` meta variable.

Incorrect usage of meta variables in 𝜑-expression patterns leads to
parsing errors.

## Benchmark

To run performance benchmarks, you need [Java 8+][java] and [curl][curl].
Maven is downloaded automatically on first run via `benchmark/mvnw`.

The benchmark uses the compiled [`Native`][jna-native] class from
[JNA][jna] — a large real-world Java class — as its test input.
On first run, `make bench` downloads the class, disassembles it to
[XMIR][xmir] via [jeo-maven-plugin][jeo], converts it to 𝜑 using
`phino rewrite`, and caches the results in `benchmark/tmp/`.
Subsequent runs skip straight to the benchmarks.

```bash
make bench
```

<!-- benchmark_begin -->

```text
=== parse/phi ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      1333129.634 μs
  avg:        133312.963 μs
  min:        123350.827 μs
  max:        154501.079 μs
  std dev:    11819.397 μs
=== parse/xmir ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      6095344.927 μs
  avg:        609534.493 μs
  min:        552583.460 μs
  max:        644040.036 μs
  std dev:    25360.610 μs
=== rewrite/normalize ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      562461.372 μs
  avg:        56246.137 μs
  min:        54840.174 μs
  max:        57549.116 μs
  std dev:    778.435 μs
=== print/sweet/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      2804710.586 μs
  avg:        280471.059 μs
  min:        263431.677 μs
  max:        306476.931 μs
  std dev:    12002.611 μs
=== print/sweet/flat ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      2784962.131 μs
  avg:        278496.213 μs
  min:        264622.121 μs
  max:        291750.726 μs
  std dev:    10124.274 μs
=== print/salty/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      9124364.174 μs
  avg:        912436.417 μs
  min:        892913.298 μs
  max:        935173.006 μs
  std dev:    15007.336 μs
```

The results were calculated in [this GHA job][benchmark-gha]
on 2026-09-14 at 19:01,
on Linux with 4 CPUs.

<!-- benchmark_end -->

## How to Contribute

Fork repository, make changes, then send us a [pull request][guidelines].
We will review your changes and apply them to the `master` branch shortly,
provided they don't violate our quality standards. To avoid frustration,
before sending us your pull request please make sure all your tests pass:

```bash
make all
```

To generate a local coverage report for development, run:

```bash
make coverage
```

To build a `phino` executable into the root of the repository, run:

```bash
make phino
```

This produces an executable `phino` (or `phino.exe` on Windows) in the
project root, which you can run directly for quick local testing:

```bash
./phino --version
```

You will need [GHC ≥ 9.6.7][GHC] and [Cabal ≥ 3.0 (recommended)][cabal]
or [Stack ≥ 3.0][stack] installed.

[cabal]: https://www.haskell.org/cabal/
[stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[GHC]: https://www.haskell.org/ghc/
[guidelines]: https://www.yegor256.com/2014/04/15/github-guidelines.html
[xmir]: https://news.eolang.org/2022-11-25-xmir-guide.html
[latex]: https://en.wikipedia.org/wiki/LaTeX
[java]: https://www.java.com/en/download/
[curl]: https://curl.se/
[jna]: https://github.com/java-native-access/jna
[jna-native]: https://github.com/java-native-access/jna/blob/master/src/com/sun/jna/Native.java
[jeo]: https://github.com/objectionary/jeo-maven-plugin
[benchmark-gha]: https://github.com/objectionary/phino/actions/runs/34883926782
