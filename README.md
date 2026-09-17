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
cabal install --overwrite-policy=always phino-0.0.133
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

### Symbolic λ functions

Which λ functions exist is a property of the object model being dataized, not
of the calculus, so `phino` implements none of them. They come from a YAML file
given with `--symbolic`, one entry per λ function:

```yaml
- λ: L_number_(plus|times)
  dataize:
    𝛿1: $.ρ
    𝛿2: $.x
  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )
```

The `λ` of an entry is the λ names it answers for, as a regular expression, so
the one above stands for `L_number_plus` and `L_number_times`. Under `dataize`
stand the operands it brings down to data through 𝔻, each binding a bytes meta
`𝛿1`, `𝛿2` and so on; under `morph` the operands it reduces to a normal
form through 𝕄, each binding an expression meta `𝑛1`, `𝑛2`. Both blocks are
terms of the calculus, read against the formation being fired, so `ξ` is that
formation and `$.x` its `x`, while `Φ` is the universe. Every entry numbers its
own metas from `𝛿1` and `𝑛1`, and the entry is what tells two `𝛿1` apart.

The term under `𝑛` is what the firing answers with. `phino` normalizes it
exactly as it normalizes anything else, so `--protocol`, `--partial` and
`--max-steps` work on it unchanged.

### Symbols

An entry answers, it never computes. The job of these functions is symbolic
morphing: what `5.plus( 6 )` comes to is the arithmetic of the object model and
not `phino`'s, so an entry answers a term carrying a symbol standing for a
value nobody worked out, and the data its `dataize` operands came down to is
not its to read. An answer mentioning a `𝛿` is refused where the file is read.

`𝜎` is a meta of the calculus, beside `𝑛`, `𝛿` and `𝑓`, and it stands where
a λ name stands. In a term, `𝜎1` is a concrete symbol: a λ function nothing
answers, which is what makes the value the term carries unknown. In an answer, a
bare `𝜎` asks for a fresh one, minted as the firing happens and numbered by the
run, so no two unknowns are ever spelled alike. Minting starts after the symbols
the program already carries, so a run over the 𝜑-program an earlier run wrote
never spells a fresh symbol like one already standing there.

Dataizing a symbol never gets stuck. It answers a fixed datum, 42
(`40-45-00-00-00-00-00-00`), the same one for every symbol, so 𝔻 always
answers, a `𝛿` always holds concrete data and no firing ever declines for the
lack of it:

```bash
$ cat sum.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  φ ↦ 5.plus( 6 )
⟧
$ phino dataize --symbolic=atoms.yaml --sweet --hide-rho sum.phi
40-45-00-00-00-00-00-00
```

### The keys of the file

Each `λ` is a regular expression, and it must match the whole λ name, so a
plain name such as `L_number_plus` means that one function and nothing else,
while `L_number_.*` stands for every function of `number`. The keys are unique:
nothing tells two entries under one key apart, so a second entry under a key is
unreachable and the file is refused rather than merely redundant.

A λ name no key matches has no λ function at all, so 𝔼 gets stuck on it.
Without `--symbolic` there is no entry at all and every λ function gets stuck:

```bash
$ phino dataize --sweet --hide-rho sum.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_plus'
```

The file is read before anything is parsed or reduced, so a key that is no
regular expression, an operand that is no meta of the kind its block binds, or
an answer the calculus cannot read fails the run up front rather than half-way
through a derivation.

### Recording what fired

Every λ function fired on the way to the answer may be recorded in a
machine-readable protocol, with the `--protocol` option. The protocol is a
tree: the run at the top, one block per firing under it, and inside the block
the operands the firing bound and the term it answered with.

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.txt --quiet \
    --sweet --hide-rho sum.phi
$ cat atoms.txt
𝔻(Φ)
  𝔼(L_number_plus)
    𝛿1.1 := 40-14-00-00-00-00-00-00
    𝛿2.1 := 40-18-00-00-00-00-00-00
    𝑛.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )
```

`𝔻(…)` is the run and the term it was aimed at, `𝕄(…)` where the run is a
morphing, and `𝔼(…)` is one firing, named by the entry that answered it. The
firings are numbered across the whole run, in the order they open, so `𝛿1.2`
is the value bound to `𝛿1` by the second firing of the run, whichever λ
function that was, `𝑛1.2` the same for a `morph` meta, and `𝑛.k` the k-th
answer of the whole run, so `𝑛1.2 := 𝑛.3` reads "the `𝑛1` of this firing is
the third answer". One firing binds a meta once and no two firings share a
number, so every one of these names stands on exactly one line of the file and
a line naming another one points at it and no other. Where an
operand came down to the datum a symbol stands for, the protocol writes `𝔻(𝜎1)`
in place of that 42, so a reader sees that the value was manufactured rather
than read out of the program.

`?(…)` is a λ name no entry answers, standing where the block of its firing
would have stood. Nothing fired, so nothing opens under it. The line is written
whether or not `--partial` goes on to park the run, since the protocol records
what 𝔼 was asked for, and a question it could not answer belongs there as much
as one it could:

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.txt --quiet \
    --sweet --hide-rho stuck.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_nope'
$ cat atoms.txt
𝔻(Φ)
  𝔼(L_number_plus)
    𝛿1.1 := 40-14-00-00-00-00-00-00
    𝛿2.1 := 40-18-00-00-00-00-00-00
    𝑛.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )
  ?(L_number_nope)
```

The very same file comes back with `--partial`, where the run answers the
residue instead of failing: what `phino` could not decide is a property of the
program and not of the option that decides what to do about it.

Every term is 𝜑 on a single line, whatever `--output` and `--flat` say about
the result of the run, so a program reading the protocol back never has to know
what the run printed. The file is truncated at the beginning of every run, so
it always holds the firings of exactly one run.

The blocks come in the order the reduction walks the term, and that order is
not what the dependencies are read from — the symbols are. Take a comparison
nobody can decide, a fork branching on it, and an `atoms.yaml` carrying the
entry above beside these two:

```yaml
- λ: L_number_gt
  dataize:
    𝛿1: $.ρ
    𝛿2: $.x
  𝑛: Φ.bool( if ↦ ⟦ λ ⤍ L_fork, then ↦ ∅, else ↦ ∅, φ ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧ )
- λ: L_fork
  dataize:
    𝛿1: $.φ
  morph:
    𝑛1: $.then
    𝑛2: $.else
  𝑛: 𝑛1
```

```bash
$ cat fork.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  bool ↦ ⟦ if ↦ ∅ ⟧,
  number ↦ ⟦
    φ ↦ ∅,
    plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧,
    gt(x) ↦ ⟦ λ ⤍ L_number_gt ⟧
  ⟧,
  foo(x) ↦ ⟦
    φ ↦ ξ.x.gt( 0 ).if( ξ.x.plus( ξ.x.plus( 1 ) ), ξ.x.plus( ξ.x ) ).plus( 5 )
  ⟧,
  demo ↦ ⟦ a ↦ Φ.foo( Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) ) ⟧
⟧
$ phino morph --deep --symbolic=atoms.yaml --locator=Q.demo.a \
    --protocol=fork.txt --quiet --sweet --hide-rho fork.phi
$ cat fork.txt
𝕄(Φ.demo.a)
  𝔼(L_number_gt)
    𝛿1.1 := 𝔻(𝜎1)
    𝛿2.1 := 00-00-00-00-00-00-00-00
    𝑛.1 := Φ.bool( if ↦ ⟦ λ ⤍ L_fork, then ↦ ∅, else ↦ ∅, φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧ )
  𝔼(L_number_plus)
    𝛿1.2 := 𝔻(𝜎1)
    𝛿2.2 := 3F-F0-00-00-00-00-00-00
    𝑛.2 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ )
  𝔼(L_number_plus)
    𝛿1.3 := 𝔻(𝜎1)
    𝛿2.3 := 𝔻(𝜎3)
    𝑛.3 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎4 ⟧ )
  𝔼(L_number_plus)
    𝛿1.4 := 𝔻(𝜎1)
    𝛿2.4 := 𝔻(𝜎1)
    𝑛.4 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )
  𝔼(L_fork)
    𝛿1.5 := 𝔻(𝜎2)
    𝑛1.5 := 𝑛.3
    𝑛2.5 := 𝑛.4
    𝑛.5 := 𝑛1.5
  𝔼(L_number_plus)
    𝛿1.6 := 𝔻(𝜎4)
    𝛿2.6 := 40-14-00-00-00-00-00-00
    𝑛.6 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎6 ⟧ )
```

`𝜎3` is minted by the second firing and consumed by the third as `𝔻(𝜎3)`,
`𝜎4` by the third and consumed by the last, `𝜎2` by the first and consumed by
the fork. `𝜎5` is minted and never consumed, which is how a reader sees that
the right branch was computed and thrown away. The two `morph` lines of
`𝔼(L_fork)` are recorded although nothing fires under them, since they are the
only edge from the fork back to the branch it answered with: without them
`𝑛.5 := 𝑛1.5` would name a meta nothing ever bound.

A firing that happened while an operand of another was being reduced stands one
level deeper, under the firing that asked for it. Here it never happens,
because deep morphing reduces both branches where they sit as arguments of
`if`, long before the dispatch that fires the fork.

### The protocol as XML

The name of the file decides which of the two formats `--protocol` writes: a
name ending in `.xml` gets the same tree as markup, every other name gets the
indented text above. There is no option for it, since a caller who asks for a
file called `atoms.xml` and gets text back has been told nothing useful. Here
is the run at the top of this section again:

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.xml --quiet \
    --sweet --hide-rho sum.phi
$ cat atoms.xml
<?xml version="1.0" encoding="UTF-8"?>
<protocol judgment="𝔻" of="Φ">
  <fire λ="L_number_plus" id="1">
    <bind meta="𝛿1" bytes="40-14-00-00-00-00-00-00"/>
    <bind meta="𝛿2" bytes="40-18-00-00-00-00-00-00"/>
    <answer symbol="𝜎1">Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )</answer>
  </fire>
</protocol>
```

`<protocol>` is the run, `judgment` saying which of 𝔻 and 𝕄 it was and `of`
the term it was aimed at. `<fire>` is one firing, `λ` naming the entry that
answered it and `id` numbering it within the run. `<bind>` is one meta the
firing bound: `meta` names it and the element holds the term it took, or
`bytes` holds the datum instead where a `dataize` operand came down to one.
`<answer>` holds the term the firing answered with.

Where a value denotes a symbol, the element says so with `symbol="𝜎1"`
instead of the 42 that dataizing a symbol answers, and that name is the only
thing a reader joins lines on. It is spelled the way every term carrying it is
spelled, so the join compares two strings that look alike rather than a number
against a name. In the fork above, `𝔼(L_fork)` becomes a `<fire>` whose
condition is `<bind meta="𝛿1" symbol="𝜎2"/>` and whose answer is
`<answer symbol="𝜎4">`: the condition is the symbol the first firing minted
and the answer the one the third minted. The text format says the same with
`𝛿1.5 := 𝔻(𝜎2)` and `𝑛.5 := 𝑛1.5`, which name a line by counting; the
markup names the value, so the counting is gone. A term standing for nothing
takes no attribute at all, the terminator ⊥ included, since its own text
already says what it is.

A λ name no entry answers is `<stuck λ="…"/>`, standing where its `<fire>`
would have stood, and a firing that happened while an operand of another was
being reduced is a `<fire>` inside the one that asked, which is what the
deeper indentation means in the text. Elements are written as the run goes and
the open ones are closed when it ends, so a run that fails still leaves a
well-formed document behind:

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.xml --quiet \
    --sweet --hide-rho stuck.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_nope'
$ cat atoms.xml
<?xml version="1.0" encoding="UTF-8"?>
<protocol judgment="𝔻" of="Φ">
  <fire λ="L_number_plus" id="1">
    <bind meta="𝛿1" bytes="40-14-00-00-00-00-00-00"/>
    <bind meta="𝛿2" bytes="40-18-00-00-00-00-00-00"/>
    <answer symbol="𝜎1">Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )</answer>
  </fire>
  <stuck λ="L_number_nope"/>
</protocol>
```

### Reducing a term inside a universe

A term that is no part of the program may still be reduced against it, with the
`--inside` option: the expression it names is bound to a synthetic attribute
prepended to the input expression, which the run takes as the universe Φ,
normalized there and then reduced.

```bash
$ cat universe.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧
⟧
$ phino dataize --symbolic=atoms.yaml --inside='5.plus( 6 )' universe.phi
40-45-00-00-00-00-00-00
```

This is the very trick `phino` plays internally to reduce the operands of a
firing, made available to whoever asks it to reduce a term the program does not
hold. The option cannot be combined with `--locator`, since it aims the run at
the binding it mints itself. Both `dataize` and `morph` take `--symbolic` and
`--inside`.

### Partial evaluation

A λ function no entry of the `--symbolic` file answers fails the run. This is
what happens when an operation is deliberately left out — an input the object
model has not declared yet, or an operation whose answer is not known. With
`--partial`, dataization becomes partial evaluation instead: what the known
inputs decide is computed, the rest survives as the residual program, which is
printed in place of the bytes, and the run ends successfully:

```bash
$ cat partial.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦
    φ ↦ ∅,
    plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧,
    times(x) ↦ ⟦ λ ⤍ L_number_times ⟧,
    as-bool ↦ ⟦ λ ⤍ L_number_as_bool ⟧
  ⟧,
  φ ↦ 2.times( 3 ).plus( 4 ).as-bool
⟧
$ phino dataize --symbolic=atoms.yaml --sweet --hide-rho partial.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_as_bool'
$ phino dataize --symbolic=atoms.yaml --partial --sweet --hide-rho partial.phi
⟦ λ ⤍ L_number_as_bool ⟧
```

Here `2.times( 3 ).plus( 4 )` was answered by the entries the file carries, so
it was reduced — the symbol it came to sits in the hidden `ρ` of the residual
program — while `as-bool` names a λ function no entry answers, so it stays in
place as a normal-form subterm. A stuck site writes nothing into the
`--protocol` file, since nothing fired there:

```bash
$ phino dataize --symbolic=atoms.yaml --partial --protocol=atoms.txt --quiet \
    --sweet --hide-rho partial.phi
$ cat atoms.txt
𝔻(Φ)
  𝔼(L_number_times)
    𝛿1.1 := 40-00-00-00-00-00-00-00
    𝛿2.1 := 40-08-00-00-00-00-00-00
    𝑛.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )
  𝔼(L_number_plus)
    𝛿1.2 := 𝔻(𝜎1)
    𝛿2.2 := 40-10-00-00-00-00-00-00
    𝑛.2 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )
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
$ phino dataize --symbolic=atoms.yaml --sweet --hide-rho two.phi
40-45-00-00-00-00-00-00
$ phino morph --symbolic=atoms.yaml --locator=Q.φ --sweet --hide-rho two.phi
⟦ x ↦ 7, λ ⤍ L_number_plus ⟧
```

The inner `5.plus( 6 )` fires, because `.plus` is dispatched on its result,
and the symbol it answered with lands in the `ρ` hidden by `--hide-rho`. The
outer application is saturated but bare, so 𝕄 returns it and is finished;
firing it is dataization's job and takes `dataize` on to a datum.

The default locator `Q` morphs the whole top formation, which 𝕄 returns
unchanged, so `--locator` is how one aims 𝕄 at a subterm, exactly as in
`dataize`. Unlike 𝔻, 𝕄 is total: where no formation is reachable the answer
is the terminator `⊥`, printed rather than reported as a failed run:

```bash
$ phino morph --locator=Q.x <<< '⟦ x ↦ ξ ⟧'
⊥
```

The whole `dataize` option surface applies unchanged — `--symbolic`,
`--inside`, `--sequence`, `--headers`, `--steps-dir`, `--protocol`,
`--partial`, `--max-steps`, `--shuffle`/`--seed`, `--output`, `--focus` and the
rest.

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
  number ↦ ⟦ φ ↦ ∅, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧ ⟧,
  bar(x) ↦ ⟦ λ ⤍ L_bar ⟧,
  demo ↦ ⟦ foo ↦ ⟦ n ↦ 3, φ ↦ Φ.bar( ξ.n.times( 5 ).times( 7 ) ) ⟧ ⟧
⟧
$ phino morph --symbolic=atoms.yaml --inside='Q.demo.foo' \
    --sweet --hide-rho gap.phi
⟦ n ↦ 3, φ ↦ Φ.bar( n.times( 5 ).times( 7 ) ) ⟧
$ phino morph --deep --symbolic=atoms.yaml --inside='Q.demo.foo' \
    --sweet --hide-rho gap.phi
⟦ n ↦ 3, φ ↦ Φ.bar( Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ) ⟧
```

Every binding of the formation is entered, recursively. 𝕄 is asked about the
term standing there and, where it lands on a saturated formation whose λ an
entry answers, that λ is fired and 𝕄 is asked about the answer again. A term on
whose way a λ function fired is replaced by the answer of the last firing,
which is the 𝜑-program the entry wrote rather than the normal form of it, so
the symbol standing for the arithmetic stands where the arithmetic stood. A
term nothing fired on stays exactly as it was written and only its own parts
are walked, so `Φ.bar` keeps its name and what comes back is still the same
program, reduced as far as the file allows. The step joins the chain under the
name `deep`, so `--sequence` shows it, and `--max-steps` bounds the walk.

Two things are left alone. A λ no entry answers is not fired at all, so
`--deep` stays as total as 𝕄 itself and needs no `--partial`; a λ function that
gets stuck deeper on a spine still fails the run, and `--partial` parks it,
leaving that term as it was written. A formation still holding a void
binding is not fired either: the void is an argument the program has not given
yet, so `times(x) ↦ ⟦ λ ⤍ L_number_times ⟧` is a method waiting to be applied,
not an application waiting to be computed. Walking the whole program therefore
folds what it can and leaves the object model as it was declared:

```bash
$ phino morph --deep --symbolic=atoms.yaml --sweet --hide-rho gap.phi
⟦
  bytes(φ) ↦ ⟦⟧,
  number(φ) ↦ ⟦ times(x) ↦ ⟦ λ ⤍ L_number_times ⟧ ⟧,
  bar(x) ↦ ⟦ λ ⤍ L_bar ⟧,
  demo ↦ ⟦ foo ↦ ⟦ n ↦ 3, φ ↦ Φ.bar( Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ) ) ⟧ ⟧
⟧
```

### Acyclic morphing

Whether a program terminates is the object model's business, not the
calculus's, so phino prevents no recursion of its own and `--max-steps` is what
ends a run that never finishes. An entry answering with a firing of itself
therefore spends the whole budget before it fails, and what it fails on is the
limit rather than the loop:

```bash
$ cat loop.yaml
- λ: L_loop
  𝑛: ⟦ λ ⤍ L_loop ⟧
$ cat loop.phi
⟦ x ↦ ⟦ λ ⤍ L_loop ⟧.foo ⟧
$ phino morph --symbolic=loop.yaml --locator='Q.x' --max-steps=40 loop.phi
[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=40
```

The `--acyclic` flag makes morphing notice. Every frame of 𝕄 remembers the
terms the frames above it are reducing, and a term that comes back is a
question only ever answered by asking it again, so the flag stops there and
parks the site the way `--partial` parks a λ function that cannot fire: the
answer is the term the spine had reached, left where it stood, and the command
exits successfully.

```bash
$ phino morph --symbolic=loop.yaml --locator='Q.x' --acyclic \
    --max-steps=40 --hide-rho loop.phi
⟦ λ ⤍ L_loop ⟧.foo
```

What it remembers is the branch from the run down to the frame asking, never
everything the run has touched, so two sibling subterms that happen to be
written alike stay two terms and only a term genuinely reached from itself is a
loop. The cut costs one lookup and fires on the turn the repeat appears, so
raising `--max-steps` from 40 to a million changes neither the answer nor the
time. The flag belongs to `morph` alone, needs no `--partial`, and promises
nothing about programs that loop without ever repeating a term — those still
end on the budget.

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
  { \phinoDataize{ [[ B_1, D> \delta_0, B_2 ]] } }
  { \delta_0 }
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
* `!d` || `𝛿` - bytes in meta delta binding
* `!F` || `𝑓` - function name in meta lambda binding
* `!S` || `𝜎` - a symbol standing where a λ name stands (see
                [Symbols](#symbols)). It is spelled the way a meta variable is
                spelled but is a name and no capture: `𝜎1` is one concrete
                symbol, which no substitution ever binds, and a bare `𝜎` in the
                answer of a `--symbolic` entry asks for a fresh one

A meta variable carries a suffix, like `!B1` or `𝜏2`, to name what it
captured, so that the `result`, `when`, `where` and `having` of a rule can
read it back. An index starts with one: a suffix of `0`, as in `!B0` or
`𝜏0`, is refused where it is written, because it is a first index spelled
wrong and no name. A positional argument keeps counting from zero, though,
since `α0` is an index of the calculus and no meta variable.

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
  total:      1848919.791 μs
  avg:        184891.979 μs
  min:        170130.602 μs
  max:        216709.215 μs
  std dev:    16431.857 μs
=== parse/xmir ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      7655720.406 μs
  avg:        765572.041 μs
  min:        691393.465 μs
  max:        920732.034 μs
  std dev:    65318.032 μs
=== rewrite/normalize ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      850714.162 μs
  avg:        85071.416 μs
  min:        71749.588 μs
  max:        101463.906 μs
  std dev:    11177.734 μs
=== print/sweet/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      4148684.815 μs
  avg:        414868.482 μs
  min:        395431.818 μs
  max:        434185.487 μs
  std dev:    12718.188 μs
=== print/sweet/flat ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      4247376.776 μs
  avg:        424737.678 μs
  min:        398252.179 μs
  max:        477707.984 μs
  std dev:    20693.247 μs
=== print/salty/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      14096309.847 μs
  avg:        1409630.985 μs
  min:        1371330.848 μs
  max:        1439867.094 μs
  std dev:    21527.414 μs
```

The results were calculated in [this GHA job][benchmark-gha]
on 2026-09-16 at 15:53,
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
[benchmark-gha]: https://github.com/objectionary/phino/actions/runs/35116857202
