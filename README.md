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
`--max-steps` work on it unchanged. It may name any meta the entry bound,
those of the two blocks below among them.

### Standing data into unknowns

There is a third block, `symbolize`, and it reduces nothing. It takes a term
another meta of the entry is already bound to and binds an expression meta of
its own to that same term with every datum in it standing for an unknown:

```yaml
- λ: L_fork
  dataize:
    𝛿1: $.φ
  morph:
    𝑛1: $.left
    𝑛2: $.right
  symbolize:
    𝑛3: 𝑛1
    𝑛4: 𝑛2
  𝑛: 𝑛3
```

The right-hand side of a line names a meta bound by `morph` or by a
`symbolize` line above it, and nothing else; a term nobody reduced has no data
to stand. Every `Δ ⤍ b` binding of that term becomes a `λ ⤍ 𝜎k` naming a
fresh symbol, one per occurrence, so `⟦ Δ ⤍ b ⟧` reads as `⟦ λ ⤍ 𝜎k ⟧` and a
literal tuple gets several. A term carrying no datum passes through as it was.

Only the `φ` chain is walked. A term carries the value it stands for where
that chain ends, so a datum standing anywhere else says nothing about the term
and is left alone, the whole subtree of it. What sits under `ρ` belongs to the
object around this one, and a normal form drags the universe it was reduced
inside along under `ρ`, so a walk reaching into it would stand the data of the
whole program into unknowns to say one thing about one term. What sits under a
method is code and not data: the `-1` of a `neg ↦ ⟦ φ ↦ ξ.ρ.times( -1 ) ⟧`
nobody has called is the body of a method, and minting a symbol for it, and for
every other literal every method of the carrier declares, would write unknowns
nobody ever reads.

This is what lets an entry compare two branches of a fork. A literal is sugar
for `Φ.number( Φ.bytes( ⟦ Δ ⤍ … ⟧ ) )`, so a branch computed from a literal
keeps a datum three levels down where a branch computed from an unknown keeps
`⟦ λ ⤍ 𝜎 ⟧`. A `Δ` against a `λ` is a difference in kind and not in value, and
after the stage both branches carry `⟦ λ ⤍ 𝜎 ⟧` where they differ.

### Joining the branches of a fork

A branching λ function answers neither of its branches. Which one the program
takes is decided by a value nobody worked out, so handing one of them through
would drop the branch point from the program altogether and a reader would see
the condition computed and thrown away. `join` is the fourth block, and it
reduces nothing either: it takes two metas the entry has bound already and
binds one of its own to the two terms joined into one.

```yaml
- λ: L_fork
  dataize:
    𝛿1: $.φ
  morph:
    𝑛1: $.left
    𝑛2: $.right
  symbolize:
    𝑛3: 𝑛1
    𝑛4: 𝑛2
  join:
    𝑛5: [𝑛3, 𝑛4]
  𝑛: 𝑛5
```

A line names two metas bound by `morph`, by `symbolize` or by a `join` line
above it, and never three: it stands for a choice between two branches, and a
walk over three terms in parallel is no such choice. The meta it binds is one
like any other, so the answer may name it alone, as above, or stand it inside a
larger term.

`phino` takes the two terms and requires them to match verbatim, with one
exception: where `⟦ λ ⤍ 𝜎A ⟧` in one meets a different `⟦ λ ⤍ 𝜎B ⟧` in the
other, it mints a fresh `𝜎C` and stands it there. The same symbol on both sides
stays as it is, and the same pair met again further down gets the same fresh
symbol, since it is one choice however often the two terms differ by it; two
different pairs get two fresh symbols. Two identical terms join into that same
term and nothing is minted at all. The join keeps the type by construction,
being the terms' own shape, so the file needs to know nothing about carriers.

Only the `φ` chain is compared, exactly as `symbolize` stands only that chain
into unknowns: a term carries the value it stands for where its `φ` chain ends,
so every other binding is taken from the first branch, the whole subtree of it,
and never compared at all. The two branches of a fork reach their normal forms
in scopes of their own, so their `ρ` differ wherever the reduction left a
trace, and comparing them would refuse the join over something saying nothing
about either branch; a method is the same, its body being code nobody has
called, so two branches differing inside one are not two values. The joined
term keeps the methods and the `ρ` of the first of the two, being of its shape,
which is what lets the program go on dispatching on what the fork answered.

A join is only ever between two expressions and a datum is never joined with
anything, which is why `symbolize` runs before it: a known symbol, one that
stage minted for a datum, is a symbol like any other here, so a literal branch
joins with a computed one and two literal branches join too. Any other
difference — a datum against a symbol, two different data, a binding one term
carries and the other does not — is no join at all, and the firing gets stuck
the way a λ function no entry answers does, so `--partial` parks it rather than
aborting the run. A fork whose branches differ in structure, such as a `Φ.true`
and a `Φ.false` written as `φ ↦ ξ.left` against `φ ↦ ξ.right`, is stuck, and
bringing two such branches to one shape is the program's job and not `phino`'s.

Every symbol a join mints is written into the protocol as a fact of its own,
so a reader ties it to the two it stands for without diffing the terms; the
section on `--protocol` below shows one.

### Symbols

An entry answers, it never computes. The job of these functions is symbolic
morphing: what `5.plus( 6 )` comes to is the arithmetic of the object model and
not `phino`'s, so an entry answers a term carrying a symbol standing for a
value nobody worked out, and the data its `dataize` operands came down to is
not its to read. An answer mentioning a `𝛿` is refused where the file is read.

`𝜎` is a meta of the calculus, beside `𝑛`, `𝛿` and `𝑓`, and it stands where
a λ name stands. In a term, `𝜎1` is a concrete symbol: a λ function nothing
answers, which is what makes the value the term carries unknown. Firing it is
therefore the same question as firing a λ name the `--symbolic` file does not
carry, and gets the same answer: 𝔼 stops there, the protocol records the site as
`?(𝜎1)`, and `--partial` leaves the term where it stands. Dispatching an
attribute off a symbol — `⟦ λ ⤍ 𝜎1 ⟧.plus( 5 )` — therefore taints its own
binding and nothing else; what stands beside it still computes. In an answer, a
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

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.txt --quiet \
    --sweet --hide-rho sum.phi
$ cat atoms.txt
𝔻(Φ)
  𝔼(L_number_plus)  # 𝔻(Φ)
    𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)
    𝛿2.1 := 40-18-00-00-00-00-00-00  # 𝔻(ξ.x)
    𝑛.1.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )  # 𝑛
    𝑛.1.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧  # 𝕄(𝑛.1.1)
```

<!-- markdownlint-enable MD013 -->

`𝔻(…)` is the run and the term it was aimed at, `𝕄(…)` where the run is a
morphing, and `𝔼(…)` is one firing, named by the entry that answered it and
commented with the judgment that asked for it and the site it was fired at.
Every comment of the file is of that shape: a judgment applied to a term, which
is the intent the value beside it came from. The firings are numbered across the
whole run, in the order they open, so `𝛿1.2` is the value bound to `𝛿1` by
the second firing of the run, whichever λ function that was, `𝑛1.2` the same
for a `morph` meta, and `𝑛.3.2` the answer of the third firing, so
`𝑛1.2 := 𝑛.3.2` reads "the `𝑛1` of this firing is what the third firing
answered". One firing binds a meta once and no two firings
share a number, so every one of these names stands on exactly one line of the
file and a line naming another one points at it and no other.

An answer stands on two lines and not one. A firing answers the term its entry
wrote and `phino` morphs that term before standing it back into the program, so
`𝑛.1.1` is what the entry wrote, with the symbols this firing minted already in
it, commented with `𝑛` to name the key it was read from, and `𝑛.1.2` is the
normal form 𝕄 made of it, commented with `𝕄(𝑛.1.1)` to say where it came from.
It is the same morphing every other term goes through, and writing only its
outcome would have the formation of `number` appear in place of the three
tokens the entry wrote with nothing saying why. Whatever that morphing fires
opens its own block between the two lines, exactly where a firing an operand
took opens one, so the order the lines come in is the order the work was done
in.

Where an operand came down to the datum a symbol stands for, the protocol writes
`𝔻(⟦ λ ⤍ 𝜎1 ⟧)` in place of that 42, so a reader sees that the value was
manufactured rather than read out of the program. A `𝜎` is the name of a λ
function and no term of its own, so 𝔻 is applied to the formation carrying it
and never to the name alone.

A `symbolize` line writes a line per fresh symbol it minted, ahead of the line
binding the term that carries them, and that line is a fact and no assignment:
`𝔻(⟦ λ ⤍ 𝜎44 ⟧) == 3F-F0-00-00-00-00-00-00` says that dataizing the formation
`𝜎44` names answers those bytes. Nothing binds bytes to a `𝜎`, since it is
neither a datum nor a term. A consumer reading the protocol back treats a
symbol with such a fact as a constant and every other symbol as an unknown.

The line binding the term of a `symbolize` one is commented with the meta it
was told to stand, `𝑛3.1 := ⟦ λ ⤔ 𝜆8 ⟧  # 𝑛1`, and with no judgment
around it: standing the data of a term into unknowns is the file's own
operation and nothing of the calculus runs there, so the line names a meta of
the entry the way a `join` line names the two it joined. A comment carries the
letter of a judgment exactly where a judgment made the value.

The site of a firing is a locator, written as a comment the way an operand
line writes the term it came from, under the letter of the judgment that asked
for the firing: 𝔼 is fired by the `ml` rule of morphing and by the `fire` rule
of dataization, so `𝕄(Φ.demo.a.φ)` is a λ function fired while 𝕄 was reducing
that binding and `𝔻(Φ)` one fired because dataization demanded data of `Φ`.
A chain such as `5.plus( 6 ).plus( 7 )` writes both: the inner call is fired
while 𝕄 reduces the head of the outer dispatch, the outer one because 𝔻 asked
for the data. The site itself is where in the program the firing
belongs: the term the run was aimed at, so `Φ` for a run that was aimed at
nothing in particular, and, under `--deep`, the binding the walk had entered
when the λ function fired, since that walk reduces every part of the program
in turn and one entry answers the same way wherever it is fired. A locator
names a binding and reaches no further, so a firing standing deeper inside a
term than that — under a dispatch, or in the argument of an application — is
written under the last binding the walk entered, which is the smallest part of
the program a reader can aim a run of their own at. An operand of a firing is
reduced bound to a synthetic attribute of the universe (see `--inside` below),
so a λ function fired while it came down is written under that attribute and
not under the site of the firing that asked for it.

An operand line ends in the judgment that reduced it and the term it was
reduced from, written as a comment after two spaces and `#`. The value alone
says what the meta was bound to and neither what it was bound from nor what
was done to it, so `𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)` reads "the
`𝛿1` of this firing is the `ρ` of the formation brought down through 𝔻, and
it came down to 20", where a `morph` operand reads `𝑛1.5 := 𝑛.3.2  # 𝕄(ξ.then)`
and says that the `then` of the formation reached its normal form through 𝕄.
Which of the two judgments ran is the whole difference between a line ending
in data and one ending in a term. It is the very term the entry wrote under
that meta, spelled the way the calculus reads it — `$` is read as `ξ` — so a
reader never has to open the `--symbolic` file beside the protocol and match
every line by λ name and meta number.

`?(…)` is a λ name no entry answers, standing where the block of its firing
would have stood. Nothing fired, so nothing opens under it. The line is
commented with the judgment that asked and the formation it was asking about,
`𝕄(⟦ λ ⤍ L_none ⟧)`, the way an operand line is commented with the term it was
reduced from: 𝔼 is fired by the `ml` rule of morphing and by the `fire` rule
of dataization, so the letter says where in the reduction the site stands and
the term says which object the λ function that could not fire belongs to. It
is written
whether or not `--partial` goes on to park the run, since the protocol records
what 𝔼 was asked for, and a question it could not answer belongs there as much
as one it could — once per site and not once per attempt, since a site
`--partial` parks stays in the residue and `--deep` walks over it again:

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.txt --quiet \
    --sweet --hide-rho stuck.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_nope'
$ cat atoms.txt
𝔻(Φ)
  𝔼(L_number_plus)  # 𝕄(Φ)
    𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)
    𝛿2.1 := 40-18-00-00-00-00-00-00  # 𝔻(ξ.x)
    𝑛.1.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )  # 𝑛
    𝑛.1.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧, nope ↦ ⟦ λ ⤍ L_number_nope ⟧ ⟧  # 𝕄(𝑛.1.1)
  ?(L_number_nope)  # 𝔻(⟦ λ ⤍ L_number_nope ⟧)
```

<!-- markdownlint-enable MD013 -->

The very same file comes back with `--partial`, where the run answers the
residue instead of failing: what `phino` could not decide is a property of the
program and not of the option that decides what to do about it.

Every term is 𝜑 on a single line, whatever `--output` and `--flat` say about
the result of the run, so a program reading the protocol back never has to know
what the run printed. The file is truncated at the beginning of every run, so
it always holds the firings of exactly one run.

The blocks come in the order the reduction walks the term, and that order is
not what the dependencies are read from — the symbols are. Take a comparison
nobody can decide, a fork branching on it, and an `atoms.yaml` of three
entries, the λ functions named briefly to keep the lines below short:

```yaml
- λ: L_plus
  dataize:
    𝛿1: $.ρ
    𝛿2: $.x
  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )
- λ: L_gt
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
  join:
    𝑛3: [𝑛1, 𝑛2]
  𝑛: 𝑛3
```

<!-- markdownlint-disable MD013 -->

```bash
$ cat fork.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  bool ↦ ⟦ if ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(x) ↦ ⟦ λ ⤍ L_plus ⟧, gt(x) ↦ ⟦ λ ⤍ L_gt ⟧ ⟧,
  foo(x) ↦ ⟦
    φ ↦ ξ.x.gt( 0 ).if( ξ.x.plus( ξ.x.plus( 1 ) ), ξ.x.plus( ξ.x ) ).plus( 5 )
  ⟧,
  demo ↦ ⟦ a ↦ Φ.foo( Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) ) ⟧
⟧
$ phino morph --deep --symbolic=atoms.yaml --locator=Q.demo.a \
    --protocol=fork.txt --quiet --sweet --hide-rho fork.phi
$ cat fork.txt
𝕄(Φ.demo.a)
  𝔼(L_gt)  # 𝕄(Φ.demo.a.φ)
    𝛿1.1 := 𝔻(⟦ λ ⤍ 𝜎1 ⟧)  # 𝔻(ξ.ρ)
    𝛿2.1 := 00-00-00-00-00-00-00-00  # 𝔻(ξ.x)
    𝑛.1.1 := Φ.bool( if ↦ ⟦ λ ⤍ L_fork, then ↦ ∅, else ↦ ∅, φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧ )  # 𝑛
    𝑛.1.2 := ⟦ if ↦ ⟦ λ ⤍ L_fork, then ↦ ∅, else ↦ ∅, φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧ ⟧  # 𝕄(𝑛.1.1)
  𝔼(L_plus)  # 𝕄(Φ.demo.a.φ)
    𝛿1.2 := 𝔻(⟦ λ ⤍ 𝜎1 ⟧)  # 𝔻(ξ.ρ)
    𝛿2.2 := 3F-F0-00-00-00-00-00-00  # 𝔻(ξ.x)
    𝑛.2.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ )  # 𝑛
    𝑛.2.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎3 ⟧, plus(x) ↦ ⟦ λ ⤍ L_plus ⟧, gt(x) ↦ ⟦ λ ⤍ L_gt ⟧ ⟧  # 𝕄(𝑛.2.1)
  𝔼(L_plus)  # 𝕄(Φ.demo.a.φ)
    𝛿1.3 := 𝔻(⟦ λ ⤍ 𝜎1 ⟧)  # 𝔻(ξ.ρ)
    𝛿2.3 := 𝔻(⟦ λ ⤍ 𝜎3 ⟧)  # 𝔻(ξ.x)
    𝑛.3.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎4 ⟧ )  # 𝑛
    𝑛.3.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎4 ⟧, plus(x) ↦ ⟦ λ ⤍ L_plus ⟧, gt(x) ↦ ⟦ λ ⤍ L_gt ⟧ ⟧  # 𝕄(𝑛.3.1)
  𝔼(L_plus)  # 𝕄(Φ.demo.a.φ)
    𝛿1.4 := 𝔻(⟦ λ ⤍ 𝜎1 ⟧)  # 𝔻(ξ.ρ)
    𝛿2.4 := 𝔻(⟦ λ ⤍ 𝜎1 ⟧)  # 𝔻(ξ.x)
    𝑛.4.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎5 ⟧ )  # 𝑛
    𝑛.4.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎5 ⟧, plus(x) ↦ ⟦ λ ⤍ L_plus ⟧, gt(x) ↦ ⟦ λ ⤍ L_gt ⟧ ⟧  # 𝕄(𝑛.4.1)
  𝔼(L_fork)  # 𝕄(Φ.demo.a.φ)
    𝛿1.5 := 𝔻(⟦ λ ⤍ 𝜎2 ⟧)  # 𝔻(ξ.φ)
    𝑛1.5 := 𝑛.3.2  # 𝕄(ξ.then)
    𝑛2.5 := 𝑛.4.2  # 𝕄(ξ.else)
    𝔻(⟦ λ ⤍ 𝜎6 ⟧) ∈ { 𝔻(⟦ λ ⤍ 𝜎4 ⟧), 𝔻(⟦ λ ⤍ 𝜎5 ⟧) }
    𝑛3.5 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎6 ⟧, plus(x) ↦ ⟦ λ ⤍ L_plus ⟧, gt(x) ↦ ⟦ λ ⤍ L_gt ⟧ ⟧  # [𝑛1, 𝑛2]
    𝑛.5.1 := 𝑛3.5  # 𝑛
    𝑛.5.2 := 𝑛3.5  # 𝕄(𝑛.5.1)
  𝔼(L_plus)  # 𝕄(Φ.demo.a.φ)
    𝛿1.6 := 𝔻(⟦ λ ⤍ 𝜎6 ⟧)  # 𝔻(ξ.ρ)
    𝛿2.6 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.x)
    𝑛.6.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎7 ⟧ )  # 𝑛
    𝑛.6.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎7 ⟧, plus(x) ↦ ⟦ λ ⤍ L_plus ⟧, gt(x) ↦ ⟦ λ ⤍ L_gt ⟧ ⟧  # 𝕄(𝑛.6.1)
```

<!-- markdownlint-enable MD013 -->

`𝜎3` is minted by the second firing and consumed by the third as
`𝔻(⟦ λ ⤍ 𝜎3 ⟧)`, and `𝜎2` by the first and consumed by the fork. `𝜎4` and
`𝜎5` are what the two branches came to, and the fork consumes both: its `join`
line makes them one term carrying `𝜎6`, which the `plus( 5 )` standing after
the fork then reads as `𝔻(⟦ λ ⤍ 𝜎6 ⟧)`. The line
`𝔻(⟦ λ ⤍ 𝜎6 ⟧) ∈ { 𝔻(⟦ λ ⤍ 𝜎4 ⟧), 𝔻(⟦ λ ⤍ 𝜎5 ⟧) }` is what ties the three
together: dataizing the formation `𝜎6` names answers what dataizing one of the
other two answers. A reader who knows the entry knows that `𝛿1` is what decides
between them and that the first of the two belongs to `then`. Nothing is
assigned to a `𝜎`, it being the name of a λ function, so the fact stands on a
line of its own the way what a `symbolize` line knows does, and the line under
it binds the meta, commented with the two metas it joined.

Were the fork to answer one of its branches instead, the value of the other
would be minted and never consumed, and `foo` would read as a program that
computes a condition, computes both branches and then drops the branch point.

All six firings stand under `Φ.demo.a.φ`, which is as near as a locator gets
to any of them: the walk entered the `φ` of the formation `Φ.demo.a` morphs to,
and everything under it — the dispatches of the chain, the arguments of `if` —
stands under no attribute of any formation, so the binding the walk had entered
is what the protocol writes them under.

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
<dataize locator="Φ">
  <evaluate λ="L_number_plus" id="1" judgment="dataize" locator="Φ">
    <bind meta="𝛿1.1">40-14-00-00-00-00-00-00</bind>
    <bind meta="𝛿2.1">40-18-00-00-00-00-00-00</bind>
    <minted>𝜎1</minted>
    <built meta="𝑛.1.1">Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )</built>
    <answer meta="𝑛.1.2">⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧</answer>
  </evaluate>
</dataize>
```

The root is the run itself, named after the judgment it ran — `<dataize>` for a
𝔻, `<morph>` for a 𝕄 — with `locator` naming the term it was aimed at, which is
what the text format opens with as `𝔻(Φ)`. `<evaluate>` is one firing of 𝔼, `λ`
naming the entry that answered it, `id` numbering it within the run, `judgment`
naming the one that asked for the firing — the same word the root and a
`<stuck>` carry — and `locator` naming the site it was fired at. The text
format writes those two as the comment of its line, `𝔻(Φ)`.
`<bind>` is one meta the firing bound, `meta` naming it the same way the text
format names it, counter and all, and the element holding the value it took: a
term where the operand was reduced with 𝕄, the datum itself where a `dataize`
operand came down to data. `<dataize>` inside a firing is the other thing a
`dataize` operand may come to, the datum manufactured for an unknown, and holds
the formation that unknown names rather than the 42 standing for it: a `𝜎` is
the name of a λ function and no term of its own, so what 𝔻 was applied to is
`⟦ λ ⤍ 𝜎2 ⟧` and never `𝜎2` alone. It carries `meta` where the root carries
`locator`, the same difference the text format draws between `𝔻(Φ)` at the top
and `𝛿1.2 := 𝔻(…)` in a block. The name of the element is what tells a
manufactured datum from data, the way `𝔻(…)` does in the text format, so
nothing has to be read off the presence of an attribute. `<answer>` holds the
term the firing answered with, named the same way by its own `meta`, and
`<built>` before it holds the term the entry wrote, the one 𝕄 made that answer
of: two elements rather than two attributes of one, for the same reason
`<dataize>` is no `<bind>`.

`<known symbol="𝜎44">3F-F0-00-00-00-00-00-00</known>` is the fact a `symbolize`
line writes about a symbol it minted, which the text format writes as
`𝔻(⟦ λ ⤍ 𝜎44 ⟧) == …`: the symbol stands in the attribute a reader joins
lines on and the data dataizing its formation answers are the text of the
element. It takes `symbol` and not `meta`, since the fact is about the unknown
and not about a meta the firing bound.

`<joined symbol="𝜎6">𝜎4 𝜎5</joined>` is the same kind of fact about a symbol
a `join` line minted, which the text format writes as
`𝔻(⟦ λ ⤍ 𝜎6 ⟧) ∈ { 𝔻(⟦ λ ⤍ 𝜎4 ⟧), 𝔻(⟦ λ ⤍ 𝜎5 ⟧) }`: the fresh symbol stands
in `symbol` and the two it was minted for are the text, in the order the line
listed the metas it joined. A line whose two terms differ at several places
writes one element per pair of symbols, and one whose terms are alike writes
none. The meta the line binds is a `<bind>` like every other meta of the
firing.

`<minted>𝜎1</minted>` is one symbol the firing minted, one element per bare `𝜎`
the entry wrote its answer with, standing inside the block ahead of the
`<built>` carrying them. That is the edge a reader joins on: a later
`<dataize meta="𝛿1.5">⟦ λ ⤍ 𝜎2 ⟧</dataize>` names the symbol the firing that
wrote `<minted>𝜎2</minted>` handed out. A firing minting two symbols writes two
elements and one minting none writes none, which no attribute on the answer
could say: a term may carry several symbols, or carry one where the value it
stands for is not a symbol at all. In the fork above, `𝔼(L_gt)` writes
`<minted>𝜎2</minted>` although `𝜎2` sits under `if` and not where the value of
the term is, while `𝔼(L_fork)` writes none at all, since the symbol it answers
with comes from a `join` line and stands in a `<joined>` of its own.

A λ name no entry answers is `<stuck λ="…">`, standing where its `<evaluate>`
would have stood with the formation 𝔼 was fired against as its text and the
judgment that asked in its `judgment` attribute, where the text format writes
the letter of it. A firing that happened while an operand of another was being
reduced is an `<evaluate>` inside the one that asked, which is what the deeper
indentation means in the text. Elements are written as the run goes and
the open ones are closed when it ends, so a run that fails still leaves a
well-formed document behind:

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.xml --quiet \
    --sweet --hide-rho stuck.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_nope'
$ cat atoms.xml
<?xml version="1.0" encoding="UTF-8"?>
<dataize locator="Φ">
  <evaluate λ="L_number_plus" id="1" judgment="morph" locator="Φ">
    <bind meta="𝛿1.1">40-14-00-00-00-00-00-00</bind>
    <bind meta="𝛿2.1">40-18-00-00-00-00-00-00</bind>
    <minted>𝜎1</minted>
    <built meta="𝑛.1.1">Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )</built>
    <answer meta="𝑛.1.2">⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧, nope ↦ ⟦ λ ⤍ L_number_nope ⟧ ⟧</answer>
  </evaluate>
  <stuck λ="L_number_nope" judgment="dataize">⟦ λ ⤍ L_number_nope ⟧</stuck>
</dataize>
```

<!-- markdownlint-enable MD013 -->

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
place as a normal-form subterm. A stuck site opens no block in the
`--protocol` file, since nothing fired there, and stands in it as `?(…)`:

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --partial --protocol=atoms.txt --quiet \
    --sweet --hide-rho partial.phi
$ cat atoms.txt
𝔻(Φ)
  𝔼(L_number_times)  # 𝕄(Φ)
    𝛿1.1 := 40-00-00-00-00-00-00-00  # 𝔻(ξ.ρ)
    𝛿2.1 := 40-08-00-00-00-00-00-00  # 𝔻(ξ.x)
    𝑛.1.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )  # 𝑛
    𝑛.1.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧, as-bool ↦ ⟦ λ ⤍ L_number_as_bool ⟧ ⟧  # 𝕄(𝑛.1.1)
  𝔼(L_number_plus)  # 𝕄(Φ)
    𝛿1.2 := 𝔻(⟦ λ ⤍ 𝜎1 ⟧)  # 𝔻(ξ.ρ)
    𝛿2.2 := 40-10-00-00-00-00-00-00  # 𝔻(ξ.x)
    𝑛.2.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ )  # 𝑛
    𝑛.2.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧, as-bool ↦ ⟦ λ ⤍ L_number_as_bool ⟧ ⟧  # 𝕄(𝑛.2.1)
  ?(L_number_as_bool)  # 𝔻(⟦ λ ⤍ L_number_as_bool ⟧)
```

<!-- markdownlint-enable MD013 -->

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
⟦ n ↦ 3, φ ↦ Φ.bar( ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧ ⟧ ) ⟧
```

Every binding of the formation is entered, recursively. 𝕄 is asked about the
term standing there and, where it lands on a saturated formation whose λ an
entry answers, that λ is fired and 𝕄 is asked about the answer again. A term on
whose way a λ function fired is replaced by the answer of the last firing,
morphed: an entry answering `Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )` stands the formation of
`number` there, the very one the same term written in the program morphs to, so
a value that came out of a firing and a value that was written as a literal are
one shape and can be compared leaf by leaf. That costs the size of the object's
formation in the residual, which is the price of saying the same thing one way.
A term nothing fired on stays exactly as it was written and only its own parts
are walked, so `Φ.bar` keeps its name and what comes back is still the same
program, reduced as far as the file allows. The step joins the chain under the
name `deep`, so `--sequence` shows it, and `--max-steps` bounds the walk.

Two things are left alone. A λ no entry answers is not fired at all, so
`--deep` stays as total as 𝕄 itself and needs no `--partial`; a λ function that
gets stuck deeper on a spine still fails the run, and `--partial` parks it,
leaving that term as it was written. A firing the walk does make and cannot
finish — one whose operand never comes down to data, because a λ nothing
answers stands in it — is parked by `--partial` the same way: the binding it
stood in is left as it was written, the walk enters the next one, and the
protocol shows the firing with nothing bound under it. One entry nothing can
answer therefore taints its own binding and not the whole run. A formation
still holding a void
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
  demo ↦ ⟦
    foo ↦ ⟦
      n ↦ 3,
      φ ↦ Φ.bar( ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧ ⟧ )
    ⟧
  ⟧
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

The `--acyclic` flag makes a reduction notice. Every frame of 𝕄 and of 𝔻
remembers the terms the frames above it are reducing, and a term that comes
back is a question only ever answered by asking it again, so the flag stops
there and parks the site the way `--partial` parks a λ function that cannot
fire: the answer is the term the spine had reached, left where it stood, and
the command exits successfully.

```bash
$ phino morph --symbolic=loop.yaml --locator='Q.x' --acyclic \
    --max-steps=40 --hide-rho loop.phi
⟦ λ ⤍ L_loop ⟧.foo
```

Each judgment keeps its own memory, since 𝕄 and 𝔻 call each other on the very
term they were asked about and that handover is no loop. A body dispatching the
object it stands in is one 𝔻 walks round on its own — 𝕄 stops at a formation
every round and never sees the same term twice — so `dataize` takes the flag
too, and so does the run of 𝔻 a λ function's `dataize` operand is brought down
with:

```bash
$ cat cyc.phi
⟦ cyc ↦ ⟦ x ↦ ∅, φ ↦ Φ.cyc( ξ.x ) ⟧, t ↦ Φ.cyc( ⟦⟧ ) ⟧
$ phino dataize --locator='Q.t' --acyclic --partial \
    --sweet --hide-rho --flat cyc.phi
⟦ cyc(x) ↦ ⟦ φ ↦ Φ.cyc( x ) ⟧, t ↦ ⟦ x ↦ ⟦⟧, φ ↦ Φ.cyc( x ) ⟧ ⟧
```

𝔻 insists on bytes and a parked term carries none, so under `dataize` the flag
wants `--partial` to have something to print: the residual program, exactly the
one it prints for a λ function that cannot fire. Without it the run stops on
the loop all the same, naming the term it came back to instead of running the
budget down. Under `morph` nothing is asked for: 𝕄 always has a term to answer
with, a loop 𝔻 meets under a firing parks the site the firing stands at, and
the walk of `--deep` goes on to the next binding.

What a frame remembers is the branch from the run down to it, never everything
the run has touched, so two sibling subterms that happen to be written alike
stay two terms and only a term genuinely reached from itself is a loop. The cut
costs one lookup and fires on the turn the repeat appears, so raising
`--max-steps` from 40 to a million changes neither the answer nor the time. The
flag promises nothing about programs that loop without ever repeating a term —
a body that grows on every round rather than coming back still ends on the
budget.

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
* `locator` - accepts `Φ.string`, dataizes it and converts it to a locator:
  a dispatch started with `Φ` or `ξ`, e.g. `Φ.foo.bar` or `foo.bar`.
  If dataized string can't be converted to locator - an error is thrown.
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

Besides parsing, printing and rewriting that class, the suite morphs
symbolically. `benchmark/demo.phi` is a small world whose entries name the λ
functions of `benchmark/atoms.yaml`, and each entry is a case of its own, so
that a slowdown of one of them is a line of the report rather than a share of
a single total. The smallest entry — one λ function fired against one unknown
— is timed twice, over the demo world alone and over the same world merged
into the class, and the two numbers say between them what the world around an
entry costs (see [#1291][issue-1291]).

A case whose single run is measured in seconds gets fewer warmups and fewer
batches than a case measured in microseconds, since the whole suite runs
inside one job; the report says how many of each a case was given.

```bash
make bench
```

<!-- benchmark_begin -->

```text
=== parse/phi ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      1785285.297 μs
  avg:        178528.530 μs
  min:        164868.992 μs
  max:        209121.132 μs
  std dev:    15515.800 μs
=== parse/xmir ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      7506298.288 μs
  avg:        750629.829 μs
  min:        683506.496 μs
  max:        819105.157 μs
  std dev:    44112.176 μs
=== rewrite/normalize ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      495636.638 μs
  avg:        49563.664 μs
  min:        48083.779 μs
  max:        51678.568 μs
  std dev:    1163.452 μs
=== print/sweet/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      4148167.742 μs
  avg:        414816.774 μs
  min:        390353.762 μs
  max:        438468.832 μs
  std dev:    15702.253 μs
=== print/sweet/flat ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      4103860.150 μs
  avg:        410386.015 μs
  min:        400487.779 μs
  max:        422099.714 μs
  std dev:    6878.840 μs
=== print/salty/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      13505706.601 μs
  avg:        1350570.660 μs
  min:        1323552.942 μs
  max:        1388589.251 μs
  std dev:    22111.111 μs
=== morph/symbolic/demo/e1 ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      16623322.804 μs
  avg:        1662332.280 μs
  min:        1634287.226 μs
  max:        1712122.691 μs
  std dev:    26925.390 μs
=== morph/symbolic/demo/e2 ===
  warmup:     2 iterations
  batches:    4 x 1
  total:      18971323.304 μs
  avg:        4742830.826 μs
  min:        4710439.768 μs
  max:        4785557.203 μs
  std dev:    31923.645 μs
=== morph/symbolic/demo/e3 ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      9393523.305 μs
  avg:        939352.330 μs
  min:        926573.125 μs
  max:        955737.287 μs
  std dev:    9251.489 μs
=== morph/symbolic/demo/e4 ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      6172093.232 μs
  avg:        617209.323 μs
  min:        609699.156 μs
  max:        622461.534 μs
  std dev:    3958.833 μs
=== morph/symbolic/demo/e5 ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      657478.162 μs
  avg:        65747.816 μs
  min:        64867.966 μs
  max:        66985.074 μs
  std dev:    698.720 μs
=== morph/symbolic/native/e5 ===
  warmup:     0 iterations
  batches:    1 x 1
  total:      24700300.710 μs
  avg:        24700300.710 μs
  min:        24700300.710 μs
  max:        24700300.710 μs
  std dev:    0.000 μs
```

The results were calculated in [this GHA job][benchmark-gha]
on 2026-09-18 at 21:12,
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
[issue-1291]: https://github.com/objectionary/phino/issues/1291
[benchmark-gha]: https://github.com/objectionary/phino/actions/runs/35395119960
