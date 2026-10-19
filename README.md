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
cabal install --overwrite-policy=always phino-0.0.156
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

In a [GitHub Actions][gha] workflow, one step downloads the binary
for the runner and puts it on the `PATH`:

```yaml
- uses: objectionary/phino@0.0.156
  with:
    version-file: phino-version.txt
- run: phino --version
```

The file holds the version to install, such as `0.0.150`, and the
whitespace around it is ignored.
Give `version: 0.0.150` instead of `version-file` to write the version
in the workflow itself.
The step fails if no binary is released for the runner, such as
Linux on ARM, or if `phino --version` prints another version.

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

The `--pin-file` global option does the same, but reads the version from
a file, ignoring the whitespace around it:

```bash
phino --pin-file=version.txt dataize hello.phi
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

An entry reads no key but `λ`, `dataize`, `morph`, `evaluate`, `defer`, `stay`,
`symbolize`, `fork` and `𝑛`, and any other key, such as a misspelled `forkk`,
refuses the whole file, so a typo never drops a block without a word.

### Firing what an operand came to

𝕄 fires a λ function only where a dispatch reads the formation carrying it, so
an operand reduced under `morph` may come to a formation whose λ function
nobody fired, such as a call the program passed as an argument and never read.
There is a third block, `evaluate`, and each line of it fires such a formation
through 𝔼 and binds an expression meta of its own to the answer:

```yaml
- λ: L_apply
  morph:
    𝑛1: $.f
  evaluate:
    𝑛2: 𝑛1
  𝑛: 𝑛2
```

The right-hand side of a line names a meta bound by `morph` or by another line
of the entry, and nothing else. A formation with a λ function is fired by the
entry its λ name matches, the way 𝕄 fires it, and the line binds its meta to
the normal form of the answer. A formation with no λ function is no call and
stays as it is, and so does `⊥`. A λ name no entry answers gets stuck, as it
does anywhere else, and `--partial` parks it. An answer is not fired again,
even when it is one more call: firing it takes one more line, so how many
calls an entry makes is written in its file and not guessed by `phino`. Each
firing of a line is one step deeper, the way a frame of 𝕄 or 𝔻 is, so a
recursion running through `evaluate` lines alone spends `--max-steps` and is
cut by `--acyclic` like any other.

The protocol writes the firing of a line as a block of its own, commented with
the meta it fired, `𝔼(L_second):  # 𝔼(𝑛2)`, and the line binding the answer
the same way, `𝑛3·1 := 𝑛·3·2  # 𝔼(𝑛2)`.

### Deferring a copy over an unknown

A term 𝕄 leaves may be a copy of an object of the world, such as
`Φ.step( tup ↦ Φ.items.tail )`, one of whose arguments reads to a bare symbol
`⟦ λ ⤍ 𝜎1 ⟧`. Nothing can be worked out about such a copy: its `φ` reads
`tup.next`, an attribute of `𝜎1` nobody knows, so morphing it gets stuck where
the symbol stands. There is a fourth block, `defer`, and each line of it reads
a meta and binds another, the way a line of `evaluate` does. When the term is a
copy one of whose arguments, `φ` and `ρ` aside, reads to a bare symbol, the
line binds its meta to a fresh symbol and writes what the symbol stands for:

```yaml
- λ: L_step
  morph:
    𝑛1: $.x
  defer:
    𝑛2: 𝑛1
  𝑛: 𝑛2
```

```text
deferred(𝜎3) := Φ.step( tup ↦ 𝜎1:λ )  # 𝕄(𝑛1.φ)
```

The markup writes it as `<deferred symbol="𝜎3" … of="Φ.step">`, holding a
`<with>` element with a `<symbol name="tup">𝜎1</symbol>` per argument. When
the world declares no object the copy was made of, `of` names the `<formed>`
element that spells the copy out, and there is no `<with>`. Any other term,
a copy whose arguments read to no bare symbol among them, is bound as it
stands, as `𝑛2·1 := 𝑛·1·1  # 𝑛1`. A `defer` line asks nothing of the `φ` of a
copy, so it defers one whose `φ` reads nothing of its own object too, such as
`⟦ φ ↦ Φ.bool( … ) ⟧`.

An argument that is no bare symbol, such as `w ↦ Φ.inc( ⟦ Δ ⤍ 2A- ⟧ )`, is
worked out before the line is written. The entry morphs it in the copy, fires
what that comes to, the way an `evaluate` line does, and writes every step to
the protocol under `𝕄(𝑛1.w):`. The line then names the object the firing
answered, `Φ.box( x ↦ 𝜎1:λ, w ↦ 𝑛·2·2 )`, and so does the markup,
`<meta name="w">𝑛·2·3</meta>`, with a meta of its own count. A reader follows
the meta to the application that made the object. An argument whose morph gets
stuck, is cut or runs out of steps is written as the code spells it, and in the
markup by the meta of the element that named it earlier, such as the
`<applied>` of the call, or by an expression meta, such as `𝑒1`, when none
did.

Each side of a `fork` block, described below, is deferred the same way once
it is morphed, so a path whose branch is such a copy ends in a fresh symbol
rather than getting stuck inside the unknown. The line is sited where the side
stands in the universe, `deferred(𝜎3) := …  # 𝕄(Φ.a🌵2.φ)`, and its arguments
are worked out under `𝕄(Φ.a🌵2.w):`.

### Forking on a condition nobody decided

A branching λ function answers neither of its branches. Which one the program
takes is decided by a value nobody worked out, so handing one of them through
would drop the branch point from the program altogether, and joining two
different terms into one would make up a value no run of the program ever has.
`fork` is the fifth block, and it answers nothing: it names the condition and
the two sides, and the run splits into paths, one per side, unless the two
sides turn out to be one.

```yaml
- λ: L_fork
  dataize:
    𝛿1: $.φ
  fork:
    on: 𝛿1
    left: $.left
    right: $.right
```

`on` names a bytes meta the `dataize` block bound, the condition, and `left`
and `right` are paths from `$`, read against the formation being fired the way
an operand is, or metas the `morph` block or a `stay` or `symbolize` line
bound, as described below. An entry with a `fork` block has no `𝑛`, since the
side it takes is what it answers with, and no block beside `dataize`, `morph`,
`stay` and `symbolize`: an entry forks or it computes, never both. A block
missing one of the three keys, holding another, or naming under `on` anything
but a meta of `dataize` is refused when the file is read, and so is a side
reading a meta inside a term, such as `𝑛1.x`, or naming a meta that none of
`morph`, `stay` and `symbolize` binds.

Before it decides anything, a fork morphs both of its sides, each where it
stands in the universe, under a block of its own, `𝕄(Φ.a🌵3):` for the left
side and `𝕄(Φ.a🌵4):` for the right one. A side that is a meta was morphed
already, by the `morph` block, so the fork takes the term the meta is bound to
as it is and morphs nothing again. When the two come to the same term,
nothing depends on the condition: the fork decides nothing, opens no path and
answers that term, and the line binding it is commented with both sides,
`𝑛·2·5 := 𝑛·2·3  # 𝕄(ξ.left), 𝕄(ξ.right)`. When the two differ in their
symbols alone, such as `Φ.number( φ ↦ 𝜎3:λ )` and `Φ.number( φ ↦ 𝜎4:λ )`,
the fork joins them into one term: every pair of symbols standing at the same
place gets a fresh symbol, which a line of its own says is the left one where
the condition holds and the right one where it does not, and the run goes on
with the joined term on the path it is on:

```text
joined(𝜎5) := 𝔻(𝜎2:λ) ? 𝜎3 : 𝜎4
```

A pair met twice gets the same fresh symbol both times. Only sides that differ
in anything else, a datum, a λ name or the shape of a term, split the run, and
so does a side holding a datum where the other holds a symbol, unless a
`symbolize` line turned that datum into a symbol first, as described below.

A run keeps a decision per condition it has met, none when the run starts.
When a firing reaches a fork whose condition nobody decided and whose sides
differ, the run splits there into two paths. The first path takes the left
side and says so in the protocol:

```text
forked(𝔻(𝜎2:λ)) := left  # ξ.left
```

The condition is spelled by what the `dataize` operand came down to, `𝔻(𝜎2:λ)`
for a symbol and the bytes for a datum, and the comment is the side as the
entry wrote it, with `$` read as `ξ`. The line binding the answer is commented
with the side it took, `𝑛·2·5 := 𝑛·2·3  # 𝕄(ξ.left)`, or with the meta alone,
`# 𝑛3`, where the side is a meta, since the fork morphed nothing there. What
the side fires stands beside the fork and not inside it, since the fork
answered the side rather than reduced it. A side whose morph gets stuck or is
cut cannot be compared with the other one, so the fork splits the run, and
only the path taking that side gets stuck. A side that is a meta keeps this
rule: in an entry that forks, an operand of `morph` that gets stuck or is cut
binds its meta to nothing and throws nothing yet, a `stay` or `symbolize` line
reading that meta binds nothing either, and only the path taking the side that
reads it gets stuck.

Once the first path ends, the second one goes on from the fork with the right
side, in the state the run was in at the fork: what came before the fork is
done once for both paths, and nothing the first path did after the fork is
seen by the second. A path that meets another fork nobody decided splits
again, and the paths run in the order their forks opened them. Every path
prints a result of its own, one line each, in that order. A path that gets
stuck or is cut is parked by
`--partial` the way one run is, and fails the command without it. The
`--max-paths` option bounds how many paths the command runs, and there is no
limit unless it is given: a program forking inside a recursion opens a path
per round, so `--max-paths=8` ends such a run after eight paths, whatever else
bounds it. The option is refused with zero.

A condition decided once on a path is decided for that path: a fork on a
condition the path already decided takes the same side, writes the same
`forked(…)` line and opens no path, so a program forking twice on one
condition runs two paths and not four. A decision is kept by the value the
condition came down to and not by the firing that made it, so two forks on one
symbol are one decision too. The second path goes on with the memo of
`--acyclic=plausible` as it was at the fork, so a firing the first path made
after the fork is made again on the second, and the protocol of that path
writes every line of it, while a firing made before the fork is made once.

A side may raise. EO raises with an object, a copy of
`⟦ message ↦ ∅, λ ⤍ L_throw ⟧`, whose λ function the file leaves out on
purpose, so that an error of the program stays an error at runtime. The path
taking that side ends in the throw, where `--partial` parks it as it parks any
λ name no entry answers, and the other path has a perfectly good value:

```bash
$ cat raise.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅ ⟧,
  error ↦ ⟦ message ↦ ∅, λ ⤍ L_throw ⟧,
  bool ↦ ⟦ if ↦ ∅ ⟧,
  check ↦ ⟦ λ ⤍ L_check ⟧,
  demo ↦ ⟦ a ↦ Φ.check.if( 7, Φ.error( "negative" ) ) ⟧
⟧
$ cat raise.yaml
- λ: L_check
  𝑛: Φ.bool( if ↦ ⟦ λ ⤍ L_fork, then ↦ ∅, else ↦ ∅, φ ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧ )
- λ: L_fork
  dataize:
    𝛿1: $.φ
  fork:
    on: 𝛿1
    left: $.then
    right: $.else
$ phino dataize --symbolic=raise.yaml --locator=Q.demo.a --partial \
    --sweet --hide-rho raise.phi
40-1C-00-00-00-00-00-00
⟦ message ↦ "negative", λ ⤍ L_throw ⟧
```

The first path takes `then` and comes down to the bytes of 7, the second takes
`else` and parks on the throw, so its protocol ends in `unanswered(L_throw)`.
Without `--partial` the command fails there, with
`No entry of --symbolic answers the λ function 'L_throw'`, the way one run
does.

### Dropping the ρ of a dispatch

There is a sixth block, `stay`, and it reduces nothing. Each line of it takes
the term another meta of the entry is bound to and binds an expression meta of
its own to that term with every `( ρ ↦ … )` dropped that is applied to a
dispatch:

```yaml
- λ: L_fork
  dataize:
    𝛿1: $.φ
  morph:
    𝑛1: $.then
    𝑛2: $.else
  stay:
    𝑛3: 𝑛1
    𝑛4: 𝑛2
  fork:
    on: 𝛿1
    left: 𝑛3
    right: 𝑛4
```

The right-hand side of a line names a meta bound by `morph` or by another line
of the entry, and nothing else. Every position of the term is walked, the
methods and the `ρ` of a formation among them, and `𝑒.𝜏( ρ ↦ 𝑒2 )` becomes
`𝑒.𝜏`. So does a dispatch heading a chain of applications that bind no `ρ`:
`𝑒.𝜏( φ ↦ 𝑒3 )( ρ ↦ 𝑒2 )` becomes `𝑒.𝜏( φ ↦ 𝑒3 )`. A `ρ` applied to anything
else, such as a formation, stays.

Dropping it changes no value. The `dot` rule answers a dispatch as
`𝑛1( ρ ↦ ⟦…⟧ )`, and then `copy` fills a void `ρ` of the body, `stay` keeps a
bound one and `skip` drops it where the body declares none. So once `𝑒.𝜏`
comes to a formation, a later `( ρ ↦ 𝑒2 )` meets a `ρ` that is bound or not
there, and `stay` or `skip` drops it. The block only drops it earlier, before
the dispatch is reduced.

This is how a fork brings two numbers to one shape. In EO `bytes.as-number`
reads its `data` as `ρ`, so a branch computed through it comes to
`Φ.number( φ ↦ Φ.bytes( φ ↦ 𝜎1 )( ρ ↦ ⟦ … ⟧ ) )`, with the copy hung on the
bytes as their `ρ`, while a branch made of no object comes to
`Φ.number( φ ↦ Φ.bytes( φ ↦ 𝜎2 ) )`. The fork compares the two as written,
finds a shape apart, and splits the run; after `stay` they differ in their
symbols alone, and the fork joins them. The protocol writes each line of the
block. A line that dropped a `ρ` is commented `stay(…)` with the meta it took,
so a reader sees that the term changed and what changed it, while a line that
dropped nothing is commented with the meta alone:

```text
𝑛3·1 := Φ.number( φ ↦ Φ.bytes( φ ↦ 𝜎1:λ ) ):φ  # stay(𝑛1)
𝑛4·1 := 𝑛2·1  # 𝑛2
joined(𝜎4) := 𝔻(𝜎3:λ) ? 𝜎1 : 𝜎2
```

### Standing a symbol for a datum

There is a seventh block, `symbolize`, and it reduces nothing either. Each line
of it takes the term another meta of the entry is bound to and binds an
expression meta of its own to that term with every datum along its `φ` turned
into a fresh symbol:

```yaml
- λ: L_fork
  dataize:
    𝛿1: $.φ
  morph:
    𝑛1: $.then
    𝑛2: $.else
  symbolize:
    𝑛3: 𝑛1
    𝑛4: 𝑛2
  fork:
    on: 𝛿1
    left: 𝑛3
    right: 𝑛4
```

The right-hand side of a line names a meta bound by `morph` or by another line
of the entry, and nothing else. Only the `φ` chain of the term is walked, the
path along which a term reaches the value it stands for: the `φ` of a
formation, the `φ` an application binds, and the head of an application that
binds anything else. A `Δ ⤍ 𝑏` met there becomes `λ ⤍ 𝜎4`, a symbol minted
fresh, the way a bare `𝜎` of an answer is. A datum anywhere else, under a `ρ`,
in a method or in an argument other than `φ`, stays, since it says nothing
about the value of the term.

This is how a fork joins a literal with a value some firing computed. In EO
`x.gt( 0 ).if( 1, x )` has one side that is the literal `1`, a number whose
bytes hold the datum `3F-F0-…`, and another side that is a number whose bytes
hold a symbol. The two are numbers of one shape and differ in the value alone,
yet a datum against a symbol is no pair of symbols, so the fork splits the run.
After `symbolize` both sides hold a symbol where the value stands, and the fork
joins them. The protocol writes every symbol the block minted with the datum
it stands for, so a reader of the joined term can tell that `𝜎4` was `1` on
the left path. A line that turned a datum into a symbol is commented
`symbolize(…)` with the meta it took, while a line whose term held no datum is
commented with the meta alone:

```text
symbolized(𝜎4) := 3F-F0-00-00-00-00-00-00  # 1.0
𝑛3·1 := Φ.number( φ ↦ Φ.bytes( φ ↦ 𝜎4:λ ) ):φ  # symbolize(𝑛1)
𝑛4·1 := 𝑛2·1  # 𝑛2
joined(𝜎5) := 𝔻(𝜎3:λ) ? 𝜎4 : 𝜎2
```

A line sees its own side alone, so a datum both sides share becomes two
symbols and then one joined symbol, and the path loses that datum. A file that
wants to keep it leaves the block out, and an entry without `symbolize` writes
the same protocol it wrote before. The two blocks chain: a side may go through
a `stay` line first and through a `symbolize` line after it.

### The order of the lines

The lines of `evaluate`, `defer`, `stay` and `symbolize` run in the order of the
metas they read, not in the order of their blocks. A line runs once every meta
it reads is bound, so this entry fires what `x` came to and then fires the
answer, one more call, although the line doing so stands first:

```yaml
- λ: L_twice
  morph:
    𝑛1: $.x
  evaluate:
    𝑛3: 𝑛2
    𝑛2: 𝑛1
  𝑛: 𝑛3
```

The line binding `𝑛3` reads `𝑛2`, which the line under it binds, so that line
runs first. The `dataize` and `morph` blocks read no meta and always run
before all the others. Where no meta orders two lines, they run in the order
the list above gives their blocks, and the lines of one block in the order of
their metas, `𝑛2` before `𝑛10`. A line reading a meta nothing binds is refused
when the file is read, and so is a line that reads the meta it binds, directly
or through other lines, since it could never run.

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
`unanswered(𝜎1)`, and `--partial` leaves the term where it stands. Dispatching
an attribute off a symbol — `⟦ λ ⤍ 𝜎1 ⟧.plus( 5 )` — therefore taints its own
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
  number ↦ ⟦ φ ↦ ∅, plus(ρ, x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
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
the operands the firing bound and the term it answered with. A judgment started
at a new site opens a block too, and what it does stands under it.

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.txt --quiet \
    --sweet --hide-rho sum.phi
$ cat atoms.txt
𝔻(Φ):
  𝕄(Φ):
    𝑛·0·1 := 5
    𝑛·0·2 := 𝑛·0·1.plus( x ↦ 6 )
  𝔼(L_number_plus):
    𝔻(Φ.a🌵0):
      𝕄(Φ.a🌵0):
        𝑛·1·1 := 5
        𝑛·1·2 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )
      𝛿·1·1 := 40-14-00-00-00-00-00-00  # 5.0
    𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)
    𝔻(Φ.a🌵1):
      𝕄(Φ.a🌵1):
        𝑛·1·3 := 6
        𝑛·1·4 := Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ )
      𝛿·1·2 := 40-18-00-00-00-00-00-00  # 6.0
    𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)
    𝑛·1·5 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛
    𝕄(𝑛·1·5):
      𝑛·1·6 := Φ.number( φ ↦ 𝜎1:λ )
    𝑛·1·7 := 𝑛·1·6  # 𝕄(𝑛·1·5)
```

<!-- markdownlint-enable MD013 -->

`𝔻(…)` is the run and the term it was aimed at, `𝕄(…)` where the run is a
morphing, and `𝔼(…)` is one firing, named by the entry that answered it. A line
standing in a block needs no comment to say which judgment made it and where:
the block it stands in says that already. A comment is written only where it
says something the block does not, such as the term an operand was reduced
from, `𝔻(ξ.ρ)`, or the meta an answer was read from, `𝑛`, and every comment
naming a judgment is of that shape: a judgment applied to a term, which is the
intent the value beside it came from. The firings are numbered across the
whole run, in the order they open, so `𝛿1·2` is the value bound to `𝛿1` by
the second firing of the run, whichever λ function that was, `𝑛1·2` the same
for a `morph` meta, and `𝑛·3·2` the answer of the third firing, so
`𝑛1·2 := 𝑛·3·2` reads "the `𝑛1` of this firing is what the third firing
answered". One firing binds a meta once and no two firings
share a number, so every one of these names stands on exactly one line of the
file and a line naming another one points at it and no other.

A nested `𝔻(…):` or `𝕄(…):` line opens a block where a judgment starts at a
site other than the one of the block it stands in, and names that site. What
the judgment does stands one level deeper under it: the objects its
normalization makes, the firings it demands, and the judgments it starts in
turn. A reader therefore sees which judgment made each line and at which site,
rather than a flat list of lines each saying so. 𝔻 of a term that is no
formation asks 𝕄 for the normal form of the same term first, through its
`norm` rule, so a `𝕄(…):` block often stands right inside a `𝔻(…):` block of
the same site, and `𝑛·0·1 := 5` above is made by that 𝕄 and not by 𝔻. A
judgment started at the site of the block it stands in, and by the same
judgment, opens nothing, and what it does joins that block. A block is written
only once a line stands in it, so a judgment that made nothing worth a line
leaves no empty heading behind. The line is no firing: it binds no meta and
takes no number, so the metas of the firings under it are numbered as if it
were not there. A `𝔻(…):` block ends with the datum the `delta` rule found,
such as `𝛿·1·1 := 40-14-00-00-00-00-00-00`, named by the firing it stands in
and a counter of its own, and the operand line right after the block names it.
A datum of eight bytes is commented with the number those bytes hold as an
IEEE 754 double, `# 5.0` here, since that is how a number keeps its data.
In the run above 𝔻 gets into each number the entry brings down, and through
its `φ` into the bytes that number holds, in the same block, since the site is
the same.

An answer stands on two lines and not one. A firing answers the term its entry
wrote and `phino` morphs that term before standing it back into the program, so
`𝑛·1·5` is what the entry wrote, with the symbols this firing minted already in
it, commented with `𝑛` to name the key it was read from, and `𝑛·1·7` is the
normal form 𝕄 made of it, commented with `𝕄(𝑛·1·5)` to say where it came from.
That morphing opens a block of its own, `𝕄(𝑛·1·5):`, named after the term it
started from, since that term stands nowhere in the program a locator could
reach. It is the same morphing every other term goes through, and writing only its
outcome would have the formation of `number` appear in place of the three
tokens the entry wrote with nothing saying why. Whatever that morphing fires
or applies writes its lines between the two, exactly where a firing an operand
took opens its block, so the order the lines come in is the order the work was
done in.

`𝑛·1·6 := Φ.number( φ ↦ 𝜎1:λ )` is an object an application made. The `copy`
rule of normalization fills a void of a formation with the argument it was
given, and so makes a new object, whatever judgment is running and wherever in
the term the application stands. The line binds that object to a fresh `𝑛` and
spells the application: the object applied on the left and the argument it got
on the right, so `5` is `Φ.number( φ ↦ … )` in sugar. The `ρ` a dispatch gives
the attribute it takes writes no line. The block the line stands in names the
judgment that normalized and the site it stood at. The name is counted with the
metas of the firing the line stands in, `𝑛·1·5` before it and `𝑛·1·7` after it,
and an object made outside every firing is counted under `𝑛·0`. From then on
every line spells the object by that name rather than as the formation it is,
and the application by the same name, since `copy` makes the same object of the
same application wherever it stands: `𝑛·1·1` is the object `5` made,
`𝑛·0·1.plus( x ↦ 6 )` is the `plus` of the first `5` applied to `6`, one line
for one call, and `𝑛·1·7 := 𝑛·1·6` says the firing answered with the object 𝕄
made of what the entry wrote. Such a line names its head and its argument that
way, each on its own, and never the application as a whole, so a line spells out
every application, even one making an object an earlier one already made, the
way `𝑛·1·1 := 5` does, and a later line names the latest of them. Only an object
still standing as it was made is named: once a rule rewrote a part of it, it is
another object and it is spelled out again.

Where an operand came down to the datum a symbol stands for, the protocol writes
`𝔻(𝜎1:λ)` in place of that 42 (`𝜎1:λ` is the formation `⟦ λ ⤍ 𝜎1 ⟧`, in the
sugar every sweet term is written with), so a reader sees that the value was
manufactured rather than read out of the program. A `𝜎` is the name of a λ
function and no term of its own, so 𝔻 is applied to the formation carrying it
and never to the name alone.

A `fork` block writes the side it took, `forked(𝔻(𝜎2:λ)) := left  # ξ.left`,
or the symbols it joined, `joined(𝜎5) := 𝔻(𝜎2:λ) ? 𝜎3 : 𝜎4`, and the first
line is a decision and no assignment: nothing is bound to the
condition, which stays the unknown it was, and a reader of the protocol takes
the line as the assumption every line after it stands under. The comment is
the side as the entry spells it, with no judgment around it: picking a side is
the file's own operation and nothing of the calculus runs there, so the line
names a term of the entry where every other comment names a judgment. A
comment carries the letter of a judgment exactly where a judgment made the
value and the block around the line does not already say so.

The site of a firing is a locator, and the block a firing stands in names it,
under the letter of the judgment that asked for the firing: 𝔼 is fired by the
`ml` rule of morphing and by the `fire` rule of dataization, so a firing in the
block `𝕄(Φ.demo.a.φ):` is a λ function fired while 𝕄 was reducing that
binding, and one standing right in `𝔻(Φ):` was fired because dataization
demanded data of `Φ`. A chain such as `5.plus( 6 ).plus( 7 )` writes both: the
inner call is fired inside the `𝕄(Φ):` block, while 𝕄 reduces the head of the
outer dispatch, and the outer one right in `𝔻(Φ):`, because 𝔻 asked for the
data. A line standing right in a firing has no block of its own around it, so
a line there that a judgment wrote at a site no block names, such as
`starved(…)` below, carries that judgment and site as its comment. The site
itself is where in the program the firing
belongs: the term the run was aimed at, so `Φ` for a run that was aimed at
nothing in particular. An operand of a firing is reduced bound to a synthetic
attribute of the universe (see `--inside` below), so a λ function fired while
it came down is written under that attribute and not under the site of the
firing that asked for it, and a formation an `evaluate` line fires is written
under the meta it fired, `𝔼(𝑛2)`.

An operand line ends in the judgment that reduced it and the term it was
reduced from, written as a comment after two spaces and `#`. The value alone
says what the meta was bound to and neither what it was bound from nor what
was done to it, so `𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)` reads "the `𝛿1` of this firing
is the `ρ` of the formation brought down through 𝔻, and it came down to the
datum `𝛿·1·1` above", where a `morph` operand reads `𝑛1·5 := 𝑛·3·4  # 𝕄(ξ.then)`
and says that the `then` of the formation reached its normal form through 𝕄.
Which of the two judgments ran is the whole difference between a line ending
in data and one ending in a term. It is the very term the entry wrote under
that meta, spelled the way the calculus reads it — `$` is read as `ξ` — so a
reader never has to open the `--symbolic` file beside the protocol and match
every line by λ name and meta number.

`unanswered(…)` is a λ name no entry answers, standing where the block of its
firing would have stood. Nothing fired, so nothing opens under it. The line is
commented with the judgment that asked and the formation it was asking about,
`𝕄(L_none:λ)`, which no block names, the way an operand line is commented
with the term it was reduced from: 𝔼 is fired by the `ml` rule of morphing
and by the `fire` rule of dataization, so the letter says where in the
reduction the site stands and the term says which object the λ function that
could not fire belongs to. It is written whether or not `--partial` goes on
to park the run, since the protocol records what 𝔼 was asked for, and a
question it could not answer belongs there as much
as one it could — once per site and not once per attempt, since a site
`--partial` parks stays in the residue, where a later step may meet it again:

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.txt --quiet \
    --sweet --hide-rho stuck.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_nope'
$ cat atoms.txt
𝔻(Φ):
  𝕄(Φ):
    𝑛·0·1 := 5
    𝑛·0·2 := 𝑛·0·1.plus( x ↦ 6 )
    𝔼(L_number_plus):
      𝔻(Φ.a🌵0):
        𝕄(Φ.a🌵0):
          𝑛·1·1 := 5
          𝑛·1·2 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )
        𝛿·1·1 := 40-14-00-00-00-00-00-00  # 5.0
      𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)
      𝔻(Φ.a🌵1):
        𝕄(Φ.a🌵1):
          𝑛·1·3 := 6
          𝑛·1·4 := Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ )
        𝛿·1·2 := 40-18-00-00-00-00-00-00  # 6.0
      𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)
      𝑛·1·5 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛
      𝕄(𝑛·1·5):
        𝑛·1·6 := Φ.number( φ ↦ 𝜎1:λ )
      𝑛·1·7 := 𝑛·1·6  # 𝕄(𝑛·1·5)
  unanswered(L_number_nope)  # 𝔻(L_number_nope:λ)
```

<!-- markdownlint-enable MD013 -->

The very same file comes back with `--partial`, where the run answers the
residue instead of failing: what `phino` could not decide is a property of the
program and not of the option that decides what to do about it.

Three more lines say why a firing gave no answer.
`stuck(L_outer)` is the last line of a firing that got stuck, naming the λ
function it got stuck on, which is seldom the one `unanswered(…)` names: that
one is written where it was asked for, and this one closes every firing the
failure passed on its way out.
`stall(L_outer)` stands under a firing that `--acyclic=plausible` answered
with the stall an earlier firing of the same formation kept, so a told stall
never reads as a fresh firing that wrote nothing.
`starved(3)` is where `--max-steps=3` ran out, whether or not `--partial` goes
on to park it. It stands in the block of the judgment and the site the
reduction stood at, and where the budget ran out before that block could open,
right in a firing, it is commented with them instead, `starved(3)  # 𝔻(Φ.a🌵2)`:

```text
𝕄(Φ.x):
  𝕄(Φ.x.arg):
    𝔼(L_outer):
      𝔻(Φ.a🌵0):
        starved(3)
      stuck(L_outer)
  𝔼(L_outer):
    𝔻(Φ.a🌵1):
      𝔼(L_outer):
        starved(3)  # 𝔻(Φ.a🌵2)
    stuck(L_outer)
```

The markup spells them `<stuck λ="L_outer"/>`, `<stall λ="L_outer"/>` and
`<starved limit="3" by="dataize" at="Φ.a🌵2"/>`.

`timeout(5)` is where `--max-seconds=5` ran out, placed and commented the same
way and written at the first step the deadline refused.
The run ends there, with or without `--partial`, so it is always the last line
of the protocol.
The markup spells it `<timeout limit="5" by="morph" at="…"/>`.

`spent(3)` is where `--max-firings=3` ran out, placed and commented the same
way and written where the firing it refused would have started.
The markup spells it `<spent limit="3" by="dataize" at="…"/>`.

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
  fork:
    on: 𝛿1
    left: $.then
    right: $.else
```

<!-- markdownlint-disable MD013 -->

```bash
$ cat fork.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  bool ↦ ⟦ if ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(ρ, x) ↦ ⟦ λ ⤍ L_plus ⟧, gt(ρ, x) ↦ ⟦ λ ⤍ L_gt ⟧ ⟧,
  foo(x) ↦ ⟦
    φ ↦ ξ.x.gt( 0 ).if( ξ.x.plus( ξ.x.plus( 1 ) ), ξ.x.plus( ξ.x ) ).plus( 5 )
  ⟧,
  demo ↦ ⟦ a ↦ Φ.foo( Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ) ) ⟧
⟧
$ phino dataize --symbolic=atoms.yaml --locator=Q.demo.a \
    --protocol=fork.txt --quiet --sweet --hide-rho fork.phi
$ cat fork.txt
𝔻(Φ.demo.a):
  𝕄(Φ.demo.a):
    𝑛·0·1 := Φ.foo( x ↦ Φ.number( φ ↦ 𝜎1:λ ) )
    𝑛·0·2 := Φ.number( φ ↦ 𝜎1:λ )
    𝑛·0·3 := 𝑛·0·2.gt( x ↦ 0 )
    𝔼(L_gt):
      𝔻(Φ.a🌵0):
        𝕄(Φ.a🌵0):
          𝑛·1·1 := Φ.number( φ ↦ 𝜎1:λ )
      𝛿1·1 := 𝔻(𝜎1:λ)  # 𝔻(ξ.ρ)
      𝔻(Φ.a🌵1):
        𝕄(Φ.a🌵1):
          𝑛·1·2 := 0
          𝑛·1·3 := Φ.bytes( φ ↦ 00-00-00-00-00-00-00-00:Δ )
        𝛿·1·1 := 00-00-00-00-00-00-00-00  # 0.0
      𝛿2·1 := 𝛿·1·1  # 𝔻(ξ.x)
      𝑛·1·4 := Φ.bool( if(then, else) ↦ ⟦ λ ⤍ L_fork, φ ↦ 𝜎2:λ ⟧ )  # 𝑛
      𝕄(𝑛·1·4):
        𝑛·1·5 := Φ.bool( if(then, else) ↦ ⟦ λ ⤍ L_fork, φ ↦ 𝜎2:λ ⟧ )
      𝑛·1·6 := 𝑛·1·5  # 𝕄(𝑛·1·4)
    𝑛·0·4 := ⟦ λ ⤍ L_fork, then ↦ ∅, else ↦ ∅, φ ↦ 𝜎2:λ ⟧( then ↦ 𝑛·1·1.plus( 𝑛·1·1.plus( 1 ) ) )
    𝑛·0·5 := 𝑛·0·4( else ↦ 𝑛·1·1.plus( 𝑛·1·1 ) )
    𝔼(L_fork):
      𝛿1·2 := 𝔻(𝜎2:λ)  # 𝔻(ξ.φ)
      𝕄(Φ.a🌵3):
        𝑛·2·1 := Φ.number( φ ↦ 𝜎1:λ )
        𝑛·2·2 := 𝑛·2·1.plus( x ↦ 𝑛·2·1.plus( 1 ) )
      𝕄(Φ.a🌵4):
        𝑛·2·3 := Φ.number( φ ↦ 𝜎1:λ )
        𝑛·2·4 := 𝑛·2·3.plus( x ↦ 𝑛·2·3 )
      forked(𝔻(𝜎2:λ)) := left  # ξ.then
      𝑛·2·5 := 𝑛·2·2  # 𝕄(ξ.then)
    𝔼(L_plus):
      𝔻(Φ.a🌵5):
        𝕄(Φ.a🌵5):
          𝑛·3·1 := Φ.number( φ ↦ 𝜎1:λ )
      𝛿1·3 := 𝔻(𝜎1:λ)  # 𝔻(ξ.ρ)
      𝔻(Φ.a🌵6):
        𝕄(Φ.a🌵6):
          𝑛·3·2 := Φ.number( φ ↦ 𝜎1:λ )
          𝑛·3·3 := 𝑛·3·2.plus( x ↦ 1 )
        𝔼(L_plus):
          𝔻(Φ.a🌵7):
            𝕄(Φ.a🌵7):
              𝑛·4·1 := Φ.number( φ ↦ 𝜎1:λ )
          𝛿1·4 := 𝔻(𝜎1:λ)  # 𝔻(ξ.ρ)
          𝔻(Φ.a🌵8):
            𝕄(Φ.a🌵8):
              𝑛·4·2 := 1
              𝑛·4·3 := Φ.bytes( φ ↦ 3F-F0-00-00-00-00-00-00:Δ )
            𝛿·4·1 := 3F-F0-00-00-00-00-00-00  # 1.0
          𝛿2·4 := 𝛿·4·1  # 𝔻(ξ.x)
          𝑛·4·4 := Φ.number( φ ↦ 𝜎3:λ )  # 𝑛
          𝕄(𝑛·4·4):
            𝑛·4·5 := Φ.number( φ ↦ 𝜎3:λ )
          𝑛·4·6 := 𝑛·4·5  # 𝕄(𝑛·4·4)
      𝛿2·3 := 𝔻(𝜎3:λ)  # 𝔻(ξ.x)
      𝑛·3·4 := Φ.number( φ ↦ 𝜎4:λ )  # 𝑛
      𝕄(𝑛·3·4):
        𝑛·3·5 := Φ.number( φ ↦ 𝜎4:λ )
      𝑛·3·6 := 𝑛·3·5  # 𝕄(𝑛·3·4)
    𝑛·0·6 := 𝑛·3·5.plus( x ↦ 5 )
  𝔼(L_plus):
    𝔻(Φ.a🌵9):
      𝕄(Φ.a🌵9):
        𝑛·5·1 := Φ.number( φ ↦ 𝜎4:λ )
    𝛿1·5 := 𝔻(𝜎4:λ)  # 𝔻(ξ.ρ)
    𝔻(Φ.a🌵10):
      𝕄(Φ.a🌵10):
        𝑛·5·2 := 5
        𝑛·5·3 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )
      𝛿·5·1 := 40-14-00-00-00-00-00-00  # 5.0
    𝛿2·5 := 𝛿·5·1  # 𝔻(ξ.x)
    𝑛·5·4 := Φ.number( φ ↦ 𝜎5:λ )  # 𝑛
    𝕄(𝑛·5·4):
      𝑛·5·5 := Φ.number( φ ↦ 𝜎5:λ )
    𝑛·5·6 := 𝑛·5·5  # 𝕄(𝑛·5·4)
𝔻(Φ.demo.a):
  𝕄(Φ.demo.a):
    𝔼(L_fork):
      forked(𝔻(𝜎2:λ)) := right  # ξ.else
      𝑛·2·6 := 𝑛·2·4  # 𝕄(ξ.else)
    𝔼(L_plus):
      𝔻(Φ.a🌵11):
        𝕄(Φ.a🌵11):
          𝑛·6·1 := Φ.number( φ ↦ 𝜎1:λ )
      𝛿1·6 := 𝔻(𝜎1:λ)  # 𝔻(ξ.ρ)
      𝔻(Φ.a🌵12):
        𝕄(Φ.a🌵12):
          𝑛·6·2 := Φ.number( φ ↦ 𝜎1:λ )
      𝛿2·6 := 𝔻(𝜎1:λ)  # 𝔻(ξ.x)
      𝑛·6·3 := Φ.number( φ ↦ 𝜎6:λ )  # 𝑛
      𝕄(𝑛·6·3):
        𝑛·6·4 := Φ.number( φ ↦ 𝜎6:λ )
      𝑛·6·5 := 𝑛·6·4  # 𝕄(𝑛·6·3)
    𝑛·0·7 := 𝑛·6·4.plus( x ↦ 5 )
  𝔼(L_plus):
    𝔻(Φ.a🌵13):
      𝕄(Φ.a🌵13):
        𝑛·7·1 := Φ.number( φ ↦ 𝜎6:λ )
    𝛿1·7 := 𝔻(𝜎6:λ)  # 𝔻(ξ.ρ)
    𝔻(Φ.a🌵14):
      𝕄(Φ.a🌵14):
        𝑛·7·2 := 5
        𝑛·7·3 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )
      𝛿·7·1 := 40-14-00-00-00-00-00-00  # 5.0
    𝛿2·7 := 𝛿·7·1  # 𝔻(ξ.x)
    𝑛·7·4 := Φ.number( φ ↦ 𝜎7:λ )  # 𝑛
    𝕄(𝑛·7·4):
      𝑛·7·5 := Φ.number( φ ↦ 𝜎7:λ )
    𝑛·7·6 := 𝑛·7·5  # 𝕄(𝑛·7·4)
```

<!-- markdownlint-enable MD013 -->

`𝜎2` is minted by the first firing and consumed by the fork, which forks on
it: the fork morphs both branches where they stand in the universe,
`𝕄(Φ.a🌵3):` and `𝕄(Φ.a🌵4):`, finds that they differ, and since nobody
decided `x.gt( 0 )`, the first path takes `then` and says so on its
`forked(…)` line. `𝜎3` is minted by the `plus( 1 )` inside the branch and consumed
by the `plus` around it as `𝔻(𝜎3:λ)`, and `𝜎4` is what the branch came to,
which the `plus( 5 )` standing after the fork reads as `𝔻(𝜎4:λ)`. The second
heading, `𝔻(Φ.demo.a):`, opens the second path, which starts at the fork and
not at the top of the program: it reopens the blocks the fork stood in, takes
`else` over the same `𝜎2`, and reads `𝔻(𝜎6:λ)` after it. A reader who knows
the entry knows that `𝛿1` is what decides between the two paths and that the
first of them belongs to `then`. Nothing is assigned to a `𝜎`, it being the
name of a λ function, so the decision stands on a line of its own, commented
with the side as the entry spells it.

The metas and the symbols keep counting across the paths, and a name given
before the fork names the same term on both paths, which is why the second
answers `𝑛·2·6 := 𝑛·2·4`: the branch it takes is the very term the fork morphed
before it decided. Were the fork to join two branches this different
into one term, `foo` would read as a program that computes a condition,
computes both branches and then makes up a value neither of them has.

A firing that happened while an operand of another was being reduced stands one
level deeper, under the firing that asked for it. The `plus( 1 )` of `then` is
fired while the outer `plus` brings its `x` down, so it stands inside that
firing. The branch the fork takes reaches it as a call 𝕄 left unfired, and is
fired beside the fork, in the block of the judgment that fired the fork,
`𝕄(Φ.demo.a):`, since the fork answered the branch rather than reduced it.

### The protocol as XML

The name of the file decides which of the two formats `--protocol` writes: a
name ending in `.xml` gets the same tree as markup, every other name gets the
indented text above. There is no option for it, since a caller who asks for a
file called `atoms.xml` and gets text back has been told nothing useful. Here
is the run at the top of this section again:

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.xml --quiet \
    --sweet --hide-rho sum.phi
$ cat atoms.xml
<?xml version="1.0" encoding="UTF-8"?>
<protocol>
  <path>
    <dataize at="Φ">
      <morph at="Φ">
        <applied meta="𝑛·0·1" of="Φ.bytes"><datum name="φ">40-14-00-00-00-00-00-00</datum></applied>
        <applied meta="𝑛·0·2" by="morph" at="Φ" of="Φ.number"><meta name="φ">𝑛·0·1</meta></applied>
        <applied meta="𝑛·0·3" of="Φ.bytes"><datum name="φ">40-18-00-00-00-00-00-00</datum></applied>
        <applied meta="𝑛·0·4" of="Φ.number"><meta name="φ">𝑛·0·3</meta></applied>
        <applied meta="𝑛·0·5" by="morph" at="Φ" of="𝑛·0·2.plus"><meta name="x">𝑛·0·4</meta></applied>
      </morph>
      <evaluate λ="L_number_plus" by="dataize" at="Φ">
        <dataize at="Φ.a🌵0">
          <morph at="Φ.a🌵0">
            <applied meta="𝑛·1·1" by="morph" at="Φ.a🌵0" of="Φ.number"><meta name="φ">𝑛·0·1</meta></applied>
            <applied meta="𝑛·1·2" by="morph" at="Φ.a🌵0" of="Φ.bytes"><datum name="φ">40-14-00-00-00-00-00-00</datum></applied>
          </morph>
          <dataized meta="𝛿·1·1" number="5.0">40-14-00-00-00-00-00-00</dataized>
        </dataize>
        <bound meta="𝛿1·1">𝛿·1·1</bound>
        <dataize at="Φ.a🌵1">
          <morph at="Φ.a🌵1">
            <applied meta="𝑛·1·3" by="morph" at="Φ.a🌵1" of="Φ.number"><meta name="φ">𝑛·0·3</meta></applied>
            <applied meta="𝑛·1·4" by="morph" at="Φ.a🌵1" of="Φ.bytes"><datum name="φ">40-18-00-00-00-00-00-00</datum></applied>
          </morph>
          <dataized meta="𝛿·1·2" number="6.0">40-18-00-00-00-00-00-00</dataized>
        </dataize>
        <bound meta="𝛿2·1">𝛿·1·2</bound>
        <minted symbol="𝜎1">40-14-00-00-00-00-00-00 40-18-00-00-00-00-00-00</minted>
        <applied meta="𝑛·1·5" of="Φ.number"><symbol name="φ">𝜎1</symbol></applied>
        <built meta="𝑛·1·6">𝑛·1·5</built>
        <morph at="𝑛·1·6">
          <applied meta="𝑛·1·7" by="morph" at="𝑛·1·6" of="Φ.number"><symbol name="φ">𝜎1</symbol></applied>
        </morph>
        <answered meta="𝑛·1·8">𝑛·1·7</answered>
      </evaluate>
      <manufactured meta="𝛿·0·1">𝜎1</manufactured>
    </dataize>
  </path>
</protocol>
```

<!-- markdownlint-enable MD013 -->

The root is `<protocol>`, holding a `<path>` per path the command ran, one
when nothing forks, and the run of a path stands inside its element, named
after the judgment it ran — `<dataize>` for a 𝔻, `<morph>` for a 𝕄 — with `at`
naming the term it was aimed at, which is what the text format opens with as
`𝔻(Φ)`, once per path. A path after the first starts at the fork that opened
it, inside the elements the fork stood in, which it opens again. After the
paths the root holds `<msec>`, `<firings>`
and `<fps>`: how long the command took, how many λ functions it fired, and how
many it fired per second. The text format ends with the same three as
`msec(…)`, `firings(…)` and `fps(…)`.
The examples here leave them out, since the time differs from run to run.
`<evaluate>` is one firing of 𝔼, `λ` naming the entry that answered it, `by`
naming the judgment that asked for the firing — the same word the run is named
after and an `<unanswered>` carries —
and `at` naming the site it was fired at. The text format leaves those two to
the block the firing stands in, `𝔻(Φ):`, while the markup keeps them on every
element, so a program reading it never has to track the elements around one.
A nested `<dataize at="Φ.a🌵0">` or `<morph at="Φ.a🌵0">` is a judgment started
at a new site, which the text format writes as `𝔻(Φ.a🌵0):` or `𝕄(Φ.a🌵0):`,
and `at` names that site. Whatever the judgment does is written inside the
element, so it closes where the text format drops back to the indentation it
opened at, and like the text line it counts nothing and names no meta.
`<dataized>` closes the `<dataize>` element with the datum the `delta` rule
found, `meta` naming it the way the text format does, and the `<bound>` after
the element holds that name in place of the data. A datum of eight bytes also
carries `number`, the double the text format writes as the comment of its line.
`<bound>` is one meta the firing bound, `meta` naming it the same way the text
format names it, counter and all, and the element holding the value it took:
the name of the term where the operand was reduced with 𝕄, the datum itself
where a `dataize` operand came down to data. `<manufactured>` is the other
thing a dataization may come to, the datum manufactured for an unknown. It
closes the `<dataize>` element the way `<dataized>` does, `meta` naming it with
the same counter, and holds the symbol of that unknown rather than the 42
standing for it, `𝜎2` where the text format writes the formation `𝜎2:λ`. The
`<bound>` after the element holds that name, as it does after a `<dataized>`,
while the text format writes the whole line as `𝛿1·2 := 𝔻(𝜎2:λ)`. The name of
the element is what tells a manufactured datum from data, the way `𝔻(…)` does
in the text format, so a reader never looks at the text to tell the two apart.
`<answered>` holds the name of the term the firing answered with, named the same
way by its own `meta`, and `<built>` before it holds the name of the term the
entry wrote, the one 𝕄 made that answer of: two elements rather than two
attributes of one, for the same reason `<manufactured>` is no `<dataized>`.

`<applied meta="𝑛·1·7" by="morph" at="𝑛·1·6" of="Φ.number">` is an object an
application made, which the text format writes as `𝑛·1·6 := …` in the block
`𝕄(𝑛·1·5):`: `meta` names the object, counted with the `<built>` and the
`<answered>` of the firing it stands in, `by` and `at` name the judgment that
normalized and the site it stood at, and `of` names the object applied. The
argument stands in an element named after the kind of its value, here a
`<symbol>`, with `name` naming the attribute it fills and the text holding the
value. An application nothing reduced gets an `<applied>` too, with no `by`
and no `at`, right before the element that holds it:
`<applied meta="𝑛·0·1" of="Φ.bytes">` is `Φ.bytes( 40-14-… )`, the argument of
the `Φ.number` that `𝑛·0·2` applies. A later element holding an object holds
its name instead, so `<answered meta="𝑛·1·8">𝑛·1·7</answered>` says the
firing answered with it and `<morph at="𝑛·1·6">` that 𝕄 got into what the
entry built.

The markup is for a program, which never has to parse a term of 𝜑-calculus to
read it. Every value the markup holds, in an attribute or as the text of an
element, is one of a few kinds: a symbol such as `𝜎1`, a datum such as
`40-14-00-00-00-00-00-00`, a meta such as `𝑛·0·2` or `𝛿·1·1`, a path of
attribute names off `Φ`, `ξ`, `ρ` or a meta, such as `𝑛·0·2.plus`, a name such
as `tail` or `L_fork`, or `⊥`. Inside `<applied>`, `<formed>` and `<with>`, the
element holding a value is named after its kind, so a reader never looks at the
text to tell a symbol from a meta: `<symbol>`, `<datum>`, `<meta>`, `<ref>` for
a path, and the empty `<bottom name="x"/>` for `⊥`. A term of any other kind is
named by an element of its own, written before the first element that holds
it, and is its meta from then on: an application by an `<applied>`, and a
formation by a `<formed>`. A formation that copies an object of the world,
binding its `φ` to `𝜎196`, is
`<formed meta="𝑛·3·9" of="Φ.string"><symbol name="φ">𝜎196</symbol>`: `of`
names the object by its path, and every element inside is an attribute the
copy binds to a value other than the one the object has. A copy that binds
nothing new is named by its path alone. A formation that copies no object of
the world has no `of` and holds all its bindings: an element for each bound
attribute, named after the kind of its value, a `<void name="x"/>` for each
void one, and its `λ` or `Δ`, if it has one, as an attribute of the element,
such as `<formed meta="𝑛·1·1" λ="L_fork">`. A term
the markup can name in none of these ways, such as an argument of a deferred
call that nothing worked out, is an expression meta, `𝑒1`: the same term gets
the same `𝑒` throughout the file and another term gets another, so a reader can
tell two holes apart, while what the term is stays in the text format alone.
Since the markup names terms the text format writes out, the two count their
metas each on their own: `𝑛·1·7` of the markup above is `𝑛·1·6` of the text.

`<forked side="left">𝜎2</forked>` is the decision a `fork` block made, which
the text format writes as `forked(𝔻(𝜎2:λ)) := left`: the side stands in
`side` and the condition is the text, the symbol or the datum the operand came
down to. It takes no `meta`, since the decision binds nothing, and the two
`<morph>` elements before it are the sides being morphed where they stand in
the universe; the `<answered>` of the firing follows it. The path the other side
opens is the next `<path>` of the root, which starts the run over and writes
the same element with `side="right"`. A fork that joins its two sides writes
`<joined symbol="𝜎5" left="𝜎3" right="𝜎4">𝜎2</joined>` instead, one element
per fresh symbol, which the text format writes as
`joined(𝜎5) := 𝔻(𝜎2:λ) ? 𝜎3 : 𝜎4`: the fresh symbol stands in `symbol`, the
two it stands for in `left` and `right`, and the condition is the text.

`<symbolized symbol="𝜎4" number="1.0">3F-F0-00-00-00-00-00-00</symbolized>` is
one symbol a `symbolize` line minted, which the text format writes as
`symbolized(𝜎4) := 3F-F0-00-00-00-00-00-00  # 1.0`: the fresh symbol stands in
`symbol` and the datum it stands for is the text, with the double of eight
bytes in `number`, as in `<dataized>`. The elements stand ahead of the
`<bound>` of the line, and the markup binds the meta of that line as it binds
any other.

`<minted symbol="𝜎1">40-14-… 40-18-…</minted>` is one symbol the firing
minted, one element per bare `𝜎` the entry wrote its answer with, standing
inside the block ahead of the `<built>` carrying them. The symbol stands in
`symbol` and the text is what is known about it: the values the `dataize`
lines of the entry took, in the order the entry declares them, each spelled as
its own line spells it — the bytes for a datum, `𝜎1` for a symbol — and
separated by a space. With the `λ` of the block the element reads as the fact
`𝔻(𝜎1:λ) == L_number_plus(40-14-…, 40-18-…)`, and a firing of an entry with
no `dataize` line writes `<minted symbol="𝜎1"/>`. The symbol is also the edge
a reader joins on: a later `<manufactured meta="𝛿·5·1">𝜎2</manufactured>` names
the symbol the firing that wrote `<minted symbol="𝜎2">` handed out. A firing
minting two symbols writes two elements and one minting none writes none,
which no attribute on the answer could say: a term may carry several symbols,
or carry one where the value it stands for is not a symbol at all. In the fork
above, `𝔼(L_gt)` writes `<minted symbol="𝜎2">` although `𝜎2` sits under `if`
and not where the value of the term is, while `𝔼(L_fork)` writes none at all,
since it answers a side of the program and mints nothing.

A λ name no entry answers is `<unanswered λ="…">`, standing where its
`<evaluate>` would have stood with the name of the formation 𝔼 was fired
against as its text, such as `𝑛·1·7.nope` below, and the judgment that asked
in its `by` attribute, where the text format writes the letter of it. A firing
that happened while an operand of another was being reduced is an `<evaluate>`
inside the one that asked, which is what the deeper indentation means in the
text. Elements are written as the run goes and
the open ones are closed when it ends, so a run that fails still leaves a
well-formed document behind:

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --protocol=atoms.xml --quiet \
    --sweet --hide-rho stuck.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_nope'
$ cat atoms.xml
<?xml version="1.0" encoding="UTF-8"?>
<protocol>
  <path>
    <dataize at="Φ">
      <morph at="Φ">
        <applied meta="𝑛·0·1" of="Φ.bytes"><datum name="φ">40-14-00-00-00-00-00-00</datum></applied>
        <applied meta="𝑛·0·2" by="morph" at="Φ" of="Φ.number"><meta name="φ">𝑛·0·1</meta></applied>
        <applied meta="𝑛·0·3" of="Φ.bytes"><datum name="φ">40-18-00-00-00-00-00-00</datum></applied>
        <applied meta="𝑛·0·4" of="Φ.number"><meta name="φ">𝑛·0·3</meta></applied>
        <applied meta="𝑛·0·5" by="morph" at="Φ" of="𝑛·0·2.plus"><meta name="x">𝑛·0·4</meta></applied>
        <evaluate λ="L_number_plus" by="morph" at="Φ">
          <dataize at="Φ.a🌵0">
            <morph at="Φ.a🌵0">
              <applied meta="𝑛·1·1" by="morph" at="Φ.a🌵0" of="Φ.number"><meta name="φ">𝑛·0·1</meta></applied>
              <applied meta="𝑛·1·2" by="morph" at="Φ.a🌵0" of="Φ.bytes"><datum name="φ">40-14-00-00-00-00-00-00</datum></applied>
            </morph>
            <dataized meta="𝛿·1·1" number="5.0">40-14-00-00-00-00-00-00</dataized>
          </dataize>
          <bound meta="𝛿1·1">𝛿·1·1</bound>
          <dataize at="Φ.a🌵1">
            <morph at="Φ.a🌵1">
              <applied meta="𝑛·1·3" by="morph" at="Φ.a🌵1" of="Φ.number"><meta name="φ">𝑛·0·3</meta></applied>
              <applied meta="𝑛·1·4" by="morph" at="Φ.a🌵1" of="Φ.bytes"><datum name="φ">40-18-00-00-00-00-00-00</datum></applied>
            </morph>
            <dataized meta="𝛿·1·2" number="6.0">40-18-00-00-00-00-00-00</dataized>
          </dataize>
          <bound meta="𝛿2·1">𝛿·1·2</bound>
          <minted symbol="𝜎1">40-14-00-00-00-00-00-00 40-18-00-00-00-00-00-00</minted>
          <applied meta="𝑛·1·5" of="Φ.number"><symbol name="φ">𝜎1</symbol></applied>
          <built meta="𝑛·1·6">𝑛·1·5</built>
          <morph at="𝑛·1·6">
            <applied meta="𝑛·1·7" by="morph" at="𝑛·1·6" of="Φ.number"><symbol name="φ">𝜎1</symbol></applied>
          </morph>
          <answered meta="𝑛·1·8">𝑛·1·7</answered>
        </evaluate>
      </morph>
      <unanswered λ="L_number_nope" by="dataize">𝑛·1·7.nope</unanswered>
    </dataize>
  </path>
</protocol>
```

<!-- markdownlint-enable MD013 -->

### Abridging the protocol

A formation carrying a whole object is written flat on one line, so a real
run fills the protocol with lines tens of thousands of characters long. The
`--abridged` option shortens every term the text protocol writes: a formation
longer than sixty-four characters keeps its `φ`, `Δ` and `λ` bindings and folds
the rest into a count. The markup writes no terms, only their names, so the
option leaves it as it is. The width is a value of the option,
`--abridged=120`, for a run that can read longer lines. The result the run
prints stays whole, and the option is refused without `--protocol`.
Every byte string the protocol writes stays whole too, since a reader may need
the data a firing came down to:

<!-- markdownlint-disable MD013 -->

```bash
$ cat wide.phi
⟦
  t ↦ ⟦
    φ ↦ ⟦ Δ ⤍ 48-65-6C-6C-6F-2C-20-77-6F-72-6C-64 ⟧,
    left ↦ ξ.right,
    right ↦ ξ.left,
    middle ↦ ξ.left
  ⟧
⟧
$ phino dataize --locator=Q.t --protocol=wide.txt --abridged --quiet \
    --sweet --hide-rho wide.phi
$ cat wide.txt
𝔻(Φ.t):
  𝛿·0·1 := 48-65-6C-6C-6F-2C-20-77-6F-72-6C-64
```

<!-- markdownlint-enable MD013 -->

The `--abridged-data` option cuts the data too: a byte string longer than eight
bytes keeps its first two bytes and its last two, with the count of the bytes
cut out between them. A formation folded into a count loses its data either
way, and the option is refused without `--abridged`:

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --locator=Q.t --protocol=wide.txt --abridged \
    --abridged-data --quiet --sweet --hide-rho wide.phi
$ cat wide.txt
𝔻(Φ.t):
  𝛿·0·1 := 48-65-..(8b)..-6C-64
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
  number ↦ ⟦ φ ↦ ∅, plus(ρ, x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧
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
    plus(ρ, x) ↦ ⟦ λ ⤍ L_number_plus ⟧,
    times(ρ, x) ↦ ⟦ λ ⤍ L_number_times ⟧,
    as-bool ↦ ⟦ λ ⤍ L_number_as_bool ⟧
  ⟧,
  φ ↦ 2.times( 3 ).plus( 4 ).as-bool
⟧
$ phino dataize --symbolic=atoms.yaml --sweet --hide-rho partial.phi
[ERROR]: No entry of --symbolic answers the λ function 'L_number_as_bool'
$ phino dataize --symbolic=atoms.yaml --partial --sweet --hide-rho partial.phi
L_number_as_bool:λ
```

Here `2.times( 3 ).plus( 4 )` was answered by the entries the file carries, so
it was reduced — the symbol it came to sits in the hidden `ρ` of the residual
program — while `as-bool` names a λ function no entry answers, so it stays in
place as a normal-form subterm. A stuck site opens no block in the
`--protocol` file, since nothing fired there, and stands in it as
`unanswered(…)`:

<!-- markdownlint-disable MD013 -->

```bash
$ phino dataize --symbolic=atoms.yaml --partial --protocol=atoms.txt --quiet \
    --sweet --hide-rho partial.phi
$ cat atoms.txt
𝔻(Φ):
  𝕄(Φ):
    𝑛·0·1 := 2
    𝑛·0·2 := 𝑛·0·1.times( x ↦ 3 )
    𝔼(L_number_times):
      𝔻(Φ.a🌵0):
        𝕄(Φ.a🌵0):
          𝑛·1·1 := 2
          𝑛·1·2 := Φ.bytes( φ ↦ 40-00-00-00-00-00-00-00:Δ )
        𝛿·1·1 := 40-00-00-00-00-00-00-00  # 2.0
      𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)
      𝔻(Φ.a🌵1):
        𝕄(Φ.a🌵1):
          𝑛·1·3 := 3
          𝑛·1·4 := Φ.bytes( φ ↦ 40-08-00-00-00-00-00-00:Δ )
        𝛿·1·2 := 40-08-00-00-00-00-00-00  # 3.0
      𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)
      𝑛·1·5 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛
      𝕄(𝑛·1·5):
        𝑛·1·6 := Φ.number( φ ↦ 𝜎1:λ )
      𝑛·1·7 := 𝑛·1·6  # 𝕄(𝑛·1·5)
    𝑛·0·3 := 𝑛·1·6.plus( x ↦ 4 )
    𝔼(L_number_plus):
      𝔻(Φ.a🌵2):
        𝕄(Φ.a🌵2):
          𝑛·2·1 := Φ.number( φ ↦ 𝜎1:λ )
      𝛿1·2 := 𝔻(𝜎1:λ)  # 𝔻(ξ.ρ)
      𝔻(Φ.a🌵3):
        𝕄(Φ.a🌵3):
          𝑛·2·2 := 4
          𝑛·2·3 := Φ.bytes( φ ↦ 40-10-00-00-00-00-00-00:Δ )
        𝛿·2·1 := 40-10-00-00-00-00-00-00  # 4.0
      𝛿2·2 := 𝛿·2·1  # 𝔻(ξ.x)
      𝑛·2·4 := Φ.number( φ ↦ 𝜎2:λ )  # 𝑛
      𝕄(𝑛·2·4):
        𝑛·2·5 := Φ.number( φ ↦ 𝜎2:λ )
      𝑛·2·6 := 𝑛·2·5  # 𝕄(𝑛·2·4)
  unanswered(L_number_as_bool)  # 𝔻(L_number_as_bool:λ)
```

<!-- markdownlint-enable MD013 -->

Evaluation stays demand-driven, as the calculus prescribes: an argument
that nothing asked for before the run got stuck is left as it is in the
residual program, for the next iteration.

An operand of a firing that reaches the terminator `⊥`, or a term no
dataization rule matches, such as a formation whose `φ` is a void nothing
filled, never comes down to data either, and `--partial` parks that firing
the same way, writing the dead end into the protocol as `unanswered(⊥)` with the
term that could not be dataized beside it. Dataization aimed at `⊥` itself still
fails, with or without `--partial`, since there is no firing to park.

The nested morphing and dataization recursion is bounded by the
`--max-steps` option (default `1000`): when the budget is exhausted, the run
fails with `Dataization did not finish before reaching the limit of steps`.
This guards against non-terminating terms, which used to loop forever before
the bound was introduced:

```bash
$ phino dataize --max-steps=50 problem.phi
[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=50
```

That budget bounds how deep one branch goes, not how much the whole run does.
An entry that reduces two operands, each firing it again, doubles its work at
every level and still never gets deep, so no `--max-steps` stops it. The
`--max-firings` option counts every λ function the run fires and fails the
run once the count is spent; `--partial` parks it instead, the way it parks a
spent `--max-steps`. There is no limit unless the option is given:

```bash
$ cat split.yaml
- λ: L_split
  morph:
    𝑛1: Φ.s.foo
    𝑛2: Φ.s.foo
  𝑛: ⟦ l ↦ 𝑛1, r ↦ 𝑛2 ⟧
$ cat split.phi
⟦ s ↦ ⟦ λ ⤍ L_split ⟧, x ↦ Φ.s.foo ⟧
$ phino morph --symbolic=split.yaml --locator=Q.x --max-firings=64 split.phi
[ERROR]: Evaluation did not finish before reaching the limit of firings: --max-firings=64
```

A fork of `--symbolic` whose sides differ opens a path per side, and a program
forking inside a recursion opens a path per round, so a command may run more
paths than its caller can wait for. The `--max-paths` option bounds how many
paths the command runs: once that many are open, a fork nobody decided takes
its left side and opens no path, and the command prints the results it has.
There is no limit unless the option is given, and the option is refused with
zero.

Both budgets count work, so a run inside both of them may still take longer
than its caller can wait, and a caller that kills it gets a protocol nobody
closed. The `--max-seconds` option stops the run by the clock instead: once
that many seconds have passed since the command started, the next step the
run is about to take, whether it fires a λ function or not, fails it with
`Evaluation did not finish before reaching the limit of seconds`, and so does
a check of `--acyclic` that is still comparing formations by then.
`--partial` does not park it, since a run out of time has no site to park and
nothing left to go on with. The protocol is closed as usual and its last line
says where the time ran out. There is no limit unless the option is given:

```bash
$ phino morph --symbolic=split.yaml --locator=Q.x --partial --max-seconds=5 \
    --protocol=split.txt --sweet --flat split.phi
[ERROR]: Evaluation did not finish before reaching the limit of seconds: --max-seconds=5
$ grep -o 'timeout.*' split.txt
timeout(5)
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
  number ↦ ⟦ φ ↦ ∅, plus(ρ, x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
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

𝕄 maps normal forms to formations and `morph` does not normalize what it is
given, so a term that is not a normal form, such as a dispatch off a formation
with neither `φ` nor `λ`, is reported as a failed run rather than answered:

```bash
$ phino morph --locator=Q.t <<< '⟦ t ↦ ⟦ x ↦ ⟦⟧ ⟧.x ⟧'
[ERROR]: Morphing expects a normal form, but no morphing rule matches: ⟦⟧:x.x
```

The whole `dataize` option surface applies unchanged — `--symbolic`,
`--inside`, `--sequence`, `--headers`, `--steps-dir`, `--protocol`,
`--partial`, `--max-steps`, `--shuffle`/`--seed`, `--output`, `--focus` and the
rest.

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

The `--acyclic=<mode>` option makes a reduction notice. Every frame of 𝕄 and
of 𝔻 remembers the formations the frames above it have entered, and only four
places enter one: `fire` of 𝔻, `ml` of 𝕄 and an `evaluate` line of an entry,
which fire the λ function of a formation, and `box` of 𝔻, which gets into the
`φ` body of a formation carrying no λ and no `Δ`. A frame about to enter a
formation one of the frames above it has already entered is asking a question
only ever answered by asking it again, so the option stops there and parks the
site the way `--partial` parks a λ function that cannot fire: the answer is the
term the spine had reached, left where it stood, and the command exits
successfully.

```bash
$ phino morph --symbolic=loop.yaml --locator='Q.x' --acyclic=proven \
    --max-steps=40 --hide-rho loop.phi
⟦ λ ⤍ L_loop ⟧.foo
```

𝕄 and 𝔻 share that one memory. They call each other on the very term they
were asked about, and that handover is no loop, but it enters no formation
either, so it is never remembered and never mistaken for one. A body
dispatching the object it stands in is a loop 𝔻 walks round on its own — 𝕄
stops at a formation every round, and `box` gets into it again — so `dataize`
takes the option too, and so does the run of 𝔻 a λ function's `dataize` operand
is brought down with:

```bash
$ cat cyc.phi
⟦ cyc ↦ ⟦ x ↦ ∅, φ ↦ Φ.cyc( ξ.x ) ⟧, t ↦ Φ.cyc( ⟦⟧ ) ⟧
$ phino dataize --locator='Q.t' --acyclic=proven --partial \
    --sweet --hide-rho --flat cyc.phi
⟦ cyc(x) ↦ Φ.cyc( x ):φ, t ↦ Φ.cyc( ⟦⟧ ) ⟧
```

𝔻 insists on bytes and a parked term carries none, so under `dataize` the option
wants `--partial` to have something to print: the residual program, exactly the
one it prints for a λ function that cannot fire. Without it the run stops on
the loop all the same, naming the formation it entered again instead of running
the budget down. Under `morph` nothing is asked for: 𝕄 always has a term to
answer with, and a loop 𝔻 meets under a firing parks the site the firing
stands at.

The mode says what "the same formation" means, and there is no default, since
no answer is right for every run. Under `proven` it means the same up to a
renaming of symbols: two
formations are one where some one-to-one pairing of the symbols of the first
with those of the second makes them equal, so `𝜎5` may stand where `𝜎3` stood
as long as it does so everywhere and no other symbol stands there too. Data,
attribute names and λ names must still match exactly. That is what catches a
recursion over an unknown, which never repeats a term: every round mints fresh
symbols, so the formation it enters on the second round is the one it entered
on the first with new names for the unknowns. The renaming is sound because a
symbol is an opaque value nobody worked out — every one of them dataizes to the
same manufactured datum and no entry of `--symbolic` answers one — so a
formation entered again with nothing but its symbols renamed replays the round
it is inside forever. Data still tells rounds apart: a formation entered with
`n ↦ 3` and then with `n ↦ 2` is two formations, so a recursion over data is
not cut while it goes on computing. Take an entry that answers a call of its
own object over a fresh symbol, so every round fires the same λ function over
an unknown it has never seen:

```bash
$ cat down.yaml
- λ: L_down
  dataize:
    𝛿1: $.x
  𝑛: Φ.down( x ↦ ⟦ λ ⤍ 𝜎 ⟧ )
$ cat down.phi
⟦
  down ↦ ⟦ x ↦ ∅, λ ⤍ L_down ⟧,
  y ↦ Φ.down( x ↦ ⟦ λ ⤍ 𝜎1 ⟧ ).φ
⟧
$ phino morph --symbolic=down.yaml --locator=Q.y --acyclic=proven \
    --protocol=down.txt --sweet --hide-rho down.phi
⟦ x ↦ 𝜎1:λ, λ ⤍ L_down ⟧.φ
$ cat down.txt
𝕄(Φ.y):
  𝑛·0·1 := Φ.down( x ↦ 𝜎1:λ )
  𝔼(L_down):
    𝛿1·1 := 𝔻(𝜎1:λ)  # 𝔻(ξ.x)
    𝑛·1·1 := Φ.down( x ↦ 𝜎2:λ )  # 𝑛
    𝕄(𝑛·1·1):
      𝑛·1·2 := Φ.down( x ↦ 𝜎2:λ )
    𝑛·1·3 := 𝑛·1·2  # 𝕄(𝑛·1·1)
  looped(𝑛·0·1)  # proven
```

The first round fires `down` with `x ↦ 𝜎1` and answers it with `x ↦ 𝜎2`, and
the `φ` dispatched off that answer would fire it again: the formation the first
round entered, `𝜎2` standing where `𝜎1` stood, so the firing is cut before it
opens. The cut is the `looped(…)` line, standing where the block of the cut
firing would have stood: the block it stands in names the judgment the frame
belonged to and the site it was cut at, and the comment names the mode that
cut it. What it carries is the formation the frame above entered, named by the
line that made it, `𝑛·0·1`, so the two are paired by their terms and no reader
has to rename symbols by eye or find the cut in the residue. Nothing runs under
a cut, so no block opens under the line. In the XML protocol it is
`<looped by="morph" match="proven" at="Φ.y" of="𝑛·0·1"/>`, with the site, the
mode and the name of the formation as attributes. Without the option
the same run nests one round inside another until `--max-steps` runs out.

What a frame remembers is the branch from the run down to it, never everything
the run has touched, so two siblings entering one formation enter it twice and
only a formation entered from inside itself is a loop. The cut costs one lookup
and fires on the turn the repeat appears, so raising `--max-steps` from 40 to a
million changes neither the answer nor the time.

What `proven` cannot see is a recursion that never comes back to the same
formation: one whose arguments grow by a wrapper every round. Take a factorial
with an accumulator it builds a `pair` onto, with `facta.yaml` answering
`L_zero` and `L_dec` with a fresh symbol each and `L_if` forking on its
condition. Under `proven` every round opens a path of its own, nesting one
level deeper than the last until `--max-steps` runs out, since no renaming of
symbols turns a longer chain of pairs into a shorter one, and `--max-paths` is
what ends such a run early:

<!-- markdownlint-disable MD013 -->

```bash
$ cat facta.phi
⟦
  if ↦ ⟦ c ↦ ∅, left ↦ ∅, right ↦ ∅, λ ⤍ L_if ⟧,
  zero ↦ ⟦ x ↦ ∅, λ ⤍ L_zero ⟧,
  dec ↦ ⟦ x ↦ ∅, λ ⤍ L_dec ⟧,
  box ↦ ⟦ v ↦ ∅, φ ↦ ξ.v ⟧,
  pair ↦ ⟦ head ↦ ∅, tail ↦ ∅, φ ↦ ξ.head ⟧,
  fact ↦ ⟦ n ↦ ∅, acc ↦ ∅, φ ↦ Φ.if( c ↦ Φ.zero( x ↦ ξ.n ), left ↦ ξ.acc, right ↦ Φ.fact( n ↦ Φ.dec( x ↦ ξ.n ), acc ↦ Φ.pair( head ↦ ξ.n, tail ↦ ξ.acc ) ).φ ).φ ⟧,
  x ↦ Φ.fact( n ↦ ⟦ λ ⤍ 𝜎1 ⟧, acc ↦ Φ.box( v ↦ ⟦ Δ ⤍ 00- ⟧ ) ).φ
⟧
$ cat facta.yaml
- λ: L_zero
  dataize:
    𝛿1: $.x
  𝑛: ⟦ λ ⤍ 𝜎 ⟧
- λ: L_dec
  dataize:
    𝛿1: $.x
  𝑛: ⟦ λ ⤍ 𝜎 ⟧
- λ: L_if
  dataize:
    𝛿1: $.c
  fork:
    on: 𝛿1
    left: $.left
    right: $.right
$ phino morph --acyclic=plausible --partial --sweet --hide-rho --flat \
    --symbolic=facta.yaml --locator='Q.x' --protocol=facta.txt facta.phi
00-:Δ
Φ.if( c ↦ Φ.zero( x ↦ 𝜎1:λ ), left ↦ Φ.box( v ↦ 00-:Δ ), right ↦ Φ.fact( n ↦ Φ.dec( x ↦ 𝜎1:λ ), acc ↦ Φ.pair( head ↦ 𝜎1:λ, tail ↦ Φ.box( v ↦ 00-:Δ ) ) ).φ ).φ
$ grep looped facta.txt
      looped(𝑛·0·5)  # plausible
      looped(𝑛·0·10)  # plausible
  looped(𝑛·0·10)  # plausible
```

<!-- markdownlint-enable MD013 -->

Under `plausible` the same formation means one the formation entered earlier
is embedded in: the two have the same attributes, data and λ function at the
top, and every term the earlier one bound there is found again in the later
one, as it stands or somewhere below a wrapper it gained. Any symbol stands for
any other, and nothing is looked for under a `ρ`, which holds the object a term
came from rather than a term it grew into. The first `if` morphs both of its
sides before it decides, and its `right` holds a second `if`, which holds the
first under one more `pair` and one more `dec`, so that morph is cut on both
paths, and its `looped(…)` line says `plausible`. The first path takes the
`left` and answers the accumulator. The second takes the `right`, which comes
back as the term the cut left, the path meets the same `if` once more and is
cut again, and `--partial` parks it. A call
nested in its own operand, such as a sum of sums, is never
cut, since the inner call is smaller than the outer one and cannot hold it.
The mode is not sound, though: a recursion whose argument grows on its way to
stopping is cut too, which is why the line names the mode that made the cut.

The mode also fires every formation once. Every use of a binding copies the
term bound to it, so a program reading `truncated ↦ ρ.abs.floor` in five
places fires `abs` and `floor` five times over and mints five symbols for one
value, and a guard written that way spends its whole budget saying the same
thing again. 𝔼 is a function of the formation it fires: the entry that
answers is found by the λ name the formation carries, every operand is
reduced from its bindings inside the one universe of the run, and the answer
is built from what they came down to. So a run under `plausible` keeps what
every firing answered, by the formation it fired, and a later firing of the
same formation takes that answer, with the very symbols the first one minted,
instead of making it again. Two arguments spelling one term then come to one
symbol, here through an entry `L_pair` that fires both with `evaluate` lines:

```bash
$ cat twins.yaml
- λ: L_number_plus
  dataize:
    𝛿1: $.ρ
    𝛿2: $.x
  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )
- λ: L_pair
  morph:
    𝑛1: $.a
    𝑛2: $.b
  evaluate:
    𝑛3: 𝑛1
    𝑛4: 𝑛2
  𝑛: ⟦ φ ↦ ⟦ a ↦ 𝑛3, b ↦ 𝑛4 ⟧ ⟧
$ cat twins.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(ρ, x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  pair ↦ ⟦ a ↦ ∅, b ↦ ∅, λ ⤍ L_pair ⟧,
  t ↦ Φ.pair( 7.plus( 5.plus( 6 ) ), 7.plus( 5.plus( 6 ) ) ).φ
⟧
$ phino morph --symbolic=twins.yaml --acyclic=plausible --locator=Q.t \
    --sweet --hide-rho twins.phi
⟦
  a ↦ ⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧,
  b ↦ ⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧
⟧
```

Under `proven` `b` lands on `𝜎4`, since its `evaluate` line fires the outer sum
and the inner one once more. The protocol of the run above holds three firings
of `L_number_plus`, two for `a` and one for `b`, since it records where 𝔼 was
asked and what it answered there; the one for `b` carries the answer lines of
the outer one for `a` and no operand line, since nothing was reduced for it,
and it is not charged to `--max-firings`, which counts the firings the run
made. The formation is compared with everything it carries, `ρ` included, so
a firing on another object is another firing, yet it may take the same answer
all the same: the run keeps every answer by the λ name and what the operands
came down to as well, and a firing whose operands come down to the data, the
symbols and the normal forms of one already answered takes that answer once
they are down, minting nothing. An attribute read from the `φ` of an object
and then reached as a binding of it is read through two `ξ`, each the object
minus the attribute being read, so it is two formations, but one firing, and
so one symbol. A symbol counts as the symbol it is, not as the datum every
symbol manufactures, so two firings over two symbols stay two. The protocol
writes such a firing with its operand lines and the answer lines of the one
it took the answer from, and `--max-firings` charges it like a firing made,
since a firing is charged as it starts bringing its operands down, which is
the only place a recursion widening inside its operands can be cut. A firing
cut by the mode on its way to an answer keeps the cut, and the next firing of
the same formation is cut at its own site without reducing anything first. A
firing that got stuck keeps the λ function it got stuck on, and the next
firing of the same formation gets stuck at its own site the same way, with
nothing reduced under it, as long as nothing new was answered in between.
Once something was, the formation is fired again, since an operand that could
not be brought down the first time may come down now.

The `--max-loops=<n>` option makes either mode more patient. A frame is cut
only once more than `n` of the formations the frames above it entered are the
same as the one it is about to enter, so a recursion that stops within `n`
rounds is never cut, and one that goes on is cut `n` rounds later. The default
is `0`, which cuts on the first repeat. The option needs `--acyclic`, since
nothing else cuts a loop. Here the second `f` holds `Φ.start`, which the first
`f` is bound to, under a `Φ.stop`, so `plausible` cuts it, even though `stop`
answers data at once:

<!-- markdownlint-disable MD013 -->

```bash
$ cat once.phi
⟦
  f ↦ ⟦ x ↦ ∅, φ ↦ ξ.x.again ⟧,
  start ↦ ⟦ again ↦ Φ.f( x ↦ Φ.stop( v ↦ Φ.start ) ) ⟧,
  stop ↦ ⟦ v ↦ ∅, again ↦ ⟦ Δ ⤍ 01- ⟧ ⟧,
  t ↦ Φ.f( x ↦ Φ.start )
⟧
$ phino dataize --locator=Q.t --acyclic=plausible once.phi
[ERROR]: Reduction entered a formation it is already inside: ⟦ x ↦ Φ.stop( v ↦ Φ.start ), φ ↦ x.again ⟧
$ phino dataize --locator=Q.t --acyclic=plausible --max-loops=1 once.phi
01-
```

<!-- markdownlint-enable MD013 -->

## Rewrite

You can rewrite this expression with the help of [rules](#rule-structure)
defined in the `my-rule.yml` YAML file (here, the `!d1` and `!B1` are capturing
groups, similar to regular expressions):

```yaml
name: My custom rule
pattern: ⟦ Δ ⤍ !d1, !B1 ⟧
result: ⟦ Δ ⤍ 62-79-65, !B1 ⟧
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
  φ ↦ Φ.io.stdout( α0 ↦ Φ.string( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧ ) ) )
⟧
```

The two styles may be mixed in one expression, so `[[ x -> ∅ ]]` reads the
same as `⟦ x ↦ ∅ ⟧`. The `--monoform` option forbids that: the first symbol
that has two spellings picks the style of the whole expression, and a later
symbol of the other style is an error. Under `--monoform`, `[[ x -> ∅ ]]`
fails, since `∅` at 1:9 mixes with the ASCII `[[` at 1:1. The option is
accepted by `rewrite`, `dataize`, `morph`, `merge` and `match`.

A formation of a single binding may be written as its asset, a colon,
and the attribute the asset is bound to:

```text
⟦ Δ ⤍ FF-AA ⟧    = FF-AA:Δ    = FF-AA:D
⟦ λ ⤍ 𝜎1 ⟧       = 𝜎1:λ       = !S1:L
⟦ a ↦ ∅ ⟧        = ∅:a        = ?:a
⟦ φ ↦ ξ.a ⟧      = ξ.a:φ      = $.a:@
```

The colon binds as tightly as a dot, so `ξ.a:φ.b` is `⟦ φ ↦ ξ.a ⟧.b`.
With `--sweet`, `phino` prints every such formation this way, so
`⟦ x ↦ ⟦ φ ↦ ξ.a ⟧ ⟧` comes out as `a:φ:x` and `⟦ x(a) ↦ ⟦ φ ↦ a ⟧ ⟧`
as `⟦ x(a) ↦ a:φ ⟧`: the formation inline voids open takes the sugar,
while the one holding the voids keeps its brackets, and so does
every formation in the salty syntax and in [LaTeX][latex].

A formation has a receiver `ρ` only when it declares one among its voids, the
way EO declares `^`: `⟦ ρ ↦ ∅, t ↦ ξ.ρ.k ⟧`, or `a(ρ) ↦ ⟦ t ↦ ξ.ρ.k ⟧` with
the void inline. `phino` adds none of its own, and a dispatch into a formation
that declares none hands it no `ρ` (the `skip` rule), so `ξ.ρ` there is `⊥`:

<!-- markdownlint-disable MD013 -->

```bash
$ echo '⟦ x ↦ ⟦ k ↦ ⟦ Δ ⤍ 01- ⟧, a ↦ ⟦ ρ ↦ ∅, t ↦ ξ.ρ.k ⟧ ⟧.a.t ⟧' | phino rewrite --normalize --sweet
01-:Δ:x
$ echo '⟦ x ↦ ⟦ k ↦ ⟦ Δ ⤍ 01- ⟧, b ↦ ⟦ t ↦ ξ.ρ.k ⟧ ⟧.b.t ⟧' | phino rewrite --normalize --sweet
⊥:x
```

<!-- markdownlint-enable MD013 -->

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
  number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ, minus(x) ↦ L_number_minus:λ ⟧
⟧
```

## Match

You can test the 𝜑-expression matches against the [rule](#rule-structure)
pattern. The result output contains matched substitutions:

```bash
$ phino match --pattern='⟦ Δ ⤍ !d, !B ⟧' hello.phi
B >> ⟦⟧
d >> 68-65-6C-6C-6F
```

## Compile

By default, `phino` reads its rules from YAML and interprets them at every
step. The `compile` command turns the rules into Haskell instead. Then a
second build of `phino` runs them as plain functions:

```bash
phino compile
cabal build all
```

The command writes the module `compiled/generated/Compiled.hs`, which git
ignores. It compiles the built-in rules of normalization, the
contextualization function 𝒞, and every file you pass with `--rule`. The
`--target` option writes the module somewhere else.

A build links the module in only when the Cabal flag `compiled` is on. If
there is no `cabal.project.local`, `compile` creates one that turns the flag
on. If the file already exists, `compile` leaves it alone and prints the two
lines to add to it:

```text
package phino
  flags: +compiled
```

The compiled rules take exactly the same steps as the YAML ones, so the
output and every `--sequence` stay the same. A few things are still read from
YAML at runtime:

* the rules of morphing (𝕄) and dataization (𝔻);
* a `--rule` file that changed after `compile`;
* the pattern of `match` and the `rewrite:` blocks of the `--symbolic` file.

The `explain` command also reads the rules from YAML.

`compile` refuses a rule it cannot turn into Haskell and names the reason.
For example, it refuses a rule with `having`, a `where` function other than
`contextualize` or `named`, or the conditions `matches` and `part-of`.

A binary built this way refuses to run if the built-in rules changed after
the last `compile`, since it would run rules nobody wrote. Run `compile`
again and rebuild. To test the whole suite against the compiled rules, run:

```bash
make compiled
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
  { $ [ \tau \char44{} @ \char44{} L ] \cap B = \emptyset $ }
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
  | in:                  # returns True if every given attribute exists in
      - Attribute' | [Attribute'] # the union of the given bindings
      - Binding' | [Binding']
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
* `random-tau` - creates an attribute with a random unique name. It accepts no
  arguments; uniqueness is guaranteed across all names already taken in the
  document.
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
by. Writing one outside a `pattern` (or the `match`, `universe` and `c-match` of
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
  total:      2029593.832 μs
  avg:        202959.383 μs
  min:        184828.981 μs
  max:        242789.481 μs
  std dev:    21354.788 μs
=== parse/xmir ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      8307581.623 μs
  avg:        830758.162 μs
  min:        779299.821 μs
  max:        924632.124 μs
  std dev:    53711.689 μs
=== rewrite/normalize ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      19.482 μs
  avg:        1.948 μs
  min:        1.582 μs
  max:        3.055 μs
  std dev:    0.421 μs
=== print/sweet/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      4835328.824 μs
  avg:        483532.882 μs
  min:        447493.225 μs
  max:        513457.074 μs
  std dev:    21659.187 μs
=== print/sweet/flat ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      612379.660 μs
  avg:        61237.966 μs
  min:        55340.044 μs
  max:        74714.542 μs
  std dev:    8600.783 μs
=== print/salty/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      15074459.519 μs
  avg:        1507445.952 μs
  min:        1479762.422 μs
  max:        1553789.172 μs
  std dev:    19762.987 μs
=== morph/symbolic/demo/e1 ===
  warmup:     3 iterations
  batches:    10 x 10
  total:      10542.298 μs
  avg:        105.423 μs
  min:        99.952 μs
  max:        111.447 μs
  std dev:    2.921 μs
=== morph/symbolic/demo/e2 ===
  warmup:     3 iterations
  batches:    10 x 159
  total:      169201.484 μs
  avg:        106.416 μs
  min:        105.596 μs
  max:        108.170 μs
  std dev:    0.688 μs
=== morph/symbolic/demo/e3 ===
  warmup:     3 iterations
  batches:    10 x 159
  total:      170575.414 μs
  avg:        107.280 μs
  min:        106.746 μs
  max:        107.734 μs
  std dev:    0.276 μs
=== morph/symbolic/demo/e4 ===
  warmup:     3 iterations
  batches:    10 x 160
  total:      171520.967 μs
  avg:        107.201 μs
  min:        106.576 μs
  max:        108.529 μs
  std dev:    0.641 μs
=== morph/symbolic/demo/e5 ===
  warmup:     3 iterations
  batches:    10 x 154
  total:      167245.805 μs
  avg:        108.601 μs
  min:        107.868 μs
  max:        110.226 μs
  std dev:    0.635 μs
=== morph/symbolic/native/e5 ===
  warmup:     3 iterations
  batches:    10 x 4
  total:      62729.187 μs
  avg:        1568.230 μs
  min:        1536.999 μs
  max:        1611.166 μs
  std dev:    20.908 μs
=== morph/symbolic/accum/0 ===
  warmup:     3 iterations
  batches:    10 x 92
  total:      83785.377 μs
  avg:        91.071 μs
  min:        90.430 μs
  max:        91.710 μs
  std dev:    0.337 μs
=== morph/symbolic/accum/400 ===
  warmup:     3 iterations
  batches:    10 x 78
  total:      86403.817 μs
  avg:        110.774 μs
  min:        109.748 μs
  max:        112.988 μs
  std dev:    1.000 μs
```

The results were calculated in [this GHA job][benchmark-gha]
on 2026-10-17 at 17:27,
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
[gha]: https://docs.github.com/en/actions
[jna]: https://github.com/java-native-access/jna
[jna-native]: https://github.com/java-native-access/jna/blob/master/src/com/sun/jna/Native.java
[jeo]: https://github.com/objectionary/jeo-maven-plugin
[issue-1291]: https://github.com/objectionary/phino/issues/1291
[benchmark-gha]: https://github.com/objectionary/phino/actions/runs/39053629890
