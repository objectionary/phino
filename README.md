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
cabal install --overwrite-policy=always phino-0.0.158
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
- uses: objectionary/phino@0.0.158
  with:
    version-file: phino-version.txt
- run: phino --version
```

The file holds the version to install, such as `0.0.150`.
Give `version: 0.0.150` instead of `version-file` to write the version
in the workflow itself.

## Build

To build `phino` from source, clone this repository and build it with
[Cabal][cabal]:

```bash
git clone git@github.com:objectionary/phino.git
cd phino
cabal build all
sudo cp "$(cabal list-bin phino)" /usr/local/bin/phino
```

The global `--pin` option fails the run when the installed version is not
the one given, and `--pin-file` reads that version from a file:

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

`phino` implements no λ function.
They come from a YAML file given with `--symbolic`, one entry per λ function.
An entry never computes a value.
It answers with a term that holds a symbol, `𝜎`, which names a value nobody
worked out.
Here is `atoms.yaml`, which the examples below use:

```yaml
- λ: L_number_(plus|times)
  dataize:
    𝛿1: $.ρ
    𝛿2: $.x
  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )
```

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

Dataizing a symbol always gives the same bytes, the number 42, as above.

Inside an entry, `$` is the formation being fired and `Φ` is the universe.
A meta is a named placeholder: `𝛿1`, `𝛿2` and so on hold bytes, while `𝑛1`,
`𝑛2` and so on hold terms.
An entry may hold only the keys below, and any other key refuses the whole
file:

* `λ` is a regular expression that matches the whole λ name.
  No two entries may match the same name.
* `dataize` binds a bytes meta to the data a term comes down to through
  dataization, 𝔻.
  The term is a path from `$`, as in `𝛿1: $.ρ`, or a meta another line binds,
  as in `𝛿2: 𝑛1`.
* `morph` binds a term meta to the normal form a term reaches through
  morphing, 𝕄.
  The term is a path from `$`, as in `𝑛1: $.x`, or a meta another line binds,
  as in `𝑛4: 𝑛3`.
* `undress` binds a term meta as `morph` does, and then, while the term is a
  formation binding `φ` alone beside its `ρ`, takes its `φ` and morphs that.
* `evaluate` binds a term meta to the answer of firing the formation another
  meta holds, through evaluation, 𝔼, as in `𝑛2: 𝑛1`.
* `defer` binds a term meta to a fresh symbol when another meta holds a copy
  of an object whose argument is a bare symbol, `⟦ λ ⤍ 𝜎1 ⟧`, and to that
  term as it is otherwise.
* `stay` binds a term meta to the term another meta holds, with every
  `( ρ ↦ … )` applied to a dispatch dropped.
* `symbolize` binds a term meta to the term another meta holds, with every
  datum turned into a fresh symbol.
* `rewrite` binds a term meta to the term the meta under `of` holds, with the
  rules under `rules` applied and nothing normalized afterwards.
  The rules are written as in a [rule file](#rule-structure).
* `cast` holds exactly two lines and binds their two metas as one pair.
  When one term is a bare symbol, `⟦ λ ⤍ 𝜎1 ⟧`, and the φ chain of the other
  ends in the only symbol it holds, the bare symbol becomes a copy of the other
  term with `𝜎1` at the end of that chain.
* `𝑛` is the term the entry answers with.
  A bare `𝜎` in it stands for a fresh symbol, and no `𝛿` may appear in it.
* `join` answers with one term made of two sides, in place of `𝑛`.
  It holds `on`, the condition, which is a bytes meta of `dataize` or a path
  from `$`.
  It holds `left` and `right`, the two sides, each a path from `$` or a term
  meta.
  It may hold `otherwise`, a λ name that joins two formations of different
  shapes one attribute at a time, and the file must answer that name too.

An entry holds either `𝑛` or `join`, never both.
The lines of all blocks run in the order of the metas they read, not in the
order of the blocks.
A line that reads a meta nothing binds refuses the file.

### Partial evaluation

A λ name that no entry answers stops the run with an error.
With `--partial`, the run prints the residual program instead: what the known
inputs decide is computed, and the rest stays in place:

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

Here `2.times( 3 ).plus( 4 )` was computed, and the symbol it came to sits in
the hidden `ρ`, while `as-bool` stays where it was.
An operand that reaches the terminator `⊥` is left in place the same way.

### Reducing a term inside a universe

The `--inside` option reduces a term the program does not hold, taking the
program as the universe Φ.
It cannot be combined with `--locator`:

```bash
$ cat universe.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(ρ, x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧
⟧
$ phino dataize --symbolic=atoms.yaml --inside='5.plus( 6 )' universe.phi
40-45-00-00-00-00-00-00
```

### Limits

Three options stop a run that goes on for too long:

* `--max-steps` bounds how deep morphing and dataization may nest,
  `1000` by default;
* `--max-firings` bounds how many λ functions the whole run may fire;
* `--max-seconds` bounds how many seconds the whole run may take.

`--partial` leaves a site in place when it runs out of steps or firings, but
a run out of time always fails.

## Morph

Morphing, 𝕄, evaluates as far as the object model allows without asking
for data, and stops at the first formation it reaches:

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
⟦ x ↦ 7.0, λ ⤍ L_number_plus ⟧
```

The inner `5.plus( 6 )` fires, because `.plus` is dispatched on its result,
while the outer call is left for dataization.
Where no formation is reachable, `morph` prints `⊥`.
It fails on a term that is not a normal form.
It takes every option of `dataize`.

### Acyclic morphing

`phino` stops no recursion by itself, so a run that never ends fails once
`--max-steps` runs out:

```bash
$ cat loop.yaml
- λ: L_loop
  𝑛: ⟦ λ ⤍ L_loop ⟧
$ cat loop.phi
⟦ x ↦ ⟦ λ ⤍ L_loop ⟧.foo ⟧
$ phino morph --symbolic=loop.yaml --locator='Q.x' --max-steps=40 loop.phi
[ERROR]: Morphing did not finish before reaching the limit of steps: --max-steps=40
```

The `--acyclic=<mode>` option makes the run notice a loop instead.
When a reduction is about to enter a formation it is already inside, the run
stops there and leaves the term in place, as `--partial` does.
The mode says when two formations are the same:

* `proven` means equal up to a renaming of symbols;
* `plausible` also means that the later one holds the earlier one under
  wrappers it gained, such as a growing accumulator, so it may cut a
  recursion that would have stopped.

Under `plausible`, a formation is fired only once, and a later firing of it
takes the answer of the first one.
The `--max-loops=<n>` option lets a reduction enter such a formation `n` more
times before it is cut.
`dataize` takes `--acyclic` too, and fails on a cut unless `--partial` is
given, since a cut term holds no data to print.

## Protocol

The `--protocol` option writes every λ function the run fired into a file,
as a tree.
A file whose name ends in `.xml` gets the tree as XML, for a program to read,
and any other name gets indented text.
Here, `L_down` answers with a call of its own object over a fresh symbol, so
without `--acyclic` the run would go on until `--max-steps` runs out:

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
  𝑛·0·2 := ⟦ x ↦ 𝜎1:λ, λ ⤍ L_down ⟧
  𝔼(L_down):
    𝔻(ξ.x):
      𝛿·1·1 := 𝔻(𝜎1:λ)
    𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.x)
    𝑛·1·1 := Φ.down( x ↦ 𝜎2:λ )  # 𝑛
    𝕄(𝑛·1·1):
      𝑛·1·2 := Φ.down( x ↦ 𝜎2:λ )
      𝑛·1·3 := ⟦ x ↦ 𝜎2:λ, λ ⤍ L_down ⟧
      morphed 𝑛·1·4 := 𝑛·1·3
    𝑛·1·5 := 𝑛·1·3  # 𝕄(𝑛·1·1)
  looped 𝑛·0·2  # proven
```

In a name such as `𝑛·1·2`, the first number counts the firings, `0` outside
every firing, and the second counts the names within that firing.
The name `𝛿1·1` is the meta `𝛿1` of the first firing.
A protocol may hold these lines:

* `# …` lines open the file with the phino that wrote it, the command, the
  world, the λ functions, the locator, and the expression it points at.
* `𝔻(…):` and `𝕄(…):` open a block where dataization or morphing starts at a
  new site, and what it does stands under it.
* `𝔼(…):` opens one firing, named by its λ function.
* `𝑛·0·1 := …` names an application, and the line after it names the
  object the application made.
* `𝛿1·1 := …` binds a meta of the entry, and the comment names the judgment
  and the term it came from.
* `𝑛·1·1 := …  # 𝑛` is the term the entry answered with, and the line
  commented with `𝕄(𝑛·1·1)` is its normal form.
* `boxed …` is dataization stepping into the `φ` of a formation.
* `undressed …` is an `undress` line taking the `φ` of a formation.
* `𝛿·1·1 := …` closes a `𝔻` block with the data it found, or with
  `𝔻(𝜎1:λ)` where it came down to a symbol.
* `morphed …` closes a `𝕄` block with the term it came to.
* `joined …`, `fabricated …` and `diverged …` are what a `join` made of its
  two sides, or why it could not join them.
* `deferred …` and `symbolized …` are the symbols a `defer` or a `symbolize`
  line minted.
* `cast …` binds a meta to the copy a `cast` block made of a bare symbol, and
  the comment names the meta it read and the meta whose shape it took.
* `unanswered …` is a λ name no entry answers.
* `stuck …` closes a firing that got stuck, and `stall …` stands under one
  that `--acyclic=plausible` answered with an earlier stall.
* `looped …` is where `--acyclic` cut a loop.
* `starved …`, `spent …` and `timeout …` are where `--max-steps`,
  `--max-firings` and `--max-seconds` ran out.
* `msec …`, `firings …` and `fps …` close the file, and the listing above
  leaves them and the `# …` lines out, since they differ from run to run.

The `--abridged` option folds every formation longer than sixty-four
characters, or than `--abridged=WIDTH`, into its `φ`, `Δ` and `λ` and a count
of the rest.
The `--abridged-data` option also cuts every byte string longer than eight
bytes down to its first two bytes and its last two.

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
to normalize expressions, alone or together with your own rules:

```bash
phino rewrite --normalize --rule=my-rule.yaml hello.phi
```

Some rules mint fresh names with the `random-string` function, and `--seed`
picks the seed of its random generator, `0` by default.

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

The two styles may be mixed in one expression, unless `--monoform` is given.

A formation of a single binding may be written as its asset, a colon,
and the attribute the asset is bound to:

```text
⟦ Δ ⤍ FF-AA ⟧    = FF-AA:Δ    = FF-AA:D
⟦ λ ⤍ 𝜎1 ⟧       = 𝜎1:λ       = !S1:L
⟦ a ↦ ∅ ⟧        = ∅:a        = ?:a
⟦ φ ↦ ξ.a ⟧      = ξ.a:φ      = $.a:@
```

The colon binds as tightly as a dot, so `ξ.a:φ.b` is `⟦ φ ↦ ξ.a ⟧.b`.
With `--sweet`, `phino` prints every such formation this way.

A formation has a receiver `ρ` only when it declares one among its voids, the
way EO declares `^`, as in `⟦ ρ ↦ ∅, t ↦ ξ.ρ.k ⟧`.
`phino` adds none of its own, so `ξ.ρ` in a formation that declares none
is `⊥`.

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
step.
The `compile` command turns the built-in rules of normalization,
contextualization, morphing and dataization, and every `--rule` file, into
the Haskell module `compiled/generated/Compiled.hs`, and a second build links
it in:

```bash
phino compile
cabal build all
```

The build links the module in only when the Cabal flag `compiled` is on.
If there is no `cabal.project.local`, `compile` creates one that turns the
flag on, and otherwise it prints the lines to add to it.
The compiled rules take exactly the same steps as the YAML ones.
`compile` refuses a rule it cannot turn into Haskell and names the reason.
A binary built this way refuses to run once the built-in rules changed after
the last `compile`.
To test the whole suite against the compiled rules, run `make compiled`.

## Explain

You can _explain_ the built-in rules by printing them in [LaTeX][latex]
format. Pass exactly one of `--normalize`, `--morph`, `--dataize` or
`--contextualize` for the rewriting, morphing (𝕄), dataization (𝔻) or
contextualization (𝒞) rules (or `--rule` for a custom rule file):

```bash
phino explain --normalize
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
                [Symbolic λ functions](#symbolic-λ-functions)). It is spelled
                the way a meta variable is spelled but is a name and no
                capture: `𝜎1` is one concrete symbol, which no substitution
                ever binds, and a bare `𝜎` in the answer of a `--symbolic`
                entry asks for a fresh one

A meta variable carries a suffix, like `!B1` or `𝜏2`, to name what it
captured, so that the `result`, `when`, `where` and `having` of a rule can
read it back.
A suffix starts with one, so `!B0` is refused.
Without a suffix, a meta variable is anonymous: it matches any term on its
own, binds no name, and is refused anywhere but in a pattern, so
`⟦ 𝜏 ↦ 𝑒, 𝜏 ↦ 𝑒 ⟧` matches any two attributes.

A positional (α) application argument is written as `α0`, `~0` (ASCII), or
`α𝑖`/`~!i` when its index is captured by an `!i`/`𝑖` meta variable.

## Benchmark

To run performance benchmarks, you need [Java 8+][java] and [curl][curl]:

```bash
make bench
```

On the first run, `make bench` downloads the compiled [`Native`][jna-native]
class of [JNA][jna], turns it into [XMIR][xmir] with
[jeo-maven-plugin][jeo] and then into 𝜑, and caches the results in
`benchmark/tmp/`.
Besides parsing, printing and rewriting that class, the suite morphs the
entries of `benchmark/demo.phi` symbolically, one case per entry.

<!-- benchmark_begin -->

```text
=== parse/phi ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      1409532.338 μs
  avg:        140953.234 μs
  min:        123104.580 μs
  max:        169752.210 μs
  std dev:    19431.803 μs
=== parse/xmir ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      6269190.661 μs
  avg:        626919.066 μs
  min:        570410.014 μs
  max:        708906.404 μs
  std dev:    53168.480 μs
=== rewrite/normalize ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      10.204 μs
  avg:        1.020 μs
  min:        0.842 μs
  max:        1.442 μs
  std dev:    0.179 μs
=== print/sweet/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      3999871.155 μs
  avg:        399987.116 μs
  min:        357813.221 μs
  max:        448393.174 μs
  std dev:    27725.936 μs
=== print/sweet/flat ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      455172.450 μs
  avg:        45517.245 μs
  min:        40716.412 μs
  max:        55514.792 μs
  std dev:    6068.706 μs
=== print/salty/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      11916902.966 μs
  avg:        1191690.297 μs
  min:        1124787.007 μs
  max:        1324046.797 μs
  std dev:    62960.748 μs
=== morph/symbolic/demo/e1 ===
  warmup:     3 iterations
  batches:    10 x 15
  total:      8063.096 μs
  avg:        53.754 μs
  min:        52.409 μs
  max:        56.184 μs
  std dev:    0.955 μs
=== morph/symbolic/demo/e2 ===
  warmup:     3 iterations
  batches:    10 x 294
  total:      163954.216 μs
  avg:        55.767 μs
  min:        54.415 μs
  max:        56.517 μs
  std dev:    0.716 μs
=== morph/symbolic/demo/e3 ===
  warmup:     3 iterations
  batches:    10 x 277
  total:      161047.794 μs
  avg:        58.140 μs
  min:        57.025 μs
  max:        58.757 μs
  std dev:    0.553 μs
=== morph/symbolic/demo/e4 ===
  warmup:     3 iterations
  batches:    10 x 271
  total:      156357.626 μs
  avg:        57.697 μs
  min:        57.150 μs
  max:        58.248 μs
  std dev:    0.363 μs
=== morph/symbolic/demo/e5 ===
  warmup:     3 iterations
  batches:    10 x 260
  total:      160590.902 μs
  avg:        61.766 μs
  min:        59.089 μs
  max:        79.533 μs
  std dev:    5.932 μs
=== morph/symbolic/native/e5 ===
  warmup:     3 iterations
  batches:    10 x 5
  total:      35947.906 μs
  avg:        718.958 μs
  min:        682.191 μs
  max:        764.860 μs
  std dev:    22.764 μs
=== morph/symbolic/accum/0 ===
  warmup:     3 iterations
  batches:    10 x 142
  total:      68966.685 μs
  avg:        48.568 μs
  min:        47.428 μs
  max:        49.936 μs
  std dev:    0.778 μs
=== morph/symbolic/accum/400 ===
  warmup:     3 iterations
  batches:    10 x 112
  total:      63083.756 μs
  avg:        56.325 μs
  min:        55.561 μs
  max:        57.281 μs
  std dev:    0.537 μs
```

The results were calculated in [this GHA job][benchmark-gha]
on 2026-10-26 at 12:05,
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
[benchmark-gha]: https://github.com/objectionary/phino/actions/runs/40319368788
