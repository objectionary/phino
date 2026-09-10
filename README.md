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
cabal install --overwrite-policy=always phino-0.0.127
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

### Atoms

Which λ functions exist is a property of the object model being dataized, not
of the calculus, so `phino` implements none of them. They come from a JSON
registry given with `--atoms`, keyed by regular expressions over λ names:

```json
{
  "L_number_plus": {
    "rt": "node",
    "script": "const readline = require('readline'); ..."
  }
}
```

The `rt` field names the interpreter the `script` is run under. Only `node` is
supported for now; a registry naming any other interpreter is refused when the
file is read, before dataization starts.

When 𝔼 reaches a λ function the registry carries, `phino` writes its `script`
to a temporary file and runs it as a POSIX process under that interpreter:

```text
node /tmp/phino-atom-4f2a.js
```

An atom that is already a program needs no interpreter and no staging. Such an
entry says `exec` and gives a `path` instead of a `script`:

```json
{
  "L_number_plus": {
    "rt": "exec",
    "path": "/opt/eo/atoms/number-plus"
  }
}
```

`phino` spawns that file directly, as the executable binary it is, with no
arguments. A `path` that names no file, or a file nobody may run, is refused
where the registry is read, together with the unknown runtimes.

Whichever way it is run, the program is talked to over `stdin` and `stdout`,
one JSON object per line, in the letters of the evaluation rule of the
[𝜑-calculus paper](https://github.com/objectionary/calculus-paper),
𝔼(𝑏, 𝑒, 𝑠) = 𝑛, where 𝑏 is the formation, 𝑒 the universe and 𝑛 the normal
form the atom answers with:

```text
{"𝑒": "⟦ bytes ↦ ⟦ … ⟧, number ↦ ⟦ … ⟧, φ ↦ … ⟧"}
{"id": 1, "λ": "L_number_plus", "𝑏": "⟦ x ↦ Φ.number( … ), ρ ↦ ⟦ … ⟧ ⟧"}
{"id": 1, "𝑛": "11"}
```

The first two lines are `phino`'s, the third is the program's. The universe Φ
goes under `𝑒`, in a line of its own, before the first request. Then comes the
request: an `id`, the λ name under `λ` — one program may be registered under
several names and branch on it — and, under `𝑏`, the formation being
evaluated, with its λ binding removed. Both payloads are canonical 𝜑-calculus
on a single line — no syntax sugar, whatever `--sweet` says about the output of
the run — so a program never has to know about `phino`'s sugar in order to find
a datum: every byte array is spelled out as a Δ binding.

The program answers with one line carrying the same `id` and, under `𝑛`, the
𝜑-expression the atom answers with, in any syntax `phino`'s parser reads —
syntax sugar included, so the `11` above and the `Φ.number( … )` it stands for
are the same answer. `phino` parses it back and hands it to 𝔼 as the atom's
raw result, normalizing it exactly as it normalizes anything else, so
`--evaluations`, `--partial` and `--max-steps` keep working unchanged.

A program started for the fire is asked one request, always `id` 1, and its
`stdin` is closed behind it, so it may read its input whole or line by line, as
it pleases. It is waited for once it has answered, and a non-zero exit fails
the run. So does a reply that is not JSON, carries neither `𝑛`, nor `ask`, nor
`of` with `attr` (the next section is about the questions), answers another
`id`, or an `𝑛` that does not parse, or a program that quits without answering
— always with the program's own `stderr` in the message.

Each key of the registry is a regular expression, and it must match the whole
λ name, so a plain name such as `L_number_plus` means that one atom and nothing
else, while `L_number_.*` stands for every atom of `number`. When 𝔼 reaches a
λ function, the keys are tried top to bottom, in the order the file lists them,
and the first one that matches is the entry fired, so a key placed above
another hides whatever the two have in common. A key that is not a regular
expression is refused where the registry is read.

A λ name no key matches has no λ function at all, so 𝔼 gets stuck on it.
Without `--atoms` the registry is empty and every atom gets stuck.

One process per fire is where a program that is slow to start — a JVM, say —
spends most of the run. An entry saying `serve` has `phino` start its program
once, on the first fire, and keep it for the rest of the run, whether it is a
`script` or a `path`. Together with a key that matches many names, this is how
one program stands for a whole object model without being spelled once per
atom:

```json
{
  "L_bytes_eq": {
    "rt": "node",
    "script": "const readline = require('readline'); ..."
  },
  ".*": {
    "rt": "exec",
    "path": "/opt/eo/atoms/resident",
    "serve": true
  }
}
```

Every λ name registered on the same program, under one key or under several,
is served by the same process, so there is one of it, however many atoms it
stands for. The lines are the same:
the program reads request after request off its `stdin`, each with the next
`id`, and answers each in turn. The universe is told again only when a fire
comes with a different one; the program keeps the last one it was told. When
the run is over, whatever it ended with, `phino` closes the program's `stdin`,
which is its cue to quit, and terminates it if it has not quit within a second.

### Reducing the operands of an atom

An operand reaches a program as it was written: `5.plus( 6.plus( 7 ) )` fires
`L_number_plus` with `x ↦ Φ.number( … ).plus( … )`, and getting a number out of
that is dataization, which is `phino`'s business and not a program's. So the
program asks, and it may ask by name. A line of its own carries an `id` it
mints and the `of` of the request being served, plus one of that receiver's
attributes under `attr`; `phino` answers with that `id` and the result under
`𝑛`, taking the value straight out of the receiver it still holds for the
request — neither side ever re-prints or re-parses it:

```text
{"𝑒": "⟦ bytes ↦ ⟦ … ⟧, number ↦ ⟦ … ⟧, φ ↦ … ⟧"}
{"id": 1, "λ": "L_number_plus", "𝑏": "⟦ x ↦ Φ.number( … ).plus( … ) ⟧"}
{"id": 7, "of": 1, "attr": "ρ", "reduce": true}
{"id": 7, "𝑛": "⟦ Δ ⤍ 40-14-00-00-00-00-00-00 ⟧"}
{"id": 8, "of": 1, "attr": "x", "reduce": true}
{"id": 8, "𝑛": "⟦ Δ ⤍ 40-2A-00-00-00-00-00-00 ⟧"}
{"id": 1, "𝑛": "Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-32-00-00-00-00-00-00 ⟧ ) )"}
```

The universe, the request and the two answers are `phino`'s; the two questions
and the last line are the program's. A question mints an `id` of its own,
which `phino` echoes, so a program may keep several of them open and still
tell the answers apart. Without `reduce` — or with it saying `false` — the
answer is the node the attribute carries, as it was written; with `"reduce":
true` it is the dataization of that node. A question about an `of` whose
request is no longer in flight, or an `attr` the receiver does not carry,
fails the fire.

The other way to ask quotes the 𝜑-expression itself, under `ask`; `phino`
serves such a question by binding it to a fresh synthetic attribute of the
universe, normalizing it there and dataizing it — the same trick `--inside`
plays — so the answer is a byte formation and the program reads its `Δ`; where
an atom on the way cannot fire and `--partial` parks it, the answer is the
residual program instead. A quoted question is fine for terms the program
assembled itself; a question that quotes a receiver is not, because the
receiver carries its `ρ` and the receiver of that carries its own, all the way
to the universe: three levels of nesting turn a question of a few hundred
bytes into one of megabytes. A program kept for the run therefore gets a lean
`𝑏`, and every answer `phino` sends it is lean too: canonical 𝜑-calculus
without any ρ chain, because such a program can always ask for what the chain
holds — by name, cheaply, or by `ask`.

Serving a question re-enters the evaluator, so a question may cost a fire of
the very atom that asked it. That request arrives while the question is still
open, which is why a program that asks reads on instead of waiting for one
line. The step budget of the run, `--max-steps`, bounds the nesting.

Only a program kept for the run may ask. `phino` closes the `stdin` of a
program started for the fire behind its request, since such a program may read
its input whole before it answers, so there is nothing left to answer a
question over, and one that asks anyway fails the fire — which is also why the
lean `𝑏` is tied to `serve` and not to a flag of its own: a program that is
handed the whole receiver cannot ask for what it left out.

So a `serve` entry of `L_number_plus` that has `phino` reduce its operands
reads like this:

```js
const readline = require('readline');
const open = new Map();
let minted = 0;
const said = (message) => process.stdout.write(`${JSON.stringify(message)}\n`);
const number = (answer) => Buffer
  .from(/Δ ⤍ ([0-9A-F-]+)/.exec(answer)[1].replace(/-/g, ''), 'hex')
  .readDoubleBE(0);
const hex = (value) => {
  const bytes = Buffer.alloc(8);
  bytes.writeDoubleBE(value);
  return [...bytes]
    .map((octet) => octet.toString(16).toUpperCase().padStart(2, '0'))
    .join('-');
};
function* plus(request) {
  const rho = number(yield {of: request, attr: 'ρ', reduce: true});
  const x = number(yield {of: request, attr: 'x', reduce: true});
  return `Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ ${hex(rho + x)} ⟧ ) )`;
}
const advance = (atom, id, answer) => {
  const step = atom.next(answer);
  if (step.done) {
    said({ id, '𝑛': step.value });
    return;
  }
  minted += 1;
  open.set(minted, { atom, id });
  said({ id: minted, ...step.value });
};
readline.createInterface({ input: process.stdin }).on('line', (line) => {
  const message = JSON.parse(line);
  if ('λ' in message) {
    advance(plus(message.id), message.id, undefined);
  } else if ('𝑛' in message) {
    const waiting = open.get(message.id);
    open.delete(message.id);
    advance(waiting.atom, waiting.id, message['𝑛']);
  }
});
```

Every request is a coroutine there, so a question suspends the request that
asked it rather than the program: whatever `phino` says next, the answer or
another request, is served on the spot.

A program may run a `phino` of its own instead of asking, and the `--inside`
option is how it does that: the expression it names is bound to a fresh
synthetic attribute of the input expression, which the run takes as the
universe, normalized there, and then dataized.

```bash
$ phino dataize --atoms=atoms.json --inside='5.plus( 6 )' universe.phi
40-26-00-00-00-00-00-00
```

Here `universe.phi` is the 𝜑-program the atom is being fired inside — the very
text the program was told under `𝑒`, which it feeds back on `stdin`. That costs
a process and a re-parse of the whole universe per operand, which is what the
`ask` line is for.

The `--inside` option cannot be combined with `--locator`, since it aims the
run at the binding it mints itself. Both `dataize` and `morph` take `--atoms`
and `--inside`.

### Recording what fired

Every atom fired on the way to the bytes may be recorded in a machine-readable
protocol, with the `--evaluations` option. One firing is one line of three
tab-separated fields: the name of the λ function, the formation it was applied
to, and the expression it returned:

```bash
$ cat sum.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  φ ↦ 5.plus( 6 )
⟧
$ phino dataize --atoms=atoms.json --evaluations=atoms.tsv --quiet \
    --sweet --hide-rho sum.phi
$ cat -T atoms.tsv
L_number_plus^I⟦ x ↦ 6 ⟧^I11
```

Records follow the syntax of the other options, such as `--sweet` and
`--hide-rho`, but always stay on one line. The file is truncated at the
beginning of every run, and `--output=phi` is the only output format it
works with, since one record must fit into one line.

### Partial evaluation

An atom that cannot fire fails the run: its λ function is not in the registry
given with `--atoms`. This is what happens when an operation is deliberately
left unimplemented — a data input replaced by a placeholder formation such as
`⟦ λ ⤍ Sym_arg_0 ⟧`, or an operation whose answer is not known yet. With
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
$ phino dataize --atoms=atoms.json --partial --sweet --hide-rho partial.phi
⟦ λ ⤍ L_number_as_bool ⟧
```

Here `2.times( 3 ).plus( 4 )` was decided by the atoms the registry carries, so
it was computed (its result, `10`, sits in the hidden `ρ` of the residual
program), while `as-bool` names a λ function no script answers for, so it stays
in place as a normal-form subterm. Each such stuck site also lands in the
`--evaluations` file, as a record with the first two fields only, since there is
no result to report:

```bash
$ phino dataize --atoms=atoms.json --partial --evaluations=atoms.tsv --quiet \
    --sweet --hide-rho partial.phi
$ cat -T atoms.tsv
L_number_times^I⟦ x ↦ 3 ⟧^I6
L_number_plus^I⟦ x ↦ 4 ⟧^I10
L_number_as_bool^I⟦⟧
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
normalization, fires whichever atoms sit under a dispatch, and stops at the
first formation it reaches, handing that formation back untouched. The
`morph` command runs 𝕄 on its own:

```bash
$ cat two.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  φ ↦ 5.plus( 6 ).plus( 7 )
⟧
$ phino dataize --atoms=atoms.json --sweet --hide-rho two.phi
40-32-00-00-00-00-00-00
$ phino morph --atoms=atoms.json --locator=Q.φ --sweet --hide-rho two.phi
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

The whole `dataize` option surface applies unchanged — `--atoms`, `--inside`,
`--sequence`, `--headers`, `--steps-dir`, `--evaluations`, `--partial`,
`--max-steps`, `--shuffle`/`--seed`, `--output`, `--focus` and the rest.

### Deep morphing

𝕄 stops at the first formation it reaches and hands its bindings back as they
were written, since firing a bare λ is dataization's job, and `dataize`
follows the one path dataization demands and ends in bytes. What a program
holds but nothing demands — the argument of an atom the registry does not
serve, for one — is therefore reduced by neither. The `--deep` flag enters it:

```bash
$ cat gap.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧ ⟧,
  bar(x) ↦ ⟦ λ ⤍ L_bar ⟧,
  demo ↦ ⟦ foo ↦ ⟦ n ↦ 3, φ ↦ Φ.bar( ξ.n.times( 5 ).times( 7 ) ) ⟧ ⟧
⟧
$ phino morph --atoms=atoms.json --inside='Q.demo.foo' \
    --sweet --hide-rho gap.phi
⟦ n ↦ 3, φ ↦ Φ.bar( n.times( 5 ).times( 7 ) ) ⟧
$ phino morph --deep --atoms=atoms.json --inside='Q.demo.foo' \
    --sweet --hide-rho gap.phi
⟦ n ↦ 3, φ ↦ Φ.bar( 105 ) ⟧
```

Every binding of the formation is entered, recursively. 𝕄 is asked about the
term standing there and, where it lands on a saturated formation whose λ the
registry serves, that λ is fired and 𝕄 is asked about the answer again. A term
on whose way an atom fired is replaced by the answer of the last firing, which
is the 𝜑-program the atom wrote rather than the normal form of it, so `105`
stands where the arithmetic stood. A term no atom touched stays exactly as it
was written and only its own parts are walked, so `Φ.bar` keeps its name and
what comes back is still the same program, reduced as far as the registry
allows. The step joins the chain under the name `deep`, so `--sequence` shows
it, and `--max-steps` bounds the walk.

Two things are left alone. A λ the registry does not serve is not fired at
all, so `--deep` stays as total as 𝕄 itself and needs no `--partial`; an atom
that gets stuck deeper on a spine still fails the run, and `--partial` parks
it, leaving that term as it was written. A formation still holding a void
binding is not fired either: the void is an argument the program has not given
yet, so `times(x) ↦ ⟦ λ ⤍ L_number_times ⟧` is a method waiting to be applied,
not an application waiting to be computed. Walking the whole program therefore
folds what it can and leaves the object model as it was declared:

```bash
$ phino morph --deep --atoms=atoms.json --sweet --hide-rho gap.phi
⟦
  bytes(φ) ↦ ⟦⟧,
  number(φ) ↦ ⟦ times(x) ↦ ⟦ λ ⤍ L_number_times ⟧ ⟧,
  bar(x) ↦ ⟦ λ ⤍ L_bar ⟧,
  demo ↦ ⟦ foo ↦ ⟦ n ↦ 3, φ ↦ Φ.bar( 105 ) ⟧ ⟧
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
  total:      1203133.969 μs
  avg:        120313.397 μs
  min:        111097.206 μs
  max:        145372.675 μs
  std dev:    13171.342 μs
=== parse/xmir ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      6197884.929 μs
  avg:        619788.493 μs
  min:        577169.173 μs
  max:        673663.732 μs
  std dev:    27593.457 μs
=== rewrite/normalize ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      634466.289 μs
  avg:        63446.629 μs
  min:        53366.681 μs
  max:        75309.285 μs
  std dev:    6242.848 μs
=== print/sweet/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      3822597.855 μs
  avg:        382259.786 μs
  min:        355383.107 μs
  max:        417501.175 μs
  std dev:    22021.732 μs
=== print/sweet/flat ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      3894710.946 μs
  avg:        389471.095 μs
  min:        365168.795 μs
  max:        413499.862 μs
  std dev:    15062.412 μs
=== print/salty/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      11731725.067 μs
  avg:        1173172.507 μs
  min:        1151496.455 μs
  max:        1195128.811 μs
  std dev:    13479.650 μs
```

The results were calculated in [this GHA job][benchmark-gha]
on 2026-09-10 at 15:55,
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
[benchmark-gha]: https://github.com/objectionary/phino/actions/runs/34498380492
