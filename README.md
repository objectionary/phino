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
cabal install --overwrite-policy=always phino-0.0.115
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
registry given with `--atoms`, keyed by λ name:

```json
{
  "L_number_plus": {
    "rt": "node",
    "script": "const fs = require('fs'); ..."
  }
}
```

The `rt` field names the executable the `script` is run under. Only `node` is
supported for now; a registry naming any other runtime is refused when the file
is read, before dataization starts.

When 𝔼 reaches a λ function the registry carries, `phino` writes its `script`
to a temporary file and runs it as a POSIX process under that interpreter, with
the λ name as the first command-line argument:

```text
node /tmp/phino-atom-4f2a.js L_number_plus
```

The name matters: one script may be registered under several λ names and branch
on it, which is where `node` puts it — `process.argv[2]`. The script is then
fed one JSON object on `stdin`:

```json
{
  "b": "⟦ x ↦ Φ.number( as-bytes ↦ … ), ρ ↦ ⟦ … ⟧ ⟧",
  "s": "⟦ bytes ↦ ⟦ … ⟧, number ↦ ⟦ … ⟧, φ ↦ … ⟧"
}
```

Here `b` is the formation being evaluated, with its λ binding removed so that
the script may dispatch on it, and `s` is the universe Φ. Both are canonical
𝜑-calculus on a single line — no syntax sugar, whatever `--sweet` says about
the output of the run — so a script never has to know about `phino`'s sugar in
order to find a datum: every byte array is spelled out as a Δ binding.

The script writes one JSON object to `stdout`:

```json
{ "n": "11" }
```

The `n` field is the 𝜑-expression the atom answers with, in any syntax
`phino`'s parser reads — syntax sugar included, so the `11` above and the
`Φ.number( … )` it stands for are the same answer. `phino` parses it back
and hands it to 𝔼 as the atom's raw result, normalizing it exactly as it
normalizes anything else, so `--evaluations`, `--partial` and `--max-steps`
keep working unchanged. A non-zero exit, output that is not JSON, a missing
`n` or an `n` that does not parse fails the run, with the script's own
`stderr` in the message.

A λ name the registry does not carry has no λ function at all, so 𝔼 gets stuck
on it. Without `--atoms` the registry is empty and every atom gets stuck.

### Reducing the operands of an atom

A script gets at the parts of `b` by calling `phino` again, so no API has to be
exposed for it. The `--inside` option is how it asks: the expression it names
is bound to a fresh synthetic attribute of the input expression, which the run
takes as the universe, normalized there, and then dataized. This is the same
trick `phino` plays internally whenever it has to reduce a sub-expression the
program does not contain:

```bash
$ phino dataize --atoms=atoms.json --inside='5.plus( 6 )' universe.phi
40-26-00-00-00-00-00-00
```

Here `universe.phi` is the 𝜑-program the atom is being fired inside — the very
text the script was handed as `s`, which it feeds back on `stdin`.

So a `L_number_plus` that reduces its own operands reads like this:

```js
const fs = require('fs');
const { execFileSync } = require('child_process');
const atom = process.argv[2];
if (atom !== 'L_number_plus') {
  throw new Error(`unsupported atom ${atom}`);
}
const { b, s } = JSON.parse(fs.readFileSync(0, 'utf8'));
const dataized = (expr) => execFileSync(
  'phino',
  ['dataize', '--atoms=atoms.json', `--inside=${expr}`],
  { input: s, encoding: 'utf8' }
).trim();
const number = (expr) => Buffer.from(dataized(expr).replace(/-/g, ''), 'hex').readDoubleBE(0);
const sum = Buffer.alloc(8);
sum.writeDoubleBE(number(`${b}.ρ`) + number(`${b}.x`));
const hex = [...sum]
  .map((octet) => octet.toString(16).toUpperCase().padStart(2, '0'))
  .join('-');
process.stdout.write(JSON.stringify({
  n: `Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ ${hex} ⟧ ) )`
}));
```

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
  bytes(data) ↦ ⟦ φ ↦ data ⟧,
  number(as-bytes) ↦ ⟦ φ ↦ as-bytes, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
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
  bytes(data) ↦ ⟦ φ ↦ data ⟧,
  number(as-bytes) ↦ ⟦
    φ ↦ as-bytes,
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
  bytes(data) ↦ ⟦ φ ↦ data ⟧,
  number(as-bytes) ↦ ⟦ φ ↦ as-bytes, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
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
    α0 ↦ Φ.string(
      α0 ↦ Φ.bytes(
        α0 ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧
      )
    )
  )
⟧
```

## Merge

You can merge several 𝜑-expressions into a single one by merging their
top level formations:

```bash
$ cat bytes.phi
⟦ bytes(data) ↦ ⟦ φ ↦ data ⟧ ⟧
$ cat number.phi
⟦
  number(as-bytes) ↦ ⟦
    φ ↦ as-bytes,
    plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧
  ⟧
⟧
$ cat minus.phi
⟦ number ↦ ⟦ minus(x) ↦ ⟦ λ ⤍ L_number_minus ⟧ ⟧ ⟧
$ phino merge bytes.phi number.phi minus.phi --sweet
⟦
  bytes(data) ↦ ⟦ φ ↦ data ⟧,
  number(as-bytes) ↦ ⟦
    φ ↦ as-bytes,
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

Every meta variable may also be used with an integer index, like `!B1` or `𝜏0`.

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
  total:      1781621.932 μs
  avg:        178162.193 μs
  min:        163679.993 μs
  max:        209804.570 μs
  std dev:    17409.000 μs
=== parse/xmir ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      7611171.011 μs
  avg:        761117.101 μs
  min:        679176.096 μs
  max:        899930.605 μs
  std dev:    69464.089 μs
=== rewrite/normalize ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      811837.328 μs
  avg:        81183.733 μs
  min:        67331.161 μs
  max:        92232.373 μs
  std dev:    8117.233 μs
=== print/sweet/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      4199718.146 μs
  avg:        419971.815 μs
  min:        396063.240 μs
  max:        442595.822 μs
  std dev:    16504.492 μs
=== print/sweet/flat ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      4060839.345 μs
  avg:        406083.934 μs
  min:        387257.807 μs
  max:        417907.724 μs
  std dev:    8861.891 μs
=== print/salty/multiline ===
  warmup:     3 iterations
  batches:    10 x 1
  total:      14257603.693 μs
  avg:        1425760.369 μs
  min:        1405945.748 μs
  max:        1449825.539 μs
  std dev:    11882.320 μs
```

The results were calculated in [this GHA job][benchmark-gha]
on 2026-09-07 at 19:51,
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
[benchmark-gha]: https://github.com/objectionary/phino/actions/runs/34156988456
