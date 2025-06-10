# Command-Line Manipulator of 𝜑-Calculus Expressions

[![DevOps By Rultor.com](https://www.rultor.com/b/objectionary/phino)](https://www.rultor.com/p/objectionary/phino)

[![`phino` on Hackage](https://img.shields.io/hackage/v/phino)](http://hackage.haskell.org/package/phino)
[![cabal-linux](https://github.com/objectionary/phino/actions/workflows/cabal.yml/badge.svg)](https://github.com/objectionary/phino/actions/workflows/cabal.yml)
[![stack-linux](https://github.com/objectionary/phino/actions/workflows/stack.yml/badge.svg)](https://github.com/objectionary/phino/actions/workflows/stack.yml)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSES/MIT.txt)
[![Hits-of-Code](https://hitsofcode.com/github/objectionary/phino?branch=master&label=Hits-of-Code)](https://hitsofcode.com/github/objectionary/phino/view?branch=master&label=Hits-of-Code)
[![PDD status](https://www.0pdd.com/svg?name=objectionary/phino)](https://www.0pdd.com/p?name=objectionary/phino)

This is a command-line normalizer, rewriter, and dataizer
of [𝜑-calculus](https://www.eolang.org) expressions.

Install [Cabal][cabal] first and then:

```bash
cabal update
cabal install phino
phino --version
```

Then, you write a simple [𝜑-calculus](https://www.eolang.org) expression
in the `hello.phi` file:

```text
Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧, t ↦ ξ.k, k ↦ ⟦⟧ ⟧
```

Then, you dataize it (**under development**):

```bash
$ phino dataize hello.phi
"hello"
```

You can rewrite this expression (**under development**) with the help of rules
defined in the `my-rule.yml` YAML file (here, the `!b` is a capturing group,
similar to regular expressions):

```yaml
name: My custom rule
pattern: Δ ⤍ !b
result: Δ ⤍ 62-79-65
```

Then, rewrite:

```bash
$ phino rewrite --rule=my-rule.yml --phi-input=hello.phi
Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 62-79-65 ⟧, t ↦ ξ.k, k ↦ ⟦⟧ ⟧
```

If you want to use many rules, just use `--rule` as many times as you need:

```bash
phino rewrite --rule=rule1.yaml --rule=rule2.yaml ...
```

If `--phi-input` is not provided, the 𝜑-expression is taken from `stdin`:

```bash
$ echo 'Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧ ⟧' | phino rewrite --rule=my-rule.yml
Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 62-79-65 ⟧ ⟧
```

You can also use [built-in rules](resources/normalize.yaml), which are designed
to normalize expressions (**under development**):

```bash
$ phino rewrite --normalize --phi-input=hello.phi
Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧, t ↦ ⟦⟧, k ↦ ⟦⟧ ⟧
```

Also `phino` supports 𝜑-expressions in
[ASCII](https://en.wikipedia.org/wiki/ASCII) format and with
syntax sugar. The `rewrite` command also allows you to desugar the expression
and print it in canonical syntax:

```bash
$ echo 'Q -> [[ @ -> QQ.io.stdout("hello") ]]' | phino rewrite --nothing
Φ ↦ ⟦
  φ ↦ Φ.org.eolang.io.stdout(
    α0 ↦ Φ.org.eolang.string(
      α0 ↦ Φ.org.eolang.bytes(
        α0 ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧
      )
    )
  )
⟧
```

## Rule structure

This is BNF-like yaml rule structure:

```bnfc
Rule:
  name: String?
  pattern: String
  result: String
  when: Condition?

Condition:
  = and: [Condition]     # logical AND
  | or:  [Condition]     # logical OR
  | not: Condition       # logical NOT
  | alpha: Attribute     # check if given attribute is alpha
  | eq:                  # compare two comparable objects
      - Comparable
      - Comparable
  | in:                  # check if attributes exist in bindings
      - Attribute
      - Binding

Comparable:              # comparable object that may be used in 'eq' condition
  = Attribute
  | Number

Number:                  # comparable number
  = Integer              # just regular integer
  | ordinal: Attribute   # calculate index of alpha attribute
  | length: Meta Binding # calculate length of bindings by given meta binding
  | plus:                # calculate sum of 2 given comparable numbers
      - Number
      - Number
```

Check [this](resources) to find pre defined normalization rules.

## Meta variables

The `phino` supports meta variables to write 𝜑-expression patterns for
capturing attributes, bindings, etc.

This is the list of supported meta variables:

* `!a` || `𝜏` - attribute
* `!e` || `𝑒` - any expression
* `!B` || `𝐵` - list of bindings
* `!t` - tail after expression, sequence of applications and/or dispatches
* `!b` - bytes in meta delta binding
* `!F` - function name in meta lambda binding

Every meta variable may also be used with an integer index, like `!B1` or `𝜏0`.

Incorrect usage of meta variables in 𝜑-expression patterns leads to
parsing errors.

## How to Contribute

Fork repository, make changes, then send us a [pull request][guidelines].
We will review your changes and apply them to the `master` branch shortly,
provided they don't violate our quality standards. To avoid frustration,
before sending us your pull request please make sure all your tests pass:

```bash
cabal build all
cabal test
```

You will need [GHC] and [Cabal ≥3.0][cabal] or [Stack ≥ 3.0][stack] installed.

[cabal]: https://www.haskell.org/cabal/
[stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[GHC]: https://www.haskell.org/ghc/
[guidelines]: https://www.yegor256.com/2014/04/15/github-guidelines.html
