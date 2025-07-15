# Command-Line Manipulator of 𝜑-Calculus Expressions

[![DevOps By Rultor.com](https://www.rultor.com/b/objectionary/phino)](https://www.rultor.com/p/objectionary/phino)

[![`phino` on Hackage](https://img.shields.io/hackage/v/phino)](http://hackage.haskell.org/package/phino)
[![cabal-linux](https://github.com/objectionary/phino/actions/workflows/cabal.yml/badge.svg)](https://github.com/objectionary/phino/actions/workflows/cabal.yml)
[![stack-linux](https://github.com/objectionary/phino/actions/workflows/stack.yml/badge.svg)](https://github.com/objectionary/phino/actions/workflows/stack.yml)
[![codecov](https://codecov.io/gh/objectionary/phino/branch/master/graph/badge.svg)](https://codecov.io/gh/objectionary/phino)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSES/MIT.txt)
[![Hits-of-Code](https://hitsofcode.com/github/objectionary/phino?branch=master&label=Hits-of-Code)](https://hitsofcode.com/github/objectionary/phino/view?branch=master&label=Hits-of-Code)
[![PDD status](https://www.0pdd.com/svg?name=objectionary/phino)](https://www.0pdd.com/p?name=objectionary/phino)

This is a command-line normalizer, rewriter, and dataizer
of [𝜑-calculus](https://www.eolang.org) expressions.

Install [Cabal][cabal] first and then:

```bash
cabal update
cabal install --overwrite-policy=always phino-0.0.0.19
phino --version
```

Then, you write a simple [𝜑-calculus](https://www.eolang.org) program
in the `hello.phi` file:

```text
Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧, t ↦ ξ.k, k ↦ ⟦⟧ ⟧
```

## Dataize

Then, you dataize it:

```bash
$ phino dataize hello.phi
68-65-6C-6C-6F
```

## Rewrite

You can rewrite this expression with the help of rules
defined in the `my-rule.yml` YAML file (here, the `!d` is a capturing group,
similar to regular expressions):

```yaml
name: My custom rule
pattern: Δ ⤍ !d
result: Δ ⤍ 62-79-65
```

Then, rewrite:

```bash
$ phino rewrite --rule=my-rule.yml --input-file=hello.phi
Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 62-79-65 ⟧, t ↦ ξ.k, k ↦ ⟦⟧ ⟧
```

If you want to use many rules, just use `--rule` as many times as you need:

```bash
phino rewrite --rule=rule1.yaml --rule=rule2.yaml ...
```

If `--input-file` is not provided, the 𝜑-expression is taken from `stdin`:

```bash
$ echo 'Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧ ⟧' | phino rewrite --rule=my-rule.yml
Φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 62-79-65 ⟧ ⟧
```

You can also use [built-in rules](resources), which are designed
to normalize expressions:

```bash
phino rewrite --normalize --input-file=hello.phi
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

For more details, use `--help` option.

## Rule structure

This is BNF-like yaml rule structure. Here types ended with
apostrophe, like `Attribute'` are built types from 𝜑-program [AST](src/Ast.hs)

```bnfc
Rule:
  name: String?
  pattern: String
  result: String
  when: Condition?
  where: [Extension]

Condition:
  = and: [Condition]      # logical AND
  | or:  [Condition]      # logical OR
  | not: Condition        # logical NOT
  | alpha: Attribute'     # check if given attribute is alpha
  | eq:                   # compare two comparable objects
      - Comparable
      - Comparable
  | in:                   # check if attributes exist in bindings
      - Attribute'
      - Binding'
  | nf: Expression'       # returns True if given expression in normal form
                          # which means that no more other normalization rules
                          # can be applied
  | xi: Expression'       # special condition for Rcopy normalization rule to 
                          # avoid infinite recursion while the condition checking
                          # returns True if there's no ξ outside of the formation
                          # in given expression.
  | match:                # returns True if given expression after dataization
      - String            # matches to given regex
      - Expression

Comparable:               # comparable object that may be used in 'eq' condition
  = Attribute'
  | Number

Number:                   # comparable number
  = Integer               # just regular integer
  | ordinal: Attribute'   # calculate index of alpha attribute
  | length: BiMeta'       # calculate length of bindings by given meta binding

Extension:                # substitutions extension used to introduce new meta variables
  meta: [ExtArgument]     # new introduced meta variable
  function: String        # name of the function
  args: [ExtArgument]     # arguments of the function

ExtArgument
  = Bytes'                # !d
  | Binding'              # !B
  | Expression'           # !e
  | Attribute'            # !a
```

Here's list of functions that are supported for extensions:

* `contextualize` - function of two arguments, that rewrites given expression
  depending on provided context according to the contextualization
  [rules](assets/contextualize.jpg)
* `scope` - resolve the scope for given expression. Works only with meta
  expressions denotes as `𝑒` or `!e`. The scope is nearest outer formation,
  if it's present. In all other cases the default scope is used, which is
  anonymous formation `⟦ ρ ↦ ∅ ⟧`.
* `dataize` - dataizes given expression and returns bytes.
* `concat` - accepts bytes or dataizable expressions as arguments,
  concatenates them into single sequence and convert it to expression
  that can be pretty printed as human readable string:
  `Φ.org.eolang.string(Φ.org.eolang.bytes⟦ Δ ⤍ !d ⟧)`.
* `sed` - pattern replacer, works like unix `sed` function.
  Accepts two arguments: target expression and pattern.
  Pattern must start with `s/`, consists of three parts
  separated by `/`, for example, this pattern `s/\\s+//g`
  replaces all the spaces with empty string.
* `random-string` - accepts dataizable expression or bytes as pattern.
  Replaces `%x` and `%d` formatters with random hex numbers and
  decimals accordingly. Uniqueness is guaranteed during one
  execution of `phino`.

## Meta variables

The `phino` supports meta variables to write 𝜑-expression patterns for
capturing attributes, bindings, etc.

This is the list of supported meta variables:

* `!a` || `𝜏` - attribute
* `!e` || `𝑒` - any expression
* `!B` || `𝐵` - list of bindings
* `!d` || `δ` - bytes in meta delta binding
* `!t` - tail after expression, sequence of applications and/or dispatches,
         must start only with dispatch
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

To generate a local coverage report for development, run:

```bash
cabal test --enable-coverage
```

You will need [GHC] and [Cabal ≥3.0][cabal] or [Stack ≥ 3.0][stack] installed.

[cabal]: https://www.haskell.org/cabal/
[stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[GHC]: https://www.haskell.org/ghc/
[guidelines]: https://www.yegor256.com/2014/04/15/github-guidelines.html
