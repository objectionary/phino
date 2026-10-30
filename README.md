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

`phino` is a command-line normalizer, rewriter, and dataizer of
[𝜑-calculus](https://www.eolang.org) expressions. It reads a 𝜑-expression
from a file or `stdin` and prints the result, as data or as a term.

Install [Cabal][cabal] first and then:

```bash
cabal update
cabal install --overwrite-policy=always phino-0.0.160
phino --version
```

Or take the binary for your platform from
<http://phino.objectionary.com/releases/>, where the last path segment is
`ubuntu-22.04`, `ubuntu-24.04`, `macos-15`, `macos-14-large`, or
`windows-2022` (its file ends in `.exe`):

```bash
curl -o /usr/local/bin/phino \
  http://phino.objectionary.com/releases/ubuntu-24.04/phino-latest
chmod +x /usr/local/bin/phino
```

In a [GitHub Actions][gha] workflow, one step downloads the binary for the
runner and puts it on the `PATH`:

```yaml
- uses: objectionary/phino@0.0.160
  with:
    version-file: phino-version.txt
- run: phino --version
```

Here is `hello.phi`:

```text
⟦ φ ↦ ⟦ Δ ⤍ 68-65-6C-6C-6F ⟧, t ↦ ξ.k, k ↦ ⟦⟧ ⟧
```

Dataize it:

```bash
$ phino dataize hello.phi
68-65-6C-6C-6F
```

## How to rewrite

A rule is a YAML file with a name, a pattern to find, and a result to put in
its place. Write `my-rule.yml` (here, `!d1` and `!B1` are capturing groups,
like in a regular expression):

```yaml
name: My custom rule
pattern: ⟦ Δ ⤍ !d1, !B1 ⟧
result: ⟦ Δ ⤍ 62-79-65, !B1 ⟧
```

Then rewrite:

```bash
$ phino rewrite --rule=my-rule.yml hello.phi
⟦ φ ↦ ⟦ Δ ⤍ 62-79-65 ⟧, t ↦ ξ.k, k ↦ ⟦⟧ ⟧
```

Use `--rule` as many times as you need, add `--normalize` to apply the
built-in rules of [`resources`](resources), and `--input=xmir` to read
[XMIR][xmir] instead of 𝜑. Run `phino rewrite --help` for every option.

## Commands

`dataize` prints the data an expression comes down to. With `--symbolic`,
every λ function comes from a YAML file, one entry per name:

```yaml
- λ: L_number_plus
  dataize:
    𝛿1: $.ρ
    𝛿2: $.x
  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )
```

Here is `sum.phi`:

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

`morph` reduces as far as the object model allows without asking for data,
and stops at the first formation it reaches:

```bash
$ cat two.phi
⟦
  bytes ↦ ⟦ φ ↦ ∅ ⟧,
  number ↦ ⟦ φ ↦ ∅, plus(ρ, x) ↦ ⟦ λ ⤍ L_number_plus ⟧ ⟧,
  φ ↦ 5.plus( 6 ).plus( 7 )
⟧
$ phino morph --symbolic=atoms.yaml --locator=Q.φ --sweet --hide-rho two.phi
⟦ x ↦ 7.0, λ ⤍ L_number_plus ⟧
```

`merge` joins the top level formations of several expressions into one:

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

`match` tests an expression against a rule pattern and prints the matched
substitutions:

```bash
phino match --pattern='⟦ Δ ⤍ !d, !B ⟧' hello.phi
```

`compile` turns the rules into the Haskell module
`compiled/generated/Compiled.hs`, which the next build links in when the
`compiled` Cabal flag is on:

```bash
phino compile
cabal build all
```

`explain` prints the rules in [LaTeX][latex], one family at a time:

```bash
phino explain --normalize
```

Every command takes `--help`.

## How to contribute

Fork the repository, make your changes, and send us a
[pull request][guidelines]. Make sure every test passes and that coverage
and benchmarks still run:

```bash
make all
make coverage
make bench
```

You need [GHC ≥ 9.6.7][GHC] and [Cabal ≥ 3.0 (recommended)][cabal] or
[Stack ≥ 3.0][stack].

[cabal]: https://www.haskell.org/cabal/
[stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[GHC]: https://www.haskell.org/ghc/
[guidelines]: https://www.yegor256.com/2014/04/15/github-guidelines.html
[xmir]: https://news.eolang.org/2022-11-25-xmir-guide.html
[latex]: https://en.wikipedia.org/wiki/LaTeX
[gha]: https://docs.github.com/en/actions
