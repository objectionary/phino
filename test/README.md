# Phino Tests

To run all the tests you can do:

```bash
cabal test
# OR
stack test
# OR
make test
```

To run [`hlint`](https://github.com/ndmitchell/hlint) you can do:

```bash
make hlint
```

To run [`fourmolu`](https://github.com/fourmolu/fourmolu) you can do:

```bash
make fourmolu
```

To check [coverage](https://en.wikipedia.org/wiki/Code_coverage) you can do:

```bash
make coverage
```

To run full pipeline before pull request just do:

```bash
make all
```

To run a specific test you can do:

```bash
cabal v2-run spec -- --match "parse packs"
```

Here "parse packs" is the part of description of the test which may go:

1. Before `Spec` in spec file name (e.g. "Parser" in `ParserSpec`)
2. After `describe` function call
3. After `it` function call
