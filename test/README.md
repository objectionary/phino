# Phino Tests

To run all the tests you can do:

```bash
cabal test
# OR
stack test
```

To run a specific test you can do:

```bash
cabal v2-run spec -- --match "parse packs"
```

Here "parse packs" is the part of description of the test which may go:

1. Before `Spec` in spec file name (e.g. "Parser" in `ParserSpec`)
2. After `describe` function call
3. After `it` function call
