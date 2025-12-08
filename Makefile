# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

.ONESHELL:
.SHELLFLAGS := -e -o pipefail -c
SHELL := bash
.PHONY: all test hlint fourmolu coverage

all: hlint fourmolu coverage

.SILENT:
test:
	cabal test

.SILENT:
hlint:
	hlint src app test

.SILENT:
fourmolu:
	fourmolu --mode check src app test

.SILENT:
coverage: test
	cabal test --enable-coverage
	tix=$$(find dist-newstyle -name "spec.tix" 2>/dev/null | head -1)
	if [ -z "$$tix" ]; then echo "The spec.tix file not found"; exit 1; fi
	mixlib=$$(find dist-newstyle -type d -path "*/phino-*/build/extra-compilation-artifacts/hpc/vanilla/mix" 2>/dev/null | head -1)
	if [ -z "$$mixlib" ]; then echo "The mixlib directory not found"; exit 1; fi
	mixtest=$$(find dist-newstyle -type d -path "*/t/spec/build/*/hpc/vanilla/mix" 2>/dev/null | head -1)
	output=$$(hpc report "$$tix" --hpcdir="$$mixlib" --hpcdir="$$mixtest" --exclude=phino-*-inplace-spec)
	echo "$$output"
	coverage=$$(echo "$$output" | grep "expressions used" | grep -oE '[0-9]+%' | tr -d '%')
	if [ -z "$$coverage" ]; then echo "Could not extract coverage percentage"; exit 1; fi
	threshold=$${COVERAGE_THRESHOLD:-85}
	if [ "$$coverage" -lt "$$threshold" ]; then echo "Coverage $$coverage% is below threshold $$threshold%"; exit 1; fi
	echo "Coverage $$coverage% meets threshold $$threshold%"
