# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

.ONESHELL:
.SHELLFLAGS := -e -o pipefail -c
SHELL := bash
.PHONY: all test hlint fourmolu coverage

all: test hlint fourmolu

.SILENT:
test:
	cabal test

.SILENT:
hlint:
	if command -v hlint &> /dev/null; then
		hlint src app test
	else
		echo "hlint not found, skipping." >&2
	fi

.SILENT:
fourmolu:
	if command -v fourmolu &> /dev/null; then
		fourmolu --mode check src app test
	else
		echo "fourmolu not found, skipping." >&2
	fi

.SILENT:
coverage:
	cabal test --enable-coverage
	tix=$$(find dist-newstyle -name "spec.tix" | head -1)
	if [ -z "$$tix" ]; then echo "Error: spec.tix file not found" >&2; exit 1; fi
	mixlib=$$(find dist-newstyle -type d -path "*/phino-0.0.0.0/build/*/hpc/vanilla/mix" | head -1)
	if [ -z "$$mixlib" ]; then echo "Error: mixlib directory not found" >&2; exit 1; fi
	mixtest=$$(find dist-newstyle -type d -path "*/spec/build/*/hpc/vanilla/mix" | head -1)
	if [ -z "$$mixtest" ]; then echo "Error: mixtest directory not found" >&2; exit 1; fi
	output=$$(hpc report "$$tix" --hpcdir="$$mixlib" --hpcdir="$$mixtest" --exclude=phino-0.0.0.0-inplace-spec)
	echo "$$output"
	coverage=$$(echo "$$output" | grep "expressions used" | grep -oE '[0-9]+%' | tr -d '%')
	if [ -z "$$coverage" ]; then
		echo "Error: Could not extract coverage percentage from hpc output" >&2
		exit 1
	fi
	threshold=75
	if [ "$$coverage" -lt "$$threshold" ]; then
		echo "Coverage $$coverage% is below threshold $$threshold%" >&2
		exit 1
	fi
	echo "Coverage $$coverage% meets threshold $$threshold%"
