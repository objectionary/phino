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
	echo "Looking for spec.tix in dist-newstyle..."
	find dist-newstyle -name "*.tix" -o -name "spec.tix" 2>/dev/null || true
	echo "All tix files found:"
	find . -name "*.tix" 2>/dev/null || true
	echo "Searching for spec.tix specifically..."
	tix=$$(find dist-newstyle -name spec.tix 2>/dev/null | head -1)
	if [ -z "$$tix" ]; then
		echo "The spec.tix file not found in dist-newstyle"
		echo "Checking current directory for spec.tix..."
		if [ ! -f "spec.tix" ]; then
			echo "spec.tix not found anywhere"
			exit 1
		fi
		echo "Found spec.tix in current directory"
		tix="spec.tix"
	fi
	echo "Using tix file: $$tix"
	mixlib=$$(find dist-newstyle -type d -path "*/phino-*/build/extra-compilation-artifacts/hpc/vanilla/mix" | head -1)
	if [ -z "$$mixlib" ]; then
		echo "The mixlib directory not found"
		exit 1
	fi
	mixtest=$$(find dist-newstyle -type d -path "*/t/spec/build/*/hpc/vanilla/mix" | head -1)
	output=$$(hpc report "$$tix" --hpcdir="$$mixlib" --hpcdir="$$mixtest" --exclude=phino-*-inplace-spec)
	echo "$$output"
	coverage=$$(echo "$$output" | grep "expressions used" | grep -oE '[0-9]+%' | tr -d '%')
	if [ -z "$$coverage" ]; then
		echo "Error: Could not extract coverage percentage from hpc output"
		exit 1
	fi
	threshold=$${COVERAGE_THRESHOLD:-85}
	if [ "$$coverage" -lt "$$threshold" ]; then
		echo "Coverage $$coverage% is below threshold $$threshold%"
		exit 1
	fi
	echo "Coverage $$coverage% meets threshold $$threshold%"
