# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

.ONESHELL:
.SHELLFLAGS := -e -o pipefail -c
.PHONY: all test hlint fourmolu coverage clean

SHELL := bash

all: coverage hlint fourmolu

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
coverage:
	threshold=$${COVERAGE_THRESHOLD:-75}
	cabal test --enable-coverage
	tix=$$(find ./dist-newstyle -name 'spec.tix' -type f 2>/dev/null | head -1)
	if [ -z "$${tix}" ]; then echo "The spec.tix file not found"; tree dist-newstyle; exit 1; fi
	mixlib=$$(find ./dist-newstyle -type d -name 'mix' -path '*/build/extra-compilation-artifacts/hpc/vanilla/*' ! -path '*/t/spec/*' 2>/dev/null | head -1)
	if [ -z "$${mixlib}" ]; then echo "The mixlib directory not found"; exit 1; fi
	mixtest=$$(find ./dist-newstyle -type d -name 'mix' -path '*/t/spec/build/*' 2>/dev/null | head -1)
	excludes=$$(find test -name '*Spec.hs' -exec basename {} .hs \; | sed 's/^/--exclude=/' | tr '\n' ' ')
	output=$$(hpc report "$${tix}" --hpcdir="$${mixlib}" --hpcdir="$${mixtest}" --exclude=Main --exclude=Spec $${excludes})
	coverage=$$(echo "$$output" | grep "expressions used" | grep -oE '[0-9]+%' | tr -d '%')
	if [ -z "$${coverage}" ]; then echo "Could not extract coverage percentage"; exit 1; fi
	if [ "$${coverage}" -lt "$${threshold}" ]; then echo "Coverage $${coverage}% is below threshold $${threshold}%"; exit 1; fi
	echo "Coverage $${coverage}% meets threshold $${threshold}%"

.SILENT:
clean:
	cabal clean
