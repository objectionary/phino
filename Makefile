# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

.ONESHELL:
.SHELLFLAGS := -e -o pipefail -c
.PHONY: all test hlint fourmolu coverage clean

SHELL := bash

all: coverage hlint fourmolu

.SILENT:
test:
	cabal test --ghc-options=-Werror

.SILENT:
hlint: test
	cabal exec hlint -- src app test

.SILENT:
fourmolu: test
	cabal exec fourmolu -- --mode check src app test

.SILENT:
coverage: test
	threshold=$${COVERAGE_THRESHOLD:-70}
	cabal test --enable-coverage --ghc-options=-Werror
	excludes=$$(find test -name '*.hs' -exec basename {} .hs \; | paste -sd, -)
	hpc-codecov cabal:spec -x "$${excludes}" -x "Paths_phino" --out=coverage.json
	coverage=$$(python3 -c "import json; d=json.load(open('coverage.json')); t=sum(len(v) for v in d['coverage'].values()); c=sum(1 for v in d['coverage'].values() for h in v.values() if isinstance(h,int) and h>0); print(int(100*c/t) if t else 0)")
	rm -f coverage.json
	if [ "$${coverage}" -lt "$${threshold}" ]; then echo "Coverage $${coverage}% is below threshold $${threshold}%"; exit 1; fi
	echo "Coverage $${coverage}% meets threshold $${threshold}%"

.SILENT:
clean:
	cabal clean
