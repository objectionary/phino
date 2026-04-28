# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

.ONESHELL:
.SHELLFLAGS := -e -o pipefail -c
.PHONY: all test hlint fourmolu coverage bench binary clean

SHELL := bash

JNA_VERSION := 5.15.0
JEO_VERSION := 0.15.1
ifeq ($(OS),Windows_NT)
  MVNW := $(CURDIR)/benchmark/mvnw.cmd
  BIN_EXT := .exe
  RELEASE_SPLIT :=
  RELEASE_LDOPT :=
else
  MVNW := $(CURDIR)/benchmark/mvnw
  BIN_EXT :=
  ifeq ($(shell uname -s),Darwin)
    RELEASE_SPLIT :=
    RELEASE_LDOPT := -optl-Wl,-dead_strip
  else
    RELEASE_SPLIT := -split-sections
    RELEASE_LDOPT := -optl-Wl,--gc-sections
  endif
endif

all: coverage hlint fourmolu

.SILENT:
test:
	cabal test --ghc-options=-Werror

.SILENT:
hlint:
	hlint src app test

.SILENT:
fourmolu:
	fourmolu --mode check src app test

.SILENT:
coverage:
	threshold=$${COVERAGE_THRESHOLD:-65}
	cabal test --enable-coverage --ghc-options=-Werror
	excludes=$$(find test -name '*.hs' -exec basename {} .hs \; | paste -sd, -)
	hpc-codecov cabal:spec -x "$${excludes}" -x "Paths_phino" --out=coverage.json
	coverage=$$(python3 -c "import json; d=json.load(open('coverage.json')); t=sum(len(v) for v in d['coverage'].values()); c=sum(1 for v in d['coverage'].values() for h in v.values() if isinstance(h,int) and h>0); print(int(100*c/t) if t else 0)")
	rm -f coverage.json
	if [ "$${coverage}" -lt "$${threshold}" ]; then echo "Coverage $${coverage}% is below threshold $${threshold}%"; exit 1; fi
	echo "Coverage $${coverage}% meets threshold $${threshold}%"

.SILENT:
bench: benchmark/tmp/native.phi
	cabal bench --enable-benchmarks

benchmark/tmp/native.phi: benchmark/tmp/Native.xmir
	phino rewrite --input xmir --sweet benchmark/tmp/Native.xmir > benchmark/tmp/native.phi

benchmark/tmp/Native.xmir:
	command -v java >/dev/null 2>&1 || { echo "java is required to run benchmarks but was not found in PATH"; exit 1; }
	mkdir -p $(CURDIR)/benchmark/tmp/jeo/target/classes/com/sun/jna
	curl -sSL https://repo1.maven.org/maven2/net/java/dev/jna/jna/$(JNA_VERSION)/jna-$(JNA_VERSION).jar -o $(CURDIR)/benchmark/tmp/jna.jar
	cd $(CURDIR)/benchmark/tmp/jeo/target/classes && jar xf $(CURDIR)/benchmark/tmp/jna.jar com/sun/jna/Native.class
	cd $(CURDIR)/benchmark/tmp/jeo && $(MVNW) org.eolang:jeo-maven-plugin:$(JEO_VERSION):disassemble -Djeo.disassemble.sourcesDir=target/classes -Djeo.disassemble.outputDir=xmir -q
	mv $(CURDIR)/benchmark/tmp/jeo/xmir/com/sun/jna/Native.xmir $(CURDIR)/benchmark/tmp/Native.xmir
	rm -rf $(CURDIR)/benchmark/tmp/jeo

.SILENT:
binary:
	cabal build --ghc-options="$(RELEASE_SPLIT) $(RELEASE_LDOPT)"
	mkdir -p dist-release
	cp "$$(cabal list-bin phino)" dist-release/phino$(BIN_EXT)
	strip dist-release/phino$(BIN_EXT)

.SILENT:
clean:
	cabal clean
	rm -rf .stack-work benchmark/tmp
