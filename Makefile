# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

.ONESHELL:
.SHELLFLAGS := -e -o pipefail -c -x
SHELL := bash
.PHONY: all test hlint fourmolu

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
