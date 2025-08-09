# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

.ONESHELL:
.SHELLFLAGS := -e -o pipefail -c -x
SHELL := bash
.PHONY: all test hlint

all: test hlint

.SILENT:
test:
	cabal test

.SILENT:
hlint:
	hlint src app test