#!/usr/bin/env bash
# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT
#
# Compare two benchmark runs and gate on regressions.
#
# Usage: bench-compare.sh BASE.txt HEAD.txt THRESHOLD
#
# BASE/HEAD: stdout captures of `cabal bench` (the bench prints `=== name ===`
# blocks with an `avg: X.YYY us` line per case).
#
# THRESHOLD: max allowed positive delta as a fraction, e.g. 0.20 for +20%.
#
# Prints a markdown comparison table to stdout, exits 1 if any case present in
# both runs regressed by more than THRESHOLD.
set -euo pipefail

if [ "$#" -ne 3 ]; then
  sed -n '6,15p' "$0" >&2
  exit 2
fi

base=$1
head=$2
threshold=$3

# Extract "<name> <avg-us>" lines from a bench output file.
extract() {
  awk '
    /^=== .* ===$/ { name = $2; next }
    /^[[:space:]]*avg:/ { if (name != "") { print name, $2; name = "" } }
  ' "$1"
}

# Build the table body as "<name>\t<row>" lines, one row per case, plus a final
# trailing "REGRESSIONS=N" line for the gate decision.
body=$(
  {
    extract "$base" | awk '{ print "base", $1, $2 }'
    extract "$head" | awk '{ print "head", $1, $2 }'
  } | awk -v threshold="$threshold" '
    { vals[$1, $2] = $3; names[$2] = 1 }
    END {
      regressions = 0
      for (k in names) {
        b = (("base", k) in vals) ? vals["base", k] : ""
        h = (("head", k) in vals) ? vals["head", k] : ""
        if (b == "") {
          printf "%s\t| `%s` | - | %.0f | new |\n", k, k, h
        } else if (h == "") {
          printf "%s\t| `%s` | %.0f | - | removed |\n", k, k, b
        } else {
          d = (h - b) / b
          pct = d * 100
          note = ""
          if (d > threshold) {
            note = " **REGRESSION**"
            regressions++
          } else if (d < -0.05) {
            note = " (improved)"
          }
          printf "%s\t| `%s` | %.0f | %.0f | %+.1f%%%s |\n", k, k, b, h, pct, note
        }
      }
      print "REGRESSIONS=" regressions
    }
  '
)

regressions=$(awk -F= '/^REGRESSIONS=/ { print $2 }' <<<"$body")
rows=$(awk '!/^REGRESSIONS=/' <<<"$body" | sort | cut -f2-)
pct=$(awk -v t="$threshold" 'BEGIN { printf "%d", t * 100 }')

printf '## Benchmark comparison\n\n'
printf 'Regression threshold: +%s%%. Same-runner A/B against the PR merge-base; the bench fixture is frozen across both runs.\n\n' "$pct"
printf '| Bench | base (us) | head (us) | delta |\n'
printf '|---|---:|---:|---:|\n'
printf '%s\n\n' "$rows"

if [ "$regressions" -gt 0 ]; then
  printf '**Failing**: %s case(s) regressed above the +%s%% threshold.\n' "$regressions" "$pct"
  exit 1
fi
printf 'OK: no case regressed above the +%s%% threshold.\n' "$pct"
