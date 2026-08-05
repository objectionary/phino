#!/usr/bin/env bash
# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT
set -euo pipefail

usage() {
  cat <<'EOF'
Compare two benchmark runs and report the deltas.

Usage: bench-compare.sh BASE.txt HEAD.txt THRESHOLD

BASE/HEAD: stdout captures of the bench binary (it prints `=== name ===`
blocks with an `avg: X.YYY us` line per case). Either file may hold several
rounds appended one after another; every round counts as one sample, and the
median of the samples is what gets reported.

THRESHOLD: how far a case has to move to be called out, as a fraction, e.g.
0.20 for 20%. A case is called out only when it also moves further than the
noise measured across its own rounds.

Prints a markdown comparison table to stdout. A delta never fails the run:
shared CI runners are too noisy to gate on, so the report is informational.
EOF
}

if [ "$#" -ne 3 ]; then
  usage >&2
  exit 2
fi

base=$1
head=$2
threshold=$3

# Extract one "<name> <avg-us>" line per case per round from a bench output file.
extract() {
  awk '
    /^=== .* ===$/ { name = $2; next }
    /^[[:space:]]*avg:/ { if (name != "") { print name, $2; name = "" } }
  ' "$1"
}

# Build the table body as "<name>\t<row>" lines, one row per case, plus trailing
# "CALLED=N" and "ROUNDS=N" lines for the summary.
body=$(
  {
    extract "$base" | awk '{ print "base", $1, $2 }'
    extract "$head" | awk '{ print "head", $1, $2 }'
  } | awk -v threshold="$threshold" '
    function sorted(side, name, out,   total, idx, pos, held) {
      total = count[side, name]
      for (idx = 1; idx <= total; idx++) { out[idx] = sample[side, name, idx] + 0 }
      for (idx = 2; idx <= total; idx++) {
        held = out[idx]
        for (pos = idx - 1; pos >= 1 && out[pos] > held; pos--) { out[pos + 1] = out[pos] }
        out[pos + 1] = held
      }
      return total
    }
    function median(values, total) { return values[int((total + 1) / 2)] }
    function spread(values, total) { return total > 1 ? (values[total] - values[1]) / median(values, total) : 0 }
    function noiseText(value, total) { return total > 1 ? sprintf("±%.1f%%", value * 100) : "n/a" }
    { count[$1, $2]++; sample[$1, $2, count[$1, $2]] = $3; names[$2] = 1 }
    END {
      called = 0
      rounds = 0
      for (name in names) {
        bases = sorted("base", name, low)
        heads = sorted("head", name, high)
        if (bases > rounds) { rounds = bases }
        if (heads > rounds) { rounds = heads }
        if (bases == 0) {
          printf "%s\t| `%s` | - | %.2f | new | %s |\n", name, name, median(high, heads), noiseText(spread(high, heads), heads)
          continue
        }
        if (heads == 0) {
          printf "%s\t| `%s` | %.2f | - | removed | %s |\n", name, name, median(low, bases), noiseText(spread(low, bases), bases)
          continue
        }
        delta = (median(high, heads) - median(low, bases)) / median(low, bases)
        noise = spread(low, bases) > spread(high, heads) ? spread(low, bases) : spread(high, heads)
        samples = bases > heads ? bases : heads
        note = ""
        if (delta > threshold && delta > noise) {
          note = " **slower**"
          called++
        } else if (-delta > threshold && -delta > noise) {
          note = " **faster**"
        }
        printf "%s\t| `%s` | %.2f | %.2f | %+.1f%%%s | %s |\n", name, name, median(low, bases), median(high, heads), delta * 100, note, noiseText(noise, samples)
      }
      print "CALLED=" called
      print "ROUNDS=" rounds
    }
  '
)

called=$(awk -F= '/^CALLED=/ { print $2 }' <<<"$body")
rounds=$(awk -F= '/^ROUNDS=/ { print $2 }' <<<"$body")
rows=$(awk '!/^(CALLED|ROUNDS)=/' <<<"$body" | sort | cut -f2-)
pct=$(awk -v t="$threshold" 'BEGIN { printf "%d", t * 100 }')

printf '## Benchmark comparison\n\n'
printf 'Median of %s round(s) per side, interleaved on the same runner against the PR merge-base, with the bench fixture frozen across both builds. Noise is the spread between the fastest and the slowest round, taken from the noisier side.\n\n' "$rounds"
printf '| Bench | base (μs) | head (μs) | delta | noise |\n'
printf '|---|---:|---:|---:|---:|\n'
printf '%s\n\n' "$rows"

if [ "$called" -gt 0 ]; then
  printf 'Worth a look: %s case(s) moved by more than %s%% and by more than their own noise.\n' "$called" "$pct"
else
  printf 'No case moved by more than %s%% beyond its own noise.\n' "$pct"
fi
printf 'This report is informational, a delta never fails the build.\n'
