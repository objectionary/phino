#!/usr/bin/env python3
# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT
"""Compare two benchmark runs and gate on regressions.

Usage: bench-compare.py BASE.txt HEAD.txt THRESHOLD

BASE/HEAD: stdout captures of `cabal bench` (the bench prints `=== name ===`
blocks with an `avg: X.YYY us` line per case).

THRESHOLD: max allowed positive delta as a fraction, e.g. 0.20 for +20%.

Prints a markdown comparison table to stdout, exits 1 if any case present in
both runs regressed by more than THRESHOLD.
"""
import re
import sys


def parse(path):
    results = {}
    name = None
    with open(path, encoding="utf-8") as f:
        for line in f:
            m = re.match(r"^=== (.+) ===$", line)
            if m:
                name = m.group(1)
                continue
            m = re.match(r"^\s+avg:\s+([0-9.]+)", line)
            if m and name is not None:
                results[name] = float(m.group(1))
                name = None
    return results


def main():
    if len(sys.argv) != 4:
        print(__doc__, file=sys.stderr)
        sys.exit(2)
    base = parse(sys.argv[1])
    head = parse(sys.argv[2])
    threshold = float(sys.argv[3])

    print("## Benchmark comparison")
    print()
    print(
        f"Regression threshold: +{threshold:.0%}. "
        "Same-runner A/B against the PR merge-base; "
        "the bench fixture is frozen across both runs."
    )
    print()
    print("| Bench | base (us) | head (us) | delta |")
    print("|---|---:|---:|---:|")

    regressions = []
    for name in sorted(set(base) | set(head)):
        b = base.get(name)
        h = head.get(name)
        if b is None:
            print(f"| `{name}` | - | {h:,.0f} | new |")
        elif h is None:
            print(f"| `{name}` | {b:,.0f} | - | removed |")
        else:
            d = (h - b) / b
            note = ""
            if d > threshold:
                note = " **REGRESSION**"
                regressions.append((name, b, h, d))
            elif d < -0.05:
                note = " (improved)"
            print(f"| `{name}` | {b:,.0f} | {h:,.0f} | {d:+.1%}{note} |")

    print()
    if regressions:
        print(
            f"**Failing**: {len(regressions)} case(s) regressed above the "
            f"+{threshold:.0%} threshold."
        )
        sys.exit(1)
    print(f"OK: no case regressed above the +{threshold:.0%} threshold.")


if __name__ == "__main__":
    main()
