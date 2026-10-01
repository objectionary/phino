#!/usr/bin/env python3
# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

"""Fail if a .hs file carries a comment that documents nothing named.

A comment is allowed only where it documents a module, a data/type/newtype/
class/instance declaration, a record field, a data constructor, a function's
type signature (top-level, where-bound or let-bound), a function equation, or
an hspec 'describe'/'it'/'context' block. A comment sitting inside a case
alternative, an if/then/else branch, a do-block statement, a let-bound value
carrying no signature of its own, or trailing a call-site argument, is banned.

SPDX license headers and GHC pragmas ('{-# ... #-}') are not comments for
this purpose and are always allowed.
"""

import re
import sys

PRAGMA = re.compile(r"^\{-#")
BLOCK_OPEN = re.compile(r"^\{-(?!#)")
LEADING_MARKER = re.compile(r"^([|=,]|\{)\s*(.*)$")
DECL_HEAD = re.compile(
    r"^("
    r"module\b|data\b|type\b|newtype\b|class\b|instance\b|"
    r"pattern\s+[A-Za-z_][A-Za-z0-9_']*\s*::|"
    r"let\s+[A-Za-z_][A-Za-z0-9_']*\s*::|"
    r"_?[A-Za-z][A-Za-z0-9_']*\s*::|"
    r"(describe|it|context)\s+\""
    r")"
)
FIELD_CONT = re.compile(r"^[A-Za-z_][A-Za-z0-9_']*\s*,\s*$")
ASSIGN = re.compile(r"(?<![=!<>/])=(?!=)")
LET_NOSIG = re.compile(r"^let\s+[A-Za-z_]")
TRAILING_COMMENT = re.compile(r"--(?![-!#$%&*+./<=>?@\\^|~:])")


def strip_marker(line):
    m = LEADING_MARKER.match(line)
    return (m.group(2), True) if m else (line, False)


def is_decl_like(line):
    core, marker = strip_marker(line.strip())
    if marker:
        return True
    if LET_NOSIG.match(line.strip()) and "::" not in line:
        return False
    if DECL_HEAD.match(line.strip()):
        return True
    if FIELD_CONT.match(line.strip()):
        return True
    return bool(ASSIGN.search(line.strip()))


def prev_opens_slot(line):
    s = line.rstrip("\n").rstrip()
    return bool(re.search(r"[|=,]\s*$", s)) or s.endswith("{")


def scan(path):
    with open(path, encoding="utf-8") as handle:
        lines = handle.readlines()
    n = len(lines)
    violations = []
    i = 0
    in_block = False
    while i < n:
        raw = lines[i]
        stripped = raw.strip()

        if in_block:
            if "-}" in raw:
                in_block = False
            i += 1
            continue

        if PRAGMA.match(stripped):
            i += 1
            continue

        core, had_marker = strip_marker(stripped)
        starts_comment = (
            stripped.startswith("--")
            or BLOCK_OPEN.match(stripped)
            or (had_marker and (core.startswith("--") or BLOCK_OPEN.match(core)))
        )

        if starts_comment:
            start = i
            while i < n:
                s = lines[i].strip()
                if s == "":
                    i += 1
                    continue
                c, m = strip_marker(s)
                if s.startswith("--") or (m and c.startswith("--")):
                    i += 1
                    continue
                if BLOCK_OPEN.match(s) or (m and BLOCK_OPEN.match(c)):
                    while i < n and "-}" not in lines[i]:
                        i += 1
                    i += 1
                    continue
                break

            if had_marker:
                i = max(i, start + 1)
                continue

            p = start - 1
            while p >= 0 and lines[p].strip() == "":
                p -= 1
            prevline = lines[p] if p >= 0 else ""
            nxt = lines[i] if i < n else ""

            keep = prev_opens_slot(prevline) or nxt.strip() == "" or is_decl_like(nxt)
            if not keep:
                violations.append((path, start + 1, lines[start].strip()[:100]))
            continue

        if not stripped.startswith("\\"):
            segments = raw.rstrip("\n").split('"')
            for idx, segment in enumerate(segments):
                if idx % 2 == 0:
                    match = TRAILING_COMMENT.search(segment)
                    if match:
                        code_before = segment[: match.start()].strip()
                        if code_before and not is_decl_like(code_before):
                            violations.append((path, i + 1, raw.strip()[:100]))
                        break

        i += 1

    return violations


def main(argv):
    paths = argv[1:]
    if not paths:
        print("usage: no-comments.py FILE...", file=sys.stderr)
        return 2

    violations = []
    for path in paths:
        violations.extend(scan(path))

    for path, line, text in violations:
        print(f"{path}:{line}: inline comment is not documenting a declaration: {text}")

    if violations:
        print(
            f"\n{len(violations)} inline comment(s) found; see CONTRIBUTING or CLAUDE.md for the policy",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
