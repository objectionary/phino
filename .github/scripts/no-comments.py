#!/usr/bin/env python3
# SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
# SPDX-License-Identifier: MIT

"""Fail if a .hs file carries a comment.

Only two kinds of comment are allowed: an SPDX header, the copyright and
license lines on top of every file, and a '@todo' puzzle together with the
line comments right below it whose text is indented by two spaces or more.
GHC pragmas ('{-# ... #-}') are not comments and are always allowed.

The file is lexed the way GHC lexes it, so a '--' or a '{-' inside a string
or a character literal is no comment, and neither is an operator such as
'-->' or '|--'.
"""

import re
import sys

SYMBOL = set("!#$%&*+./<=>?@\\^|~:-")
CHAR = re.compile(r"'(?:[^'\\\n]|\\[^\n][^'\n]*)'")


def named(char):
    """Tell whether the character may end a name, which a prime then extends."""
    return char.isalnum() or char in "_'"


def comments(text):
    """Yield the offset and the text of every comment."""
    size = len(text)
    pos = 0
    while pos < size:
        if text.startswith("{-#", pos):
            end = text.find("#-}", pos + 3)
            pos = size if end < 0 else end + 3
        elif text.startswith("{-", pos):
            start = pos
            depth = 0
            while pos < size:
                if text.startswith("{-", pos):
                    depth += 1
                    pos += 2
                elif text.startswith("-}", pos):
                    depth -= 1
                    pos += 2
                    if depth == 0:
                        break
                else:
                    pos += 1
            yield start, text[start:pos]
        elif text.startswith("--", pos) and (pos == 0 or text[pos - 1] not in SYMBOL):
            end = pos
            while end < size and text[end] == "-":
                end += 1
            if end < size and text[end] in SYMBOL:
                pos = end
                continue
            stop = text.find("\n", pos)
            stop = size if stop < 0 else stop
            yield pos, text[pos:stop]
            pos = stop
        elif text[pos] == '"':
            pos += 1
            while pos < size and text[pos] != '"':
                if text[pos] == "\\" and pos + 1 < size and text[pos + 1].isspace():
                    gap = text.find("\\", pos + 1)
                    pos = size if gap < 0 else gap + 1
                elif text[pos] == "\\":
                    pos += 2
                else:
                    pos += 1
            pos += 1
        elif text[pos] == "'" and not (pos > 0 and named(text[pos - 1])):
            match = CHAR.match(text, pos)
            pos = match.end() if match else pos + 1
        else:
            pos += 1


def banned(text):
    """Yield the line and the text of every comment that is not allowed."""
    puzzle = None
    for offset, comment in comments(text):
        line = text.count("\n", 0, offset) + 1
        column = offset - text.rfind("\n", 0, offset)
        body = comment.lstrip("-{").rstrip("}-")
        if comment.startswith("--") and body.lstrip().startswith("SPDX-"):
            continue
        if body.lstrip().startswith("@todo"):
            puzzle = (line, column)
            continue
        follows = puzzle == (line - 1, column) and comment.startswith("--")
        if follows and body.startswith("  "):
            puzzle = (line, column)
            continue
        puzzle = None
        yield line, comment.splitlines()[0]


def main(argv):
    paths = argv[1:]
    if not paths:
        print("usage: no-comments.py FILE...", file=sys.stderr)
        return 2
    found = 0
    for path in paths:
        with open(path, encoding="utf-8") as handle:
            text = handle.read()
        for line, comment in banned(text):
            print(f"{path}:{line}: comment is not allowed: {comment[:100]}")
            found += 1
    if found:
        print(
            f"\n{found} comment(s) found, only SPDX headers and @todo puzzles may stay",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
