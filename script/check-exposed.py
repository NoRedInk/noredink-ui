#!/usr/bin/env python3
#
# Checks that `elm-package.json` contains an `exposed-modules` entry for every
# module in `src/Nri` (recursively) named `Vn.elm`, where `n` is a decimal
# number.
#
# On success, this emits nothing and exits 0.
#
# On failure, this emits a list of those modules not exposed, and exits 2.
#

import json
import os
import re
import sys


re_v_file = re.compile(r"^V[0-9]+[.]elm$")
re_module = re.compile(
    r"""
    # Zero or more blank lines or single-line comments.
      (?:^$\n|^--.*$\n)*
    # Optional port or effect modifier.
      ^(?:(?:port|effect)\s+)?
    # The module declaration itself begins.
      module
    # Optional newline, comment, newline.
      (?:\n\s+--.+\n)?
    # After some whitespace, capture the module name.
      \s+(?P<module>[^ ]+)\b
    """,
    flags=re.MULTILINE | re.VERBOSE)


def p_v_file(filename):
    """Is `filename` a versioned widget file?"""
    return re_v_file.match(filename) is not None


def read_module(filename):
    """Read the module name from the Elm source."""
    with open(filename, "r", encoding="utf-8") as elm:
        return re_module.search(elm.read()).group("module")


def find_v_modules(root):
    for dirname, dirnames, filenames in os.walk(root + "/Nri"):
        filenames = filter(p_v_file, filenames)
        filepaths = (os.path.join(dirname, filename) for filename in filenames)
        yield from map(read_module, filepaths)


def read_exposed_modules(path):
    with open(path, "r", encoding="utf-8") as pkg:
        yield from json.load(pkg).get("exposed-modules")


if __name__ == "__main__":
    available_18 = set(find_v_modules("src-0.18"))
    available_19 = set(find_v_modules("src"))
    exposed_18 = set(read_exposed_modules("elm-package.json"))
    exposed_19 = set(read_exposed_modules("elm.json"))
    # XXX: Do we need to check that parent modules are exposed too?
    missing_18 = available_18 - exposed_18
    missing_19 = available_19 - exposed_19
    missing = missing_18.union(missing_19)
    for module in sorted(missing):
        print("Not exposed:", module, file=sys.stderr)
    raise SystemExit(
        0 if len(missing) == 0 else 2)
