#!/usr/bin/env python
"""
Run elm-format targets and present changes in a structured format.
"""
import argparse
import json
import os
import subprocess
import sys
import re


class DiffHunk:
    def __init__(self, name, start_line, hunk):
        self.name = name
        self.start_line = start_line
        self.hunk = hunk

    def __repr__(self):
        return f"<DiffHunk: {self.name} at line {self.start_line}>"

    def new_code(self):
        suggestion = []

        for (i, line) in enumerate(self.hunk.split(b"\n")):
            if line.startswith(b"+") or line.startswith(b" "):
                suggestion.append(line[1:])

            elif line.startswith(b"-"):
                continue

        return (
            len(suggestion),
            b"\n".join(suggestion[:]),
        )


class File:
    def __init__(self, name, diff):
        self.name = name
        self.diff = diff

    def __repr__(self):
        return f"<File: {self.name}>"

    def __bytes__(self):
        return self.diff

    def hunks(self):
        matches = re.finditer(
            b"@@ -\d+,\d+ \+(?P<start_line>\d+),\d+ @@\n(?P<hunk>.+?)((?=\n@@)|$)",
            self.diff,
            re.DOTALL,
        )
        for match in matches:
            parts = match.groupdict()

            yield DiffHunk(
                name=self.name,
                start_line=int(parts["start_line"]),
                hunk=parts["hunk"],
            )

    @classmethod
    def from_universal_diff(cls, diff):
        return cls(
            re.match(b"--- (.+?)\t", diff).groups()[0],
            diff
        )


class Report:
    def __init__(self):
        self.files = []

    def __repr__(self):
        return f"<Report: {self.files}>"

    def __bytes__(self):
        return b'\n'.join(bytes(file) for file in self.files)

    def append(self, diff):
        self.files.append(File.from_universal_diff(diff))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--buck2-bin", default="buck2")
    parser.add_argument("--patch-bin", default="patch")
    parser.add_argument("--fix", action="store_true")

    args = parser.parse_args()

    targets = subprocess.check_output([args.buck2_bin, "uquery", "kind(elm_format_diff, //...)"]).split()
    report = json.loads(subprocess.check_output([args.buck2_bin, "build", "--build-report=-"] + targets))

    out = Report()

    for target, result in report['results'].items():
        print(target)
        diffs = result['outputs']['DEFAULT']

        for diff in diffs:
            with open(diff, 'rb') as fh:
                content = fh.read()

            if content:
                out.append(content)

    if args.fix:
        subprocess.run([args.patch_bin, "-p0"], check=True, input=b"\n".join(out))

    # print(repr(out))
    # print(bytes(out.files[0]))
    # import pprint
    # pprint.pprint(list(out.files[0].hunks()))
    # pprint.pprint(next(out.files[0].hunks()).new_code())

    for file in out.files:
        for hunk in file.hunks():
            suggestion_line_length, suggestion = hunk.new_code()
            end_line = hunk.start_line + suggestion_line_length

            sys.stdout.buffer.write(b"In ")
            sys.stdout.buffer.write(file.name)
            sys.stdout.buffer.write(b", replace lines ")
            sys.stdout.buffer.write(str(hunk.start_line).encode("utf-8"))
            sys.stdout.buffer.write(b" to ")
            sys.stdout.buffer.write(str(end_line).encode("utf-8"))
            sys.stdout.buffer.write(b" with this:\n\n```suggestion\n")
            sys.stdout.buffer.write(suggestion)
            sys.stdout.buffer.write(b"\n```\n\n")
