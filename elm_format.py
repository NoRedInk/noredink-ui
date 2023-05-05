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
        return cls(re.match(b"--- (.+?)\t", diff).groups()[0], diff)


class Report:
    def __init__(self):
        self.files = []

    def __repr__(self):
        return f"<Report: {self.files}>"

    def __bytes__(self):
        return b"\n".join(bytes(file) for file in self.files)

    def append(self, diff):
        self.files.append(File.from_universal_diff(diff))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--buck2-bin",
        default="buck2",
        help="where does `buck2` live?",
    )

    # FIX
    parser.add_argument(
        "--fix", action="store_true", help="Automatically fix all formatting errors."
    )
    parser.add_argument(
        "--patch-bin",
        default="patch",
        help="where does `patch` live? (only necessary with `--fix`)",
    )

    # GITHUB PR REVIEW
    parser.add_argument(
        "--review-github-pr",
        action="store_true",
        help="Leave a PR review on GitHub with suggested changes from this diff.",
    )
    parser.add_argument(
        "--github-token",
        default=os.environ.get("GITHUB_TOKEN", None),
        help="What GitHub token to use (only necessary with `--review-github-pr`. Reads from `GITHUB_TOKEN` if present.)",
    )
    parser.add_argument(
        "--github-pr",
        help="What GitHub PR should this review go to? (only necessary with `--review-github-pr`)",
    )

    args = parser.parse_args()

    # TODO: make this generic! The only thing specific to elm-format is the
    # target kind here, and could accept that on the CLI.
    targets = subprocess.check_output(
        [args.buck2_bin, "uquery", "kind(elm_format_diff, //...)"]
    ).split()
    report = json.loads(
        subprocess.check_output([args.buck2_bin, "build", "--build-report=-"] + targets)
    )

    out = Report()

    for target, result in report["results"].items():
        print(target)
        diffs = result["outputs"]["DEFAULT"]

        for diff in diffs:
            with open(diff, "rb") as fh:
                content = fh.read()

            if content:
                out.append(content)

    if args.fix:
        subprocess.run(
            [args.patch_bin, "-p0"],
            check=True,
            input=b"\n".join(bytes(file) for file in out.files),
        )

    if not args.fix and out.files:
        print(
            f"{len(out.files)} file(s) need fixes! Re-run me with `--fix` or `--review-github-pr` to fix these."
        )
        sys.exit(1)
