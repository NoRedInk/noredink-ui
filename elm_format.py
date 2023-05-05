#!/usr/bin/env python
"""
Run elm-format targets and present changes in a structured format.
"""
import argparse
import json
import os
import subprocess
import sys

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--buck2-bin", default="buck2")
    parser.add_argument("--patch-bin", default="patch")
    parser.add_argument("--fix", action="store_true")

    args = parser.parse_args()

    targets = subprocess.check_output([args.buck2_bin, "uquery", "kind(elm_format_diff, //...)"]).split()
    report = json.loads(subprocess.check_output([args.buck2_bin, "build", "--build-report=-"] + targets))

    out = []

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

    sys.stdout.buffer.write(b"\n".join(out))
