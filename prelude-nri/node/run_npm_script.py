#!/usr/bin/env python
import argparse
import os
import subprocess
import sys
import shutil
import tempfile

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--node-modules",
        help="Which node_modules folder do the dependencies live in?",
        default="node_modules",
    )
    parser.add_argument(
        "--node-bin",
        help="Path to node binaries",
    )
    parser.add_argument(
        "script",
        help="What script should we run?",
    )
    parser.add_argument(
        "args",
        help="What additional arguments should exist? (If they have flags, put them after a --)",
        nargs=argparse.REMAINDER,
    )

    args = parser.parse_args()

    if args.node_bin:
        os.environ["PATH"] = f"{args.node_bin}:{os.environ['PATH']}"

    if args.node_modules:
        os.environ["PATH"] = f"{args.node_modules}/.bin:{os.environ['PATH']}"
        os.environ["NODE_PATH"] = args.node_modules

    sys.exit(subprocess.call(["npm", "run", args.script, "--"] + args.args))
