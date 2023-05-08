#!/usr/bin/env python
"""
Run an npm script in an isolated directory.
"""
import argparse
import os
import subprocess
import sys
import shutil
import tempfile

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--package",
        help="The package.json to install dependencies from",
        default="package.json",
    )
    parser.add_argument(
        "--package-lock",
        help="The package-lock.json corresponding to package.json",
        default="package-lock.json",
    )
    parser.add_argument(
        "--node-modules",
        help="Which node_modules folder do the dependencies live in?",
        default="node_modules",
    )
    parser.add_argument(
        "--bin-dir",
        help="Path to node binaries",
    )
    parser.add_argument(
        "--extra-file",
        action="append",
        metavar="FILE=SRC",
        help="Add a file that the package needs, sourced from the given path. This may be used, for example, to add files needed by a prepublish script.",
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

    if args.bin_dir:
        os.environ["PATH"] = f"{args.bin_dir}:{os.environ['PATH']}"

    if args.node_modules:
        os.environ["PATH"] = f"{args.node_modules}/.bin:{os.environ['PATH']}"

    os.environ["NODE_PRESERVE_SYMLINKS"] = "1"

    with tempfile.TemporaryDirectory() as tempdir:
        # npm wants these to be real files for whatever reason, and will throw
        # errors if they're symlinks.
        shutil.copy(args.package, os.path.join(tempdir, "package.json"))
        shutil.copy(args.package_lock, os.path.join(tempdir, "package-lock.json"))

        # grab extra files that may be needed
        for extra in args.extra_file or []:
            target, src = extra.split("=")

            dir = os.path.join(tempdir, os.path.dirname(target))
            if not os.path.exists(dir):
                os.makedirs(dir)

            os.symlink(os.path.abspath(src), os.path.join(tempdir, target))

        os.symlink(
            os.path.abspath(args.node_modules),
            os.path.join(tempdir, "node_modules"),
        )

        exit_code = subprocess.call(
            ["npm", "run", args.script, "--"] + args.args,
            cwd = tempdir,
        )

    sys.exit(exit_code)
