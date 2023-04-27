#!/usr/bin/env python3
from argparse import ArgumentParser, ArgumentTypeError
import os
import os.path
import shutil
from subprocess import Popen, PIPE
import sys


def symlink_if_necessary(source, target):
    if os.path.exists(target):
        if os.readlink(target) == source:
            return

        os.unlink(target)

    os.symlink(source, target)


def run_docs(args):
    shutil.copy(args.elm_json, os.path.join(args.build_dir, "elm.json"))

    # for libraries, the Elm compiler always assumes the source lives in a
    # directory named "src". We have one of those from the arguments, so let's
    # set it up. For the sake of being able to reuse this build directory's
    # `elm-stuff`, we take care to repoint the symlink if necessary.
    symlink_if_necessary(
        os.path.abspath(args.src),
        os.path.join(args.build_dir, "src")
    )

    process = Popen(
        [
            args.elm_compiler,
            "make",
            "--docs",
            args.out,
        ],
        cwd = args.build_dir
    )
    process.communicate()

    return process.returncode


def run_make(args):
    print(args)
    print("MAKE!")


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("build_dir", help="Where to perform the build. Should be an empty directory, or one you've already run a build in.")
    parser.add_argument("elm_json", help="Location of the elm.json")
    parser.add_argument("--elm-compiler", help="path to the Elm compiler", default="elm")

    subparsers = parser.add_subparsers(required = True)

    docs = subparsers.add_parser('docs', help="build docs for an Elm library")
    docs.set_defaults(func=run_docs)
    docs.add_argument("out", help="Path for the resulting docs JSON file")
    docs.add_argument("--src", help="Path to library source", default="src")

    args = parser.parse_args()

    sys.exit(args.func(args))
