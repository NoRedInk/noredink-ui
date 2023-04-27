#!/usr/bin/env python3
from argparse import ArgumentParser, ArgumentTypeError
import json
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
            os.path.abspath(args.out),
        ],
        cwd = args.build_dir
    )
    process.communicate()

    return process.returncode


def run_make(args):
    with open(args.elm_json, 'r') as fh:
        elm_json = json.load(fh)
    try:
        original_source_directories = elm_json["source-directories"]
    except KeyError:
        print(f"`{args.elm_json}` did not have a \"source-directories\" entry. Is it a package?")
        return 1

    source_directory_replacements = dict((sd.src, sd.target) for sd in args.source_directory or [])

    new_source_directories = []
    for (i, directory) in enumerate(original_source_directories):
        try:
            replacement = source_directory_replacements[directory]
        except KeyError:
            print(f"I don't have a replacement path for `{directory}`. Please specify one with --source-directory {directory}=real-path-to-directory")
            return 1

        dest = f"sd-{i}"
        symlink_if_necessary(
            os.path.abspath(replacement),
            os.path.join(args.build_dir, dest)
        )

        new_source_directories.append(dest)

    elm_json["source-directories"] = new_source_directories

    with open(os.path.join(args.build_dir, "elm.json"), "w") as fh:
        json.dump(elm_json, fh)

    # TODO: this is not necessarily going to work if the name is not `Main`
    # because of the module declaration not matching the file name.
    main = "Main.elm"
    symlink_if_necessary(os.path.abspath(args.main), os.path.join(args.build_dir, main))

    command = [args.elm_compiler, "make", main, "--output", os.path.abspath(args.out)]

    if args.debug and args.optimize:
        print("Only one of --debug or --optimize can be set.")
        return 1

    if args.debug:
        command.append("--debug")

    if args.optimize:
        command.append("--optimize")

    process = Popen(command, cwd = args.build_dir)
    process.communicate()

    return process.returncode


class SourceDirectory:
    def __init__(self, s):
        try:
            src, target = s.split('=')
            self.src = src
            self.target = target
        except:
            raise ArgumentTypeError("A pair must be `a=b`")


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

    make = subparsers.add_parser('make', help="compile an Elm app to JavaScript")
    make.set_defaults(func=run_make)
    make.add_argument("out", help="Path for the resulting JavaScript or HTML file")
    make.add_argument("main", help="Main.elm file to build")
    make.add_argument("--source-directory", type=SourceDirectory, metavar="SRC=TARGET", action="append")
    make.add_argument("--debug", action="store_true", help="Build in debug mode")
    make.add_argument("--optimize", action="store_true", help="Build in optimize mode")

    args = parser.parse_args()

    sys.exit(args.func(args))
