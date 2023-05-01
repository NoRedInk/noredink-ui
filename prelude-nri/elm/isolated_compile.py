#!/usr/bin/env python3
"""
Build an Elm app, but isolate the working directory so that multiple Elm
compiler invocations can run in parallel without corrupting `.elmi` files.
"""
from argparse import ArgumentParser, ArgumentTypeError
import json
import logging
import os
import os.path
import shutil
from subprocess import Popen, PIPE
import sys
import tempfile


def symlink_if_necessary(source, target):
    """
    Do the same thing as `os.symlink`, but with these edge cases checked:

     - If the target already exists and points to the source, do nothing.
     - If the target already exists and points somewhere else, remove it
       and relink.
    """
    if os.path.exists(target):
        if os.path.isfile(target):
            logging.debug(f"`{target}` is a regular file")

        elif os.readlink(target) == source:
            logging.debug(f"`{target}` already points to `{source}`")
            return

        os.unlink(target)

    logging.info(f"linking `{target}` to `{source}`")
    os.symlink(source, target)


def write_if_necessary(target, content):
    """
    Read the target file and check the content. If it's the same, don't write.
    Otherwise, replace it.
    """
    if os.path.exists(target):
        with open(target, "r") as fh:
            if fh.read() == content:
                logging.debug(f"`{target}` already had the requested content.")
                return

    logging.info(f"writing `{target}`")
    with open(target, "w") as fh:
        fh.write(content)


def run_docs(args):
    """
    Compile JSON docs for an Elm library.
    """
    logging.info(f"copying {args.elm_json} to {args.build_dir}")
    symlink_if_necessary(args.elm_json, os.path.join(args.build_dir, "elm.json"))

    # for libraries, the Elm compiler always assumes the source lives in a
    # directory named "src". We have one of those from the arguments, so let's
    # set it up under the build root. For the sake of being able to reuse this
    # build directory's `elm-stuff`, we take care to repoint the symlink if
    # necessary.
    symlink_if_necessary(os.path.abspath(args.src), os.path.join(args.build_dir, "src"))

    command = [
        args.elm_compiler,
        "make",
        "--docs",
        # since we're changing cwd, we need to output the absolute path
        # instead of a (potentially) relative one.
        os.path.abspath(args.output),
    ]
    logging.debug(f"running {command} in `{args.build_dir}`")
    process = Popen(command, cwd=args.build_dir)
    process.communicate()

    return process.returncode


def run_make(args):
    """
    Compile an Elm app to HTML or JavaScript.

    Our basic approach here is to symlink all the source directories and the
    Main.elm file into place and modify elm.json to look in those places. This
    means that we can use the build directory as a working directory so we get
    an isolated elm-stuff directory.
    """
    logging.debug(f"reading `{args.elm_json}`")
    with open(args.elm_json, "r") as fh:
        elm_json = json.load(fh)

    ##########################################################################
    # STEP 1: symlink entries in `source-directories` to the right locations #
    ##########################################################################

    try:
        original_source_directories = elm_json["source-directories"]
    except KeyError:
        logging.error(
            f'`{args.elm_json}` did not have a "source-directories" entry. Is it a package?'
        )
        return 1

    source_directory_replacements = dict(
        (sd.src, sd.target) for sd in args.source_directory or []
    )

    # we're creating a mapping here so we can change the Main file to be relative
    # to the symlinks later.
    new_source_directories = {}
    for i, directory in enumerate(original_source_directories):
        try:
            replacement = source_directory_replacements[directory]
        except KeyError:
            logging.error(
                f"I don't have a replacement path for `{directory}`. Please specify one with --source-directory {directory}=real-path-to-directory"
            )
            return 1

        dest = f"src-{i}"

        # special case: if the replacement is inside `buck-out` and contains
        # only a single directory, we're being passed a Buck2 artifact directory
        # and should take the single directory inside.
        if replacement.startswith("buck-out"):
            contents = os.listdir(replacement)
            if len(contents) == 1 and os.path.isdir(contents[0]):
                logging.info(
                    f"I think `{replacement}` is a Buck2 artifact, so I'm symlinking `{contents[0]}` inside it instead."
                )
                replacement = os.path.join(replacement, contents[0])

        symlink_if_necessary(
            os.path.abspath(replacement), os.path.join(args.build_dir, dest)
        )

        new_source_directories[replacement] = dest

    logging.debug(f"new source directories: {new_source_directories}")

    #############################################################
    # STEP 2: Modify and write `elm.json` to the right location #
    #############################################################

    elm_json["source-directories"] = list(new_source_directories.values())
    new_elm_json_path = os.path.join(args.build_dir, "elm.json")
    logging.debug(f"writing `{new_elm_json_path}`")

    # the compiler will do a full rebuild anytime the `elm.json` file changes.
    # Writing only if necessary about halves the runtime for this script for
    # noredink-ui's component catalog, with more expected savings the bigger the
    # app gets.
    write_if_necessary(
        new_elm_json_path,
        json.dumps(elm_json, indent=4),
    )

    ##########################################################
    # STEP 3: Make sure we're poining at the right main file #
    ##########################################################

    main = args.main
    replaced = False
    logging.debug(f"original main: {main}")
    for original, replacement in new_source_directories.items():
        if main.startswith(original):
            main = os.path.join(replacement, main[len(original) + 1 :])
            logging.debug(f"using `{main}` instead of `{args.main}`")
            replaced = True
            break

    if not replaced:
        # it's fine to build a main file outside a source directory, but let's
        # take the absolute path so we can make sure to get it.
        main = os.path.abspath(main)

    #####################################################
    # STEP 4: Prepare and run the `elm make` invocation #
    #####################################################

    command = [
        args.elm_compiler,
        "make",
        main,
        "--output",
        os.path.abspath(args.output),
    ]

    if args.debug and args.optimize:
        print("Only one of --debug or --optimize can be set.")
        return 1

    if args.debug:
        command.append("--debug")

    if args.optimize:
        command.append("--optimize")

    logging.debug(f"running {command} in `{args.build_dir}`")
    process = Popen(command, cwd=args.build_dir)
    process.communicate()

    return process.returncode


class SourceDirectory:
    def __init__(self, s):
        try:
            src, target = s.split("=")
            self.src = src
            self.target = target
        except:
            raise ArgumentTypeError("A pair must be `a=b`")


if __name__ == "__main__":
    parser = ArgumentParser(description=__doc__)
    parser.add_argument("elm_json", help="Location of the elm.json")
    parser.add_argument(
        "--build-dir",
        help="Where to perform the build. Should be an empty directory, or one you've already run a build in.",
    )
    parser.add_argument(
        "--elm-compiler", help="path to the Elm compiler", default="elm"
    )
    parser.add_argument(
        "--verbose", help="Turn on verbose logging", action="store_true"
    )

    subparsers = parser.add_subparsers(required=True)

    docs = subparsers.add_parser("docs", help=run_docs.__doc__)
    docs.set_defaults(func=run_docs)
    docs.add_argument(
        "--output", help="Path for the resulting docs JSON file", default="docs.json"
    )
    docs.add_argument("--src", help="Path to library source", default="src")

    make = subparsers.add_parser("make", help=run_make.__doc__)
    make.set_defaults(func=run_make)
    make.add_argument("main", help="Main.elm file to build")
    make.add_argument(
        "--output",
        help="Path for the resulting JavaScript or HTML file",
        default="app.js",
    )
    make.add_argument(
        "--source-directory",
        type=SourceDirectory,
        metavar="SRC=TARGET",
        action="append",
    )
    make.add_argument("--debug", action="store_true", help="Build in debug mode")
    make.add_argument("--optimize", action="store_true", help="Build in optimize mode")

    args = parser.parse_args()

    # logging
    logger = logging.getLogger()
    handler = logging.StreamHandler(sys.stderr)
    formatter = logging.Formatter("%(asctime)s [%(levelname)s] %(message)s")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    logger.setLevel(logging.DEBUG if args.verbose else logging.WARNING)

    # prep
    if os.path.exists(args.elm_compiler):
        args.elm_compiler = os.path.abspath(args.elm_compiler)

    # run!
    if args.build_dir is None:
        with tempfile.TemporaryDirectory() as temp:
            logging.warning(
                f"building in temporary directory `{temp}`, which will be cleaned up after the build. If you want a persistent place to build, pass it in with `--build-dir`!"
            )
            args.build_dir = temp
            sys.exit(args.func(args))
    else:
        if not os.path.exists(args.build_dir):
            logging.info(f"creating build dir at `{args.build_dir}`")
            os.mkdir(args.build_dir)
        sys.exit(args.func(args))
