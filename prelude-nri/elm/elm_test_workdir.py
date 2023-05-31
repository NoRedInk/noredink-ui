#!/usr/bin/env python
import argparse
import subprocess
import sys

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "root", help="Where to change directory to before running elm-test"
    )
    parser.add_argument(
        "--elm-test-binary",
        default="elm-test",
        help="The binary to invoke for elm-test",
    )
    parser.add_argument(
        "elm_test_args",
        nargs=argparse.REMAINDER,
        help="Args to be passed on to elm-test",
    )

    args = parser.parse_args()

    returncode = subprocess.call(
        [args.elm_test_binary] + args.elm_test_args, cwd=args.root
    )
    sys.exit(returncode)
