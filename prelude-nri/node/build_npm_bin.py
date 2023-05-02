#!/usr/bin/env python3
"""
Make a independently-runnable npm binary.
"""
import argparse
import os
import stat
import sys

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "node_modules",
        help="The path to the node_modules you want to get binaries from.",
    )
    parser.add_argument(
        "bin",
        help="The binary to pull from node_modules",
    )
    parser.add_argument("out", help="Where you want the resulting binary.")
    parser.add_argument(
        "--bin-dir",
        help="Path to a node installation's binary directory. If present, will be treated as an extra entry in PATH for the duration of this command.",
    )

    args = parser.parse_args()

    if not os.path.exists(args.node_modules):
        print("The given node_modules does not exist!")
        sys.exit(1)

    lines = [
        "#!/usr/bin/env bash",
        "set -e",
    ]

    bins = os.path.join(os.path.abspath(args.node_modules), ".bin")
    path = [bins]
    if args.bin_dir:
        path.append(os.path.abspath(args.bin_dir))

    lines.append("export PATH={}:$PATH".format(":".join(path)))

    bin = os.path.join(bins, args.bin)
    if not os.path.exists(bin):
        print("The given binary does not exist!")
        sys.exit(1)

    lines.append(f"exec {bin} $@")

    with open(args.out, "w") as fh:
        fh.write("\n".join(lines))

    os.chmod(
        args.out,
        stat.S_IRUSR
        | stat.S_IXUSR
        | stat.S_IRGRP
        | stat.S_IXGRP
        | stat.S_IROTH
        | stat.S_IXOTH,
    )
