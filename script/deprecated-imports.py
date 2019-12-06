#!/usr/bin/env python3
"""
Make sure we don't use deprecated versions of Nri.Ui modules. We do this by only
allowing new imports for the highest version of every module. We keep an
artifact with old imports which haven't been removed yet, and are temporarily
allowed. See the subcommands of this script for managing this artifact!
"""

import argparse
from collections import defaultdict
import csv
import json
import os
import os.path
import re
import sys
import textwrap


class ElmJson:
    """parse elm.json for things we will need to determine version usage"""

    def __init__(self, path):
        with open(path, "r") as fh:
            self.data = json.load(fh)

        self.elm_version = self.data["elm-version"]
        self.source_directories = ["src"]


class NriUiModules:
    """
    look in a given version of noredink-ui to get the versioned modules and the
    latest version for each.
    """

    MODULE_RE = "(?P<name>Nri\.Ui\.\w+).V(?P<version>\d+)"

    def __init__(self):
        with open("elm.json", "r") as fh:
            self.module_list = json.load(fh)["exposed-modules"]

        self.versions = defaultdict(list)
        for module in self.module_list:
            match = re.match(self.MODULE_RE, module)
            if match is None:
                continue

            parts = match.groupdict()
            self.versions[parts["name"]].append(int(parts["version"]))
            self.versions[parts["name"]].sort()

    def latest_version(self, name):
        if name in self.versions:
            return max(self.versions[name])

    def is_latest_version(self, name, version):
        return self.latest_version(name) == version


class Import:
    """a single import."""
    DEPRECATED = "DEPRECATED"

    def __init__(self, filename, name, version):
        self.filename = filename
        self.name = name
        try:
            self.version = int(version)
        except ValueError:  # DEPRECATED
            self.version = self.DEPRECATED

    def to_dict(self):
        return {"filename": self.filename, "name": self.name, "version": self.version}

    def __eq__(self, other):
        return (
            self.filename == other.filename
            and self.name == other.name
            and self.version == other.version
        )

    def __hash__(self):
        # objects aren't hashable by default but we need to use this one in a
        # set. So we just make one big hash out of all the data we have!
        return hash(self.filename + self.name + str(self.version))

    def __str__(self):
        return "{} imports {} version {}".format(self.filename, self.name, self.version)


class Imports(list):
    """
    get all the imports in the project according to specific rules.

    this class is all about loading and storing this data, so we just subclass
    `list` to save some hassle on writing iteration etc.
    """

    NRI_UI_IMPORT_RE = "import\s+" + NriUiModules.MODULE_RE
    DEPRECATED_IMPORT_RE = "import\s+(?P<import>[\w\.]*deprecated[\w\.]*)"
    GENERIC_IMPORT_RE = "import\s+(?P<import>[\w\.]+)"

    @classmethod
    def from_source_directories(cls, source_directories, extras):
        """
        construct a list of project imports given an elm.json's source directories.
        """
        results = cls()

        for source_directory in source_directories:
            for (dirpath, _, filenames) in os.walk(source_directory):
                for filename in filenames:
                    if os.path.splitext(filename)[1] != ".elm":
                        continue

                    full_path = os.path.join(dirpath, filename)
                    with open(full_path, "r") as fh:
                        lines = [line.strip() for line in fh.readlines()]

                    for line in lines:
                        line = line.strip()

                        # add whatever imports we want to track here, continue
                        # after each.

                        match = re.match(cls.NRI_UI_IMPORT_RE, line)
                        if match is not None:
                            parts = match.groupdict()

                            results.append(
                                Import(
                                    filename=full_path,
                                    name=parts["name"],
                                    version=parts["version"],
                                )
                            )
                            continue

                        match = re.match(
                            cls.DEPRECATED_IMPORT_RE, line, flags=re.IGNORECASE
                        )
                        if match is not None:
                            parts = match.groupdict()
                            results.append(
                                Import(
                                    filename=full_path,
                                    name=parts["import"],
                                    version=Import.DEPRECATED,
                                )
                            )
                            continue

                        match = re.match(cls.GENERIC_IMPORT_RE, line)
                        if match is not None:
                            parts = match.groupdict()
                            if parts["import"] not in extras:
                                continue

                            results.append(
                                Import(
                                    filename=full_path,
                                    name=parts["import"],
                                    version=Import.DEPRECATED
                                )
                            )
                            continue

        return results

    @classmethod
    def from_file(cls, filename):
        try:
            with open(filename, "r") as fh:
                return cls([Import(**entry) for entry in csv.DictReader(fh)])
        except FileNotFoundError:
            return cls([])

    def to_file(self, filename, filter=None):
        filter = filter or (lambda _: True)

        out = (entry.to_dict() for entry in self if filter(entry))

        with open(filename, "w") as fh:
            writer = csv.DictWriter(fh, ["filename", "name", "version"])
            writer.writeheader()
            writer.writerows(out)


class Main:
    def __init__(self, args):
        self.args = args
        self.elm_json = ElmJson("elm.json")

        self.deprecated = set(args.deprecate)

        # cache-y things. These get set away from `None` the first time their
        # associated methods are called. This has the same effect as doing
        # `@foo ||= bar` in Ruby.
        self._current_imports = None
        self._previous_imports = None
        self._modules = None

    def run(self):
        if self.args.command == "update":
            return self.update()
        elif self.args.command == "check":
            return self.check()
        elif self.args.command == "report":
            return self.report()
        else:
            print("unrecognized command. Make sure the cases in main match the parser.")
            return 1

    def is_latest_version(self, name, version):
        return self.modules().is_latest_version(name, version) and name not in self.deprecated

    def update(self):
        self.current_imports().to_file(
            self.args.imports_file,
            filter=lambda import_: not self.is_latest_version(import_.name, import_.version),
        )

        return 0

    def check(self):
        new_imports = set(self.current_imports()) - set(self.previous_imports())
        new_deprecated_imports = set()

        for entry in new_imports:
            if self.is_latest_version(entry.name, entry.version):
                continue

            new_deprecated_imports.add(entry)

        status = 0

        if new_deprecated_imports:
            print("==== {} new deprecated imports".format(len(new_deprecated_imports)))
            print(
                "\n".join(
                    "{} (latest version: {})".format(
                        entry,
                        self.modules().latest_version(entry.name)
                        or "completely deprecated.",
                    )
                    for entry in new_deprecated_imports
                )
            )
            print()
            print(
                textwrap.fill(
                    textwrap.dedent(
                        """
                        If you meant to import a deprecated version, please run
                        `{}` and commit the changes to mark these deprecated
                        imports as acceptable. Otherwise, please upgrade to the
                        latest version (listed in each error above.)
                        """
                    ),
                    width=80,
                ).format(self.args.check_message_fix_command)
            )
            status = 1

        newly_removed = set(self.previous_imports()) - set(self.current_imports())
        if newly_removed:
            print()
            print("==== {} newly removed imports".format(len(newly_removed)))
            print("\n".join(str(entry) for entry in newly_removed))
            print()
            print(
                "good job! Please run `{}` and commit the output to make sure these don't come back".format(
                    self.args.check_message_fix_command
                )
            )
            status = 1

        return status

    def report(self):
        allowed_counts = {True: 0, False: 0}
        counts = defaultdict(lambda: {True: 0, False: 0})

        for entry in self.current_imports():
            allowed = self.is_latest_version(entry.name, entry.version)

            allowed_counts[allowed] += 1
            counts[entry.name][allowed] += 1

        try:
            widest = max(len(name) for name in counts)
        except ValueError:  # empty sequence
            widest = 1

        lines = []
        for name, alloweds in counts.items():
            total = alloweds[False] + alloweds[True]
            percent = alloweds[True] / total

            out = "{} {: 4} of {: 4} ({:> 8.2%}) imports use the latest version".format(
                name.ljust(widest), alloweds[True], total, percent
            )

            lines.append((percent, out))

        lines.sort()
        print("\n".join(line for (_, line) in lines))
        print()

        total = allowed_counts[False] + allowed_counts[True]
        percent = allowed_counts[True] / (total or 1)
        print(
            "in total, {} of {} ({:> 8.2%}) imports use the latest version".format(
                allowed_counts[True], total, percent
            )
        )
        return 0

    def previous_imports(self):
        if self._previous_imports is None:
            self._previous_imports = Imports.from_file(self.args.imports_file)

        return self._previous_imports

    def current_imports(self):
        if self._current_imports is None:
            self._current_imports = Imports.from_source_directories(
                [
                    os.path.join(os.path.dirname("elm.json"), directory)
                    for directory in self.elm_json.source_directories
                ],
                self.deprecated
            )

        return self._current_imports

    def modules(self):
        if self._modules is None:
            self._modules = NriUiModules()

        return self._modules


def parser():
    out = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    out.add_argument(
        "--check-message-fix-command",
        help="update command to show in check error messages",
        default=" ".join(sys.argv).replace("check", "update").replace("report", "update"),
    )
    out.add_argument(
        "--imports-file",
        help="where do we store acceptable deprecated imports?",
        default=os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            os.path.basename(__file__).replace(".py", ".csv"),
        ),
    )
    out.add_argument(
        '--deprecate',
        help="explicitly deprecate a module by name",
        action="append",
        default=[m for m in os.environ.get("DEPRECATED_MODULES", "").split(",") if m],
    )

    sub = out.add_subparsers()
    sub.required = True
    sub.dest = "command"

    sub.add_parser("update", help="update the acceptable import file")
    sub.add_parser(
        "check", help="check that we do not have any new instances of old modules"
    )
    sub.add_parser("report", help="print an import report")

    return out


if __name__ == "__main__":
    sys.exit(Main(parser().parse_args()).run())
