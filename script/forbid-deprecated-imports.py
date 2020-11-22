#!/usr/bin/env python
import json
import os.path as path
import re
import subprocess

HERE = path.realpath(path.abspath(path.join(path.dirname(__file__), '..')))

with open(path.join(HERE, 'elm.json'), 'r') as fh:
    modules = json.load(fh)["exposed-modules"]

widgets = {}

for module in modules:
    match = re.match(r'^(.+)\.V(\d+)$', module)
    if match is None:
        continue

    widget, current_version = match.groups()
    current_version = int(current_version)

    if widget in widgets:
        highest_version, rest = widgets[widget]
        if current_version > highest_version:
            rest.append(highest_version)
            widgets[widget] = (current_version, rest)
        else:
            rest.append(current_version)
    else:
        widgets[widget] = (current_version, [])

for (widget, (latest, deprecated)) in widgets.items():
    for version in deprecated:
        subprocess.check_call([
            'elm-forbid-import',
            'forbid',
            '{}.V{}'.format(widget, version),
            '--hint',
            'latest is V{}'.format(latest),
        ])
