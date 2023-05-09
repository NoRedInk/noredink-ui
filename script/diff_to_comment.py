#!/usr/bin/env python
"""
Run Buck targets and present changes in a structured format.
"""
import argparse
import http.client
import json
import json
import os
import re
import subprocess
import sys
import textwrap


def graphql(api_key, query, variables=None):
    request_body = {"query": query}
    if variables:
        request_body["variables"] = variables

    connection = http.client.HTTPSConnection("api.github.com")

    connection.request(
        "POST",
        "/graphql",
        body=json.dumps(request_body),
        headers={"Authorization": f"Bearer {api_key}", "User-Agent": "Python"},
    )
    response = connection.getresponse()

    response_body_json = response.read().decode("utf-8")
    response_body = json.loads(response_body_json)

    connection.close()

    return response_body


class DiffHunk:
    def __init__(self, name, start_line, hunk):
        self.name = name
        self.start_line = start_line
        self.hunk = hunk

    def __repr__(self):
        return f"<DiffHunk: {self.name} at line {self.start_line}>"

    def suggestion(self):
        lines = [
            {
                "offset": i,
                "sigil": line[0],
                "line": line[1:],
            }
            for (i, line) in enumerate(self.hunk.decode("utf-8").split("\n"))
        ]

        context_lines = 0
        for line in lines:
            if line["sigil"] == " ":
                context_lines += 1
            else:
                break

        suggestion_lines = lines[context_lines:-context_lines]

        # we keep track of the amount of lines we added, since we're targeting
        # the final line to be calculated on the right-hand side of the GitHub
        # diff, so we need to subtract from the offset we have.
        old_lines = []
        new_lines = []

        for line in suggestion_lines:
            if line["sigil"] == " ":
                old_lines.append(line)
                new_lines.append(line)

            elif line["sigil"] == "+":
                new_lines.append(line)

            elif line["sigil"] == "-":
                old_lines.append(line)

            else:
                raise Exception(f"Unknown sigil: {line['sigil']}")

        start_line = self.start_line + old_lines[0]["offset"]
        # subtracting one so as not to double-count the first line
        end_line = start_line + len(old_lines) - 1

        return (
            start_line,
            end_line,
            "\n".join(line["line"] for line in new_lines),
        )


class File:
    def __init__(self, name, diff):
        self.name = name
        self.diff = diff

    def __repr__(self):
        return f"<File: {self.name}>"

    def __bytes__(self):
        return self.diff

    def hunks(self):
        matches = re.finditer(
            b"@@ -\d+,\d+ \+(?P<start_line>\d+),\d+ @@\n(?P<hunk>.+?)((?=\n@@)|$)",
            self.diff,
            re.DOTALL,
        )
        for match in matches:
            parts = match.groupdict()

            yield DiffHunk(
                name=self.name,
                start_line=int(parts["start_line"]),
                hunk=parts["hunk"],
            )

    @classmethod
    def from_universal_diff(cls, diff):
        return cls(re.match(b"--- (.+?)\t", diff).groups()[0], diff)


class Report:
    def __init__(self):
        self.files = []

    def __repr__(self):
        return f"<Report: {self.files}>"

    def __bytes__(self):
        return b"\n".join(bytes(file) for file in self.files)

    def append(self, diff):
        self.files.append(File.from_universal_diff(diff))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "target",
        nargs="+",
        help="Target(s) to build and search for files.",
    )
    parser.add_argument(
        "--buck2-bin",
        default="buck2",
        help="where does `buck2` live?",
    )
    parser.add_argument(
        "--call-fix-command-base",
        default=sys.argv[0],
        help="How should someone call this script to add the `--fix` option?"
    )

    # FIX
    parser.add_argument(
        "--fix", action="store_true", help="Automatically fix all formatting errors."
    )
    parser.add_argument(
        "--patch-bin",
        default="patch",
        help="where does `patch` live? (only necessary with `--fix`)",
    )

    # GITHUB PR REVIEW
    parser.add_argument(
        "--review-github-pr",
        action="store_true",
        help="Leave a PR review on GitHub with suggested changes from this diff.",
    )
    parser.add_argument(
        "--github-token",
        default=os.environ.get("GITHUB_TOKEN", None),
        help="What GitHub token to use (only necessary with `--review-github-pr`. Reads from `GITHUB_TOKEN` if present.)",
    )
    parser.add_argument(
        "--github-repo",
        help="The repo that owns the PR that will get the comment. (Format: person-or-org/repo-name)",
    )
    parser.add_argument(
        "--github-pr-number",
        type=int,
        help="The PR number.",
    )

    args = parser.parse_args()

    report = json.loads(
        subprocess.check_output([args.buck2_bin, "build", "--build-report=-"] + args.target)
    )

    out = Report()

    for target, result in report["results"].items():
        diffs = result["outputs"]["DEFAULT"]

        for diff in diffs:
            with open(diff, "rb") as fh:
                content = fh.read()

            if content:
                out.append(content)

    file_or_files = "file" if len(out.files) == 1 else "files"

    if args.fix:
        subprocess.run(
            [args.patch_bin, "-p0"],
            check=True,
            input=b"\n".join(bytes(file) for file in out.files),
        )

    elif args.review_github_pr:
        if not args.github_repo:
            sys.stdout.write(
                "Please specify --github-repo to say which repo's PR to make a comment on.\n"
            )
            sys.exit(1)

        if not args.github_pr_number:
            sys.stdout.write(
                "Please specify --github-pr-number to say which PR to comment on.\n"
            )
            sys.exit(1)

        if not out.files:
            sys.exit(0)

        sys.stdout.write(
            f"Getting mutation ID for {args.github_repo}#{args.github_pr_number}\n"
        )

        org, repo = args.github_repo.split("/")

        id_resp = graphql(
            args.github_token,
            textwrap.dedent(
                """
                query ($org: String!, $repo: String!, $pullRequestNumber: Int!) {
                  organization(login: $org) {
                    repository(name: $repo) {
                      pullRequest(number: $pullRequestNumber) {
                        id
                      }
                    }
                  }
                }
            """
            ),
            {
                "org": org,
                "repo": repo,
                "pullRequestNumber": args.github_pr_number,
            },
        )

        try:
            id = id_resp["data"]["organization"]["repository"]["pullRequest"]["id"]
        except (KeyError, TypeError):
            sys.stdout.write(
                f"Could not get the pull request ID from the GraphQL query. Here's the response so you can debug:\n\n{json.dumps(id_resp, indent=2)}"
            )
            sys.exit(1)

        threads = []
        for file in out.files:
            for hunk in file.hunks():
                start_line, end_line, suggestion = hunk.suggestion()

                thread = {
                    "path": file.name.decode("utf-8"),
                    "line": end_line,
                    "side": "RIGHT",
                    "body": f"Formatting suggestion:\n\n```suggestion\n{suggestion}\n```\n\n✨ 🎨 ✨",
                }

                if start_line != end_line:
                    thread["startLine"] = start_line
                    thread["startSide"] = "RIGHT"

                threads.append(thread)

        params = {
            "pullRequestId": id,
            "body": f"🤖 I have suggestions for {len(out.files)} {file_or_files}. Run `{args.call_fix_command_base} --fix {' '.join(args.target)}` in your local checkout to fix these, or accept the suggestions attached to this review comment. Have a very stylish day!",
            "event": "REQUEST_CHANGES",
            "threads": threads,
        }

        comment_resp = graphql(
            args.github_token,
            textwrap.dedent(
                """
                    mutation ($params: AddPullRequestReviewInput!) {
                      addPullRequestReview(input: $params) {
                        pullRequestReview {
                          url
                        }
                      }
                    }
                """
            ),
            {"params": params},
        )

        try:
            url = comment_resp["data"]["addPullRequestReview"]["pullRequestReview"][
                "url"
            ]
        except (KeyError, TypeError):
            sys.stdout.write(
                f"Could not get the pull request URL from the GraphQL query. Here's the response so you can debug:\n\n{json.dumps(comment_resp, indent=2)}"
            )
            sys.exit(1)

        sys.stdout.write(f"{url}\n")
        # not exiting 1 here beacuse we're giving feedback in a different way!

    elif out.files:
        for file in out.files:
            print(file.diff.decode("utf-8"))

        sys.stderr.write(
            f"I have fixes for {len(out.files)} {file_or_files}! Re-run as `{args.call_fix_command_base} --fix {' '.join(args.target)}` to fix these!\n"
        )
        sys.exit(1)
