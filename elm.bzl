load("@toolchains//:elm.bzl", "ElmToolchainInfo")

def _elm_docs_impl(ctx: "context"):
    docs = ctx.actions.declare_output(ctx.attrs.out)

    cmd = cmd_args([ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm, "make", "--docs", docs.as_output()])
    cmd.hidden(ctx.attrs.elm_json, ctx.attrs.srcs)
    cmd.relative_to(ctx.attrs.elm_json, parent = 1)

    ctx.actions.run(cmd, category = "elm_docs")

    return [DefaultInfo(default_output = docs)]

elm_docs = rule(
    impl = _elm_docs_impl,
    attrs = {
        "out": attrs.string(default="docs.json"),
        "elm_json": attrs.source(),
        "srcs": attrs.list(attrs.source()),
        "_elm_toolchain": attrs.toolchain_dep(default="toolchains//:elm"),
    }
)

def _elm_app_impl(ctx: "context"):
    out = ctx.actions.declare_output(ctx.attrs.out)

    cmd = cmd_args([
        "bash",
        "-xeuo", "pipefail",
        "-c",
        """
        ELM="${1:-}"
        ELM_JSON="${PWD}/${2:-}"
        ELM_MAIN="${PWD}/${3:-}"
        OUT="${PWD}/${4:-}"
        shift 4
        ELM_ARGS="$@"

        cd "$(dirname "$ELM_JSON")"

        $ELM make "$ELM_MAIN" --output "$OUT" $ELM_ARGS
        """,
        "--",
        ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm,
        ctx.attrs.elm_json,
        ctx.attrs.main,
        out.as_output(),
    ])

    if ctx.attrs.debug:
        ctx.arg("--debug")

    if ctx.attrs.optimize:
        ctx.arg("--optimize")

    cmd.hidden(ctx.attrs.elm_json, ctx.attrs.srcs)
    ctx.actions.run(cmd, category = "elm_app")

    return [DefaultInfo(default_output = out)]

elm_app = rule(
    impl = _elm_app_impl,
    attrs = {
        "out": attrs.string(default="app.js"),
        "elm_json": attrs.source(),
        "main": attrs.source(),
        "srcs": attrs.list(attrs.source()),
        "debug": attrs.bool(default = False),
        "optimize": attrs.bool(default = False),
        "_elm_toolchain": attrs.toolchain_dep(default="toolchains//:elm"),
    }
)

def _elm_format_impl(ctx: "context"):
    suggested_changes = []

    for src in ctx.attrs.srcs:
        suggestion = ctx.actions.declare_output("__suggested_changes__", src.short_path)
        ctx.actions.run(
            [
                "bash",
                "-xo", "pipefail",
                "-c",
                """
                $1 --stdin < $2 | diff -u $2 - > $3
                if test "$?" -eq 1; then
                  # we're fine with files being different; we'll catch that in
                  # the BXL script.
                  exit 0
                fi
                """,
                "--",
                ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm_format,
                src,
                suggestion.as_output(),
            ],
            category = "elm_format_diffs",
            identifier = src.short_path,
        )
        suggested_changes.append(suggestion)

    return [DefaultInfo(default_outputs = suggested_changes)]

elm_format_diffs = rule(
    impl = _elm_format_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "_elm_toolchain": attrs.toolchain_dep(default="toolchains//:elm"),
    }
)

def _elm_test_impl(ctx: "context"):
    command = cmd_args([
        "bash",
        "-euo", "pipefail",
        "-c",
        """
        ELM="${1:-}"
        ELM_TEST="${2:-}"
        ELM_JSON="${3:-}"
        shift 3

        cd "$(dirname "$ELM_JSON")"
        "$ELM_TEST" --compiler "$ELM" $@
        """,
        "--",
        ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm,
        ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm_test,
        ctx.attrs.elm_json,
    ])
    command.add("--fuzz", str(ctx.attrs.fuzz))
    command.add(ctx.attrs.test_srcs)
    command.hidden(ctx.attrs.srcs)

    return [
        ExternalRunnerTestInfo(
            type = "elm",
            command = [command],
            run_from_project_root = False,
        ),
        DefaultInfo(),
    ]

elm_test = rule(
    impl = _elm_test_impl,
    attrs = {
        "elm_json": attrs.source(),
        "srcs": attrs.list(attrs.source()),
        "test_srcs": attrs.list(attrs.source()),
        "fuzz": attrs.int(default=100),
        "_elm_toolchain": attrs.toolchain_dep(default="toolchains//:elm"),
    }
)

def _elm_review_impl(ctx: "context"):
    command = cmd_args(ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm_review[RunInfo])
    command.add("--compiler", ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm)
    command.hidden(ctx.attrs.srcs, ctx.attrs.review_srcs)

    return [
        ExternalRunnerTestInfo(
            type = "elm_review",
            command = [command],
        ),
        DefaultInfo(),
    ]

elm_review = rule(
    impl = _elm_review_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "review_srcs": attrs.list(attrs.source()),
        "_elm_toolchain": attrs.toolchain_dep(default="toolchains//:elm"),
    }
)

def _elm_verify_examples_impl(ctx: "context"):
    command = cmd_args([
        ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm_verify_examples,
        "--run-tests",
    ])
    command.hidden(ctx.attrs.srcs)

    return [
        ExternalRunnerTestInfo(
            type = "elm_review",
            command = [command],
        ),
        DefaultInfo(),
    ]

elm_verify_examples = rule(
    impl = _elm_verify_examples_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "_elm_toolchain": attrs.toolchain_dep(default="toolchains//:elm"),
    }
)
