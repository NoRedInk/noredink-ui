def _prettier_impl(ctx: "context"):
    suggested_changes = []

    tool = "prettier"
    if ctx.attrs.tool:
        tool = ctx.attrs.tool[RunInfo]

    for src in ctx.attrs.srcs:
        suggestion = ctx.actions.declare_output("__suggested_changes__", src.short_path)
        ctx.actions.run(
            [
                "bash",
                "-xo", "pipefail",
                "-c",
                """
                $1 $2 | diff -u $2 - > $3
                if test "$?" -eq 1; then
                  # we're fine with files being different; we'll catch that in
                  # the BXL script.
                  exit 0
                fi
                """,
                "--",
                tool,
                src,
                suggestion.as_output(),
            ],
            category = "prettier_diffs",
            identifier = src.short_path,
        )
        suggested_changes.append(suggestion)

    return [DefaultInfo(default_outputs = suggested_changes)]

prettier_diffs = rule(
    impl = _prettier_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "tool": attrs.dep(providers = [RunInfo]),
    }
)
