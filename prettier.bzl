def _prettier_impl(ctx: "context"):
    suggested_changes = []

    for src in ctx.attrs.srcs:
        suggestion = ctx.actions.declare_output("__suggested_changes__", src.short_path)
        ctx.actions.run(
            [
                "bash",
                "-xo", "pipefail",
                "-c",
                """
                prettier $1 | diff -u $1 - > $2
                if test "$?" -eq 1; then
                  # we're fine with files being different; we'll catch that in
                  # the BXL script.
                  exit 0
                fi
                """,
                "--",
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
    }
)
