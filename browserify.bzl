def _browserify_impl(ctx: "context"):
    out = ctx.actions.declare_output(ctx.attrs.out or ctx.attrs.name)

    cmd = cmd_args([
        ctx.attrs.tool[RunInfo],
        "--entry", ctx.attrs.entry,
        "--outfile", out.as_output(),
    ])
    cmd.hidden(ctx.attrs.srcs)

    ctx.actions.run(cmd, category="browserify")

    return [DefaultInfo(default_output = out)]

browserify = rule(
    impl = _browserify_impl,
    attrs = {
        "out": attrs.option(attrs.string(), default=None),
        "entry": attrs.source(),
        "srcs": attrs.list(attrs.source()),
        "tool": attrs.dep(providers = [RunInfo], default="toolchains//:browserify"),
    }
)
