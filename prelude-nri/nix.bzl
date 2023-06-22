def _nix_bin_impl(ctx) -> [[DefaultInfo.type, RunInfo.type]]:
    bin_name = ctx.attrs.bin_name or ctx.attrs.name
    package_name = "{}#{}".format(
        ctx.attrs.source,
        ctx.attrs.package_name or ctx.attrs.name
    )

    out = ctx.actions.declare_output(bin_name)

    cmd = cmd_args("nix", "shell")
    cmd.add(package_name)
    cmd.add(
        "-c",
        "bash",
        "-c",
        cmd_args(out.as_output(), format = "ln -s $(which {}) {{}}".format(bin_name)),
    )

    ctx.actions.run(
        cmd,
        category = "nix",
    )

    return [
        DefaultInfo(default_output = out),
        RunInfo(out),
    ]

nix_bin = rule(
    impl = _nix_bin_impl,
    attrs = {
        "bin_name": attrs.option(attrs.string(), default = None),
        "package_name": attrs.option(attrs.string(), default = None),
        "source": attrs.string(default = "nixpkgs"),
    }
)
