def _nix_binary_impl(ctx: "context"):
    binary = ctx.actions.declare_output(ctx.attrs.binary or ctx.attrs.name)

    cmd = cmd_args([
        "bash",
        "-xeuo", "pipefail",
        "-c",
        """
        PACKAGE="$(nix-build --no-out-link '<nixpkgs>' -A "${1:-}")"
        BINARY="${2:-}"
        ln -s "${PACKAGE}/bin/$(basename ${BINARY})" "$BINARY"
        """,
        "--",
        ctx.attrs.package or ctx.attrs.name,
        binary.as_output(),
    ])

    ctx.actions.run(cmd, category="nix_binary")

    return [
        DefaultInfo(default_output = binary),
        RunInfo(binary,)
    ]

nix_binary = rule(
    impl = _nix_binary_impl,
    attrs = {
        "package": attrs.option(attrs.string(), default=None),
        "binary": attrs.option(attrs.string(), default=None),
    }
)
