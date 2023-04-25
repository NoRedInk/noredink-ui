def _nix_binary_impl(ctx: "context"):
    binary = ctx.actions.declare_output(ctx.attrs.binary or ctx.attrs.name)

    cmd = cmd_args([
        "bash",
        "-xeuo", "pipefail",
        "-c",
        """
        PACKAGE="$(nix-build --no-out-link "${1:-}" -A "${2:-}")"
        BINARY="${3:-}"

        if ! test -e "$BINARY"; then
          echo "the package exists but the binary does not. Here's what I have:"
          echo
          ls -alhR "${PACKAGE}"
          exit 1
        fi

        ln -s "${PACKAGE}/bin/$(basename ${BINARY})" "$BINARY"
        """,
        "--",
        ctx.attrs.nixpkgs,
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
        "nixpkgs": attrs.source(),
    }
)
