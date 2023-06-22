NixToolchainInfo = provider(fields = [
    "nix",
])

def _system_nix_toolchain_impl(ctx) -> [[DefaultInfo.type, NixToolchainInfo.type]]:
    return [
        DefaultInfo(),
        NixToolchainInfo(
            nix = RunInfo(args = ["nix"]),
        ),
    ]

system_nix_toolchain = rule(
    impl = _system_nix_toolchain_impl,
    attrs = {},
    is_toolchain_rule = True,
)
