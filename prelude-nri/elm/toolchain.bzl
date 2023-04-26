ElmToolchainInfo = provider(fields=[
    "elm",
])

def _system_elm_toolchain_impl(_ctx) -> [[DefaultInfo.type, ElmToolchainInfo.type]]:
    """
    An Elm toolchain that assumes the current environment has all the binaries
    it needs in PATH.
    """
    return [
        DefaultInfo(),
        ElmToolchainInfo(
            elm = RunInfo(args = ["elm"]),
        ),
    ]


system_elm_toolchain = rule(
    impl = _system_elm_toolchain_impl,
    attrs = {},
    is_toolchain_rule = True,
)

def _elm_toolchain_impl(ctx: "context") -> [[DefaultInfo.type, ElmToolchainInfo.type]]:
    """
    An Elm toolchain which you can source binaries from wherever it makes sense
    to you.
    """
    return [
        DefaultInfo(),
        ElmToolchainInfo(
            elm = ctx.attrs.elm[RunInfo],
        ),
    ]

elm_toolchain = rule(
    impl = _elm_toolchain_impl,
    attrs = {
        "elm": attrs.dep(providers = [RunInfo]),
    },
    is_toolchain_rule = True,
)
