ElmToolchainInfo = provider(fields = [
    "elm",
    "isolated_compile",
])

def _system_elm_toolchain_impl(ctx) -> [[DefaultInfo.type, ElmToolchainInfo.type]]:
    """
    An Elm toolchain that assumes the current environment has all the binaries
    it needs in PATH.
    """
    return [
        DefaultInfo(),
        ElmToolchainInfo(
            elm = RunInfo(args = ["elm"]),
            isolated_compile = ctx.attrs._isolated_compile,
        ),
    ]

system_elm_toolchain = rule(
    impl = _system_elm_toolchain_impl,
    attrs = {
        "_isolated_compile": attrs.dep(default = "prelude-nri//elm:isolated_compile.py"),
    },
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
            isolated_compile = ctx.attrs._isolated_compile,
        ),
    ]

elm_toolchain = rule(
    impl = _elm_toolchain_impl,
    attrs = {
        "elm": attrs.dep(
            providers = [RunInfo],
            default = "prelude-nri//elm:elm_compiler_binary",
        ),
        "_isolated_compile": attrs.dep(default = "prelude-nri//elm:isolated_compile.py"),
    },
    is_toolchain_rule = True,
)
