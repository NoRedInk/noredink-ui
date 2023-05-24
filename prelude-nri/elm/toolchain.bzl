ElmToolchainInfo = provider(fields = [
    "elm",
    "elm_format",
    "isolated_compile",
    "elm_test_workdir",
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
            elm_format = RunInfo(args = ["elm-format"]),
            isolated_compile = ctx.attrs._isolated_compile,
            elm_test_workdir = ctx.attrs._elm_test_workdir,
        ),
    ]

system_elm_toolchain = rule(
    impl = _system_elm_toolchain_impl,
    attrs = {
        "_isolated_compile": attrs.dep(default = "prelude-nri//elm:isolated_compile.py"),
        "_elm_test_workdir": attrs.dep(default = "prelude-nri//elm:elm_test_workdir.py"),
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
            elm_format = ctx.attrs.elm_format[RunInfo],
            isolated_compile = ctx.attrs._isolated_compile,
            elm_test_workdir = ctx.attrs._elm_test_workdir,
        ),
    ]

elm_toolchain = rule(
    impl = _elm_toolchain_impl,
    attrs = {
        "elm": attrs.exec_dep(
            providers = [RunInfo],
            default = "prelude-nri//elm:elm_compiler_binary",
        ),
        "elm_format": attrs.exec_dep(
            providers = [RunInfo],
            default = "prelude-nri//elm:elm_format_binary",
        ),
        "_isolated_compile": attrs.dep(default = "prelude-nri//elm:isolated_compile.py"),
        "_elm_test_workdir": attrs.dep(default = "prelude-nri//elm:elm_test_workdir.py"),
    },
    is_toolchain_rule = True,
)
