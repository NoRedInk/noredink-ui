ElmToolchainInfo = provider(fields=[
    "elm",
    "elm_format",
    "elm_test",
    "elm_review",
    "elm_verify_examples",
])

def _system_elm_toolchain_impl(_ctx):
    """
    An Elm toolchain that assumes the current environment has all the binaries
    it needs.
    """

    return [
        DefaultInfo(),
        ElmToolchainInfo(
            elm = RunInfo(args = ["elm"]),
            elm_format = RunInfo(args = ["elm-format"]),
            elm_test = RunInfo(args = ["elm-test"]),
            elm_review = RunInfo(args = ["elm-review"]),
            elm_verify_examples = RunInfo(args = ["elm-verify-examples", "--elm-test", "elm-test"]),
        ),
    ]


system_elm_toolchain = rule(
    impl = _system_elm_toolchain_impl,
    attrs = {},
    is_toolchain_rule = True,
)