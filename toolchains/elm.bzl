ElmToolchainInfo = provider(fields=[
    "elm",
    # "elm_format",
    "elm_test",
    # "elm_review",
    # "elm_verify_examples",
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
            # elm_format = RunInfo(args = ["elm-format"]),
            elm_test = RunInfo(args = ["elm-test"]),
            # elm_review = RunInfo(args = ["elm-review"]),
            # elm_verify_examples = RunInfo(args = ["elm-verify-examples", "--elm-test", "elm-test"]),
        ),
    ]


system_elm_toolchain = rule(
    impl = _system_elm_toolchain_impl,
    attrs = {},
    is_toolchain_rule = True,
)

def _elm_toolchain_impl(ctx: "context"):
    """
    An Elm toolchain which you can source binaries from wherever it makes sense
    to you.
    """
    elm_test = None
    if ctx.attrs.elm_test:
        elm_test = ctx.attrs.elm_test[RunInfo]

    return [
        DefaultInfo(),
        ElmToolchainInfo(
            elm = ctx.attrs.elm[RunInfo],
            # elm_format = ctx.attrs.elm_format,
            elm_test = elm_test,
            # elm_review = ctx.attrs.elm_review,
            # elm_verify_examples = ctx.attrs.elm_verify_examples,
        ),
    ]

elm_toolchain = rule(
    impl = _elm_toolchain_impl,
    attrs = {
        "elm": attrs.dep(providers = [RunInfo]),
        # "elm_format": attrs.dep(),
        "elm_test": attrs.option(attrs.dep(providers = [RunInfo]), default = None),
        # "elm_review": attrs.dep(),
        # "elm_verify_examples": attrs.dep(),
    },
    is_toolchain_rule = True,
)