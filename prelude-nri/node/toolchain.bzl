NodeToolchainInfo = provider(fields=[
    "bin",
    "node",
])

def _node_toolchain_impl(ctx) -> [[DefaultInfo.type, NodeToolchainInfo.type]]:
    """
    A Node toolchain which you can source binaries from wherever it makes sense
    to you.
    """
    return [
        DefaultInfo(),
        NodeToolchainInfo(
            bin = ctx.attrs.bin[DefaultInfo].default_outputs,
            node = ctx.attrs.node[RunInfo],
        ),
    ]

node_toolchain = rule(
    impl = _node_toolchain_impl,
    attrs = {
        "bin": attrs.dep(
            default="prelude-nri//node:bin"
        ),
        "node": attrs.dep(
            providers = [RunInfo],
            default="prelude-nri//node:node"
        ),
    },
    is_toolchain_rule = True,
)
