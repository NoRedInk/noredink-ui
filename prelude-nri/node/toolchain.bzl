NodeToolchainInfo = provider(fields=[
    "bin",
    "node",
    "build_node_modules",
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
            build_node_modules = ctx.attrs._build_node_modules,
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
        "_build_node_modules": attrs.dep(
            default="prelude-nri//node:build_node_modules.py",
        ),
    },
    is_toolchain_rule = True,
)
