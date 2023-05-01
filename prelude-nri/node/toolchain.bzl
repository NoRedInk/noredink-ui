NodeToolchainInfo = provider(fields=[
    "bin_dir",
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
            bin_dir = ctx.attrs.bin_dir,
            node = ctx.attrs.node[RunInfo],
            build_node_modules = ctx.attrs._build_node_modules,
        ),
    ]

node_toolchain = rule(
    impl = _node_toolchain_impl,
    attrs = {
        "bin_dir": attrs.dep(
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
