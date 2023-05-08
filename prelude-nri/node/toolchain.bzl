NodeToolchainInfo = provider(fields = [
    "bin_dir",
    "node",
    "build_node_modules",
    "build_npm_bin",
    "run_npm_script",
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
            build_npm_bin = ctx.attrs._build_npm_bin,
            run_npm_script = ctx.attrs._run_npm_script,
        ),
    ]

node_toolchain = rule(
    impl = _node_toolchain_impl,
    attrs = {
        "bin_dir": attrs.dep(
            default = "prelude-nri//node:bin",
        ),
        "node": attrs.dep(
            providers = [RunInfo],
            default = "prelude-nri//node:node",
        ),
        "_build_node_modules": attrs.dep(
            default = "prelude-nri//node:build_node_modules.py",
        ),
        "_build_npm_bin": attrs.dep(
            default = "prelude-nri//node:build_npm_bin.py",
        ),
        "_run_npm_script": attrs.dep(
            default = "prelude-nri//node:run_npm_script.py",
        ),
    },
    is_toolchain_rule = True,
)
