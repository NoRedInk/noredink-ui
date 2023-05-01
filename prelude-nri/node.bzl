load("//node:toolchain.bzl", "NodeToolchainInfo")
load("@prelude//python:toolchain.bzl", "PythonToolchainInfo")

def _node_modules_impl(ctx: "context") -> [DefaultInfo.type]:
    out = ctx.actions.declare_output("node_modules")

    ctx.actions.run(
        [
            ctx.attrs._python_toolchain[PythonToolchainInfo].interpreter,
            ctx.attrs._node_toolchain[NodeToolchainInfo].build_node_modules[DefaultInfo].default_outputs,
            out.as_output(),
            "--package", ctx.attrs.package,
            "--package-lock", ctx.attrs.package_lock,
        ],
        category = "npm",
    )

    return [DefaultInfo(default_output = out)]

node_modules = rule(
    impl = _node_modules_impl,
    attrs = {
        "package": attrs.source(),
        "package_lock": attrs.source(),
        "_node_toolchain": attrs.toolchain_dep(
            default="toolchains//:node",
            providers=[NodeToolchainInfo]
        ),
        "_python_toolchain": attrs.toolchain_dep(
            default="toolchains//:python",
            providers=[PythonToolchainInfo]
        ),
    }
)
