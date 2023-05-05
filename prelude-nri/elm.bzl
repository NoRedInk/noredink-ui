load("//elm:toolchain.bzl", "ElmToolchainInfo")
load("@prelude//python:toolchain.bzl", "PythonToolchainInfo")

def _elm_docs_impl(ctx: "context") -> [DefaultInfo.type]:
    build = ctx.actions.declare_output("build", dir = True)
    docs = ctx.actions.declare_output(ctx.attrs.out)

    elm_toolchain = ctx.attrs._elm_toolchain[ElmToolchainInfo]

    cmd = cmd_args(
        ctx.attrs._python_toolchain[PythonToolchainInfo].interpreter,
        elm_toolchain.isolated_compile[DefaultInfo].default_outputs,
        ctx.attrs.elm_json,
        "--build-dir",
        build.as_output(),
        "--elm-compiler",
        elm_toolchain.elm,
        "--verbose",
        "docs",
        "--out",
        docs.as_output(),
        "--src",
        ctx.attrs.src,
    )

    ctx.actions.run(
        cmd,
        category = "elm",
        no_outputs_cleanup = True,
    )

    return [DefaultInfo(default_output = docs)]

elm_docs = rule(
    impl = _elm_docs_impl,
    attrs = {
        "out": attrs.string(default = "docs.json"),
        "elm_json": attrs.source(),
        "src": attrs.source(allow_directory = True),
        "_elm_toolchain": attrs.toolchain_dep(
            default = "toolchains//:elm",
            providers = [ElmToolchainInfo],
        ),
        "_python_toolchain": attrs.toolchain_dep(
            default = "toolchains//:python",
            providers = [PythonToolchainInfo],
        ),
    },
)

def _elm_app_impl(ctx: "context") -> [DefaultInfo.type]:
    build = ctx.actions.declare_output("build", dir = True)
    out = ctx.actions.declare_output(ctx.attrs.out)

    elm_toolchain = ctx.attrs._elm_toolchain[ElmToolchainInfo]

    cmd = cmd_args(
        ctx.attrs._python_toolchain[PythonToolchainInfo].interpreter,
        elm_toolchain.isolated_compile[DefaultInfo].default_outputs,
        ctx.attrs.elm_json,
        "--build-dir",
        build.as_output(),
        "--elm-compiler",
        elm_toolchain.elm,
        "--verbose",
        "make",
        ctx.attrs.main,
        "--output",
        out.as_output(),
    )

    for (name, value) in ctx.attrs.source_directories.items():
        cmd.add(cmd_args(value, format = "--source-directory=" + name + "={}"))

    if ctx.attrs.debug and ctx.attrs.optimize:
        fail("Only one of `optimize` and `debug` may be true!")

    if ctx.attrs.debug:
        cmd.add("--debug")

    if ctx.attrs.optimize:
        cmd.add("--optimize")

    ctx.actions.run(
        cmd,
        category = "elm",
        no_outputs_cleanup = True,
    )

    return [DefaultInfo(default_output = out)]

elm_app = rule(
    impl = _elm_app_impl,
    attrs = {
        "out": attrs.string(default = "app.js"),
        "elm_json": attrs.source(),
        "main": attrs.source(),
        "source_directories": attrs.dict(
            attrs.string(),
            attrs.source(allow_directory = True),
        ),
        "debug": attrs.bool(default = False),
        "optimize": attrs.bool(default = False),
        "_elm_toolchain": attrs.toolchain_dep(
            default = "toolchains//:elm",
            providers = [ElmToolchainInfo],
        ),
        "_python_toolchain": attrs.toolchain_dep(
            default = "toolchains//:python",
            providers = [PythonToolchainInfo],
        ),
    },
)

def _elm_format_diffs_impl(ctx: "context"):
    suggested_changes = []

    for src in ctx.attrs.srcs:
        suggestion = ctx.actions.declare_output("__suggested_changes__", src.short_path)
        ctx.actions.run(
            [
                "bash",
                "-xuo", "pipefail",
                "-c",
                """
                $ELM_FORMAT --stdin < $SRC | diff -u${CONTEXT_SIZE} $SRC - > $OUT
                if test "$?" -eq 1; then
                  # we're fine with files being different; we'll catch that in
                  # the BXL script.
                  exit 0
                fi
                """,
            ],
            category = "elm_format_diffs",
            identifier = src.short_path,
            env = {
                "ELM_FORMAT": ctx.attrs._elm_toolchain.get(ElmToolchainInfo).elm_format,
                "SRC": src,
                "OUT": suggestion.as_output(),
                "CONTEXT_SIZE": str(ctx.attrs.context_size),
            },
        )
        suggested_changes.append(suggestion)

    return [DefaultInfo(default_outputs = suggested_changes)]

elm_format_diffs = rule(
    impl = _elm_format_diffs_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "context_size": attrs.int(default = 3),
        "_elm_toolchain": attrs.toolchain_dep(
            default="toolchains//:elm",
            providers = [ElmToolchainInfo],
        ),
    }
)