# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Rule for the inplace pex builder, and some utility methods for generic pex builder
execution
"""

load("@prelude//:local_only.bzl", "package_python_locally")
load("@prelude//cxx:debug.bzl", "project_external_debug_info")
load(
    "@prelude//linking:link_info.bzl",
    "LinkedObject",  # @unused Used as a type
)
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load(":interface.bzl", "PythonLibraryManifestsInterface")
load(":manifest.bzl", "ManifestInfo")  # @unused Used as a type
load(":toolchain.bzl", "PackageStyle", "PythonToolchainInfo")

# This represents the input to the creation of a Pex. Manifests provide source
# files, extensions are native extensions, and compile indicates whether we
# should also include bytecode from manifests.
PexModules = record(
    manifests = field(PythonLibraryManifestsInterface.type),
    extensions = field([ManifestInfo.type, None], None),
    extra_manifests = field([ManifestInfo.type, None], None),
    compile = field(bool.type, False),
)

# The output of pex creation. It's everything needed to make the DefaultInfo and RunInfo
# providers.
PexProviders = record(
    default_output = field("artifact"),
    other_outputs = ["artifact", "_arglike"],
    sub_targets = {str.type: ["provider"]},
    run_cmd = cmd_args.type,
)

def make_pex_providers(pex: PexProviders.type) -> ["provider"]:
    return [
        DefaultInfo(
            default_output = pex.default_output,
            other_outputs = pex.other_outputs,
            sub_targets = pex.sub_targets,
        ),
        RunInfo(pex.run_cmd),
    ]

def _srcs(srcs: [""], format = "{}") -> "cmd_args":
    args = cmd_args()
    for src in srcs:
        args.add(cmd_args(src, format = format))
    return args

def _fail_at_build_time(
        ctx: "context",
        python_toolchain: "PythonToolchainInfo",
        msg: str.type) -> PexProviders.type:
    error_message = ctx.actions.write("__error_message", msg)
    dummy_output = ctx.actions.declare_output("__dummy_output")
    cmd = cmd_args([
        python_toolchain.fail_with_message,
        error_message,
        dummy_output.as_output(),
    ])
    ctx.actions.run(cmd, category = "par", identifier = "failure")
    return PexProviders(
        default_output = dummy_output,
        other_outputs = [],
        sub_targets = {},
        run_cmd = cmd_args(),
    )

def _fail(
        ctx: "context",
        python_toolchain: "PythonToolchainInfo",
        suffix: str.type,
        msg: str.type) -> PexProviders.type:
    if suffix:
        return _fail_at_build_time(ctx, python_toolchain, msg)

    # suffix is empty, which means this is the default subtarget. All failures must
    # occur at analysis time
    fail(msg)

# TODO(nmj): Resources
# TODO(nmj): Figure out how to harmonize these flags w/ existing make_xar
#                 invocations. It might be perfectly reasonable to just have a wrapper
#                 script that invokes make_xar in a slightly different way.
def make_pex(
        ctx: "context",
        python_toolchain: "PythonToolchainInfo",
        # A rule-provided tool to use to build the PEX.
        make_pex_cmd: [RunInfo.type, None],
        package_style: PackageStyle.type,
        build_args: ["_arglike"],
        pex_modules: PexModules.type,
        shared_libraries: {str.type: (LinkedObject.type, bool.type)},
        main_module: str.type,
        hidden_resources: [None, "_arglike"]) -> PexProviders.type:
    """
    Passes a standardized set of flags to a `make_pex` binary to create a python
    "executable".

    Arguments:
        - python_toolchain: Used to locate the PEX binaries.
        - package_style: How to package this binary. Might be controlled by the
          toolchain, but also by the rule.
        - build_args: Extra arguments to pass to the PEX binary.
        - pex_modules: Manifests for sources to package.
        - shared_libraries: Shared libraries to link in. Mapping of soname to
          artifact and whether they should be preloaded.
        - main_module: the name of the module to execute when running the
          resulting binary.
        - hidden_resources: extra resources the binary depends on.
    """

    preload_libraries = _preload_libraries_args(ctx, shared_libraries)
    manifest_module = generate_manifest_module(ctx, python_toolchain, pex_modules.manifests.src_manifests())
    common_modules_args, dep_artifacts = _pex_modules_common_args(
        ctx,
        pex_modules,
        {name: lib for name, (lib, _) in shared_libraries.items()},
    )

    default = _make_pex_impl(
        ctx,
        python_toolchain,
        make_pex_cmd,
        package_style,
        build_args,
        shared_libraries,
        preload_libraries,
        common_modules_args,
        dep_artifacts,
        main_module,
        hidden_resources,
        manifest_module,
        pex_modules,
        output_suffix = "",
    )
    for style in PackageStyle.values():
        pex_providers = default if style == package_style.value else _make_pex_impl(
            ctx,
            python_toolchain,
            make_pex_cmd,
            PackageStyle(style),
            build_args,
            shared_libraries,
            preload_libraries,
            common_modules_args,
            dep_artifacts,
            main_module,
            hidden_resources,
            manifest_module,
            pex_modules,
            output_suffix = "-{}".format(style),
        )
        default.sub_targets[style] = make_pex_providers(pex_providers)
    return default

def _make_pex_impl(
        ctx: "context",
        python_toolchain: "PythonToolchainInfo",
        make_pex_cmd: [RunInfo.type, None],
        package_style: PackageStyle.type,
        build_args: ["_arglike"],
        shared_libraries: {str.type: (LinkedObject.type, bool.type)},
        preload_libraries: "cmd_args",
        common_modules_args: "cmd_args",
        dep_artifacts: ["_arglike"],
        main_module: str.type,
        hidden_resources: [None, "_arglike"],
        manifest_module: [None, "_arglike"],
        pex_modules: PexModules.type,
        output_suffix: str.type) -> PexProviders.type:
    name = "{}{}".format(ctx.attrs.name, output_suffix)
    standalone = package_style == PackageStyle("standalone")

    runtime_files = []
    if standalone and hidden_resources != None:
        # constructing this error message is expensive, only do it when we abort analysis
        error_msg = "standalone builds don't support hidden resources" if output_suffix else _hidden_resources_error_message(ctx.label, hidden_resources)

        return _fail(ctx, python_toolchain, output_suffix, error_msg)
    if hidden_resources != None:
        runtime_files.extend(hidden_resources)

    if not (standalone or
            package_style == PackageStyle("inplace") or
            package_style == PackageStyle("inplace_lite")):
        fail("unsupported package style: {}".format(package_style))

    symlink_tree_path = None
    if standalone:
        if python_toolchain.make_pex_standalone == None:
            return _fail(
                ctx,
                python_toolchain,
                output_suffix,
                "Python toolchain does not provide make_pex_standalone",
            )

        # TODO: Other par_styles should migrate to using this manifest as well
        if ctx.attrs.par_style != "pex":
            # manifest generation is handled by make_pex_standalone (make_par), except for pex
            manifest_module = None
    else:
        symlink_tree_path = ctx.actions.declare_output("{}#link-tree".format(name), dir = True)
    if make_pex_cmd != None:
        manifest_module = None  # manifest generation is handled by make_pex_cmd

    modules_args = _pex_modules_args(
        ctx,
        common_modules_args,
        dep_artifacts,
        symlink_tree_path,
        manifest_module,
        pex_modules,
    )

    output = ctx.actions.declare_output("{}{}".format(name, python_toolchain.pex_extension))

    bootstrap_args = _pex_bootstrap_args(
        python_toolchain.interpreter,
        None,
        python_toolchain.host_interpreter,
        main_module,
        output,
        shared_libraries,
        preload_libraries,
        symlink_tree_path,
        package_style,
    )
    bootstrap_args.add(build_args)
    if package_style == PackageStyle("standalone"):
        bootstrap_args.add(ctx.attrs.standalone_build_args)
    else:
        bootstrap_args.add(ctx.attrs.inplace_build_args)

    if standalone:
        # We support building _standalone_ packages locally to e.g. support fbcode's
        # current style of build info stamping (e.g. T10696178).
        prefer_local = package_python_locally(ctx, python_toolchain)

        cmd = cmd_args(
            make_pex_cmd if make_pex_cmd != None else python_toolchain.make_pex_standalone,
        )
        cmd.add(modules_args)
        cmd.add(bootstrap_args)
        ctx.actions.run(cmd, prefer_local = prefer_local, category = "par", identifier = "standalone{}".format(output_suffix))

    else:
        runtime_files.extend(dep_artifacts)
        runtime_files.append(symlink_tree_path)
        if make_pex_cmd != None:
            cmd = cmd_args(make_pex_cmd)
            cmd.add(modules_args)
            cmd.add(bootstrap_args)
            ctx.actions.run(cmd, category = "par", identifier = "inplace{}".format(output_suffix))
        else:
            modules = cmd_args(python_toolchain.make_pex_modules)
            modules.add(modules_args)
            ctx.actions.run(modules, category = "par", identifier = "modules{}".format(output_suffix))

            bootstrap = cmd_args(python_toolchain.make_pex_inplace)
            bootstrap.add(bootstrap_args)
            ctx.actions.run(bootstrap, category = "par", identifier = "bootstrap{}".format(output_suffix))

    run_args = []

    # Windows can't run PAR directly.
    if ctx.attrs._exec_os_type[OsLookup].platform == "windows":
        run_args.append(ctx.attrs._python_toolchain[PythonToolchainInfo].interpreter)
    run_args.append(output)

    return PexProviders(
        default_output = output,
        other_outputs = runtime_files,
        sub_targets = {},
        run_cmd = cmd_args(run_args).hidden(runtime_files),
    )

def _preload_libraries_args(ctx: "context", shared_libraries: {str.type: (LinkedObject.type, bool.type)}) -> "cmd_args":
    preload_libraries_path = ctx.actions.write(
        "__preload_libraries.txt",
        cmd_args([
            "--preload={}".format(name)
            for name, (_, preload) in shared_libraries.items()
            if preload
        ]),
    )
    return cmd_args(preload_libraries_path, format = "@{}")

def _pex_bootstrap_args(
        python_interpreter: "_arglike",
        python_interpreter_flags: [None, str.type],
        python_host_interpreter: "_arglike",
        main_module: str.type,
        output: "artifact",
        shared_libraries: {str.type: (LinkedObject.type, bool.type)},
        preload_libraries: "cmd_args",
        symlink_tree_path: [None, "artifact"],
        package_style: PackageStyle.type) -> "cmd_args":
    cmd = cmd_args()
    cmd.add(preload_libraries)
    cmd.add([
        "--python",
        python_interpreter,
        "--host-python",
        python_host_interpreter,
        "--entry-point",
        main_module,
    ])
    if python_interpreter_flags:
        cmd.add("--python-interpreter-flags", python_interpreter_flags)
    if symlink_tree_path != None:
        cmd.add(cmd_args(["--modules-dir", symlink_tree_path]).ignore_artifacts())

    # Package style `inplace_lite` cannot be used with shared libraries
    if package_style == PackageStyle("inplace_lite") and not shared_libraries:
        cmd.add("--use-lite")
    cmd.add(output.as_output())

    return cmd

def _pex_modules_common_args(
        ctx: "context",
        pex_modules: PexModules.type,
        shared_libraries: {str.type: LinkedObject.type}) -> ("cmd_args", ["_arglike"]):
    srcs = []
    src_artifacts = []

    srcs.extend(pex_modules.manifests.src_manifests())
    src_artifacts.extend(pex_modules.manifests.src_artifacts())

    if pex_modules.extensions:
        srcs.append(pex_modules.extensions.manifest)
        src_artifacts.extend([a for a, _ in pex_modules.extensions.artifacts])

    if pex_modules.extra_manifests:
        srcs.append(pex_modules.extra_manifests.manifest)
        src_artifacts.extend([a for a, _ in pex_modules.extra_manifests.artifacts])

    resources = pex_modules.manifests.resource_manifests()
    resource_artifacts = pex_modules.manifests.resource_artifacts()

    src_manifests_path = ctx.actions.write(
        "__src_manifests.txt",
        _srcs(srcs, format = "--module-manifest={}"),
    )
    resource_manifests_path = ctx.actions.write(
        "__resource_manifests.txt",
        _srcs(resources, format = "--resource-manifest={}"),
    )

    native_libraries = [s.output for s in shared_libraries.values()]
    native_library_srcs_path = ctx.actions.write(
        "__native_libraries___srcs.txt",
        _srcs(native_libraries, format = "--native-library-src={}"),
    )
    native_library_dests_path = ctx.actions.write(
        "__native_libraries___dests.txt",
        ["--native-library-dest={}".format(lib) for lib in shared_libraries],
    )

    src_manifest_args = cmd_args(src_manifests_path).hidden(srcs)
    resource_manifest_args = cmd_args(resource_manifests_path).hidden(resources)
    native_library_srcs_args = cmd_args(native_library_srcs_path)

    cmd = cmd_args()
    cmd.add(cmd_args(src_manifest_args, format = "@{}"))
    cmd.add(cmd_args(resource_manifest_args, format = "@{}"))
    cmd.add(cmd_args(native_library_srcs_args, format = "@{}"))
    cmd.add(cmd_args(native_library_dests_path, format = "@{}"))

    dwp = []
    if ctx.attrs.package_split_dwarf_dwp:
        dwp = [s.dwp for s in shared_libraries.values() if s.dwp != None]
        dwp_srcs_path = ctx.actions.write(
            "__dwp___srcs.txt",
            _srcs(dwp, format = "--dwp-src={}"),
        )
        dwp_dests_path = ctx.actions.write(
            "__dwp___dests.txt",
            ["--dwp-dest={}.dwp".format(lib) for lib, s in shared_libraries.items() if s.dwp != None],
        )
        dwp_srcs_args = cmd_args(dwp_srcs_path)
        cmd.add(cmd_args(dwp_srcs_args, format = "@{}"))
        cmd.add(cmd_args(dwp_dests_path, format = "@{}"))
    deps = (src_artifacts +
            resource_artifacts +
            native_libraries +
            dwp +
            project_external_debug_info(
                ctx.actions,
                label = ctx.label,
                infos = [lib.external_debug_info for lib in shared_libraries.values()],
            ))
    return (cmd, deps)

def _pex_modules_args(
        ctx: "context",
        common_args: "cmd_args",
        dep_artifacts: ["_arglike"],
        symlink_tree_path: [None, "artifact"],
        manifest_module: ["_arglike", None],
        pex_modules: PexModules.type) -> "cmd_args":
    """
    Produces args to deal with a PEX's modules. Returns args to pass to the
    modules builder, and artifacts the resulting modules would require at
    runtime (this might be empty for e.g. a standalone pex).
    """

    cmd = cmd_args()
    cmd.add(common_args)

    if manifest_module != None:
        cmd.add(cmd_args(manifest_module, format = "--module-manifest={}"))

    if symlink_tree_path != None:
        cmd.add(["--modules-dir", symlink_tree_path.as_output()])
    else:
        if pex_modules.compile:
            bytecode_manifests = pex_modules.manifests.bytecode_manifests()
            bytecode_manifests_path = ctx.actions.write(
                "__bytecode_manifests.txt",
                _srcs(
                    bytecode_manifests,
                    format = "--module-manifest={}",
                ),
            )
            cmd.add(cmd_args(bytecode_manifests_path, format = "@{}"))
            cmd.hidden(bytecode_manifests)
            dep_artifacts.extend(pex_modules.manifests.bytecode_artifacts())

        # Accumulate all the artifacts we depend on. Only add them to the command
        # if we are not going to create symlinks.
        cmd.hidden(dep_artifacts)

    return cmd

def _hidden_resources_error_message(current_target: "label", hidden_resources) -> str.type:
    """
    Friendlier error message about putting non-python resources into standalone bins
    """
    owner_to_artifacts = {}

    for resource_set in hidden_resources:
        for resources in resource_set.traverse():
            for r in resources:
                # TODO: `r` is sometimes a tset projection, but it shouldn't be
                if getattr(r, "is_source", True):
                    # Source files; do a string repr so that we get the
                    # package path in there too
                    owner_to_artifacts.setdefault("", []).append(str(r))
                else:
                    owner_to_artifacts.setdefault(r.owner, []).append(r.short_path)

    msg = (
        "Cannot package hidden srcs/resources in a standalone python_binary. " +
        'Eliminate resources in non-Python dependencies of this python binary, use `package_style = "inplace"`, ' +
        'use `strip_mode="full"` or turn off Split DWARF `-c fbcode.split-dwarf=false` on C++ binary resources.\n'
    )

    for (rule, resources) in owner_to_artifacts.items():
        if rule != "":
            msg += "Hidden srcs/resources for {}\n".format(rule)
        else:
            msg += "Source files:\n"
            msg += "Find the reason this file was included with `buck2 cquery 'allpaths({}, owner(%s))' <file paths>`\n".format(current_target.raw_target())
        for resource in sorted(resources):
            msg += "  {}\n".format(resource)
    return msg

def generate_manifest_module(
        ctx: "context",
        python_toolchain: PythonToolchainInfo.type,
        src_manifests: ["_arglike"]) -> ["_arglike", None]:
    """
    Generates a __manifest__.py module, and an extra entry to add to source manifests.

    The contents of the manifest are taken from an attribute if it exists. If the
    attribute is None, this function does nothing.
    """

    if ctx.attrs.manifest_module_entries == None:
        return None
    module = ctx.actions.declare_output("manifest/__manifest__.py")
    entries_json = ctx.actions.write_json("manifest/entries.json", ctx.attrs.manifest_module_entries)
    src_manifests_path = ctx.actions.write(
        "__module_manifests.txt",
        _srcs(src_manifests, format = "--module-manifest={}"),
    )
    cmd = cmd_args(python_toolchain.make_pex_manifest_module)
    cmd.add(["--manifest-entries", entries_json])
    cmd.add(cmd_args(src_manifests_path, format = "@{}"))
    cmd.hidden(src_manifests)
    cmd.add(["--output", module.as_output()])
    ctx.actions.run(cmd, category = "par", identifier = "manifest-module")

    src_manifest = ctx.actions.write_json("manifest/module_manifest.json", [["__manifest__.py", module, "foo"]], with_inputs = True)

    return src_manifest
