# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:local_only.bzl", "link_cxx_binary_locally")
load(
    "@prelude//:resources.bzl",
    "create_resource_db",
    "gather_resources",
)
load(
    "@prelude//apple:apple_frameworks.bzl",
    "build_link_args_with_deduped_framework_flags",
    "create_frameworks_linkable",
    "get_frameworks_link_info_by_deduping_link_infos",
)
load(
    "@prelude//cxx:cxx_bolt.bzl",
    "cxx_use_bolt",
)
load(
    "@prelude//ide_integrations:xcode.bzl",
    "XCODE_DATA_SUB_TARGET",
    "XcodeDataInfo",
    "generate_xcode_data",
)
load(
    "@prelude//linking:link_groups.bzl",
    "gather_link_group_libs",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkInfo",
    "LinkOrdering",  # @unused Used as a type
    "LinkStyle",
    "Linkage",
    "LinkedObject",  # @unused Used as a type
    "ObjectsLinkable",
    "merge_link_infos",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
    "get_linkable_graph_node_map_func",
)
load(
    "@prelude//linking:linkables.bzl",
    "linkables",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "merge_shared_libraries",
    "traverse_shared_library_info",
)
load(
    "@prelude//utils:utils.bzl",
    "flatten_dict",
    "map_val",
)
load(
    ":comp_db.bzl",
    "CxxCompilationDbInfo",  # @unused Used as a type
    "create_compilation_database",
    "make_compilation_db_info",
)
load(
    ":compile.bzl",
    "compile_cxx",
    "create_compile_cmds",
)
load(":cxx_context.bzl", "get_cxx_platform_info", "get_cxx_toolchain_info")
load(
    ":cxx_library_utility.bzl",
    "ARGSFILES_SUBTARGET",
    "cxx_attr_deps",
    "cxx_attr_link_style",
    "cxx_attr_linker_flags",
    "cxx_attr_resources",
    "cxx_is_gnu",
)
load(
    ":cxx_link_utility.bzl",
    "executable_shared_lib_arguments",
)
load(
    ":cxx_types.bzl",
    "CxxRuleConstructorParams",  # @unused Used as a type
)
load(
    ":debug.bzl",
    "ExternalDebugInfoTSet",
    "maybe_external_debug_info",
    "project_external_debug_info",
)
load(
    ":link.bzl",
    "CxxLinkResultType",
    "CxxLinkerMapData",
    "cxx_link",
)
load(
    ":link_groups.bzl",
    "LINK_GROUP_MAPPINGS_FILENAME_SUFFIX",
    "LINK_GROUP_MAPPINGS_SUB_TARGET",
    "LINK_GROUP_MAP_DATABASE_SUB_TARGET",
    "create_link_groups",
    "find_relevant_roots",
    "get_filtered_labels_to_links_map",
    "get_filtered_links",
    "get_filtered_targets",
    "get_link_group",
    "get_link_group_map_json",
    "get_link_group_preferred_linkage",
)
load(
    ":preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
    "cxx_private_preprocessor_info",
)

CxxExecutableOutput = record(
    binary = "artifact",
    dwp = field(["artifact", None]),
    # Files that will likely need to be included as .hidden() arguments
    # when executing the executable (ex. RunInfo())
    runtime_files = ["_arglike"],
    sub_targets = {str.type: [DefaultInfo.type]},
    # The LinkArgs used to create the final executable in 'binary'.
    link_args = [LinkArgs.type],
    # External components needed to debug the executable.
    external_debug_info = [ExternalDebugInfoTSet.type, None],
    shared_libs = {str.type: LinkedObject.type},
    # All link group links that were generated in the executable.
    auto_link_groups = field({str.type: LinkedObject.type}, {}),
    compilation_db = CxxCompilationDbInfo.type,
    xcode_data = XcodeDataInfo.type,
    linker_map_data = CxxLinkerMapData.type,
)

def cxx_executable(ctx: "context", impl_params: CxxRuleConstructorParams.type, is_cxx_test: bool.type = False) -> CxxExecutableOutput.type:
    # Gather preprocessor inputs.
    preprocessor_deps = cxx_attr_deps(ctx) + filter(None, [ctx.attrs.precompiled_header])
    (own_preprocessor_info, test_preprocessor_infos) = cxx_private_preprocessor_info(
        ctx,
        impl_params.headers_layout,
        raw_headers = ctx.attrs.raw_headers,
        extra_preprocessors = impl_params.extra_preprocessors,
        non_exported_deps = preprocessor_deps,
        is_test = is_cxx_test,
    )
    inherited_preprocessor_infos = cxx_inherited_preprocessor_infos(preprocessor_deps) + impl_params.extra_preprocessors_info

    # The link style to use.
    link_style = cxx_attr_link_style(ctx)

    sub_targets = {}

    # Compile objects.
    compile_cmd_output = create_compile_cmds(
        ctx,
        impl_params,
        [own_preprocessor_info] + test_preprocessor_infos,
        inherited_preprocessor_infos,
    )
    cxx_outs = compile_cxx(ctx, compile_cmd_output.source_commands.src_compile_cmds, pic = link_style != LinkStyle("static"))
    sub_targets[ARGSFILES_SUBTARGET] = [compile_cmd_output.source_commands.argsfiles_info]

    # Compilation DB.
    comp_db = create_compilation_database(ctx, compile_cmd_output.comp_db_commands.src_compile_cmds)
    sub_targets["compilation-database"] = [comp_db]
    comp_db_info = make_compilation_db_info(compile_cmd_output.comp_db_commands.src_compile_cmds, get_cxx_toolchain_info(ctx), get_cxx_platform_info(ctx))

    # Link deps
    link_deps = linkables(cxx_attr_deps(ctx)) + impl_params.extra_link_deps

    # Link Groups
    link_group = get_link_group(ctx)
    link_group_info = impl_params.link_group_info

    if link_group_info:
        link_groups = link_group_info.groups
        link_group_mappings = link_group_info.mappings
        link_group_deps = [
            mapping.root.node.linkable_graph
            for group in link_group_info.groups
            for mapping in group.mappings
            # Preferred-linkage mappings are just used to override the linkage
            # of existing nodes in the graph, so don't "add" in the root node
            # (in case it wasn't actually in the graph to begin with).
            if mapping.root != None and mapping.preferred_linkage == None
        ]
    else:
        link_groups = []
        link_group_mappings = {}
        link_group_deps = []
    link_group_preferred_linkage = get_link_group_preferred_linkage(link_groups)

    # Create the linkable graph with the binary's deps and any link group deps.
    linkable_graph = create_linkable_graph(
        ctx,
        children = filter(None, (
            # Some subtargets (like :Foo[headers]) do not have a linkable_graph.
            [d.linkable_graph for d in link_deps] +
            # We also need to include additional link roots, so that we find
            # deps that might need to be linked into the main executable.
            [d.linkable_graph for d in impl_params.extra_link_roots] +
            # For non-auto-link-group cases, also search the targets specified
            # in the link group mappings, as they won't be available in other
            # ways.
            link_group_deps
        )),
    )

    # Gather link inputs.
    own_link_flags = cxx_attr_linker_flags(ctx) + impl_params.extra_link_flags + impl_params.extra_exported_link_flags
    own_binary_link_flags = ctx.attrs.binary_linker_flags + impl_params.extra_binary_link_flags + own_link_flags
    inherited_link = merge_link_infos(ctx, [d.merged_link_info for d in link_deps])
    frameworks_linkable = create_frameworks_linkable(ctx)

    # Link group libs.
    link_group_libs = {}
    auto_link_groups = {}
    labels_to_links_map = {}
    disabled_link_groups = []

    if not link_group_mappings:
        dep_links = build_link_args_with_deduped_framework_flags(
            ctx,
            inherited_link,
            frameworks_linkable,
            link_style,
            prefer_stripped = impl_params.prefer_stripped_objects,
        )
    else:
        linkable_graph_node_map = get_linkable_graph_node_map_func(linkable_graph)()

        # Although these aren't really deps, we need to search from the
        # extra link group roots to make sure we find additional libs
        # that should be linked into the main link group.
        link_group_other_roots = [d.linkable_graph.nodes.value.label for d in impl_params.extra_link_roots]

        # If we're using auto-link-groups, where we generate the link group links
        # in the prelude, the link group map will give us the link group libs.
        # Otherwise, pull them from the `LinkGroupLibInfo` provider from out deps.
        if impl_params.auto_link_group_specs != None:
            linked_link_groups = create_link_groups(
                ctx = ctx,
                link_group_mappings = link_group_mappings,
                link_group_preferred_linkage = link_group_preferred_linkage,
                executable_deps = [d.linkable_graph.nodes.value.label for d in link_deps],
                linker_flags = own_link_flags,
                link_group_specs = impl_params.auto_link_group_specs,
                root_link_group = link_group,
                linkable_graph_node_map = linkable_graph_node_map,
                other_roots = link_group_other_roots,
                prefer_stripped_objects = impl_params.prefer_stripped_objects,
            )
            for name, linked_link_group in linked_link_groups.libs.items():
                auto_link_groups[name] = linked_link_group.artifact
                if linked_link_group.library != None:
                    link_group_libs[name] = linked_link_group.library
            own_binary_link_flags += linked_link_groups.symbol_ldflags
            disabled_link_groups = linked_link_groups.disabled_link_groups

        else:
            # NOTE(agallagher): We don't use version scripts and linker scripts
            # for non-auto-link-group flow, as it's note clear it's useful (e.g.
            # it's mainly for supporting dlopen-enabled libs and extensions).
            link_group_libs = gather_link_group_libs(
                children = [d.link_group_lib_info for d in link_deps],
            )

        # TODO(T110378098): Similar to shared libraries, we need to identify all the possible
        # scenarios for which we need to propagate up link info and simplify this logic. For now
        # base which links to use based on whether link groups are defined.
        labels_to_links_map = get_filtered_labels_to_links_map(
            linkable_graph_node_map,
            link_group,
            link_group_mappings,
            link_group_preferred_linkage,
            link_group_libs = {
                name: (lib.label, lib.shared_link_infos)
                for name, lib in link_group_libs.items()
            },
            link_style = link_style,
            roots = (
                [d.linkable_graph.nodes.value.label for d in link_deps] +
                find_relevant_roots(
                    link_group = link_group,
                    linkable_graph_node_map = linkable_graph_node_map,
                    link_group_mappings = link_group_mappings,
                    roots = link_group_other_roots,
                )
            ),
            is_executable_link = True,
            prefer_stripped = impl_params.prefer_stripped_objects,
            force_static_follows_dependents = impl_params.link_groups_force_static_follows_dependents,
            disabled_link_groups = disabled_link_groups,
        )

        if is_cxx_test and link_group != None:
            # if a cpp_unittest is part of the link group, we need to traverse through all deps
            # from the root again to ensure we link in gtest deps
            labels_to_links_map = labels_to_links_map | get_filtered_labels_to_links_map(
                linkable_graph_node_map,
                None,
                link_group_mappings,
                link_group_preferred_linkage,
                link_style,
                roots = [d.linkable_graph.nodes.value.label for d in link_deps],
                is_executable_link = True,
                prefer_stripped = impl_params.prefer_stripped_objects,
            )

        filtered_links = get_filtered_links(labels_to_links_map)
        filtered_targets = get_filtered_targets(labels_to_links_map)

        # Unfortunately, link_groups does not use MergedLinkInfo to represent the args
        # for the resolved nodes in the graph.
        # Thus, we have no choice but to traverse all the nodes to dedupe the framework linker args.
        frameworks_link_info = get_frameworks_link_info_by_deduping_link_infos(ctx, filtered_links, frameworks_linkable)
        if frameworks_link_info:
            filtered_links.append(frameworks_link_info)

        dep_links = LinkArgs(infos = filtered_links)
        sub_targets[LINK_GROUP_MAP_DATABASE_SUB_TARGET] = [get_link_group_map_json(ctx, filtered_targets)]

    # Set up shared libraries symlink tree only when needed
    shared_libs = {}

    # Only setup a shared library symlink tree when shared linkage or link_groups is used
    gnu_use_link_groups = cxx_is_gnu(ctx) and link_group_mappings
    if link_style == LinkStyle("shared") or gnu_use_link_groups:
        shlib_info = merge_shared_libraries(
            ctx.actions,
            deps = (
                [d.shared_library_info for d in link_deps] +
                [d.shared_library_info for d in impl_params.extra_link_roots]
            ),
        )

        def is_link_group_shlib(label: "label"):
            # If this maps to a link group which we have a `LinkGroupLibInfo` for,
            # then we'll handlet his below.
            # buildifier: disable=uninitialized
            if label in link_group_mappings and link_group_mappings[label] in link_group_libs:
                return False

            # buildifier: disable=uninitialized
            if link_group_preferred_linkage.get(label, Linkage("any")) == Linkage("shared"):
                return True

            # if using link_groups, only materialize the link_group shlibs
            # buildifier: disable=uninitialized
            node_link = labels_to_links_map.get(label)
            if node_link != None and node_link.link_style == LinkStyle("shared"):
                return True

            return False

        for name, shared_lib in traverse_shared_library_info(shlib_info).items():
            label = shared_lib.label
            if not gnu_use_link_groups or is_link_group_shlib(label):
                shared_libs[name] = shared_lib.lib

    if gnu_use_link_groups:
        # When there are no matches for a pattern based link group,
        # `link_group_mappings` will not have an entry associated with the lib.
        for _name, link_group_lib in link_group_libs.items():
            shared_libs.update(link_group_lib.shared_libs)

    toolchain_info = get_cxx_toolchain_info(ctx)
    linker_info = toolchain_info.linker_info
    links = [
        LinkArgs(infos = [
            LinkInfo(
                pre_flags = own_binary_link_flags,
                linkables = [ObjectsLinkable(
                    objects = [out.object for out in cxx_outs],
                    linker_type = linker_info.type,
                    link_whole = True,
                )],
                external_debug_info = maybe_external_debug_info(
                    actions = ctx.actions,
                    label = ctx.label,
                    artifacts = [out.object for out in cxx_outs if out.object_has_external_debug_info],
                    children = [out.external_debug_info for out in cxx_outs],
                ),
            ),
        ]),
        dep_links,
    ] + impl_params.extra_link_args

    link_result = _link_into_executable(
        ctx,
        links,
        # If shlib lib tree generation is enabled, pass in the shared libs (which
        # will trigger the necessary link tree and link args).
        shared_libs if impl_params.exe_shared_libs_link_tree else {},
        linker_info.link_weight,
        linker_info.binary_extension,
        link_ordering = map_val(LinkOrdering, ctx.attrs.link_ordering),
        prefer_local = False if impl_params.force_full_hybrid_if_capable else link_cxx_binary_locally(ctx),
        enable_distributed_thinlto = ctx.attrs.enable_distributed_thinlto,
        strip = impl_params.strip_executable,
        strip_args_factory = impl_params.strip_args_factory,
        link_postprocessor = impl_params.link_postprocessor,
        force_full_hybrid_if_capable = impl_params.force_full_hybrid_if_capable,
        category_suffix = impl_params.exe_category_suffix,
    )
    binary = link_result.exe
    runtime_files = link_result.runtime_files
    shared_libs_symlink_tree = link_result.shared_libs_symlink_tree
    linker_map_data = link_result.linker_map_data

    # Define the xcode data sub target
    xcode_data_default_info, xcode_data_info = generate_xcode_data(
        ctx,
        rule_type = impl_params.rule_type,
        output = binary.output,
        populate_rule_specific_attributes_func = impl_params.cxx_populate_xcode_attributes_func,
        srcs = impl_params.srcs + impl_params.additional.srcs,
        argsfiles_by_ext = compile_cmd_output.source_commands.argsfile_by_ext,
        product_name = get_cxx_executable_product_name(ctx),
    )
    sub_targets[XCODE_DATA_SUB_TARGET] = xcode_data_default_info

    # Info about dynamic-linked libraries for fbpkg integration:
    # - the symlink dir that's part of RPATH
    # - sub-sub-targets that reference shared library dependencies and their respective dwp
    # - [shared-libraries] - a json map that references the above rules.
    if shared_libs_symlink_tree:
        sub_targets["rpath-tree"] = [DefaultInfo(default_output = shared_libs_symlink_tree)]
    sub_targets["shared-libraries"] = [DefaultInfo(
        default_output = ctx.actions.write_json(
            binary.output.basename + ".shared-libraries.json",
            {
                "libraries": ["{}:{}[shared-libraries][{}]".format(ctx.label.path, ctx.label.name, name) for name in shared_libs.keys()],
                "librariesdwp": ["{}:{}[shared-libraries][{}][dwp]".format(ctx.label.path, ctx.label.name, name) for name, lib in shared_libs.items() if lib.dwp],
                "rpathtree": ["{}:{}[rpath-tree]".format(ctx.label.path, ctx.label.name)] if shared_libs_symlink_tree else [],
            },
        ),
        sub_targets = {
            name: [DefaultInfo(
                default_output = lib.output,
                sub_targets = {"dwp": [DefaultInfo(default_output = lib.dwp)]} if lib.dwp else {},
            )]
            for name, lib in shared_libs.items()
        },
    )]
    if link_group_mappings:
        readable_mappings = {}
        for node, group in link_group_mappings.items():
            readable_mappings[group] = readable_mappings.get(group, []) + ["{}//{}:{}".format(node.cell, node.package, node.name)]

        sub_targets[LINK_GROUP_MAPPINGS_SUB_TARGET] = [DefaultInfo(
            default_output = ctx.actions.write_json(
                binary.output.basename + LINK_GROUP_MAPPINGS_FILENAME_SUFFIX,
                readable_mappings,
            ),
        )]

    # TODO(T110378140): We can't really enable this yet, as Python binaries
    # consuming C++ binaries as resources don't know how to handle the
    # extraneous debug paths and will crash.  We probably need to add a special
    # exported resources provider and make sure we handle the workflows.
    # Add any referenced debug paths to runtime files.
    #runtime_files.extend(binary.external_debug_info)

    # If we have some resources, write it to the resources JSON file and add
    # it and all resources to "runtime_files" so that we make to materialize
    # them with the final binary.
    resources = flatten_dict(gather_resources(
        label = ctx.label,
        resources = cxx_attr_resources(ctx),
        deps = cxx_attr_deps(ctx),
    ).values())
    if resources:
        runtime_files.append(create_resource_db(
            ctx = ctx,
            name = binary.output.basename + ".resources.json",
            binary = binary.output,
            resources = resources,
        ))
        for resource, other in resources.values():
            runtime_files.append(resource)
            runtime_files.extend(other)

    if binary.dwp:
        # A `dwp` sub-target which generates the `.dwp` file for this binary.
        sub_targets["dwp"] = [DefaultInfo(default_output = binary.dwp)]

    # If bolt is not ran, binary.prebolt_output will be the same as binary.output. Only
    # expose binary.prebolt_output if cxx_use_bolt(ctx) is True to avoid confusion
    if cxx_use_bolt(ctx):
        sub_targets["prebolt"] = [DefaultInfo(default_output = binary.prebolt_output)]

    sub_targets["linker-map"] = [DefaultInfo(default_output = linker_map_data.map, other_outputs = [linker_map_data.binary])]

    sub_targets["linker.argsfile"] = [DefaultInfo(
        default_output = binary.linker_argsfile,
    )]

    if linker_info.supports_distributed_thinlto and ctx.attrs.enable_distributed_thinlto:
        sub_targets["index.argsfile"] = [DefaultInfo(
            default_output = binary.index_argsfile,
        )]

    # Provide a debug info target to make sure debug info is materialized.
    external_debug_info = maybe_external_debug_info(
        actions = ctx.actions,
        label = ctx.label,
        children = (
            [binary.external_debug_info] +
            [s.external_debug_info for s in shared_libs.values()]
        ),
    )
    sub_targets["debuginfo"] = [DefaultInfo(
        other_outputs = project_external_debug_info(
            actions = ctx.actions,
            label = ctx.label,
            infos = [external_debug_info],
        ),
    )]

    return CxxExecutableOutput(
        binary = binary.output,
        dwp = binary.dwp,
        runtime_files = runtime_files,
        sub_targets = sub_targets,
        link_args = links,
        external_debug_info = external_debug_info,
        shared_libs = shared_libs,
        auto_link_groups = auto_link_groups,
        compilation_db = comp_db_info,
        xcode_data = xcode_data_info,
        linker_map_data = linker_map_data,
    )

_CxxLinkExecutableResult = record(
    # The resulting executable
    exe = LinkedObject.type,
    # List of files/directories that should be present for executable to be run successfully
    runtime_files = ["_arglike"],
    # Optional shared libs symlink tree symlinked_dir action
    shared_libs_symlink_tree = ["artifact", None],
    linker_map_data = CxxLinkerMapData.type,
)

def _link_into_executable(
        ctx: "context",
        links: [LinkArgs.type],
        shared_libs: {str.type: LinkedObject.type},
        link_weight: int.type,
        binary_extension: str.type,
        prefer_local: bool.type = False,
        enable_distributed_thinlto: bool.type = False,
        strip: bool.type = False,
        link_ordering: [LinkOrdering.type, None] = None,
        strip_args_factory = None,
        link_postprocessor: ["cmd_args", None] = None,
        force_full_hybrid_if_capable: bool.type = False,
        category_suffix: [str.type, None] = None) -> _CxxLinkExecutableResult.type:
    output = ctx.actions.declare_output("{}{}".format(get_cxx_executable_product_name(ctx), "." + binary_extension if binary_extension else ""))
    extra_args, runtime_files, shared_libs_symlink_tree = executable_shared_lib_arguments(
        ctx.actions,
        ctx.label,
        get_cxx_toolchain_info(ctx),
        output,
        shared_libs,
    )
    links = [LinkArgs(flags = extra_args)] + links

    shared_link_and_linker_map_args = {
        "force_full_hybrid_if_capable": force_full_hybrid_if_capable,
        "link_ordering": link_ordering,
        "link_weight": link_weight,
        "prefer_local": prefer_local,
    }

    exe = cxx_link(
        ctx,
        links,
        output,
        CxxLinkResultType("executable"),
        enable_distributed_thinlto = enable_distributed_thinlto,
        category_suffix = category_suffix,
        strip = strip,
        strip_args_factory = strip_args_factory,
        link_postprocessor = link_postprocessor,
        **shared_link_and_linker_map_args
    )

    linker_map_data = _linker_map(
        ctx,
        links,
        exe,
        **shared_link_and_linker_map_args
    )

    return _CxxLinkExecutableResult(
        exe = exe,
        runtime_files = runtime_files,
        shared_libs_symlink_tree = shared_libs_symlink_tree,
        linker_map_data = linker_map_data,
    )

def _linker_map(
        ctx: "context",
        links: [LinkArgs.type],
        binary: LinkedObject.type,
        **kwargs) -> CxxLinkerMapData.type:
    identifier = binary.output.short_path + ".linker-map-binary"
    binary_for_linker_map = ctx.actions.declare_output(identifier)
    linker_map = ctx.actions.declare_output(binary.output.short_path + "-LinkMap.txt")
    cxx_link(
        ctx,
        links,
        binary_for_linker_map,
        CxxLinkResultType("executable"),
        category_suffix = "linker_map",
        linker_map = linker_map,
        identifier = identifier,
        allow_bolt_optimization_and_dwp_generation = False,
        **kwargs
    )
    return CxxLinkerMapData(
        map = linker_map,
        binary = binary_for_linker_map,
    )

def get_cxx_executable_product_name(ctx: "context") -> str.type:
    return ctx.label.name + ("-wrapper" if cxx_use_bolt(ctx) else "")
