# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_stripping.bzl", "apple_strip_args")
load("@prelude//cxx:cxx_executable.bzl", "cxx_executable")
load("@prelude//cxx:cxx_library_utility.bzl", "cxx_attr_deps", "cxx_attr_exported_deps")
load("@prelude//cxx:cxx_sources.bzl", "get_srcs_with_flags")
load("@prelude//cxx:cxx_types.bzl", "CxxRuleConstructorParams")
load("@prelude//cxx:debug.bzl", "project_external_debug_info")
load(
    "@prelude//cxx:link_groups.bzl",
    "get_link_group_info",
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
)
load(":apple_bundle_types.bzl", "AppleBundleLinkerMapInfo", "AppleMinDeploymentVersionInfo")
load(":apple_bundle_utility.bzl", "get_bundle_infos_from_graph", "merge_bundle_linker_maps_info")
load(":apple_code_signing_types.bzl", "AppleEntitlementsInfo")
load(":apple_dsym.bzl", "AppleDebuggableInfo", "DEBUGINFO_SUBTARGET", "DSYM_SUBTARGET", "get_apple_dsym")
load(":apple_frameworks.bzl", "get_framework_search_path_flags")
load(":apple_target_sdk_version.bzl", "get_min_deployment_version_for_node", "get_min_deployment_version_target_linker_flags", "get_min_deployment_version_target_preprocessor_flags")
load(":apple_utility.bzl", "get_apple_cxx_headers_layout")
load(":resource_groups.bzl", "create_resource_graph")
load(":xcode.bzl", "apple_populate_xcode_attributes")

def apple_binary_impl(ctx: "context") -> ["provider"]:
    extra_link_flags = get_min_deployment_version_target_linker_flags(ctx) + _entitlements_link_flags(ctx)
    framework_search_path_pre = CPreprocessor(
        args = [get_framework_search_path_flags(ctx)],
    )
    constructor_params = CxxRuleConstructorParams(
        rule_type = "apple_binary",
        headers_layout = get_apple_cxx_headers_layout(ctx),
        extra_link_flags = extra_link_flags,
        srcs = get_srcs_with_flags(ctx),
        extra_preprocessors = get_min_deployment_version_target_preprocessor_flags(ctx) + [framework_search_path_pre],
        strip_executable = ctx.attrs.stripped,
        strip_args_factory = apple_strip_args,
        cxx_populate_xcode_attributes_func = apple_populate_xcode_attributes,
        link_group_info = get_link_group_info(ctx),
        prefer_stripped_objects = ctx.attrs.prefer_stripped_objects,
        # Some apple rules rely on `static` libs *not* following dependents.
        link_groups_force_static_follows_dependents = False,
    )
    cxx_output = cxx_executable(ctx, constructor_params)

    external_debug_info = project_external_debug_info(
        actions = ctx.actions,
        label = ctx.label,
        infos = [cxx_output.external_debug_info],
    )
    dsym_artifact = get_apple_dsym(
        ctx = ctx,
        executable = cxx_output.binary,
        external_debug_info = external_debug_info,
        action_identifier = cxx_output.binary.short_path,
    )
    cxx_output.sub_targets[DSYM_SUBTARGET] = [DefaultInfo(default_output = dsym_artifact)]
    cxx_output.sub_targets[DEBUGINFO_SUBTARGET] = [DefaultInfo(other_outputs = external_debug_info)]

    min_version = get_min_deployment_version_for_node(ctx)
    min_version_providers = [AppleMinDeploymentVersionInfo(version = min_version)] if min_version != None else []

    resource_graph = create_resource_graph(
        ctx = ctx,
        labels = ctx.attrs.labels,
        deps = cxx_attr_deps(ctx),
        exported_deps = cxx_attr_exported_deps(ctx),
    )
    bundle_infos = get_bundle_infos_from_graph(resource_graph)
    if cxx_output.linker_map_data:
        bundle_infos.append(AppleBundleLinkerMapInfo(linker_maps = [cxx_output.linker_map_data.map]))

    return [
        DefaultInfo(default_output = cxx_output.binary, sub_targets = cxx_output.sub_targets),
        RunInfo(args = cmd_args(cxx_output.binary).hidden(cxx_output.runtime_files)),
        AppleEntitlementsInfo(entitlements_file = ctx.attrs.entitlements_file),
        AppleDebuggableInfo(dsyms = [dsym_artifact], external_debug_info = cxx_output.external_debug_info),
        cxx_output.xcode_data,
        merge_bundle_linker_maps_info(bundle_infos),
    ] + [resource_graph] + min_version_providers

def _entitlements_link_flags(ctx: "context") -> [""]:
    return [
        "-Xlinker",
        "-sectcreate",
        "-Xlinker",
        "__TEXT",
        "-Xlinker",
        "__entitlements",
        "-Xlinker",
        ctx.attrs.entitlements_file,
    ] if ctx.attrs.entitlements_file else []
