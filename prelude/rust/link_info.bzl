# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Implementation of the Rust build rules.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo")
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
    "MergedLinkInfo",
    "merge_link_infos",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
)
load("@prelude//utils:platform_flavors_util.bzl", "by_platform")
load("@prelude//utils:utils.bzl", "flatten")

# Link style for targets which do not set an explicit `link_style` attribute.
DEFAULT_STATIC_LINK_STYLE = LinkStyle("static_pic")

# Override dylib crates to static_pic, so that Rust code is always
# statically linked.
# In v1 we always linked Rust deps statically, even for "shared" link style
# That shouldn't be necessary, but fully shared needs some more debugging,
# so default to v1 behaviour. (Should be controlled with the `rust.force_rlib` option)
FORCE_RLIB = True

# Output of a Rust compilation
RustLinkInfo = provider(fields = [
    # crate - crate name
    "crate",
    # styles - information about each LinkStyle as RustLinkStyleInfo
    # {LinkStyle: RustLinkStyleInfo}
    "styles",
    # Propagate non-rust native linkable dependencies through rust libraries.
    "non_rust_exported_link_deps",
    # Propagate non-rust native linkable info through rust libraries.
    "non_rust_link_info",
    # Propagate non-rust shared libraries through rust libraries.
    "non_rust_shared_libs",
])

CrateName = record(
    simple = field(str.type),
    dynamic = field(["artifact", None]),
)

# Information which is keyed on link_style
RustLinkStyleInfo = record(
    # Path to library or binary
    rlib = field("artifact"),
    # Transitive dependencies which are relevant to consumer
    # This is a dict from artifact to None (we don't have sets)
    transitive_deps = field({"artifact": CrateName.type}),

    # Path for library metadata (used for check or pipelining)
    rmeta = field("artifact"),
    # Transitive rmeta deps
    transitive_rmeta_deps = field({"artifact": CrateName.type}),
)

def style_info(info: RustLinkInfo.type, link_style: LinkStyle.type) -> RustLinkStyleInfo.type:
    if FORCE_RLIB and link_style == LinkStyle("shared"):
        link_style = DEFAULT_STATIC_LINK_STYLE

    return info.styles[link_style]

def cxx_by_platform(ctx: "context", xs: [(str.type, "_a")]) -> "_a":
    platform = ctx.attrs._cxx_toolchain[CxxPlatformInfo].name
    return flatten(by_platform([platform], xs))

# A Rust dependency
RustDependency = record(
    # The actual dependency
    dep = field("dependency"),
    # The local name, if any (for `named_deps`)
    name = field([None, str.type]),
    # Any flags for the dependency (`flagged_deps`), which are passed on to rustc.
    flags = field([str.type]),
)

# Returns all first-order dependencies, resolving the ones from "platform_deps"
def _do_resolve_deps(
        ctx: "context",
        deps: ["dependency"],
        platform_deps: [(str.type, ["dependency"])],
        named_deps: {str.type: "dependency"},
        flagged_deps: [("dependency", [str.type])] = [],
        platform_flagged_deps: [(str.type, [("dependency", [str.type])])] = []) -> [RustDependency.type]:
    return [
        RustDependency(name = name, dep = dep, flags = flags)
        for name, dep, flags in [(None, dep, []) for dep in deps + cxx_by_platform(ctx, platform_deps)] +
                                [(name, dep, []) for name, dep in named_deps.items()] +
                                [(None, dep, flags) for dep, flags in flagged_deps +
                                                                      cxx_by_platform(ctx, platform_flagged_deps)]
    ]

def resolve_deps(
        ctx: "context",
        include_doc_deps: bool.type = False) -> [RustDependency.type]:
    # The `getattr`s are needed for when we're operating on
    # `prebuilt_rust_library` rules, which don't have those attrs.
    dependencies = _do_resolve_deps(
        ctx = ctx,
        deps = ctx.attrs.deps,
        platform_deps = ctx.attrs.platform_deps,
        named_deps = getattr(ctx.attrs, "named_deps", {}),
        flagged_deps = getattr(ctx.attrs, "flagged_deps", []),
        platform_flagged_deps = getattr(ctx.attrs, "platform_flagged_deps", []),
    )

    if include_doc_deps:
        dependencies.extend(_do_resolve_deps(
            ctx = ctx,
            deps = ctx.attrs.doc_deps,
            platform_deps = ctx.attrs.doc_platform_deps,
            named_deps = getattr(ctx.attrs, "doc_named_deps", {}),
        ))

    return dependencies

# Returns native link dependencies.
def _non_rust_link_deps(
        ctx: "context",
        include_doc_deps: bool.type = False) -> ["dependency"]:
    """
    Return all first-order native linkable dependencies of all transitive Rust
    libraries.

    This emulates v1's graph walk, where it traverses through Rust libraries
    looking for non-Rust native link infos (and terminating the search there).
    """
    first_order_deps = [dep.dep for dep in resolve_deps(ctx, include_doc_deps)]
    return [
        d
        for d in first_order_deps
        if RustLinkInfo not in d and MergedLinkInfo in d
    ]

# Returns native link dependencies.
def _non_rust_link_infos(
        ctx: "context",
        include_doc_deps: bool.type = False) -> ["MergedLinkInfo"]:
    """
    Return all first-order native link infos of all transitive Rust libraries.

    This emulates v1's graph walk, where it traverses through Rust libraries
    looking for non-Rust native link infos (and terminating the search there).
    MergedLinkInfo is a mapping from link style to all the transitive deps
    rolled up in a tset.
    """
    return [d[MergedLinkInfo] for d in _non_rust_link_deps(ctx, include_doc_deps)]

# Returns native link dependencies.
def _non_rust_shared_lib_infos(
        ctx: "context",
        include_doc_deps: bool.type = False) -> ["SharedLibraryInfo"]:
    """
    Return all transitive shared libraries for non-Rust native linkabes.

    This emulates v1's graph walk, where it traverses through -- and ignores --
    Rust libraries to collect all transitive shared libraries.
    """
    first_order_deps = [dep.dep for dep in resolve_deps(ctx, include_doc_deps)]
    return [
        d[SharedLibraryInfo]
        for d in first_order_deps
        if RustLinkInfo not in d and SharedLibraryInfo in d
    ]

# Returns native link dependencies.
def _rust_link_infos(
        ctx: "context",
        include_doc_deps: bool.type = False) -> ["RustLinkInfo"]:
    first_order_deps = resolve_deps(ctx, include_doc_deps)
    return filter(None, [d.dep.get(RustLinkInfo) for d in first_order_deps])

def normalize_crate(label: str.type) -> str.type:
    return label.replace("-", "_")

def inherited_non_rust_exported_link_deps(ctx: "context") -> ["dependency"]:
    deps = {}
    for dep in _non_rust_link_deps(ctx):
        deps[dep.label] = dep
    for info in _rust_link_infos(ctx):
        for dep in info.non_rust_exported_link_deps:
            deps[dep.label] = dep
    return deps.values()

def inherited_non_rust_link_info(
        ctx: "context",
        include_doc_deps: bool.type = False) -> "MergedLinkInfo":
    infos = []
    infos.extend(_non_rust_link_infos(ctx, include_doc_deps))
    infos.extend([d.non_rust_link_info for d in _rust_link_infos(ctx, include_doc_deps)])
    return merge_link_infos(ctx, infos)

def inherited_non_rust_shared_libs(
        ctx: "context",
        include_doc_deps: bool.type = False) -> ["SharedLibraryInfo"]:
    infos = []
    infos.extend(_non_rust_shared_lib_infos(ctx, include_doc_deps))
    infos.extend([d.non_rust_shared_libs for d in _rust_link_infos(ctx, include_doc_deps)])
    return infos

def attr_simple_crate_for_filenames(ctx: "context") -> str.type:
    """
    A "good enough" identifier to use in filenames. Buck wants to have filenames
    of artifacts figured out before we begin building them. Normally we want a
    crate foo to produce artifact libfoo.rlib; but if crate_dynamic is being
    used, the true crate name is not known until later. In this situation we use
    the rule's name in place of the true crate name in filenames.

    # produces libordinary.rlib
    rust_library(
        name = "ordinary",
        crate = "ordinary",
    )

    # produces libthrift_generated.rlib
    rust_library(
        name = "thrift-generated",
        crate_dynamic = ":get-namespace-from-thrift-file",
    )
    """
    return normalize_crate(ctx.attrs.crate or ctx.label.name)

def attr_crate(ctx: "context") -> CrateName.type:
    """
    The true user-facing name of the crate, which may only be known at build
    time, not during analysis.
    """
    dynamic = getattr(ctx.attrs, "crate_dynamic", None)
    if dynamic:
        dynamic = dynamic.get(DefaultInfo).default_outputs[0]
    return CrateName(
        simple = ctx.attrs.crate or normalize_crate(ctx.label.name),
        dynamic = dynamic,
    )
