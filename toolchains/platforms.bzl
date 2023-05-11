def _platforms_impl(ctx: "context") -> [[DefaultInfo.type, ExecutionPlatformRegistrationInfo.type]]:
    constraints = dict()
    constraints.update(ctx.attrs.os_configuration[ConfigurationInfo].constraints)
    constraints.update(ctx.attrs.arch_configuration[ConfigurationInfo].constraints)

    configuration = ConfigurationInfo(
        constraints = constraints,
        values = {},
    )

    platform = ExecutionPlatformInfo(
        label = ctx.label.raw_target(),
        configuration = configuration,
        executor_config = CommandExecutorConfig(
            local_enabled = True,
            remote_enabled = True,
            use_limited_hybrid = True,
            remote_cache_enabled = True,
            allow_cache_uploads = True,
            # Set those up based on what workers you've registered with Buildbarn.
            remote_execution_properties = {
                "OSFamily": "Linux",
                "container-image": "docker://ghcr.io/catthehacker/ubuntu:act-22.04@sha256:5f9c35c25db1d51a8ddaae5c0ba8d3c163c5e9a4a6cc97acd409ac7eae239448",
            },
            remote_execution_use_case = "buck2-default",
            remote_output_paths = "output_paths",
        ),
    )

    return [
        DefaultInfo(),
        ExecutionPlatformRegistrationInfo(platforms = [platform]),
    ]

platforms = rule(
    impl = _platforms_impl,
    attrs = {
        "os_configuration": attrs.dep(
            providers = [ConfigurationInfo],
            default = "prelude//os:linux",
        ),
        "arch_configuration": attrs.dep(
            providers = [ConfigurationInfo],
            default = "prelude//cpu:x86_64",
        ),
    },
)
