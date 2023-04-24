def _git_clone_impl(ctx: "context"):
    out = ctx.actions.declare_output(ctx.attrs.out)

    # assuming system bash and git. Following the patterns that GitHub actions
    # take to check out a repo (actions/checkout@v3)
    ctx.actions.run(
        cmd_args([
            "bash",
            "-xeo", "pipefail",
            "-c",
            """
            git init "$1"
            cd "$1"

            git config --local gc.auto 0
            git remote add origin "$2"
            git fetch --no-tags --prune --progress --no-recurse-submodules --depth=1 origin "${3}:refs/remotes/origin/${3}"
            git checkout --progress --force -B "$3" "refs/remotes/origin/${3}"
            """,
            "--",
            out.as_output(),
            ctx.attrs.repo,
            ctx.attrs.ref,
        ]),
        category = "git_clone",
    )

    return [DefaultInfo(default_output = out)]

git_clone = rule(
    impl = _git_clone_impl,
    attrs = {
        "repo": attrs.string(),
        "ref": attrs.string(),
        "out": attrs.string(default = "repo"),
    }
)
