load(":elm.bzl", "elm_docs", "elm_test", "elm_review", "elm_format_diffs", "elm_verify_examples")
load(":browserify.bzl", "browserify")
load(":prettier.bzl", "prettier_diffs")

elm_docs(
    name = "docs",
    elm_json = "elm.json",
    srcs = glob(["src/**/*.elm"]),
    tests = [
        ":test",
        ":review",
    #     ":elm-forbid-import"
    ]
)

elm_test(
    name = "test",
    elm_json = "elm.json",
    srcs = glob(["src/**/*.elm"]),
    test_srcs = glob(["tests/**/*.elm"]),
)

elm_review(
    name = "review",
    srcs = glob(["src/**/*.elm"]),
    review_srcs = glob(["review/**/*.elm"]),
)

# elm_format_diffs(
#     name = "elm_format_diffs",
#     srcs = glob(["**/*.elm"]),
# )

# export_file(
#     name = "elm.json",
#     visibility = ["PUBLIC"], # TODO: lock this down?
# )

# browserify(
#     name = "bundle.js",
#     entry = "lib/index.js",
#     srcs = glob(["lib/**/*.js"]),
#     visibility = ["PUBLIC"], # TODO: lock this down?
# )

# elm_verify_examples(
#     name = "elm_verify_examples",
#     srcs = glob(["src/**/*.elm"]),
# )

# export_file(
#     name = "deprecated-modules-csv.py",
#     src = "script/deprecated-modules-csv.py",
# )

# genrule(
#     name = "deprecated-modules.csv",
#     srcs = glob(["src/**/*.elm", "elm.json"]),
#     out = "deprecated-modules.csv",
#     cmd = "$(location :deprecated-modules-csv.py) $(location :elm.json) > ${OUT}"
# )

# # this is slightly less good than what we have/had in Shake, since the config
# # file doesn't automatically get updated. That'd probably be appropriate for
# # a BXL script? Or a separate test that there won't be any changes applied to
# # the config?
# sh_test(
#     name = "elm-forbid-import",
#     test = "script/elm-forbid-import.sh",
#     args = ["check"],
#     visibility = ["PUBLIC"],
# )

# prettier_diffs(
#     name = "prettier",
#     srcs = glob(["lib/**/*.js"]),
# )
