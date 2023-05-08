# A list of available rules and their signatures can be found here: https://buck2.build/docs/api/rules/
load("@prelude-nri//:elm.bzl", "elm_docs")
load("@prelude-nri//:node.bzl", "node_modules", "npm_bin", "npm_script_test")

elm_docs(
    name = "docs.json",
    elm_json = "elm.json",
    src = "src",
)

filegroup(
    name = "src",
    srcs = glob(["src/**/*.elm"]),
    visibility = ["//component-catalog:app"],
)

node_modules(
    name = "node_modules",
    package = "package.json",
    package_lock = "package-lock.json",
    extra_files = {
        "lib": "//lib:src"
    },
    extra_args = ["--include=dev"],
)

npm_bin(
    name = "browserify",
    node_modules = ":node_modules",
    visibility = ["//lib:bundle.js"],
)

export_file(
    name = "elm.json",
    visibility = ["//component-catalog:public"],
)

npm_script_test(
    name = "puppeteer",
    node_modules = ":node_modules",
    args = ["default", "$(location //component-catalog:public)"],
    extra_files = {
        "script/puppeteer-tests.sh": "script/puppeteer-tests.sh",
        "script/puppeteer-tests.js": "script/puppeteer-tests.js",
    },
)
