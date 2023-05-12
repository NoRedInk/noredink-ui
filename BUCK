# A list of available rules and their signatures can be found here: https://buck2.build/docs/api/rules/
load("@prelude-nri//:elm.bzl", "elm_docs", "elm_format_diffs")
load("@prelude-nri//:node.bzl", "node_modules", "npm_bin", "npm_script_test", "prettier_diffs")

ELM_COMPILER_URL = select({
    "config//os:linux": "https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz",
    "config//os:macos": "https://github.com/elm/compiler/releases/download/0.19.1/binary-for-mac-64-bit.gz",
})

ELM_COMPILER_SHA256 = select({
    "config//os:linux": "e44af52bb27f725a973478e589d990a6428e115fe1bb14f03833134d6c0f155c",
    "config//os:macos": "05289f0e3d4f30033487c05e689964c3bb17c0c48012510dbef1df43868545d1",
})

http_file(
    name = "elm_compiler_archive",
    urls = [ELM_COMPILER_URL],
    sha256 = ELM_COMPILER_SHA256,
)

genrule(
    name = "elm_compiler_binary",
    cmd = "gzip --decompress --stdout --keep $(location :elm_compiler_archive) > $OUT && chmod +x $OUT",
    out = "elm",
    executable = True,
    remote = True,
    visibility = ["PUBLIC"],
)

genrule(
    name = "elm_version",
    out = "version.txt",
    cmd = "echo 4; $(exe :elm_compiler_binary) --version > $OUT",
    remote = True,
)

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

npm_bin(
    name = "prettier",
    node_modules = ":node_modules",
    visibility = [
        "//lib:prettier_diffs",
        "//component-catalog:prettier_diffs",
    ],
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

elm_format_diffs(
    name = "elm_format_diffs",
    srcs = glob(["src/**/*.elm"]),
)

genrule(
    name = "diff_to_comment",
    out = "diff_to_comment.py",
    srcs = ["script/diff_to_comment.py"],
    cmd = "cp $SRCS $OUT",
    executable = True,
)

prettier_diffs(
    name = "prettier_diffs",
    srcs = glob(["**/*.md"]),
    prettier = "//:prettier",
)
