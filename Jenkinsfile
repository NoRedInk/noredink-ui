#!groovy

properties([
    disableConcurrentBuilds()
])

// TODO: merge main branch and restart CI on the merged Jenkinsfile

node(label: env.CI_NODE) {
    stage("build") {
        sh("nix-shell --pure --run 'shake --verbose ci'")
    }
}
