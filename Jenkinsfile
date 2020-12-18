#!groovy

properties([
    disableConcurrentBuilds()
])

// TODO: merge main branch and restart CI on the merged Jenkinsfile

stage("build") {
    sh("nix-shell --pure --run 'shake --verbose ci'")
}
