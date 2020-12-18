#!groovy

properties([
    disableConcurrentBuilds()
])

// TODO: merge main branch and restart CI on the merged Jenkinsfile

pipeline {
    agent {
        docker {
            image "nixos/nix"
            args "-v /nix:/nix"
        }
    }

    stages {
        stage("build") {
            steps {
                sh("nix-shell --pure --run 'shake --verbose ci'")
            }
        }
    }
}
