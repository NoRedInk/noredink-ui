#!groovy

properties([
    disableConcurrentBuilds()
])

// TODO: merge main branch and restart CI on the merged Jenkinsfile

pipeline {
    agent {
        docker {
            image "lnl7/nix"
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
