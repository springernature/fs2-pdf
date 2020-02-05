#!/usr/bin/env bash

export artifactory_username=$ARTIFACTORY_USERNAME
export artifactory_password=$ARTIFACTORY_PASSWORD
$(dirname $0)/pipeline-sbt.bash clean test
