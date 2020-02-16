#!/usr/bin/env bash

set -eu -o pipefail

version=$(sed -e 's/-SNAPSHOT//' -e 's/.*"\(.*\)"/\1/' version.sbt)
sed -i "s/\(.*\"fs2-pdf\" % \)".*"/\1 \"$version\"/" README.md

sf-ci-publish-maven fs2-pdf master
