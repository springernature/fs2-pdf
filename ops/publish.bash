#!/usr/bin/env bash

set -eu -o pipefail

#git switch master
#version=$(sed -e 's/-SNAPSHOT//' -e 's/.*"\(.*\)"/\1/' version.sbt)
#echo ">>> updating readme version to $version"
#sed -i "s/\(.*\"fs2-pdf\" % \)".*"/\1 \"$version\"/" README.md
#git add README.md
#git commit -m "bump readme version"
echo ">>> publishing to maven"
sf-ci-publish-maven fs2-pdf master
echo ">>> published to maven"