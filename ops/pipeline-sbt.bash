#!/usr/bin/env bash

set -euf -o pipefail

here=$(dirname $0)
cache_base='/var/halfpipe/shared-cache'
cache="$cache_base/fs2-watermark"
sbt_options=''

if [[ -d $cache_base ]]
then
  echo "Using cache directory $cache to build"
  mkdir -p $cache
  rm -rf /root/.sbt
  mkdir -p $cache/.sbt
  ln -s $cache/.sbt /root/.sbt
  sbt_options="-Dsbt.repository.config=$here/repo.properties"
else
  echo "Not using halfpipe cache"
fi

$here/sbt \
  $sbt_options \
  'set every credentials := List(Credentials("Artifactory Realm", "springernature.jfrog.io", "'$artifactory_username'", "'$artifactory_password'"))' \
  "$@"
