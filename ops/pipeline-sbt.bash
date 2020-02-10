#!/usr/bin/env bash

set -euf -o pipefail

here=$(dirname $0)
cache_base='/var/halfpipe/shared-cache'
cache="$cache_base/fs2-watermark"
jfrog_user="${artifactory_username:-$ARTIFACTORY_USERNAME}"
jfrog_pass="${artifactory_password:-$ARTIFACTORY_PASSWORD}"
sbt_options="-Dsbt.repository.config=$here/repo.properties"
cached_dirs='.ivy2 .coursier .sbt'
creds="Credentials(\"Artifactory Realm\", \"springernature.jfrog.io\", \"$jfrog_user\", \"$jfrog_pass\")"

if [[ -d $cache_base ]]
then
  echo ">>> halfpipe sbt cache dir: $cache"
  mkdir -p $cache
  for dir in $cached_dirs
  do
    rm -rf /root/$dir
    mkdir -p $cache/$dir
    ln -s $cache/$dir /root/$dir
  done
else
  echo ">>> Not using halfpipe cache for sbt"
fi

$here/sbt \
  $sbt_options \
  "set every credentials := List($creds)" \
  "$@"
