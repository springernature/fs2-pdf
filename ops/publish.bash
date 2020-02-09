#!/usr/bin/env bash

set -e

cleanup()
{
  rm -f ssh-key
}

trap cleanup EXIT

git checkout master

echo -e $github_ssh_key > ssh-key
chmod 600 ssh-key

echo -e $sf_ci_gpg_main | gpg --import
echo -e $sf_ci_gpg_sub | gpg --import

$(dirname $0)/pipeline-sbt.bash \
  'set every javacOptions := List("--release", "8")' \
  'set every publishMavenStyle := true' \
  'set every publishTo := Some(Resolver.url("jfrog", url("https://springernature.jfrog.io/springernature/libs-release-local")))' \
  'set credentials += Credentials("GnuPG Key ID", "gpg", "FFF0376ED1F7E2CEEB848ABC980AD6C6E2EB6AE2", "")' \
  'release with-defaults'

GIT_SSH_COMMAND='ssh -i ssh-key -o StrictHostKeyChecking=no' git push origin master
