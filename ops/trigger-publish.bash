#!/usr/bin/env bash

set -eu -o pipefail

ref=${1:-master}

f()
{
  fly -t sprcom "$@"
}

js()
{
  jq "$@" | sed 's/"//g'
}

ask()
{
  local problem=$1
  read -n 1 -p "$problem. Publish anyway? [y/n] " answer
  echo
  if [[ $answer != 'y' ]]
  then
    exit 1
  fi
}

job_attr()
{
  local name=$1 query=$2 j=${3:-jq}
  f js -p fs2-pdf --json | $j ".[] | select(.name == \"$name\") | $query"
}

head=$(git rev-parse $ref)
echo "Publishing from ref '$ref' at $head"

latest_in_pipeline=$(f rvs -r fs2-pdf/git --json | js '.[0].version.ref')
if [[ $head != $latest_in_pipeline ]]
then
  ask "HEAD is not the latest commit in concourse ($latest_in_pipeline)"
fi

publish_has_inputs=$(job_attr 'publish' '.has_new_inputs')
if [[ $publish_has_inputs != 'true' ]]
then
  ask 'Publish job does not have new inputs'
fi

# next_test_build=$(f js -p fs2-pdf --json | jq '.[] | select(.name == "test") | .next_build')
next_test_build=$(job_attr 'test' '.next_build')
if [[ $next_test_build != 'null' ]]
then
  ask 'Test job is still running'
fi

test_build_status=$(job_attr 'test' '.finished_build.status' 'js')
if [[ $test_build_status != 'succeeded' ]]
then
  ask 'Test job failed'
fi

test_has_inputs=$(job_attr 'test' '.has_new_inputs')
if [[ $test_has_inputs == 'true' ]]
then
  ask 'Test job has new inputs'
fi

echo 'Triggering publish job'
f tj -j fs2-pdf/publish -w
