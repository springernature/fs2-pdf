#!/usr/bin/env bash

set -eu -o pipefail

ref=${1:-master} pipeline=${2:-fs2-pdf}

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
  local question=$1
  read -n 1 -p "$question [y/n] " answer
  echo
  if [[ $answer != 'y' ]]
  then
    exit 1
  fi
}

ask_problem()
{
  local problem=$1
  ask "$problem. Publish anyway?"
}

job_attr()
{
  local name=$1 query=$2 j=${3:-jq}
  f js -p $pipeline --json | $j ".[] | select(.name == \"$name\") | $query"
}

head=$(git rev-parse $ref)
echo "Publishing from ref '$ref' at $head"

latest_in_pipeline=$(f rvs -r $pipeline/git --json | js '.[0].version.ref')
if [[ $head != $latest_in_pipeline ]]
then
  ask_problem "HEAD is not the latest commit in concourse ($latest_in_pipeline)"
fi

publish_has_inputs=$(job_attr 'publish' '.has_new_inputs')
if [[ $publish_has_inputs != 'true' ]]
then
  ask_problem 'Publish job does not have new inputs'
fi

next_test_build=$(job_attr 'test' '.next_build')
if [[ $next_test_build != 'null' ]]
then
  ask_problem 'Test job is still running'
fi

test_build_status=$(job_attr 'test' '.finished_build.status' 'js')
if [[ $test_build_status != 'succeeded' ]]
then
  ask_problem 'Test job failed'
fi

test_has_inputs=$(job_attr 'test' '.has_new_inputs')
if [[ $test_has_inputs == 'true' ]]
then
  ask_problem 'Test job has new inputs'
fi

ask 'All conditions validated. Trigger publish job?'
f tj -j $pipeline/publish -w
