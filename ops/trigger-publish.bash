#!/usr/bin/env bash
# This script triggers the `publish` job of the specified concourse pipeline at the specified commit.
# To prevent the wrong commit from being published, for example because a stage has failed without the dev noticing,
# the script first calls the concourse api to obtain which commit would be processed, and waits for that commit to
# match the specified commit (master is the default).
# The user may force triggering the pipeline by pressing the `f` key while the script is polling.

set -eu -o pipefail

ref=${1:-master} pipeline=${2:-fs2-pdf}

local_head=$(git rev-parse $ref)

f()
{
  fly -t sprcom "$@"
}

concourse_api()
{
  f curl "/api/v1/teams/sprcom/pipelines/$pipeline/$1" 2>/dev/null
}

# there is no way to print json strings without the quotes in jq
json_string()
{
  jq "$@" | sed 's/"//g'
}

# the `inputs` of a job are resources like version and git.
# the api endpoint returns those that would be processed if the job was started.
# this function filters the git part of the array of resources `select(.type == "git")` and the accesses the `ref` field
# in the object at the `version` field (.version.ref).
query_git_ref='.[] | select(.type == "git") | .version.ref'
publish_ref()
{
  concourse_api "jobs/publish/inputs" | json_string "$query_git_ref"
}

# the main condition for a clean publish is that the tests have passed, which is fulfilled if the commit at the publish
# job is the same as our target commit.
ready()
{
  [[ $(publish_ref) == $local_head ]]
}

# loops in 1 second intervals (-t 1 option to `read`) to check whether the ref has arrived at the `publish` job
poll()
{
  local answer
  echo ">>> Waiting for the target ref to appear in the publish job's inputs."
  echo ">>> Current commit is $(publish_ref)"
  echo -n '>>> Press [f] to force trigger the pipeline with an older commit, [a] to abort: '
  while true
  do
    if ready
    then
      return 0
    fi
    if read -t 1 -n 1 answer
    then
      echo ''
      [[ $answer != 'f' ]]
      return $?
    fi
  done
}

trigger_publish()
{
  f tj -j $pipeline/publish -w
}

echo ">>> Publishing with local ref '$ref' at $local_head"

if ready || poll
then
  trigger_publish
else
  echo '>>> Aborted by user.'
fi
