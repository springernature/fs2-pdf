#!/usr/bin/env bash
set -euo pipefail

ci_files=$(find ./** -name "ci*.halfpipe.io.yml")

for file in $ci_files; do
  echo "Generating $file"
  pushd "$(dirname "${file}")"
  halfpipe --input "$(basename "${file}")"
  popd
done

pr_workflow_files=$(find ./.github/workflows/ -name "ci-pull-request*.yml")
for file in $pr_workflow_files; do
  echo "Fixing pull request workflow $file"
  sed -i "" 's/push:/pull_request:/g' $file
done