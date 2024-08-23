#!/bin/bash
# Delete all local branches without active remote branch,
# i.e. branches with 'gone' upstream branch

## Delete all branches without upstream
while read branch; do
  upstream=$(git rev-parse --abbrev-ref $branch@{upstream} 2>/dev/null)
  # Check if exit code of previous command was successful
  if [[ $? == 0 ]]; then
    ## Found upstream for branch
    echo $branch tracks $upstream
  else
    ## No Upstream! --> delete branch locally
    git branch -d $branch
  fi
done < <(git for-each-ref --format='%(refname:short)' refs/heads/)