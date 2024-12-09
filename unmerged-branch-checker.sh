#!/bin/bash

# This script will check all unmerged branches to see if they contain unmerged commits to files within the specified directory

# Check if the path argument is provided
if [ -z "$1" ]; then
    echo -e "Usage: \e[32m $0 <path-to-folder>\e[37m Use this script to iterate over unmerged branches and see if any commits contain changes to file in the directory supplied as the argument"
    exit 1
fi

FOLDER_PATH=$1
BRANCH_COUNT=0

IFS=" " read -r -a mainBranches <<<"$(git branch -r | grep -v '\->' | grep '/main')"
echo "Main branch =" "${mainBranches[@]}"

# Iterate over all remote branches and check for changes in the specified folder
for branch in $(git branch -r | grep -v '\->' | grep -v '/main'); do
    BRANCH_COUNT=$((BRANCH_COUNT + 1))
    echo -e "Checking branch \e[32m#$BRANCH_COUNT\e[0m: $branch"
    git log --oneline --name-status "$branch" "${mainBranches[@]/#/^}" -- "$FOLDER_PATH" | sed 's/^/\x1b[31m/;s/$/\x1b[0m/'
done

echo -e "Total number of branches checked: \e[33m$BRANCH_COUNT\e[0m"
