#!/bin/bash

# This fixes issues with WSL not mounting the Windows directories in a stable way.
cd .

COMMAND=$1
shift

SCRIPTS_DIR="$(git rev-parse --show-toplevel)/Deploy/stacks/dynamic/common-scripts"
export SCRIPTS_DIR

"${SCRIPTS_DIR}/${COMMAND}.sh" "$@"