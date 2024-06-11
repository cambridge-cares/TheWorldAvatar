#!/bin/bash

COMMAND=$1
shift

SCRIPTS_DIR="$(git rev-parse --show-toplevel)/Deploy/stacks/dynamic/common-scripts"
export SCRIPTS_DIR

"${SCRIPTS_DIR}/${COMMAND}.sh" "$@"