#!/bin/bash

COMMAND=$1
shift

export SCRIPTS_DIR="$(git rev-parse --show-toplevel)/Deploy/stacks/dynamic/common-scripts"

"${SCRIPTS_DIR}/${COMMAND}.sh" "$@"