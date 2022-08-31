#!/bin/bash

# Build latest versions of JPS_BASE_LIB and STACK_CLIENTS
./build_py4jps_resources.sh

COMMAND=$1
shift

SCRIPTS_DIR="$(git rev-parse --show-toplevel)/Deploy/stacks/dynamic/common-scripts"
export SCRIPTS_DIR

"${SCRIPTS_DIR}/${COMMAND}.sh" "$@"