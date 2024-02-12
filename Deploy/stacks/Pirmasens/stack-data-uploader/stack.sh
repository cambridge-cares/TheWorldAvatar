#!/bin/bash

COMMAND=$1
shift

SCRIPTS_DIR="$(git rev-parse --show-toplevel)/common-stack-scripts"
export SCRIPTS_DIR

"${SCRIPTS_DIR}/${COMMAND}.sh" "$@"