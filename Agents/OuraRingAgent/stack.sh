#!/bin/bash

COMMAND=$1
shift

SCRIPTS_DIR="$(git rev-parse --show-toplevel)/stack/common-scripts"
export SCRIPTS_DIR

"${SCRIPTS_DIR}/${COMMAND}.sh" "$@"