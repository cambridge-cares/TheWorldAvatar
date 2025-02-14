#!/bin/bash
# Load environment variables from target_gps_folder.env if it exists
if [ -f target_gps_folder.env ]; then
  export $(grep -v '^#' target_gps_folder.env | xargs)
fi

COMMAND=$1
shift

SCRIPTS_DIR="$(git rev-parse --show-toplevel)/Deploy/stacks/dynamic/common-scripts"
export SCRIPTS_DIR

"${SCRIPTS_DIR}/${COMMAND}.sh" "$@"