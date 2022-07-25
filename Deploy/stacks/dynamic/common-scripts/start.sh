#!/bin/bash

# Load common functions
. ${SCRIPTS_DIR}/common_functions.sh

# Ensure swarm mode is initialised
init_swarm

# Remove existing services started from this directory
${SCRIPTS_DIR}/stop.sh "${STACK_NAME}"

if [[ $# -eq 2 ]]; then
    export DEBUG_PORT=${2}
    DEBUG_COMPOSE_FILE="--compose-file=docker-compose-debug.yml"
fi

# Redeploy services
${EXECUTABLE} stack deploy --compose-file=docker-compose-stack.yml --compose-file=docker-compose.yml $DEBUG_COMPOSE_FILE --with-registry-auth "${STACK_NAME}"