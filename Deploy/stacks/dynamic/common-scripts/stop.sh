#!/bin/bash

# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

# This is added to prevent warning messages, this variable should not be used during the stop process
export EXTERNAL_PORT="EXTERNAL_PORT should only be required at runtime!"

# Remove existing services started from this directory
if [ "$EXECUTABLE" == "docker" ]; then
    for SERVICE in $(${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml convert --services); do
        if [ -n "$(docker service ls -q -f "name=${STACK_NAME}_${SERVICE}")" ]; then
            ${EXECUTABLE} service rm "${STACK_NAME}_${SERVICE}" > /dev/null
        fi
    done
else
    # Modify IFS so for-loop only splits on newline rather than all whitespace
    OIFS=$IFS
    IFS='
'
    for SERVICE in $(podman-compose -f docker-compose-stack.yml -f docker-compose.yml --dry-run pull); do
        # Ignore output of merged compose file
        if [ "${SERVICE%' '*}" == "podman pull" ]; then
            # Get service name from echoed pull image command
            SERVICE="${SERVICE%':'*}"
            SERVICE="${SERVICE#*'/'}"
            # If pod is running remove it
            if [ -n "$(podman pod ls -q -f "name=${STACK_NAME}_${SERVICE}")" ]; then
                podman pod rm --force "${STACK_NAME}_${SERVICE}" > /dev/null
            fi
        fi
    done
    IFS=$OIFS
fi