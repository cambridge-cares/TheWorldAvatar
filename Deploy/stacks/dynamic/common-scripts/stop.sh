#!/bin/bash

# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

# This is added to prevent warning messages, this variable should not be used during the stop process
export EXTERNAL_PORT="EXTERNAL_PORT should only be required at runtime!"

# Remove existing services started from this directory
if [ "$EXECUTABLE" == "docker" ]; then
    for SERVICE in $(${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml convert --services); do
        if [ -n "$(${EXECUTABLE} service ls -q -f "name=${STACK_NAME}_${SERVICE}")" ]; then
            ${EXECUTABLE} service rm "${STACK_NAME}_${SERVICE}" > /dev/null
        fi
    done
else
    # Modify IFS so for-loop only splits on newline rather than all whitespace
    OIFS=$IFS
    IFS='
'
    for SERVICE in $(${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml --dry-run pull); do
        # Ignore output of merged compose file
        if [ "${SERVICE%' '*}" == "podman pull" ]; then
            # Get service name from echoed pull image command
            SERVICE="${SERVICE%':'*}"
            SERVICE="${SERVICE##*'/'}"
            # If pod is running remove it
            for POD_ID in $(${EXECUTABLE} pod ls -q -f "name=${STACK_NAME}_${SERVICE}") ; do
                ${EXECUTABLE} pod rm --force "${POD_ID}" > /dev/null
            done
            # If container is running outside a pod remove it
            for CONTAINER_ID in $(${EXECUTABLE} ps -a -q -f "name=${STACK_NAME}_${SERVICE}"); do
                ${EXECUTABLE} rm --force "${CONTAINER_ID}" > /dev/null
            done
        fi
    done
    IFS=$OIFS
fi
