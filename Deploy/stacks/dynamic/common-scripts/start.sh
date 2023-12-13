#!/bin/bash

# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

# Ensure the host server is initialised
init_server

# Pull these images here because the "--with-registry-auth" argument
# used below only seems to add credentials for Docker Hub
private_images=(
    # Include list of images here that need to be pulled with auth
)
for private_image in "${private_images[@]}" ; do
    if [ -z "$(${EXECUTABLE} images -q "$private_image")" ]; then
        ${EXECUTABLE} pull -q "$private_image"
    fi
done

# Remove existing services started from this directory
"${SCRIPTS_DIR}/stop.sh" "${STACK_NAME}"

while (( $# >= 1 )); do
    case $1 in
        --debug-port) DEBUG_PORT=$2; shift;;
        *) export EXTERNAL_PORT=${1};;
    esac;
    shift
done

if [ "${EXECUTABLE}" == "docker" ]; then
    if [[ -n "${DEBUG_PORT}" ]]; then
        export DEBUG_PORT
        DEBUG_COMPOSE_FILE="--compose-file=docker-compose-debug.yml"
    fi

    if [ -f "docker-compose-docker.yml" ]; then
        EXECUTABLE_SPECIFIC_COMPOSE_FILE="--compose-file=docker-compose-docker.yml"
    fi

    # Redeploy services
    ${EXECUTABLE} stack deploy --compose-file=docker-compose-stack.yml --compose-file=docker-compose.yml $EXECUTABLE_SPECIFIC_COMPOSE_FILE $DEBUG_COMPOSE_FILE --with-registry-auth "${STACK_NAME}"

    # Don't exit if sub-process fails
    set +e
    # Wait until stack manager service is not in a warm-up state
    while (${EXECUTABLE} service ps "${STACK_NAME}_stack-manager" --format='{{.CurrentState}}' | grep -q -E 'New|Pending|Assigned|Accepted|Ready|Preparing|Starting' ) ; do
        # Print a dot
        echo -n .
    done
    # Clear the dots
    echo -e "\033[2K"
    # Check the final state of the container
    if(${EXECUTABLE} service ps "${STACK_NAME}_stack-manager" --format='{{.CurrentState}}' | grep -q -E 'Running|Complete'); then
        echo "Starting service ${STACK_NAME}_stack-manager succeeded"
    else
        echo "Starting service ${STACK_NAME}_stack-manager failed:"
        # Output the error message
        ${EXECUTABLE} service ps --no-trunc "${STACK_NAME}_stack-manager" --format='{{.Error}}'
    fi
else
    if [[ -n "${DEBUG_PORT}" ]]; then
        export DEBUG_PORT
        DEBUG_COMPOSE_FILE="-f docker-compose-debug.yml"
    fi

    if [ -f "docker-compose-podman.yml" ]; then
        EXECUTABLE_SPECIFIC_COMPOSE_FILE="-f docker-compose-podman.yml"
    fi

    # Redeploy services
    ${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml $EXECUTABLE_SPECIFIC_COMPOSE_FILE $DEBUG_COMPOSE_FILE -p "${STACK_NAME}" up -d
fi