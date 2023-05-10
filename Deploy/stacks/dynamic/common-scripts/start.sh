#!/bin/bash

# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

# Ensure the host server is initialised
init_server

# Pull these images here because the "--with-registry-auth" argument
# used below only seems to add credentials for Docker Hub
private_images=('docker.cmclinnovations.com/blazegraph:1.1.0'
                'docker.cmclinnovations.com/geoserver:2.20.4'
                'docker.cmclinnovations.com/vcity/py3dtilers:latest')
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