#!/bin/bash

# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

# Ensure swarm mode is initialised
init_swarm

# Pull these images here because the "--with-registry-auth" argument
# used below only seems to add credentials for Docker Hub
private_images=('docker.cmclinnovations.com/blazegraph:1.1.0' 'docker.cmclinnovations.com/geoserver:2.20.4')
for private_image in "${private_images[@]}" ; do
    if [ -z "$(docker images -q "$private_image")" ]; then
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

if [[ -n "${DEBUG_PORT}" ]]; then
    export DEBUG_PORT
    DEBUG_COMPOSE_FILE="--compose-file=docker-compose-debug.yml"
fi

# Redeploy services
${EXECUTABLE} stack deploy --compose-file=docker-compose-stack.yml --compose-file=docker-compose.yml $DEBUG_COMPOSE_FILE --with-registry-auth "${STACK_NAME}"