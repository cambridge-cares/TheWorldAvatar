#!/bin/bash

NO_STACK_NAME=TRUE

# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

# Attempt compatibility with podman
get_executables

# This is added to prevent warning messages, these variables should not be used during the build or push processes
export STACK_NAME="STACK NAME should only be required at runtime!"
export EXTERNAL_PORT="EXTERNAL_PORT should only be required at runtime!"
CACHEBUST="$(date +%s)"

if [[ -f "docker-compose-build-test.yml" ]]; then
    export IMAGE_SUFFIX=-test
    # Build the test image
    ${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml -f docker-compose-build.yml -f docker-compose-build-test.yml build --build-arg CACHEBUST="${CACHEBUST}" "$@"
    # Get the image names and iterate over them
    for test_image in $(${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml -f docker-compose-build.yml -f docker-compose-build-test.yml convert --images); do
        # Run the test image
        ${EXECUTABLE} run --rm --mount type=bind,src=/var/run/docker.sock,dst=/var/run/docker.sock "$test_image"
        # Remove the test image
        ${EXECUTABLE} rmi "$test_image"
    done
    export IMAGE_SUFFIX=
fi

while (( $# >= 1 )); do 
    case $1 in
    --debug-port) DEBUG_PORT=$2; shift;;
    esac;
    shift
done

if [[ -n "${DEBUG_PORT}" ]]; then
    export DEBUG_PORT
    DEBUG_COMPOSE_FILE="-f=docker-compose-debug.yml"
fi

# Build the images ("ARG CACHEBUST=1" can be added before a command in the Dockerfile to force it to always be rerun)
${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml -f docker-compose-build.yml $DEBUG_COMPOSE_FILE build --build-arg CACHEBUST="${CACHEBUST}" "$@"

# Push the images to the remote repo, docker swarm works better with pushed images
${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml -f docker-compose-build.yml $DEBUG_COMPOSE_FILE push