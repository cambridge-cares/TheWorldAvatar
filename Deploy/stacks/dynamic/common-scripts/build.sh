#!/bin/bash

# Load common functions
. ${SCRIPTS_DIR}/common_functions.sh

# Attempt compatibility with podman
get_executables

# This is added to prevent warning messages, this variable should not be used during the build or push processes
export STACK_NAME="STACK NAME should only be required at runtime!"

# Build the images ("ARG CACHEBUST=1" can be added before a command in the Dockerfile to force it to always be rerun)
${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml -f docker-compose-build.yml build --build-arg CACHEBUST="$(date +%s)" "$@"

# Push the images to the remote repo, docker swarm works better with pushed images
${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml -f docker-compose-build.yml push