#!/bin/bash

# Exit on first error
set -e
sed -i -e 's/"credsStore": "desktop.exe"/"credsStore": "wincred.exe"/' ~/.docker/config.json

get_executables(){
    # Ensure compatibility with podman
    if command -v docker &> /dev/null
    then
        EXECUTABLE="docker"
        COMPOSE_EXECUTABLE="docker compose"
    else

        >&2 echo "ERROR Podman cannot run in swarm mode so will have to move to Kubenetes before this actually works"
        # The "--podman-build-args" argument requires podman compose version 0.1.8
        pip3 install --user 'podman-compose==0.1.8'
        
        EXECUTABLE="podman"
        COMPOSE_EXECUTABLE="podman-compose"
    fi
}

init_swarm(){
    if [ "$(docker info --format '{{.Swarm.LocalNodeState}}')" != "active" ]; then
        get_executables
        ${EXECUTABLE} swarm init
    fi
}

# Attempt compatibility with podman
get_executables

if [[ -z "$NO_STACK_NAME" ]]; then
    # Read in the stack name as the first argument
    export STACK_NAME="$1"

    if (( $# >= 1 )); then
        shift
    fi
fi

export IMAGE_SUFFIX=