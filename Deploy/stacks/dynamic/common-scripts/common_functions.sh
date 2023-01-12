#!/bin/bash

# Exit on first error
set -e

get_executables(){
    # Ensure compatibility with podman
    if command -v podman &> /dev/null
    then
        >&2 echo "ERROR Podman cannot run in swarm mode so will have to move to Kubenetes before this actually works"
        # The "--podman-build-args" argument requires podman compose version 0.1.8
        pip3 install --user 'podman-compose==1.0.3'
        
        EXECUTABLE="podman"
        COMPOSE_EXECUTABLE="podman-compose"
        API_SOCK="$XDG_RUNTIME_DIR/podman/podman.sock"
   else
        EXECUTABLE="docker"
        COMPOSE_EXECUTABLE="docker compose"
        API_SOCK="/var/run/docker.sock"
    fi

    export EXECUTABLE
    export COMPOSE_EXECUTABLE
    export API_SOCK
}

init_server(){
    if [ "$EXECUTABLE" == "docker" ]; then
        if [ "$(docker info --format '{{.Swarm.LocalNodeState}}')" != "active" ]; then
            docker swarm init
        fi
    else
        if [ ! -S "$PODMAN_SOCK" ] || [ -z "$(pidof podman)" ]; then
            podman system service -t 0 "unix://$PODMAN_SOCK" &
        fi
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