#!/bin/bash

remove_stack_volumes() {

    STACK_NAME="$1"
    i=0
    while
        # Wait a max of 30 seconds
        ((i < 30)) &&
            [[ -n $(${EXECUTABLE} ps -a --filter "name=$STACK_NAME" --format "{{.ID}}") ]]
    do
        echo "Waiting for containers to be removed"
        sleep 1
        i=$((i + 1))
    done

    sleep 2

    ${EXECUTABLE} volume ls --filter "name=^${STACK_NAME}_" --format "{{.Name}}" | xargs -r "${EXECUTABLE}" volume rm -f
}

# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

while (($# >= 1)); do
    case $1 in
    --volumes | -v) REMOVE_VOLUMES=TRUE ;;
    *) SERVICE_NAME=$1 ;;
    esac
    shift
done

if [[ -n "$SERVICE_NAME" ]]; then
    if [ "$EXECUTABLE" == "docker" ]; then
        ${EXECUTABLE} service rm "${STACK_NAME}-${SERVICE_NAME}" || ${EXECUTABLE} service rm "${STACK_NAME}_${SERVICE_NAME}"
    else
        echo "Info: Not implemented. No action taken."
    fi
    exit
fi

if [[ -n "$REMOVE_VOLUMES" ]]; then
    # If we are removing the volumes then there is no state to loose
    timeout=0
else
    # If we are keeping the volumes then give the containers a chance to save their state
    timeout=10
fi

case "$STACK_NAME" in
"" | "--help" | "-h")
    echo "Usages:
    ./stack.sh remove all [OPTIONS]             Remove all stacks (docker)
    ./stack.sh remove all [OPTIONS]             Remove all containers and pods (podman)
    ./stack.sh remove STACK_NAME [OPTIONS]      Remove stack \"STACK_NAME\"
    ./stack.sh remove STACK_NAME SERVICE_NAME   Remove service \"STACK_NAME-SERVICE_NAME\" (docker)
    
Options:
    -v, --volumes       Also remove named volumes
"
    ;;
"all")
    if [ "$EXECUTABLE" == "docker" ]; then
        ALL_STACKS=$(${EXECUTABLE} stack ls --format="{{.Name}}")
        # Remove all stacks
        ${EXECUTABLE} swarm leave --force

        if [[ -n "$REMOVE_VOLUMES" ]]; then
            # Remove named volumes
            for STACK_NAME in $ALL_STACKS; do
                remove_stack_volumes "${STACK_NAME}"
            done
        fi
    else
        # Removing networks removes containers that are in them
        # NB Removing containers before pods is faster if they don't respond to kill signals.
        # Regex in filiter required as podman doesn't like it if you try to remove the default "podman" network
        ${EXECUTABLE} network ls -q -f "name=^(?!podman$).*$" | xargs -r -I{} "${EXECUTABLE}" network rm -ft "$timeout" "{}"
        ${EXECUTABLE} rm --force --all -t "$timeout"
        ${EXECUTABLE} pod rm --force --all -t "$timeout"
        ${EXECUTABLE} secret rm --all
        if [[ -n "$REMOVE_VOLUMES" ]]; then
            ${EXECUTABLE} volume rm --force --all
        fi
    fi
    ;;
*)
    if [ "$EXECUTABLE" == "docker" ]; then
        # Don't exit if stack has already been removed
        set -e
        # Remove named stack
        ${EXECUTABLE} stack rm "${STACK_NAME}"
        set +e
    else
        # Remove stack network (this also removes the containers within it)
        ${EXECUTABLE} network rm -ft "$timeout" "${STACK_NAME}" >/dev/null

        # Regex in "name=^${STACK_NAME}_" filters is an attempt to prevent "stack-10" being removed with "stack-1"

        # If pod is running remove it (don't need to force as relevant containers should have been removed with the network)
        ${EXECUTABLE} pod ls -q -f "name=^${STACK_NAME}_" | xargs -r -I{} "${EXECUTABLE}" pod rm "{}" >/dev/null

        # If a container is running outside a pod remove it
        ${EXECUTABLE} ps -a -q -f "name=^${STACK_NAME}_" | xargs -r -I{} "${EXECUTABLE}" rm -ft "$timeout" "{}" >/dev/null

        # Remove any secrets and configs
        ${EXECUTABLE} secret ls -q --filter "name=^${STACK_NAME}_" | xargs -r -I{} "${EXECUTABLE}" secret rm "{}" >/dev/null

    fi

    if [[ -n "$REMOVE_VOLUMES" ]]; then
        # Remove named volumes
        remove_stack_volumes "${STACK_NAME}"
    fi
    ;;

esac
