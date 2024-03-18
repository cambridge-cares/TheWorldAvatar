#!/bin/bash

remove_stack_volumes() {

    STACK_NAME="$1"
    i=0
    while
        # Wait a max of 30 seconds
        (( i < 30 )) \
        && [[ -n $(${EXECUTABLE} ps -a --filter "name=$STACK_NAME" --format "{{.ID}}") ]]
    do
        echo "Waiting for containers to be removed"
        sleep 1
        i=$((i + 1))
    done

    sleep 2

    ${EXECUTABLE} volume ls --filter "name=$STACK_NAME" --format "{{.Name}}" | xargs -r ${EXECUTABLE} volume rm -f
}


# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

while (( $# >= 1 )); do 
    case $1 in
        --volumes|-v) REMOVE_VOLUMES=TRUE;;
        *) SERVICE_NAME=$1;;
    esac;
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

case "$STACK_NAME" in
    "")
        echo "Usages:
    ./stack.sh remove all [OPTIONS]             Remove all stacks (docker)
    ./stack.sh remove all [OPTIONS]             Remove all containers and pods (podman)
    ./stack.sh remove STACK_NAME [OPTIONS]      Remove stack \"STACK_NAME\"
    ./stack.sh remove STACK_NAME SERVICE_NAME   Remove service \"STACK_NAME-SERVICE_NAME\" (docker)
    
Options:
    -v, --volumes       Also remove named volumes
"
exit;;
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
            # NB Removing containers before pods is faster if they don't respond to kill signals.
            ${EXECUTABLE} rm --force --all
            ${EXECUTABLE} pod rm --force --all
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
            # If pod is running remove it
            for POD_ID in $(${EXECUTABLE} pod ls -q -f "name=${STACK_NAME}") ; do
                ${EXECUTABLE} pod rm --force "${POD_ID}" > /dev/null
            done
            # If container is running outside a pod remove it
            for CONTAINER_ID in $(${EXECUTABLE} ps -a -q -f "name=${STACK_NAME}"); do
                ${EXECUTABLE} rm --force "${CONTAINER_ID}" > /dev/null
            done
        fi

        if [[ -n "$REMOVE_VOLUMES" ]]; then
            # Remove named volumes
            remove_stack_volumes "${STACK_NAME}"
        fi
    ;;

esac
