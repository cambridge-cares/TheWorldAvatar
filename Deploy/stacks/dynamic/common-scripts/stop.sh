#!/bin/bash

# Load common functions
. ${SCRIPTS_DIR}/common_functions.sh

set +e

# Remove existing services started from this directory
for SERVICE in $(${COMPOSE_EXECUTABLE} -f docker-compose-stack.yml -f docker-compose.yml convert --services); do
	${EXECUTABLE} service rm "${STACK_NAME}_${SERVICE}"
done

set -e