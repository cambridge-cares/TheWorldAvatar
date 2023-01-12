#!/bin/bash

PORT_FILE=.vscode/port.txt

N_PORTS_DEFAULT=1
MIN_PORT_DEFAULT=5005
MAX_PORT_DEFAULT="MIN_PORT + 50"

for ARG in "$@"
do
    if [[ "$ARG" == --help ]]; then
        echo "Usages:
    ./stack.sh ports [N_PORTS [MIN_PORT [MAX_PORT]]]    Return the numbers of the first N_PORTS free ports in the range MIN_PORT-MAX_PORT
    ./stack.sh ports write [MIN_PORT [MAX_PORT]]        Save the number of the first free port in the range MIN_PORT-MAX_PORT to file
    ./stack.sh ports read                               Load the number of the free port from file
Defaults:
    N_PORTS  = $N_PORTS_DEFAULT
    MIN_PORT = $MIN_PORT_DEFAULT
    MAX_PORT = $MAX_PORT_DEFAULT
"
        exit 0
    fi
done

NO_STACK_NAME=TRUE
# Load common functions
. "${SCRIPTS_DIR}/common_functions.sh"

ACTION=${1}
set -x
if [[ "$ACTION" == read ]]; then
    cat "${PORT_FILE}"
    set +x
    exit 0
fi

case $ACTION in
    ''|*[!0-9]*) N_PORTS=$N_PORTS_DEFAULT ;;
    *) N_PORTS=$1 ;;
esac

MIN_PORT=${2-$MIN_PORT_DEFAULT}
MAX_PORT=${3-$((MAX_PORT_DEFAULT))}

INCREMENT=1

port=$MIN_PORT
i=0
while (( port < MAX_PORT )) && (( i < N_PORTS )); do
    set -e
    if ! (${EXECUTABLE} service ls --format "{{.Ports}}" | grep ":$port->" > /dev/null); then # ! (netstat -taln | grep "$port" > /dev/null) &&
        echo "$port"
        if [[ "$ACTION" == write ]]; then
            echo "$port" > "${PORT_FILE}"
            set +x
            exit 0
        fi
        i=$((i+1))
    fi
    set +e
    port=$((port+INCREMENT))
done