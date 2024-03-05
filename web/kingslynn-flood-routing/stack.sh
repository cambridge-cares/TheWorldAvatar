#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 2 ] || [ "$1" != "start" ]; then
    echo "Usage: $0 start <STACK_PORT_NUMBER>"
    exit 1
fi

STACK_PORT_NUMBER="$2"

# Set the base URL
BASE_URL="http://localhost:$STACK_PORT_NUMBER"

# TravellingSalesmanAgent
TS_URL="$BASE_URL/travellingsalesmanagent/runtsp?function=UR"
curl -X POST "$TS_URL"

# IsochroneAgent
ISOCHRONE_URL="$BASE_URL/isochroneagent/update?function=UR&timethreshold=10&timeinterval=2"
curl -X POST "$ISOCHRONE_URL"

# NetworkAnalysisAgent
NETWORK_URL="$BASE_URL/networkanalysisagent/runtc?function=UR"
curl -X POST "$NETWORK_URL"