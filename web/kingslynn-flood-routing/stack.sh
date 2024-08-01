#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -ne 2 ] || [ "$1" != "start" ]; then
    echo "Usage: $0 start <STACK_NAME>"
    exit 1
fi

STACK_NAME="$2"

# Set the base URL
BASE_URL="http://localhost:3838"

# TravellingSalesmanAgent
TS_URL="$BASE_URL/travellingsalesmanagent/runtsp?function=UR"
curl -X POST "$TS_URL"

# IsochroneAgent
ISOCHRONE_URL="$BASE_URL/isochroneagent/update?function=UR&timethreshold=10&timeinterval=2"
curl -X POST "$ISOCHRONE_URL"

# NetworkAnalysisAgent
NETWORK_URL="$BASE_URL/networkanalysisagent/runtc?function=UR"
curl -X POST "$NETWORK_URL"