#!/bin/bash

## Load variables from .env file

source .env

# Set the base URL
BASE_URL="http://localhost:$PORT_NUMBER"

# TravellingSalesmanAgent
echo "Calling TravellingSalesmanAgent..."
start_time=$(date +%s)

TS_URL="$BASE_URL/travellingsalesmanagent/runtsp?function=UR"
curl -X POST "$TS_URL"

end_time=$(date +%s)
elapsed_time=$((end_time - start_time))
echo ""
echo "TravellingSalesmanAgent took $elapsed_time seconds to complete."

# IsochroneAgent
echo "Calling IsochroneAgent..."
start_time=$(date +%s)

ISOCHRONE_URL="$BASE_URL/isochroneagent/update?function=UR&timethreshold=10&timeinterval=2"
curl -X POST "$ISOCHRONE_URL"

end_time=$(date +%s)
elapsed_time=$((end_time - start_time))
echo ""
echo "IsochroneAgent took $elapsed_time seconds to complete."

# NetworkAnalysisAgent
echo "Calling NetworkAnalysisAgent..."
start_time=$(date +%s)

NETWORK_URL="$BASE_URL/networkanalysisagent/runtc?function=UR"
curl -X POST "$NETWORK_URL"

end_time=$(date +%s)
elapsed_time=$((end_time - start_time))
echo ""
echo "NetworkAnalysisAgent took $elapsed_time seconds to complete."