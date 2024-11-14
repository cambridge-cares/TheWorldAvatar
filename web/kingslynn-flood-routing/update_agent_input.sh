#!/bin/bash

## replace stack name in POI query of TravellingSalesmanAgent

dir_TSA_POI="./TravellingSalesmanAgent/inputs/UR/POIqueries/grid_primary_site.sparql"
STACK_PLACEHOLDER="<STACK_NAME>"
STACK_NAME="routing"
sed -i "s#$STACK_PLACEHOLDER#$STACK_NAME#g" "$dir_TSA_POI"

## overwrite input files of agents

agent_list=("TravellingSalesmanAgent" "IsochroneAgent" "NetworkAnalysisAgent")

TWA_DIR=$(dirname "$(dirname "$(pwd)")")

# Loop through each string in the list
for str in "${agent_list[@]}"; do
# Print the current string
echo "Agent: $str"
echo "$TWA_DIR/Agent/$str"
rsync -av "$str/" "$TWA_DIR/Agents/$str/"
done