#!/bin/bash

replace_string() {
    local directory="$1"
    local placeholder="$2"
    local new_string="$3"

    find "$directory" -type f \( -name "*.html" -o -name "*.json" \) | while read -r file; do
        echo "Processing file: $file"
        sed -i "s#$placeholder#$new_string#g" "$file"
    done
}

escape_url() {
local url="$1"
printf '%s\n' "$url" | sed -e 's/[\/&]/\\&/g'
}

## Load variables from .env file

source .env

## replace URL for visualisation

dir_webspace="./stack-manager-inputs/data/webspace"
URL_PLACEHOLDER="<PUBLIC_URL>"

replace_string "$dir_webspace" "$URL_PLACEHOLDER" "$PUBLIC_URL"

## replace TWA directory in stack manager config files

# Assume TWA is two level up from here
dir_config="./stack-manager-inputs/config"
DIR_PLACEHOLDER="<REPLACE_WITH_YOUR_DIRECTORY>"
TWA_DIR=$(dirname "$(dirname "$(pwd)")")

replace_string "$dir_config" "$DIR_PLACEHOLDER" "$TWA_DIR"

## rename stack config file name

mv "./stack-manager-inputs/config/routing.json" "./stack-manager-inputs/config/$STACK_NAME.json"
mv "./stack-data-uploader-inputs/config/routing.json" "./stack-data-uploader-inputs/config/$STACK_NAME.json"

## send files to deployment

DEPLOY_DIR="$TWA_DIR/Deploy/stacks/dynamic"
cp -rf ./stack-manager-inputs/* "$DEPLOY_DIR/stack-manager/inputs/"
cp -rf ./stack-data-uploader-inputs/* "$DEPLOY_DIR/stack-data-uploader/inputs/"

## replace stack name in POI query of TravellingSalesmanAgent

dir_TSA_POI="./TravellingSalesmanAgent/inputs/UR/POIqueries/grid_primary_site.sparql"
STACK_PLACEHOLDER="<STACK_NAME>"
sed -i "s#$STACK_PLACEHOLDER#$STACK_NAME#g" "$dir_TSA_POI"

## overwrite input files of agents

agent_list=("TravellingSalesmanAgent" "IsochroneAgent" "NetworkAnalysisAgent")

# Loop through each string in the list
for str in "${agent_list[@]}"; do
echo "$str/" 
echo "$TWA_DIR/Agents/$str/"
rsync -av "$str/" "$TWA_DIR/Agents/$str/"
done