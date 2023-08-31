#!/bin/bash

# Copy stack manager config file to stack manager repo
cp ./inputs/redeployment/KINGS-LYNN.json ../../dynamic/stack-manager/inputs/config/

# Change directory to stack manager repo
cd ../../dynamic/stack-manager

# Remove "old" stack first as simply restarting a crashed stack seems 
# to default to having a password protected Blazegraph instance
NETWORK_NAME="KINGS-LYNN"
bash ./stack.sh remove $NETWORK_NAME &
while [[ $(docker ps --filter "network=$NETWORK_NAME" -q) ]]; do
    echo "Waiting for containers on $NETWORK_NAME to stop..."
    sleep 5
done

# Specify required stack-manager version number
# NOTE: 1.13.3 is the latest on that uses PostGIS 14
REQUIRED_VERSION="1.13.3"
sed -i "s/docker\.cmclinnovations\.com\/stack-manager\${IMAGE_SUFFIX}:[0-9]\+\.[0-9]\+\.[0-9]\+/docker.cmclinnovations.com\/stack-manager\${IMAGE_SUFFIX}:${REQUIRED_VERSION}/" docker-compose.yml

# Restart stack with required version
bash ./stack.sh start KINGS-LYNN