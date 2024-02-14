#!/bin/bash

# Copy stack manager config file to stack manager repo
cp ./inputs/stack-manager/inputs/config/KINGS-LYNN.json ../../dynamic/stack-manager/inputs/config/

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
REQUIRED_VERSION="1.22.0"
sed -i "s/\(ghcr\.io\/cambridge-cares\/stack-manager\${IMAGE_SUFFIX}:\)[0-9]\+\.[0-9]\+\.[0-9]\+/\1${REQUIRED_VERSION}/" docker-compose.yml

# Restart stack with required version
bash ./stack.sh start $NETWORK_NAME

# Wait until stack-manager container is stopped (indicating end of stack startup)
while docker ps --format '{{.Names}}' | grep -q "stack-manager"; do
    echo "Stack-manager is still running. Waiting..."
    sleep 5
done

# Start agent containers
# Declare an associative array for all containers to start
declare -A agent_paths
agent_paths["airquality_agent"]="AirQualityAgent"
agent_paths["metoffice_agent"]="MetOfficeAgent"
agent_paths["river-data-uploader"]="FloodAgent"

agent_paths["epc_agent"]="EnergyPerformanceCertificateAgent"
agent_paths["landregistry_agent"]="HMLandRegistryAgent"
agent_paths["avgsqmprice_agent"]="AverageSquareMetrePriceAgent"
agent_paths["propertyvalue_agent"]="PropertyValueEstimationAgent"
agent_paths["floodassessment_agent"]="FloodAssessmentAgent"
agent_paths["floodwarnings_agent"]="FloodWarningAgent"

cd "../../../../Agents/"
for name in "${!agent_paths[@]}"; do
    # Change to agent directory
    cd ${agent_paths[$name]}
    # Start agent
    bash ./stack.sh start $NETWORK_NAME
    # Wait until container has started
    echo $name
    while docker ps --format '{{.Names}}' | grep -q "^$name"; do
        sleep 5
    done
    echo "$name container has started"
    cd ..
done