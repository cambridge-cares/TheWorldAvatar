#!/bin/bash

#
# This script copies the committed sample data and configurations into the
# required stack manager/uploader directories to spin up a local stack with
# enough data to test the FeatureInfoAgent.
# 
# Author: Michael Hillman (mdhillman<@>cmcl.io)
#

# Parse arguments
for ARGUMENT in "$@"
do
   KEY=$(echo $ARGUMENT | cut -f1 -d=)
   KEY_LENGTH=${#KEY}
   VALUE="${ARGUMENT:$KEY_LENGTH+1}"
   export "$KEY"="$VALUE"
done

# Check for required arguments
if [[ ! -v PASSWORD ]]; then 
    echo "No PASSWORD argument supplied, cannot continue."
    exit -1
fi

# Store starting directory
START=$(pwd)

echo 
echo "This script will remove any existing stack manager and uploader configurations,"
echo "start a new stack, then upload the supplied data sets and dashboards."
echo 
read -p "Ready to begin (Y/N)? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    # Get the root of the current Git repo
    ROOT=$(git rev-parse --show-toplevel)
    if [[ "$ROOT" == *"not a git"* ]]; then
        echo "Not within a valid Git repository, cannot continue."
        exit -1
    fi

    # Write out the stack passwords
    echo $PASSWORD > "$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/secrets/geoserver_password"
    echo $PASSWORD > "$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/secrets/postgis_password"
    
    # Get the local version of the FIA
    # Read the version from the pom.xml file
    VERSION="$(xmllint --xpath "//*[local-name()='project']/*[local-name()='version']/text()" ../code/pom.xml)"
    echo "Discovered FIA version as $VERSION"

    # Clear any existing stack manager configs
    MANAGER_CONFIG="$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/config/"
    rm -rf "$MANAGER_CONFIG/*"

    # Copy in manager config
    cp "./FIATESTSTACK.json" "$MANAGER_CONFIG/"

    # Copy in new FIA config
    cp "./feature-info-agent.json" "$MANAGER_CONFIG/services/"
    sed -i "s|VERSION|$VERSION|g" "$MANAGER_CONFIG/services/feature-info-agent.json"

    # Run the stack manager to start a new stack
    cd "$ROOT/Deploy/stacks/dynamic/stack-manager"
    echo "Running the stack start up script..."
    ./stack.sh start FIATESTSTACK 38383

    # Wait for the stack to start
    echo 
    read -p "Press enter to continue once the stack is up and running..."
    sleep 5

    cd "$START"
    
    # Clear any existing stack uploader configs
    UPLOAD_CONFIG="$ROOT/Deploy/stacks/dynamic/stack-data-uploader/inputs/config"
    rm -rf "${UPLOAD_CONFIG:?}"/*

    # Clear any existing stack uploader data
    UPLOAD_DATA="$ROOT/Deploy/stacks/dynamic/stack-data-uploader/inputs/data"
    rm -rf "${UPLOAD_DATA:?}"/*

    # Copy in the stack uploader config(s)
    cp "./sample-data.json" "$UPLOAD_CONFIG/"

    # Copy in the data for upload
    mkdir -p "$UPLOAD_DATA/sample-data/aboxes"
    mkdir -p "$UPLOAD_DATA/sample-data/tboxes"
    cp "./sample-mapping.obda" "$UPLOAD_DATA/sample-data/" 
    cp "./sample-aboxes.csv" "$UPLOAD_DATA/sample-data/aboxes/" 
    cp "./sample-tboxes.csv" "$UPLOAD_DATA/sample-data/tboxes/" 

    # Run the uploader to upload data
    cd "$ROOT/Deploy/stacks/dynamic/stack-data-uploader"
    echo "Running the stack uploader script, this may take some time..."
    ./stack.sh start FIATESTSTACK 

else
    echo "Exiting script early."
    exit 1
fi