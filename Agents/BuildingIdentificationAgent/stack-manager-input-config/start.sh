#!/bin/bash

#
# This script spins up a TWA Stack, loads sample data, and adds visualisations
# to illustrate the use of the TWA Visualisation Framework.
# 
# Arguments:
#   PASSWORD=myPassword (required, sets password for stack services)
#   TAG=myTag (optional, tag for TWA-vf images)
#
# The TAG parameter lists the tag to be used for the twa-vf image that hosts
# the visualisations. Note that this image needs to be pulled manually first.
# If no TAG parameter is supplied, the tag will be pulled from the current
# contents of the "../library/VERSION" file will be used.
#
# Author: Michael Hillman (support<@>cmcl.io)
#

# Parse arguments
for ARGUMENT in "$@"
do
   KEY=$(echo $ARGUMENT | cut -f1 -d=)
   KEY_LENGTH=${#KEY}
   VALUE="${ARGUMENT:$KEY_LENGTH+1}"
   export "$KEY"="$VALUE"
done

# Store starting directory
START=$(pwd)

echo 
echo "This script will remove any existing stack manager and uploader configurations,"
echo "then launch a new stack instance with some example TWA visualiations."
echo 
read -p "Proceed with launch (Y/N)? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    echo "Script starting at $(date)"

    # Check for required PASSWORD argument
    if [[ ! -v PASSWORD ]]; then 
        echo "No PASSWORD argument supplied, cannot continue."
        exit 1
    fi

    # Check for optional TAG arguments
    if [[ ! -v TAG ]]; then 
        echo "No TAG argument supplied, will pull TWA-VF image tag from ../library/VERSION file"
        TAG=$(cat ../library/VERSION)
    fi

    # Get the root of the current Git repo
    ROOT=$(git rev-parse --show-toplevel)
    if [[ "$ROOT" == *"not a git"* ]]; then
        echo "Not within a valid Git repository, cannot continue."
        exit 1
    fi

    # Clear any existing stack manager configs
    echo "Clearing existing manager configurations and copying in new ones..."
    MANAGER_CONFIG="$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/config"
    MANAGER_CONFIG_SERVICES="${MANAGER_CONFIG}/services"
    git clean -xdf "$MANAGER_CONFIG/"

    # Copy in the stack manager configs
    cp -r "./inputs/config/manager/." "$MANAGER_CONFIG/"

    # Check for optional DEBUG arguments
    if [[ -v DEBUG ]]; then 
        echo "Building identification agent running in debug mode."
        cp "buildingidentificationagent-debug.json" "$MANAGER_CONFIG_SERVICES/" 
    else
        echo "Building identification agent running in production mode."
        cp "buildingidentificationagent.json" "$MANAGER_CONFIG_SERVICES/" 
    fi 


    # Inject TWA-VF tag into visualisation configs
    sed -i "s|TAG|$TAG|g" "$MANAGER_CONFIG/services/mapbox-vis.json"
    sed -i "s|TAG|$TAG|g" "$MANAGER_CONFIG/services/cesium-vis.json"

    # Copy the visualisation files into the special volume populator folder
    echo "Clearing existing manager volumes and copying in new ones..."
    MANAGER_VOLUMES="$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/data/"
    git clean -xdf "$MANAGER_VOLUMES/"
    cp -r "./inputs/volumes/." "$MANAGER_VOLUMES"


    # Write out the stack passwords
    echo $PASSWORD > "$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/secrets/geoserver_password"
    echo $PASSWORD > "$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/secrets/postgis_password"
    echo $PASSWORD > "$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/secrets/grafana_password"

    # Copy in the mapbox secret files
    cp "./inputs/secrets/mapbox_username" "$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/secrets/"
    cp "./inputs/secrets/mapbox_api_key" "$ROOT/Deploy/stacks/dynamic/stack-manager/inputs/secrets/"

    # Run the stack manager to start a new stack
    cd "$ROOT/Deploy/stacks/dynamic/stack-manager" || exit
    echo "Running the stack start up script..."
    ./stack.sh start heat

    # Wait for the stack manager container to exit
    echo "Waiting for the stack manager to finish..."
    while docker ps --format '{{.Names}}' | grep -qE 'stack-manager'; do
        sleep 1
    done
        sleep 5
        cd "$START"

    # Clear any existing stack uploader configs
    # echo "Clearing existing uploader configs and copying in new ones..."
    # UPLOAD_CONFIG="$ROOT/Deploy/stacks/dynamic/stack-data-uploader/inputs/config"
    # git clean -xdf "$UPLOAD_CONFIG/"

    # Copy in uploader configs
    # cp -r "./inputs/config/uploader"/* "$UPLOAD_CONFIG/"

    # Clear any existing stack uploader data
    # echo "Clearing existing uploader data sets and copying in new ones..."
    # UPLOAD_DATA="$ROOT/Deploy/stacks/dynamic/stack-data-uploader/inputs/data"
    # git clean -xdf "$UPLOAD_DATA/"

    # Copy in the data for upload
    # cp -r -v "./inputs/data/." "$UPLOAD_DATA/" 

    # Run the uploader to upload data
    # cd "$ROOT/Deploy/stacks/dynamic/stack-data-uploader" || exit
    # echo "Running the stack uploader script, this may take some time..."
    # ./stack.sh start heat 

    # Wait for the stack uploader container to exit
    # echo "Waiting for the stack uploader to finish..."
    # while docker ps --format '{{.Names}}' | grep -qE 'stack-data-uploader'; do
    #     sleep 1
    # done
    #     sleep 5
    #     cd "$START"

    printf "\n"
    echo "----------"
    echo "Script completed at $(date)"
    echo "Visualisations should be available at following URLS:"
    echo "    http://localhost:3838/mapbox-vis/"
    echo "    http://localhost:3838/cesium-vis/"
    echo "----------"
else
    echo "Script ended without starting up stack."
    exit 1
fi

exit 0