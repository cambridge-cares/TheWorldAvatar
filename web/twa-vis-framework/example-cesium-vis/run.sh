#!/bin/bash

#
# Use this script to run the example visualisation. It expects a single parameter
# (named "TAG") that lists the tag to be used for the "ghcr.io/cambridge-cares/twa-vf"
# that hosts the visualisation.
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
if [[ ! -v TAG ]]; then 
    echo "No TAG argument supplied, cannot continue."
    exit -1
fi

# Write the '.env' file
echo "TAG=$TAG" > .env
echo "Written .env file"

# Run docker compose build command
echo "Running the example Cesium visualisation..."
docker compose -f docker-compose.yml up -d

# Clean up
rm .env