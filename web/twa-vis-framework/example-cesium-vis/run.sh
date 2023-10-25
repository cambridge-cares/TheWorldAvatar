#!/bin/bash

#
# Use this script to run the example visualisation. It expects a single parameter
# (named "TAG") that lists the tag to be used for the "ghcr.io/cambridge-cares/twa-vf"
# image that hosts the visualisation. Note that this image needs to be pulled
# manually first.
# 
# If no TAG parameter is supplied, the tag will be pulled from the current contents
# of the "../library/VERSION" file will be used.
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
    echo "No TAG argument supplied, will pull tag from "../library/VERSION" file"
    TAG=$(cat ../library/VERSION)
fi

# Write the '.env' file
echo "TAG=$TAG" > .env
echo "Written .env file"

# Run docker compose build command
echo "Running the example Cesium visualisation..."
docker compose -f docker-compose.yml up -d

# Clean up
rm .env