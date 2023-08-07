#!/bin/bash

#
# Use this script to build the latest copy of the DTVF image for deployment.
#

# Read the version
VERSION=`cat -s "./VERSION" 2>/dev/null`
echo "Determined DTVF version as: $VERSION"

# Write the '.env' file
echo "TAG=$VERSION" > ./docker/.env
echo "Written .env file"

# Run docker compose build command
echo "Building dtvf-base-image..."
docker compose -f ./docker/docker-compose.yml build

# Clean up
rm ./docker/.env