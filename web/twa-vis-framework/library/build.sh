#!/bin/bash

#
# Use this script to build the latest copy of the TWA-VF base image, strictly for
# local testing only. Building for release, with the proper Docker tags is handled
# by automated GitHub actions.
#
# This script will build the following image, it should only be used for local
# testing, do not push it to any image repositories.
# 
#   ghcr.io/cambridge-cares/twa-vf:local
#

# Build 'latest' tagged image
docker compose -f docker-compose.yml build

# Update tags
docker tag ghcr.io/cambridge-cares/twa-vf:latest ghcr.io/cambridge-cares/twa-vf:local
docker rmi ghcr.io/cambridge-cares/twa-vf:latest

echo "Image now available as ghcr.io/cambridge-cares/twa-vf:local"
echo "Only use for local testing, do not push it."