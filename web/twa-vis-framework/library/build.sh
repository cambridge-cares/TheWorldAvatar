#!/bin/bash

#
# Use this script to build the latest copy of the TWA-VF base image, strictly for
# local testing only. Building for release, with the proper Docker tags is handled
# by automated GitHub actions.
#
# This script will build an image using the tag from the "./VERSION" file. It should
# only be pushed at the start of the PR process to allow testers to pull the image
# needed to test the PR. In these cases, the image tag should follow the below
# format.
# 
#   ghcr.io/cambridge-cares/twa-vf:0.0.0-BRANCHNAME-SNAPSHOT
#

# Build 'latest' tagged image
docker compose -f docker-compose.yml build

# Read the current version
VERSION=$(cat ./VERSION)

# Update tags
docker tag ghcr.io/cambridge-cares/twa-vf:latest ghcr.io/cambridge-cares/twa-vf:$VERSION
docker rmi ghcr.io/cambridge-cares/twa-vf:latest

echo "Image now available as ghcr.io/cambridge-cares/twa-vf:$VERSION"