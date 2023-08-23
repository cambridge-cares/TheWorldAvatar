#!/bin/bash

#
# Use this script to build the latest copy of the TWA-VF base image for local use.
#
# Note that will build a local copy of a single image with the below tags, where
# VERSION is whatever is currently written in the 'library/VERSION' file.
#
#   - ghcr.io/cambridge-cares/twa-vf:latest
#   - ghcr.io/cambridge-cares/twa-vf:VERSION
# 
# These should not be pushed to the GitHub image repository unless absolutely
# necessary. Automated actions will handle the release once a PR to main is merged.
#

# Build 'latest' tagged image
docker compose -f docker-compose.yml build

# Read the version
VERSION="$(cat ./VERSION)"

# Add version specific tag
docker tag "ghcr.io/cambridge-cares/twa-vf:latest" "ghcr.io/cambridge-cares/twa-vf:$VERSION"