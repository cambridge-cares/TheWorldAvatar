#!/bin/bash

#
# Use this script to build the latest copy of the FeatureInfoAgent image for local use.
#
# Note that will build a local copy of a single image with the below tags, where
# VERSION is whatever is currently written in the project's pom.xml file.
#
#   - ghcr.io/cambridge-cares/feature-info-agent:latest
#   - ghcr.io/cambridge-cares/feature-info-agent:VERSION
# 
# These should not be pushed to the GitHub image repository unless absolutely
# necessary. Automated actions will handle the release once a PR to main is merged.
#
#

echo 
echo "This script requires the 'xmllint' tool to parse the project's pom.xml"
echo "file and determine the current version number. Is it installed?"
echo 
read -p "Is the 'xmllint' package installed (Y/N)? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
	# Read the version from the pom.xml file
    VERSION="$(xmllint --xpath "//*[local-name()='project']/*[local-name()='version']/text()" ./code/pom.xml)"
    echo "Discovered version as $VERSION"

    # Write env file for docker
    echo "TAG=$VERSION" > .env
	
    # Build 'latest' tagged image
    docker compose -f docker-compose.yml build

    # Add version specific tag
    docker tag "ghcr.io/cambridge-cares/feature-info-agent:latest" "ghcr.io/cambridge-cares/feature-info-agent:$VERSION"
else
    echo "Please run install the 'xmllint' package and try again."
    exit 1
fi