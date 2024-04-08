#!/bin/bash

#
# This script reads the "MAVEN_USERNAME" and "MAVEN_PASSWORD" environment variables,
# and uses the accompanying "template-maven-settings.xml" file to generate a new
# "settings.xml" file. This new file is then copied to the input directory so that
# it can be used when building Java projects that require access to TheWorldAvatar's
# package repository.
#
# Note: This script should be run from the directory it's contained within (at the
# time of writing, this is "/.github/scripts").
#
# Author: Michael Hillman
#

# Get first argument, should be location of output directory
TARGET_DIR=$1
echo "Will write 'settings.xml' file to directory at $TARGET_DIR"

# Get the MAVEN_USERNAME environment variable
if [[ -z "${MAVEN_USERNAME}" ]]; then
  echo "ERROR: The environment variable 'MAVEN_USERNAME' has not been set."
  exit 1
else
  MAVEN_USERNAME="${MAVEN_USERNAME}"
  echo "Retrieved the 'MAVEN_USERNAME' environment variable."
fi

# Get the MAVEN_PASSWORD environment variable
if [[ -z "${MAVEN_PASSWORD}" ]]; then
  echo "ERROR: The environment variable 'MAVEN_PASSWORD' has not been set."
  exit 1
else
  MAVEN_PASSWORD="${MAVEN_PASSWORD}"
  echo "Retrieved the 'MAVEN_PASSWORD' environment variable."
fi


# Copy the template settings file to output location
cp ./template-maven-settings.xml $TARGET_DIR/settings.xml

# Inject the maven credentials into new settings file
sed -i "s/REPO_USERNAME/$MAVEN_USERNAME/" $TARGET_DIR/settings.xml
sed -i "s/REPO_PASSWORD/$MAVEN_PASSWORD/" $TARGET_DIR/settings.xml

# Finish
echo "Script finished successfully, 'settings.xml' file can now be used."
exit 0
