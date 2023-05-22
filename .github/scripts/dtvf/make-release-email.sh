#!/bin/bash

# This script generates the HTML text for the email notification
# generated with a new version of the Digital Twin Visualisation Framework
# (DTVF) is released.
#
# Author: Michael Hillman (mdhillman<@>cmclinnovations.com)

# Read version from file
VERSION="$(cat ./web/digital-twin-vis-framework/VERSION)"

# Read changelog from file
CHANGELOG="./web/digital-twin-vis-framework/CHANGELOG.md"
regexp="^#\s.*"
SECTION=0

# Read changelog line by line
# Only store notes within the first header
NOTES = ""
while IFS= read -r line
do
  if [[ $line =~ $regexp ]]; then
	SECTION=$((SECTION + 1))
  fi
  
  if [[ $SECTION == 1 ]]; then
    NOTES="$NOTES<br/>$line"
  fi
  
done < "$CHANGELOG"

# Replace [VERSION] with version
TOKEN="\[VERSION\]"
sed -i "s/$TOKEN/$VERSION/g" "$HOME/email.html"

# Replace [NOTES] with notes
TOKEN="\[NOTES\]"
sed -i "s|$TOKEN|$NOTES|g" "$HOME/email.html"