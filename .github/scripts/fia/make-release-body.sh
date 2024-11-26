#!/bin/bash

# This script generates the body text for releases of the
# Feature Info Agent (FIA).
#
# Author: Michael Hillman (mdhillman<@>cmcl.io)

CHANGELOG="./Agents/FeatureInfoAgent/CHANGELOG.md"
regexp="^#\s.*"
SECTION=0

BODY="This release package marks a release of the TWA FeatureInfoAgent (FIA), a intelligent agent/microservice for discovering metadata on visualised assets. Release notes for this version of the FIA can be found below, for more details, see the \`CHANGELOG.md\` and \`README.md\` files within the \`/Agents/FeatureInfoAgent/\` directory.\n\n"

# Read changelog line by line
# Only store notes within the first header
while IFS= read -r line
do
  if [[ $line =~ $regexp ]]; then
	SECTION=$((SECTION + 1))
  fi
  
  if [[ $SECTION == 1 ]]; then
    BODY="$BODY\n$line"
  fi
  
done < "$CHANGELOG"
		  
# Output final notes
echo -e "$BODY"