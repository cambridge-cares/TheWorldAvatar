#!/bin/bash

# This script generates the body text for releases of the
# Digital Twin Visualisation Framework (DTVF).
#
# Author: Michael Hillman (mdhillman<@>cmclinnovations.com)

CHANGELOG="./web/digital-twin-vis-framework/CHANGELOG.md"
regexp="^#\s.*"
SECTION=0

BODY="This release package marks a release of the Digital Twin Visualisation Framework (DTVF), a framework for customisable visualisations using The World Avatar project. Release notes for this version of the DTVF can be found below, for more details, see the \`CHANGELOG.md\` and \`README.md\` files within the \`/web/digital-twin-vis-framework/\` directory.\n\n"

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