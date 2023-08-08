#!/bin/bash

# This script checks that the version of the Digital Twin Visualisation Framework
# (DTVF) is as expected, the example visualisations have been updated to use that
# version, and that the CHANGELOG has been updated acorrdingly.
#
# Author: Michael Hillman (mdhillman<@>cmclinnovations.com)


# Read (what should be) the correct version
VERSION=`cat -s "./web/digital-twin-vis-framework/library/VERSION" 2>/dev/null`
if [ "$VERSION" == "" ]; then
	echo "The VERSION file of the DTVF has no content!"
	exit -1
fi
echo "DTVF defined in file as: $VERSION"

# Get the VERSION file from the main branch of the repo, check that this new version does not match
MAIN_VERSION=$(curl -s "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/web/digital-twin-vis-framework/library/VERSION")
if [ "$VERSION" == "$MAIN_VERSION" ]; then
	echo "Contents of VERSION file on this branch match that on the main branch!"
	exit -1
fi
echo "Does not match version on main, which is: $MAIN_VERSION"

# Check that there's no -SNAPSHOT in the version
TOKEN="-SNAPSHOT"
if grep -q "$TOKEN" <<< "$VERSION"; then
  echo "Version still contains the -SNAPSHOT qualifier!"
  exit -1
fi
echo "Version does not contain -SNAPSHOT qualifier."

# Check that the change log contains an entry for that version
CHANGELOG="./web/digital-twin-vis-framework/CHANGELOG.md"
TOKEN="# $VERSION"
if ! grep -q "$TOKEN" "$CHANGELOG"; then
	echo "Could not find corresponding node in CHANGELOG.md file!"
	exit -1
fi
echo "CHANGELOG has node for the current version."

exit 0