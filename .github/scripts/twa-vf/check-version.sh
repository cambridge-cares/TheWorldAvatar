#!/bin/bash

# This script checks that the version of the TWA Visualisation Framework (TWA-VF
# is as expected, the example visualisations have been updated to use that
# version, and that the CHANGELOG has been updated acorrdingly.
#
# Author: Michael Hillman (mdhillman<@>cmcl.io)


# Read (what should be) the correct version
VERSION=$(cat -s "./web/twa-vis-framework/library/VERSION" 2>/dev/null)
if [ "$VERSION" == "" ]; then
	echo "The VERSION file of the TWA-VF has no content."
	exit 1
fi
echo "TWA-VF defined in file as: $VERSION"

# Get the VERSION file from the main branch of the repo, check that this new version is updated ie does not match
MAIN_VERSION=$(curl -s "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/web/twa-vis-framework/library/VERSION")
if [ "$VERSION" == "$MAIN_VERSION" ]; then
	echo "The TWA-VF VERSION file on this branch matches that on the main branch. Update the VERSION file before merging."
	exit 1
fi
echo "The updated version of the TWA-VF is: $MAIN_VERSION"

# Check that there's no -SNAPSHOT qualifier
TOKEN="-SNAPSHOT"
if [[ "$VERSION" == *"$TOKEN"* ]]; then
  echo "Remove the -SNAPSHOT qualifier in TWA-VF version file "
  exit 1
fi

# Check that the change log contains an entry for the updated versions
CHANGELOG="./web/twa-vis-framework/library/CHANGELOG.md"
TOKEN="# $VERSION"
if ! grep -q "$TOKEN" "$CHANGELOG"; then
	echo "Could not find corresponding node for the updated TWA-VF in CHANGELOG.md file"
	exit 1
fi

echo "CHANGELOG is updated for the TWA-VF."

exit 0