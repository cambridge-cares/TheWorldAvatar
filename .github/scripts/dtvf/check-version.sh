#!/bin/bash

# This script checks that the version of the Digital Twin Visualisation Framework
# (DTVF) is as expected, the example visualisations have been updated to use that
# version, and that the CHANGELOG has been updated acorrdingly.
#
# Author: Michael Hillman (mdhillman<@>cmclinnovations.com)


# Read (what should be) the correct version
VERSION=`cat -s "./web/digital-twin-vis-framework/VERSION" 2>/dev/null`
if [ "$VERSION" == "" ]; then
	echo "The VERSION file of the DTVF has no content!"
	exit -1
fi
echo "DTVF defined in file as: $VERSION"

# Get the VERSION file from the main branch of the repo, check that this new version does not match
MAIN_VERSION=$(curl -s "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/web/digital-twin-vis-framework/VERSION")
if [ "$VERSION" == "$MAIN_VERSION" ]; then
	echo "Contents of VERSION file on this branch match that on the main branch!"
	exit -1
fi

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


# Check that the example visualisations have been updated
TOKEN="dtvf/$VERSION/dtvf.min.js"

MAPBOX_VIS="./web/digital-twin-vis-framework/example-mapbox-vis/webspace/index.html"
if ! grep -q "$TOKEN" "$MAPBOX_VIS"; then
	echo "Example Mapbox visualisation does not use new version of DTVF!"
	exit -1
fi
echo "Example Mapbox visualisation has been updated."

CESIUM_VIS="./web/digital-twin-vis-framework/example-cesium-vis/webspace/index.html"
if ! grep -q "$TOKEN" "$CESIUM_VIS"; then
	echo "Example Cesium visualisation does not use new version of DTVF!"
	exit -1
fi
echo "Example Cesium visualisation has been updated."


exit 0