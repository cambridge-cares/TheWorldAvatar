#!/bin/bash

# This script reads the current version of the TWA-VF and breaks
# it down into parts for use in Docker image tags.
#
# Author: Michael Hillman (mdhillman<@>cmcl.io)


# Read the full version
VERSION_FULL=$(cat ./web/twa-vis-framework/library/VERSION)
echo "Full version is: $VERSION_FULL"

# Get the major and minor parts
VERSION_MAJOR=$(echo $version | cut -d. -f1)
echo "Major version is: $VERSION_MAJOR"

VERSION_MINOR=$(echo $version | cut -d. -f2)
VERSION_MINOR="$VERSION_MAJOR.$VERSION_MINOR"
echo "Minor version is: $VERSION_MINOR"

# Store as GitHub action variables
echo "::set-env name=VERSION_FULL::$VERSION_FULL"
echo "::set-env name=VERSION_MAJOR::$VERSION_MAJOR"
echo "::set-env name=VERSION_MINOR::$VERSION_MINOR"

exit 0