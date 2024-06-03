#!/bin/bash

#
# Use this script to build the TWA-VF files, but not the final docker image.
#
# Note that this will produce files within the ./output directory (which should
# not be commited), and should only be used by experienced developers.
#

# Build files
docker compose -f docker-compose.dev.yml up