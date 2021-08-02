#!/bin/bash

# As a multi-stage Image, regular docker commands will result in a slow
# build as only the last stage is cached. This script will push and pull
# Images for each stage, then use them in the caches for future builds.

# Image name for build stage
BUILD_STAGE=docker.cmclinnovations.com/email-agent:1.0.0-SNAPSHOT-build

# Image name for final stage
FINAL_STAGE=docker.cmclinnovations.com/email-agent:1.0.0-SNAPSHOT

# Try to pull images of the build and final stage
docker pull ${BUILD_STAGE} || true
docker pull ${FINAL_STAGE} || true

# Make image for the build stage
docker build --target build \
       --cache-from=${BUILD_STAGE} \
       --tag ${BUILD_STAGE} \
	   --rm \
	   -f docker/Dockerfile .

# Make image for the final stage
docker build --target final \
		--cache-from=${BUILD_STAGE} \
       --cache-from=${FINAL_STAGE} \
       --tag ${FINAL_STAGE} \
	   --rm \
	   -f docker/Dockerfile .

# Try to push the newly built images
docker push ${BUILD_STAGE} || true
docker push ${FINAL_STAGE} || true
