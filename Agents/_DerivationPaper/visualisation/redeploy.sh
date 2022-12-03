#!/bin/bash

# Removes the visualisation container, rebuilds the image, and deploys the container.
docker-compose -p kings-lynn -f ./docker/docker-compose.yml build --force-rm
docker-compose -p kings-lynn -f ./docker/docker-compose.yml up -d --force-recreate
