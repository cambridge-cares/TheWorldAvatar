#!/bin/bash

# Removes the visualisation container, rebuilds the image, and deploys the container.

docker compose -p psdt-vis-cesium -f ./docker/docker-compose.yml build --force-rm
docker compose -p psdt-vis-cesium -f ./docker/docker-compose.yml up -d --force-recreate
