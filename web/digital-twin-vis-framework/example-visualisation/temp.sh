#!/bin/bash

docker stop example-visualisation
docker container rm example-visualisation
docker-compose -f ./docker/docker-compose.yml build --force-rm
docker-compose -f ./docker/docker-compose.yml up -d --force-recreate