#!/bin/bash

# Trivial wrapper for docker-compose to bring up portainer using the config at ./portainer/docker-compose.yml

docker-compose -f $(dirname "$0")/portainer/docker-compose.yml up -d
