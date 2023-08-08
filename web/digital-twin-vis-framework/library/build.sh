#!/bin/bash

#
# Use this script to build the latest copy of the DTVF base image for local use.
#

docker compose -f docker-compose.yml build --no-cache