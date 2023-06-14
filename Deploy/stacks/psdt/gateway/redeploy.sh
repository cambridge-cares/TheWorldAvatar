#!/bin/bash

# Removes the gateway container, rebuilds the image, and deploys the container.

docker rm -f gateway-nginx
docker build -t redirect-nginx .
# NB Port mappings are disregarded in host network mode.
docker run --name gateway-nginx --net=host -d redirect-nginx
