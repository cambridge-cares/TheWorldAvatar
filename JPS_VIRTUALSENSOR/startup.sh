#!/bin/bash
# pull docker images from docker.cmclinnovations.com because stack manager can't pull these
docker pull docker.cmclinnovations.com/data-integration:1.0.0
# starts up all required components of virtual sensor
(cd stack-manager && ./stack.sh start ship-stack)
# copy required files into containers

