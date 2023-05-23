#!/bin/bash
# pull docker images from docker.cmclinnovations.com because stack manager can't pull these
docker pull ghcr.io/cambridge-cares/dispersion-interactor:1.1.0-SNAPSHOT
docker pull ghcr.io/cambridge-cares/aermod-agent:1.1.4-dev-pirmasens-vis-SNAPSHOT
docker pull ghcr.io/cambridge-cares/dispersion-vis:1.1.1-SNAPSHOT
docker pull ghcr.io/cambridge-cares/python-service:1.1.1-SNAPSHOT
docker pull ghcr.io/cambridge-cares/emissions-agent:1.0.1-SNAPSHOT
docker pull docker.cmclinnovations.com/ship-input-agent:1.0.0
docker pull docker.cmclinnovations.com/weatheragent:1.1.1
docker pull docker.cmclinnovations.com/file-server:1.0.0
docker pull ghcr.io/cambridge-cares/feature-info-agent:2.0.1
# starts up all required components of virtual sensor
(cd stack-manager && ./stack.sh start ship-stack)
# copy required files into containers
(cd DispersionVis && ./copy_vis_file.sh)
(cd ShipInputAgent && ./copy_ship_file.sh)
