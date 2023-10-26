#!/bin/bash
# pull docker images from docker.cmclinnovations.com because stack manager can't pull these
docker pull docker.cmclinnovations.com/file-server:1.0.0
# starts up all required components of virtual sensor
(cd stack-manager && ./stack.sh start ship-stack)
# copy required files into containers
(cd DispersionVis && ./copy_vis_file.sh)
(cd ShipInputAgent && ./copy_ship_file.sh)
(cd FeatureInfoAgent && ./copy_fia_config.sh)
./copy_tbox.sh
(cd stack-data-uploader && ./stack.sh start ship-stack)
