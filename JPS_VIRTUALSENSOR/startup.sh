#!/bin/bash

DEFAULT_PORT=4242

PORT="${1:-$DEFAULT_PORT}"

# starts up all required components of virtual sensor
(cd stack-manager && ./stack.sh start ship-stack "$PORT")
# copy required files into containers
(cd ShipInputAgent && ./copy_ship_file.sh)
./copy_tbox.sh
(cd stack-data-uploader && ./stack.sh start ship-stack)
