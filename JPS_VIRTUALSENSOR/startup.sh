#!/bin/bash
# starts up all required components of virtual sensor
(cd stack-manager && ./stack.sh start ship-stack)
# copy required files into containers
(cd ShipInputAgent && ./copy_ship_file.sh)
./copy_tbox.sh
(cd stack-data-uploader && ./stack.sh start ship-stack)
