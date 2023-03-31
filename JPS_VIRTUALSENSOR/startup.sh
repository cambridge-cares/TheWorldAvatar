#!/bin/bash
# starts up all required components of virtual sensor
(cd ../Agents/WeatherAgent/ && ./stack.sh start ship-stack)
(cd ../Deploy/stacks/db/fileserver/ && ./stack.sh start ship-stack)
(cd DispersionVis && ./copy_vis_file.sh)
(cd ShipInputAgent && ./copy_ship_file.sh)
