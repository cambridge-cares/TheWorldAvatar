#!/bin/bash
# starts up all required components of virtual sensor
(cd DispersionVis && ./copy_vis_file.sh)
(cd ShipInputAgent && ./copy_ship_file.sh)
