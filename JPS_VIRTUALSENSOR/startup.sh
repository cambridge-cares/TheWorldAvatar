#!/bin/bash

DEFAULT_PORT=4242

PORT="${1:-$DEFAULT_PORT}"

file_path="stack-manager/inputs/data/visualisation/data.json"

# Existing url
search_string="http://localhost:4242"

# New url - modify this if needed
replace_string="http://localhost:${PORT}"

sed -i "s|$search_string|$replace_string|g" "$file_path"

# starts up all required components of virtual sensor
(cd stack-manager && ./stack.sh start ship-stack "$PORT")
# copy required files into containers
(cd ShipInputAgent && ./copy_ship_file.sh)
./copy_tbox.sh
(cd stack-data-uploader && ./stack.sh start ship-stack)
