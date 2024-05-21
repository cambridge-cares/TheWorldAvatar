#!/bin/bash

DEFAULT_PORT=4242

DEFAULT_TIMEOUT="900s"

PORT="${1:-$DEFAULT_PORT}"

TIMEOUT="${2:-$DEFAULT_TIMEOUT}"

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

# overwrite nginx timeout

for container_name in $(docker ps --format "{{.Names}}" --filter name="ship-stack-nginx"); do
	docker exec $container_name sed -i "s/300s/${TIMEOUT}/g" /etc/nginx/conf.d/default.conf
	docker exec $container_name nginx -t 
	docker exec $container_name nginx -s reload
done

./copy_tbox.sh
(cd stack-data-uploader && ./stack.sh start ship-stack)
