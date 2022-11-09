#!/bin/bash

# Obtain name of nginx container (not the gateway!).
cname=$(docker ps | grep 'nginx\.' | sed 's/.*\s\+//')
echo "Target container: $cname"

dest=/etc/nginx/conf.d/
echo "Configuration path: $dest"

location_dir=locations/
echo "Location path: $dest$location_dir"

echo "Copying upstream configuration file..."
docker cp ./KINGS-LYNN_feature-info-agent_upstream.conf $cname:$dest

echo "Copying location configuration file..."
docker cp ./KINGS-LYNN_feature-info-agent.conf $cname:$dest$location_dir

echo "Testing nginx configuration inside the container..."
docker exec -it $cname sh -c "nginx -t"

echo "Restarting nginx inside the container..."
docker exec -it $cname sh -c "nginx -s reload"

echo "Done!"
