#!/bin/bash

# Obtain name of GeoServer container.
cname=$(docker ps | grep geoserver | sed 's/.*\s\+//')
echo "Target container: $cname"

dest=/var/geoserver/datadir/workspaces/twa/styles
echo "Destination path: $dest"

search_dir=./config/
for icon in "$search_dir"*.png
do
  echo "Copying $icon..."
  docker cp $icon $cname:$dest
done
echo "Done!"
