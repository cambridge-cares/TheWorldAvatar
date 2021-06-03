#!/bin/bash

# This script downloads the GeoJSON files from the PRODUCTION 
# version of the Gas Grid Agent running at CMCL.
mkdir -p /var/www/html/geoJSON_assets
cd /var/www/html/geoJSON_assets

wget --quiet -O terminals.geojson http://kg.cmclinnovations.com/digital-twin/gas-grid-agent/terminals.geojson
wget --quiet -O offtakes.geojson http://kg.cmclinnovations.com/digital-twin/gas-grid-agent/offtakes.geojson
wget --quiet -O pipe_network.geojson http://kg.cmclinnovations.com/digital-twin/gas-grid-agent/pipe_network.geojson
echo Production files downloaded at `date` >> /var/log/gas-grid/downloads.log