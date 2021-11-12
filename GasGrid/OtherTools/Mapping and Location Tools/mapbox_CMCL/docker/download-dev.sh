#!/bin/bash

# This script downloads the GeoJSON files from the DEVELOPMENT 
# version of the Gas Grid Agent running at CMCL.
mkdir -p /var/www/html/geoJSON_assets
cd /var/www/html/geoJSON_assets

wget -4 -O flow-data.json kg.cmclinnovations.com:81/digital-twin/gas-grid-agent/flow-data-latest.json
wget -4 -O terminals.geojson kg.cmclinnovations.com:81/digital-twin/gas-grid-agent/terminals.geojson
wget -4 -O offtakes.geojson kg.cmclinnovations.com:81/digital-twin/gas-grid-agent/offtakes.geojson
wget -4 -O pipe_network.geojson kg.cmclinnovations.com:81/digital-twin/gas-grid-agent/pipe_network.geojson

echo Development files downloaded at `date`