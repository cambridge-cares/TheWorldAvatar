#!/bin/bash

# This script downloads the GeoJSON files from the PRODUCTION 
# version of the Gas Grid Agent running at CMCL.
mkdir -p /var/www/html/geoJSON_assets
cd /var/www/html/geoJSON_assets

wget -4 -O flow-data.json kg.cmclinnovations.com/digital-twin/gas-grid-agent/flow-data-latest.json
wget -4 -O terminals.geojson kg.cmclinnovations.com/digital-twin/gas-grid-agent/terminals.geojson
wget -4 -O offtakes.geojson kg.cmclinnovations.com/digital-twin/gas-grid-agent/offtakes.geojson
wget -4 -O pipe_network.geojson kg.cmclinnovations.com/digital-twin/gas-grid-agent/pipe_network.geojson

echo Production files downloaded at `date`