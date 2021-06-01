#!/bin/bash

# This script downloads the GeoJSON files from the PRODUCTION 
# version of the Gas Grid Agent running at CMCL.
cd /var/www/html/geoJSON_assets

wget --quiet http://kg.cmclinnovations.com/digital-twin/gas-grid-agent/terminals.geojson
wget --quiet http://kg.cmclinnovations.com/digital-twin/gas-grid-agent/offtakes.geojson
wget --quiet http://kg.cmclinnovations.com/digital-twin/gas-grid-agent/pipes.geojson
echo Production files downloaded at `date` >> /var/log/gas-grid/downloads.log