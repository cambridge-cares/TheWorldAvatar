#!/bin/bash

export TARGET_MODE="CMCL"
export KG_LOCATION="http://kg.cmclinnovations.com:8055/blazegraph"
export PYTHONPATH=/usr/local/lib/python3.7/site-packages

currentDate=`date`
echo "Running output scripts, time is: $currentDate"
echo "TARGET_MODE is: $TARGET_MODE"

python3 -m pip install -r /requirements.txt > /var/log/gas-grid/pip.log 2>&1

python3 /app/output/all_offtakes_to_geojson.py > /var/log/gas-grid/offtakes.log 2>&1
python3 /app/output/all_pipes_query_to_geojson.py > /var/log/gas-grid/pipes.log 2>&1
python3 /app/output/all_terminals_to_geojson.py > /var/log/gas-grid/terminals.log 2>&1
python3 /app/output/output_flow_data.py > /var/log/gas-grid/flow-data.log 2>&1
echo "Scripts triggered."