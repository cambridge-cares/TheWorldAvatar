#!/bin/bash

currentDate=`date`
echo "Running output scripts, time is: $currentDate" 
echo "TARGET_MODE is: $TARGET_MODE"
echo "KG_LOCATION is: $KG_LOCATION"

python3 /app/output/all_offtakes_to_geojson.py > /var/log/gas-grid/all_offtakes_to_geojson.log 2>&1
python3 /app/output/all_pipes_query_to_geojson.py > /var/log/gas-grid/all_pipes_query_to_geojson.log 2>&1
python3 /app/output/all_terminals_to_geojson.py > /var/log/gas-grid/all_terminals_to_geojson.log 2>&1
python3 /app/output/output_flow_data.py > /var/log/gas-grid/output_flow_data.log 2>&1
echo "Scripts triggered."