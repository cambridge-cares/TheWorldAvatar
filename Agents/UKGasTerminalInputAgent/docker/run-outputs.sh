#!/bin/bash

currentDate=`date`
echo "Running output scripts, time is: $currentDate" 

# Output last 24 hours of flow (for all terminals)
python3 /ukgasflows/output_flow_data.py > /var/log/gas-grid/output_flow_data.log 2>&1

echo "Scripts triggered."