#!/bin/bash

startDate=$(date)
echo "Running output scripts, time is: $startDate" 

# Activate new virtual environment
echo "Activating virtual environment..."
. /root/code/gasgridagent-venv/bin/activate
echo "The 'gasgridagent-venv' should now be active."

# Output last 24 hours of flow (for all terminals)
#python3 /root/code/gasgridagent/output_flow_data.py > /var/log/gas-grid/output_flow_data.log 2>&1
echo "Recent flow data has been output."

# Output locations of all Terminals
python3 /root/code/gasgridagent/output_terminal_locations.py > /var/log/gas-grid/output_terminal_locations.log 2>&1
echo "Terminal locations have been output."

# Output locations of all Offtakes
python3 /root/code/gasgridagent/output_offtake_locations.py > /var/log/gas-grid/output_offtake_locations.log 2>&1
echo "Offtake locations have been output."

# Output locations of all Pipes
python3 /root/code/gasgridagent/output_pipe_locations.py > /var/log/gas-grid/output_pipe_locations.log 2>&1
echo "Pipe locations have been output."

endDate=$(date)
echo "Output scripts finised at: $endDate"