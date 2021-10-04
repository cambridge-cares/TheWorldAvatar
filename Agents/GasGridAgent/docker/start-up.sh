#!/bin/bash

# This script initialises the regular Gas Grid Agent tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Cron will not inherit environment variables unless they're
# written to a specific file first.
echo "Exporting environment variables to file for cron to detect..."
printenv | grep -v "no_proxy" >> /etc/environment

# Build the Python code
echo "Building Python code..."
cd /root/code
./install_script_pip.sh -v -i -e

# Load the cron-jobs file into crontab
echo "Registering cron jobs..."
crontab /root/cron-jobs
echo "Cron jobs should now be registered."

# Start cron service
echo "Starting cron..."
service cron start
echo "Cron daemon should have started."

# Start the Apache web server
echo "Starting Apache..."
service apache2 start
echo "Apache daemon should have started."

# Run the output scripts on launch
echo "Forcibly running output scripts as part of start up..."
/root/run-outputs.sh >> /var/log/gas-grid/run-outputs.log
echo "GeoJSON files for terminals, offtakes, and pipes should have been generated."

# Activate new virtual environment
echo "Activating virtual environment..."
chmod -R 755 ./gasgridagent-venv
. ./gasgridagent-venv/bin/activate
echo "The 'gasgridagent-venv' should now be active."

# Launch the terminal-update.py script in a background process.
# This scrapes data and pushes it to the KG every 12 minutes.
#echo "Launching the 'input_flow_data.py' script in a new process..."
#python /gasgridagent/input_flow_data.py -continuous > /var/log/gas-grid/input_flow_data.log 2>&1 &
#echo "The 'input_flow_data.py' script should now be running in its own process."

# Keep the container running
echo "Start up script has finished."
tail -f /dev/null