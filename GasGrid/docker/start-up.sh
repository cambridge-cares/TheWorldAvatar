#!/bin/bash

# This script initialises the regular Gas Grid Agent tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Cron will not inherit environment variables unless they're
# written to a specific file first.
echo "Exporting environment variables to file for cron to detect..."
printenv | grep -v "no_proxy" >> /etc/environment

# Launch the terminal-update.py script in a background process.
# This scrapes data and pushes it to the KG every 12 minutes.
echo "Launching the terminal-update.py script in a new process..."
python /app/input/terminal-update.py -continuous > /var/log/gas-grid/update.log 2>&1 &

# Start the Apache web server
service apache2 start

# Load the cron-jobs file into crontab
echo "Registering cron jobs..."
crontab /app/cron-jobs

# Start cron service
echo "Starting cron..."
service cron start

# Run the 'run-outputs.sh' script on boot
echo "Running 'run-outputs.sh' script (may take up to 10 minutes)..."
/app/output/run-outputs.sh

# Start the Apache web server
echo "Starting Apache..."
service apache2 start

# Keep the container running
tail -f /dev/null