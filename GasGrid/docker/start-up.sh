#!/bin/bash

# This script initialises the regular Gas Grid Agent tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Launch the terminal-update.py script in a background process.
# This scrapes data and pushes it to the KG every 12 minutes.
echo "Launching the terminal-update.py script in a new process..."
screen -d -m /bin/bash "/app/input/terminal-update.py -continuous > /var/log/gas-grid/update.log 2>&1"

# Load the cron-jobs file into crontab
echo "Registering cron jobs..."
crontab /app/cron-jobs

# Start the Apache web server
service apache2 start

# Keep the container running
tail -f /dev/null