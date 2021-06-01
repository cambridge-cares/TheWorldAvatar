#!/bin/bash

# This script initialises the regular Gas Grid visualisation tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Load the cron-jobs file into crontab
echo "Registering cron jobs..."
crontab /usr/local/cron-jobs

# Start the Apache web server
#service apache2 start

# Run the download script at start-up (in addition to via cron)
/usr/local/download.sh

# Keep the container running
tail -f /dev/null