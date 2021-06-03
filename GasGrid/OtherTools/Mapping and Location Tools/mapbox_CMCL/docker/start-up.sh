#!/bin/bash

# This script initialises the regular Gas Grid visualisation tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Start the Apache web server
/usr/local/apache2/bin/httpd -k start

# Load the cron-jobs file into crontab
echo "Registering cron jobs..."
crontab /usr/local/cron-jobs

# Run the download script at start-up (in addition to via cron)
/usr/local/download.sh