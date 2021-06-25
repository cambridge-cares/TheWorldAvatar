#!/bin/bash

# This script initialises the regular Gas Grid visualisation tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Load the cron-jobs file into crontab
echo "Registering cron jobs..."
crontab /usr/local/cron-jobs

# Run the download script at start-up (in addition to via cron)
chmod o+r /etc/resolv.conf
/usr/local/download.sh > /var/log/gas-grid/download.log 2>&1

# Start the Apache web server
httpd -D FOREGROUND