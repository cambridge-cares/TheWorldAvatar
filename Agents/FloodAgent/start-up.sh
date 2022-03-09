#!/bin/bash

# This script initialises the regular Gas Grid Agent tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

echo "Starting FloodAgent code in new process..."
java -jar /app/FloodAgent-1.0.0-SNAPSHOT.jar &
echo "FloodAgent has started."

echo "Starting Apache web server..."
/usr/sbin/httpd -D FOREGROUND