#!/bin/bash

# This script should run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Start the Apache web server
echo "Starting Apache web server..."
httpd -D FOREGROUND