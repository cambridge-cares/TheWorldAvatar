#!/bin/bash

# This script should run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Copy DTVF files into webspace.
# If this was done at build time then any volume mounted to /var/www/html would
# override the DTVF files.
cp -r /var/www/dtvf/ /var/www/html/

# Start the Apache web server
httpd -D FOREGROUND