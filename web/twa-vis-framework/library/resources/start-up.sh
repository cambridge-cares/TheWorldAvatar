#!/bin/bash

# This script should run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Copy TWA-VF files into webspace. If this was done at build time then
# any volume mounted to /var/www/html would override the TWA-VF files.
cp -r /var/www/twa-vf/ /var/www/html/

# Start the Apache web server
httpd -D FOREGROUND