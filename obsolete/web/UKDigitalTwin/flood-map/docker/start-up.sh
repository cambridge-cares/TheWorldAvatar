#!/bin/bash

# This script initialises the regular Flood Visualisation tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Start the Apache web server
service apache2 start

# Run the JSON_Generator script
export PYTHONPATH=/usr/local/lib/python3.7/site-packages
#python3 -m pip install -r /requirements.txt
#python3 /var/www/html/JSON_Generator.py 
