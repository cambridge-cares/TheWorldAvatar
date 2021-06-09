#!/bin/bash

# This script initialises the regular Power System visualisation tasks, it should
# run whenever the Docker container is started.
echo "Running start-up.sh script..."

# Run the JSON_Generator script
export PYTHONPATH=/usr/local/lib/python3.7/site-packages
python3 -m pip install -r /requirements.txt
python3 /var/www/html/JSON_Generator.py 

# Start the Apache web server
service apache2 start