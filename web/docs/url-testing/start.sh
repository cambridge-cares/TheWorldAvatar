#!/bin/bash

#
# This script handles starting up a single (i.e. non-stack)
# NGINX container to test redirects to an external web service.
#

# Run the system
echo "Building and starting containers..."
docker compose -f ./docker-compose.yml up -d --build || exit 1
sleep 3
echo "...containers should now be running."

# echo "Importing custom NGINX configuration..."
# docker cp ./nginx.conf twa-nginx:/etc/nginx/nginx.conf
# echo "Custom NGINX configuration imported."

# echo "Restarting NGINX process within container..."
# docker exec twa-nginx nginx -s reload
# echo "NGINX process has now reloaded".

echo "-----"
echo "Script finished, redirect available at:"
echo "http://localhost/redirect"
echo "-----"

# All done.
echo "Startup script completed."