#!/bin/bash

echo "Stopping containers..."
docker compose -f ./docker-compose.yml down --rmi "local"
sleep 3
echo "...containers stopped and removed."