#!/bin/bash

# Start debug containers for forecasting agent and tests
docker compose -f "docker-compose-test_dockerised_debug.yml" up -d --build

# Wait for the container to finish
CONTAINER_NAME="forecastingagent-tests-1"
docker wait $CONTAINER_NAME

# Check the exit code of the container
EXIT_CODE=$(docker inspect -f '{{.State.ExitCode}}' $CONTAINER_NAME)

# Check the exit code and perform actions based on it
if [ "$EXIT_CODE" -eq 0 ]; then
  echo "Container exited successfully!"
else
  echo "Container exited with an error (Exit Code: $EXIT_CODE)."
fi

echo "Inspect test container logs and plots now if needed. Otherwise, press any key to continue and remove both."
read -r

# Remove the containers
docker compose -f "docker-compose-test_dockerised_debug.yml" down -v

# Delete potentially created test plots
find ./tests/test_plots -type f ! -name '.gitkeep' -exec rm -f {} +

# Delete potentially downloaded pre-trained models
sudo rm -rf ./forecastingagent/fcmodels/Models