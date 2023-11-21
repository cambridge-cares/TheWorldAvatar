#!/bin/bash

# The purpose of this bash script is to delay the actual agent startup
# until the district heating instantiation is finished (to ensure that 
# all relevant data is available

# Expected curl responses:
# Running contianer: curl: (7) Failed to connect to dhstack-dh-instantiation port 80: Connection refused
# Stopped container: curl: (6) Could not resolve host: dhstack-dh-test
while true; do
  # -s -o /dev/null suppresses any output
  curl -s -o /dev/null http://dhstack-dh-instantiation
  exit_code=$?
  
  if [ $exit_code -eq 6 ]; then
    echo "District heating instantiation finished."
    break
  fi
  
  echo "District heating instantiation ongoing..."
  sleep 5
done

# Start actual agent task (i.e., CMD command in Dockerfile)
# 1) Start Redis server in the background
redis-server &
# 2) Start Celery worker in the background
celery -A agent.celery worker --loglevel=info &
# 3) Start Gunicorn with your Flask app
gunicorn -b 0.0.0.0:5000 agent:app --timeout 0