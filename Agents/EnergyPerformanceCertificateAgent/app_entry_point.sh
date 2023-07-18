#!/bin/bash

# Start Redis server with default config (required for Celery scheduling)
redis-server &
# Start Celery worker and enable periodic task scheduling ('-B' flag)
celery -A agent.flaskapp.wsgi:celery_app worker -B &
# Setting timeout to 0 disables timeouts for all workers entirely (i.e. no timeout)
gunicorn --bind 0.0.0.0:5000 agent.flaskapp.wsgi:app --timeout 0