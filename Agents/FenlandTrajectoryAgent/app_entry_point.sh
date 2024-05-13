#!/bin/bash
# Starts a Gunicorn server to serve a Flask application with detailed logging and an extended timeout.

# Setting a timeout of 2 hours (7200 seconds) to avoid exceptions for long API/KG calls.
TIMEOUT=7200
LOG_LEVEL=debug
BIND_ADDRESS="0.0.0.0:5000"
APP_MODULE="agent.flaskapp.wsgi:app"

# Start the Gunicorn server
gunicorn --bind $BIND_ADDRESS $APP_MODULE --timeout $TIMEOUT --log-level $LOG_LEVEL --preload|| {
    echo "Failed to start Gunicorn server."
    exit 1
}

echo "Gunicorn server started successfully."
