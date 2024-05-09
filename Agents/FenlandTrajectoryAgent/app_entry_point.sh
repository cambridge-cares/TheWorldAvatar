#!/bin/bash
# Starts a Gunicorn server to serve a Flask application with detailed logging and a reasonable timeout.

# Setting a timeout of 1 hour (3600 seconds) to avoid exceptions for long API/KG calls.
TIMEOUT=3600
LOG_LEVEL=debug
BIND_ADDRESS="0.0.0.0:5000"
APP_MODULE="agent.flaskapp.wsgi:app"

# Start the Gunicorn server
gunicorn --bind $BIND_ADDRESS $APP_MODULE --timeout $TIMEOUT --log-level $LOG_LEVEL || {
    echo "Failed to start Gunicorn server."
    exit 1
}

echo "Gunicorn server started successfully."

