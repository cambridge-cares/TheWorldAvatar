#!/bin/bash
# timeout set to 120min to avoid exceptions for long API/KG calls
gunicorn --bind 0.0.0.0:5000 agent.flaskapp.wsgi:app --timeout 99999999999999