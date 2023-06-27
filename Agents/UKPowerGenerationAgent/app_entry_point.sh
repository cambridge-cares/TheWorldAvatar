#!/bin/bash
# 
# Runs the `gunicorn` server to deploy the Flask application object
# `app` from the module `agent.flaskapp.wsgi`. Sets a timeout of
# 120min to avoid exceptions for long API/KG calls
gunicorn --bind 0.0.0.0:5000 agent.flaskapp.wsgi:app --timeout 7200