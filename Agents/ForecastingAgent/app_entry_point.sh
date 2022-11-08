#!/bin/bash
# timeout set to 60min to avoid exceptions for long API/KG calls
gunicorn --bind 0.0.0.0:5000 forecasting.flaskapp.wsgi:app --timeout 3600