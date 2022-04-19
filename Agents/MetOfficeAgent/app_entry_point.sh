#!/bin/bash
# timeout set to 30min to avoid exceptions for long API/KG calls
gunicorn --bind 0.0.0.0:5000 metoffice.flaskapp.wsgi:app --timeout 1800