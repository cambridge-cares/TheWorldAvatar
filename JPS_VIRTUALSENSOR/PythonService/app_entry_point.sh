#!/bin/bash
gunicorn --workers=4 --bind 0.0.0.0:5000 --timeout 600 app:app