#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)
service cron start
gunicorn --bind 0.0.0.0:5000 oscml.flaskapp.wsgi:app