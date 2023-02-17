#!/bin/bash
# Setting timeout to 0 disables timeouts for all workers entirely (i.e. no timeout)
gunicorn --bind 0.0.0.0:5000 agent.flaskapp.wsgi:app --timeout 0