#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)
gunicorn --bind 0.0.0.0:5000 stdc.flaskapp.wsgi:app