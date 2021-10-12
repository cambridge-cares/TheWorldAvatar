#!/bin/bash
# L. Pascazio (lp521@cam.ac.uk)
gunicorn --bind 0.0.0.0:5000 pesfit.flaskapp.wsgi:app