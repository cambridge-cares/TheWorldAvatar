# Elevation Utils

This directory contains a Python script to determine the elevation of a given lattitude-longitude coordinate using a remote GeoServer with a WMS endpoint. Note that this relies on having such a server, and it containing elevation data for the area around the requested location.

At the time of writing this script is not packaged into any Python module, nor are any setup files/scripts present. In future, these can be added so that Agents can easily make use of the script.