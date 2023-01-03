# Overview
Adds ship data to the knowledge graph

# Requirements
There should be a "data" folder containing json files, the name of the file should be the timestamp of the data, and the filename should have the .json extension, e.g. "1.json". The content of the file should be a JSON array, where each element of the array is a JSON object representing a ship with the following keys:
- LAT
- LON
- SPEED
- MMSI

# API
URL: http://localhost:8082/ShipInputAgent/update
Send a POST request to this URL, no parameters required. This will add 1 timestep worth of data to the knowledge graph from the data stored in the "data" folder.