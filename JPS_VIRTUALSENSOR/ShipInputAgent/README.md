# Overview
Adds ship data to the knowledge graph. This agent works in two modes described below.

# Two options
## Option 1 (static data)
There should be a "data" folder containing json files, the name of the file should be the timestamp of the data, and the filename should have the .json extension, e.g. "1.json". The content of the file should be a JSON array, where each element of the array is a JSON object representing a ship. Refer to the com.cmclinnovations.ship.Ship class for the required keys.

Route: curl -X POST http://localhost:3838/ship-input-agent/update
This will add 1 timestep worth of data to the knowledge graph from the data stored in the "data" folder.

## Option 2 (live data)
The agent pull data from https://aisstream.io/, a valid API key from is required for this service.

Relevant environment variables:
1) API_KEY: API key for aisstream.io
2) BOUNDING_BOXES: Refer to "BoundingBoxes" in https://aisstream.io/documentation
3) USE_LIVE_DATA: Accepted values - true/false. If set to true, live data updates will begin on container startup. If set to false, live updates can still be triggered manually via the route below.

Route: http://localhost:3838/ship-input-agent/live-server
To start live updates, submit a POST request, e.g.
```
curl -X POST http://localhost:3838/ship-input-agent/live-server
```
To stop updates, submit a DELETE request
```
curl -X DELETE http://localhost:3838/ship-input-agent/live-server
```

