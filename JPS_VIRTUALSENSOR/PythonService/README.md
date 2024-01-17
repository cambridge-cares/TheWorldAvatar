## Python Service
This container holds all the python functions that can be called from other containers via a REST interface.

# Get emissions from speed load map
Send GET request to http://localhost:3838/python-service/getEmissions with the following parameters
- speed
  - Value of speed in rpm
- torque
  - Value of torque in Nm

This returns a JSON object with three key parts
1) JSON object with the key "mixture" with the following JSON objects containing values
    - "molmass"
    - "cp"
    - "temperature"
    - "density"
2) JSON array with the key "particle", each element in the array is a JSON object with the following keys
    - "density"
    - "emission_rate"
    - "diameter"
3) JSON array with the key "pollutants", each element in the array is a JSON object with the following keys
    - "name"
    - "value"

This route is called by the EmissionsAgent.

# Postprocess AERMOD results into GeoJSON
Send GET request to http://localhost:3838/python-service/getAermodGeoJSON with the parameters
- dispersionMatrix
    - URL to download AERMOD output file (stored in FileServer)
- srid
    - srid for the output results, so that it can be converted to EPSG:4326 for visualisation

Returns a GeoJSON for a filled contour for visualisation. Called by AermodAgent before being uploaded to PostGIS as a vector.