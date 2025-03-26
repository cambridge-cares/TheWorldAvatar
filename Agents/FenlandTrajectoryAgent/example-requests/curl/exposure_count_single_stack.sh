#!/bin/bash
# Copy all the following commands once, paste them into the terminal together, and then press Enter
curl -X POST http://localhost:3838/fenland-trajectory-agent/exposure/simplified \
     -H "Content-Type: application/json" \
     -d '{
           "trajectoryIRIs": [
             "https://www.theworldavatar.com/kg/ontotimeseries/Timeseries_a57a5978-0d2f-41b3-ab35-35944166f322",
             "https://www.theworldavatar.com/kg/ontotimeseries/Timeseries_5674b1de-a387-433d-a650-f455291d3f73"
           ],
           "exposure_radius": 100,
           "DataIRIs": [
             "http://www.theworldavatar.com/ontology/OntoFHRS/FoodHygieneRating",
             "https://www.theworldavatar.com/kg/ontogreenspace/Greenspace"
           ]
         }'
