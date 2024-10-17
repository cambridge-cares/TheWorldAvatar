#!/bin/bash

curl -X POST http://localhost:3838/fenland-trajectory-agent/layer_generator \
     -H "Content-Type: application/json" \
     -d '{"table_name": "", "lat_column": "", "lon_column": ""}'
