#!/bin/bash
# Copy all the following commands once, paste them into the terminal together, and then press Enter
curl -X POST http://localhost:3838/fenland-trajectory-agent/preprocess \
     -H "Content-Type: application/json" \
     -d '{"file_path":"/app/agent/raw_data/gps_target_folder"}'
