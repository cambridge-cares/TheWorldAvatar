#!/bin/bash
# Copy all the following commands once, paste them into the terminal together, and then press Enter
curl -X POST http://localhost:3840/fenland-trajectory-agent/gpstasks/fenlandtrajectoryagent/load_and_preprocess \
     -H "Content-Type: application/json" \
     -d '{"file_path":"/app/agent/raw_data/gps_target_folder"}'
