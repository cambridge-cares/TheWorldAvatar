#!/bin/bash
# Copy all the following commands once, paste them into the terminal together, and then press Enter
curl -X POST http://localhost:5000/gpstasks/fenlandtrajectoryagent/process_and_instantiate \
     -H "Content-Type: application/json" \
     -d '{"file_path":"/app/agent/raw_data/gps_target_folder"}'
