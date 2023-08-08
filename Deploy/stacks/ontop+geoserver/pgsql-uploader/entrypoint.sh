#!/bin/bash

echo "Running processing code..."
/upload_to_pgsql.sh
echo "Processing code complete"

# If the KEEP_ALIVE variable was set, tail /dev/null to prevent the container exiting
if [ ! -z "$KEEP_ALIVE" ]; then
  tail -f /dev/null
fi