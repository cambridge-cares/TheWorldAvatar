#!/bin/bash

# Get the current date in YYYY-MM-DD format
current_date=$(date +"%Y-%m-%d")

# Define the output file name using the current date
output_file="ontomops_backup_${current_date}.ttl"

# Load the environment variables from the .env file to get the Blazegraph endpoint
if [ -f mops.env ]; then
  source mops.env
  echo "Environment variables loaded from mops.env file"
else
  echo "mops.env file not found!"
  exit 1
fi

# Execute the curl command and save the output to the specified file
curl -X POST --url "$SPARQL_ENDPOINT" \
     --data-urlencode 'query=CONSTRUCT { ?s ?p ?o } where { ?s ?p ?o }' \
     --header 'Accept: application/x-turtle' \
     > "$output_file"

# Print a message indicating where the output was saved
echo "Backup saved to $output_file"
