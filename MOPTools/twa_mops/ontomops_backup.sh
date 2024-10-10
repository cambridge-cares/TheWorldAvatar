#!/bin/bash

# Get the current date in YYYY-MM-DD format
current_date=$(date +"%Y-%m-%d")

# Define the output file name using the current date
output_file="ontomops_backup_${current_date}.ttl"

# Execute the curl command and save the output to the specified file
curl -X POST --url 'http://localhost:48082/blazegraph/namespace/ontomops151/sparql' \
     --data-urlencode 'query=CONSTRUCT { ?s ?p ?o } where { ?s ?p ?o }' \
     --header 'Accept: application/x-turtle' \
     > "$output_file"

# Print a message indicating where the output was saved
echo "Backup saved to $output_file"
