#!/bin/bash

# File name of the CSV
FILE="inputs/input_request.csv"

# Read the CSV file line by line, skip the header row
tail -n +2 "$FILE" | while IFS=, read -r confidence ssp quantile projectionyear
do
  # Remove carriage returns if any (for Windows-edited files)
  confidence=$(echo "$confidence" | tr -d '\r')
  ssp=$(echo "$ssp" | tr -d '\r')
  quantile=$(echo "$quantile" | tr -d '\r')
  projectionyear=$(echo "$projectionyear" | tr -d '\r')
  
  # Construct the URL
  url="http://localhost:3838/sealevelimpactagent/slrimpact?ssp=$ssp&projectionyear=$projectionyear&confidence=$confidence&quantile=$quantile"
  
  # Print the URL for debugging
  echo "Sending request to URL: $url"
  
  # Send the curl request
  curl -X POST "$url"
done
