#!/bin/bash

# Define the relative paths to the source and destination files
isochrone_source_file="IsochroneAgent/inputs/config.properties"
isochrone_destination_file="../../Agents/IsochroneAgent/inputs/config.properties"

# Check if the source file exists
if [ -e "$isochrone_source_file" ]; then
    # Copy the source file to the destination
    cp "$isochrone_source_file" "$isochrone_destination_file"
    
    # Check if the copy was successful
    if [ $? -eq 0 ]; then
        echo "File copied successfully."
    else
        echo "Error: Failed to copy the file."
    fi
else
    echo "Error: Source file not found."
fi


# Check if the correct number of arguments is provided
if [ "$#" -ne 2 ] || [ "$1" != "start" ]; then
    echo "Usage: $0 start <STACK_NAME>"
    exit 1
fi

STACK_NAME="$2"

# Define the relative paths to the source and destination files
tsp_source_file="TravellingSalesmanAgent/inputs/UR/POIqueries/grid_primary_site.sparql"
tsp_destination_file="../../Agents/TravellingSalesmanAgent/inputs/UR/POIqueries/grid_primary_site.sparql"

# Check if the source file exists
if [ -e "$tsp_source_file" ]; then
    # Replace ${STACK_NAME} with "routing" in the source file and save it to the destination
    sed "s/\${STACK_NAME}/$STACK_NAME/g" "$tsp_source_file" > "$tsp_destination_file"

    # Check if the sed command was successful
    if [ $? -eq 0 ]; then
        echo "File copied and placeholder replaced successfully."
    else
        echo "Error: Failed to replace the placeholder in the file."
        exit 1
    fi
else
    echo "Error: Source file not found."
    exit 1
fi