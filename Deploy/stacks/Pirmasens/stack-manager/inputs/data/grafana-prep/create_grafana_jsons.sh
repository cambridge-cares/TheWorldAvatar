#!/bin/bash

# Declare an associative array
declare -A variables

# Function to replace placeholders in json models
replace_placeholders() {
    local variable_name=$1
    local ts_table=$2
    sed -i "s|<$variable_name>|$ts_table|g" "$3"
}

# Read sql.results and extract variable names and tsTable values into an associative array
while IFS=$'\t' read -r variable_name ts_table || [[ -n $variable_name ]]; do
    # Append to the list of values for the variable_name
    variables["$variable_name"]=" $ts_table"
    echo "Read variable: $variable_name, ts table: $ts_table"
done < sql.results

# List of json dashboard models
dashboards=("demand.json" "generation.json")

# Iterate over the list of json models
for model in "${dashboards[@]}"; do
    # Replace placeholders in the current json model
    while IFS= read -r line; do
        for variable_name in "${!variables[@]}"; do
            replace_placeholders "$variable_name" ${variables["$variable_name"]} "$model"
        done
    done < "$model"
done
