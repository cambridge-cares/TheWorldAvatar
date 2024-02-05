#!/bin/bash

# Declare an associative array
declare -A variables

# Function to replace placeholders in sql.query
replace_placeholders() {
    local variable_name=$1
    local iri_value=$2
    sed -i "s|<$variable_name>|$iri_value|g" sql.query
}

# Read sparql.results and extract variable names and IRI values into an associative array
while IFS=$'\t' read -r variable_name iri_value || [[ -n $variable_name ]]; do
    # Strip angle brackets from IRI value
    iri_value=$(echo "$iri_value" | tr -d '<>')
    # Append to the list of values for the variable_name
    variables["$variable_name"]=" $iri_value"
    echo "Read variable: $variable_name, IRI value: $iri_value"
done < sparql.results

# Read sql.query and replace placeholders with IRI values
while IFS= read -r line; do
    for variable_name in "${!variables[@]}"; do
        #echo "Replacing placeholder for variable: $variable_name"
        replace_placeholders "$variable_name" ${variables["$variable_name"]}
    done
done < sql.query