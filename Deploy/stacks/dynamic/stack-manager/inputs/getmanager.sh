#!/bin/bash

###
# Use this to put all manager config AND data files in the right place, just specify the name of the directory in 'TheWorldAvatar\Deploy\stacks\dynamic\examples\stack-manager\inputs\'
###

# Function to check if the given source directory exists
function check_source_directory() {
    if [ ! -d "$1" ]; then
        echo "Source directory not found: $1. Note that specified directory path is relative to examples/stack-manager/inputs/"
        exit 1
    fi
}

# Function to copy the directory and its contents to the current working directory
function copy_to_current_directory() {
    for source_dir in "$@"; do
        source_dir="../../examples/stack-manager/inputs/$source_dir"
        check_source_directory "$source_dir"
        cp -r "$source_dir"/* .
        echo "Directory '$source_dir' copied successfully!"
    done
}

# Usage example: ./copy_directory.sh <source_directory1> <source_directory2> ...

# Check if at least one argument is provided
if [ "$#" -lt 1 ]; then
    echo -e "No source directories given\nUsage: $0 <source_directory1> <source_directory2> ...          <-  from examples/stack-manager/inputs/"
    exit 1
fi

copy_to_current_directory "$@"