#!/bin/bash

###
# Use this to put all config files in the right place, just specify the name of the directory in 'TheWorldAvatar\Deploy\stacks\dynamic\examples\stack-data-uploader\inputs\data'
###

# Function to check if the given source directory exists
function check_source_path() {
    if [ ! -e "$1" ]; then
        echo "Source path not found: $1. Note that specified directory or file path is relative to examples/stack-data-uploader/inputs/config/"
        exit 1
    fi
}

# Function to copy the directory and its contents to the current working directory
function copy_to_current_directory() {
    for source_path in "$@"; do
        # Check if the source path exists
        source_path="../../../examples/stack-data-uploader/inputs/config/$source_path"

        check_source_path "$source_path"
        
        # Get the filename or directory name from the source path
        file_or_dir_name=$(basename "$source_path")
        
        # Copy the file or directory to the current directory
        cp -r "$source_path" .
        
        echo "File or directory '$file_or_dir_name' copied successfully!"
    done
}

# Usage example: ./copy_directory.sh <source_path1> <source_path2> | all ... 

if [ "$1" == "all" ]; then
    # Usage: ./getconfig.sh all
    cp -r ../../../examples/stack-data-uploader/inputs/config/* .
else
    # Usage: ./getconfig.sh <file_or_directory1> <file_or_directory2> ...

# Check if at least one argument is provided
    if [ "$#" -lt 1 ]; then
        echo -e "No source directories given\nUsage: $0 <file_or_directory1> <file_or_directory2> ...  OR  $0 all          <-  from /examples/stack-data-uploader/inputs/config/"
        exit 1
    fi

copy_to_current_directory "$@"
fi