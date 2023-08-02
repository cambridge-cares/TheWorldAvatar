#!/bin/bash

###
# Use this to put all data directories in the right place, data will still need to be downloaded manually just specify the name of the directory in 'TheWorldAvatar\Deploy\stacks\dynamic\examples\stack-data-uploader\inputs\data'
###

#!/bin/bash

# Function to check if the given source directory exists
function check_source_directory() {
    if [ ! -d "$1" ]; then
        echo "Source directory not found: $1. Note that specified directory path is relative to examples/stack-data-uploader/inputs/data/"
        exit 1
    fi
}

# Function to copy the directory and its contents to the current working directory
function copy_to_current_directory() {
    for source_dir in "$@"; do
        source_dir="../../../examples/stack-data-uploader/inputs/data/$source_dir"
        check_source_directory "$source_dir"
        cp -r "$source_dir"/* .
        echo "Directory '$source_dir' copied successfully! Don't forget to download datasets linked in each README.md"
    done
}

# Usage example: ./copy_directory.sh <source_directory1> <source_directory2> ...

# Check if at least one argument is provided
if [ "$#" -lt 1 ]; then
    echo -e "No source directories given\nUsage: $0 <source_directory1> <source_directory2> ...          <-  from /examples/stack-data-uploader/inputs/data/"
    exit 1
fi

copy_to_current_directory "$@"
