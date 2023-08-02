#!/bin/bash

shopt -s extglob  # Enable extended globbing


function clear_current_directory() {
    # Prompt for confirmation
    read -p "Are you sure you want to clear the current directory? [y/n] " choice

    # Convert the choice to lowercase (to handle uppercase input)
    choice="${choice,,}"

    # Check if the choice is 'y' or 'yes'
    if [[ $choice == 'y' || $choice == 'yes' ]]; then
        # Remove all files and directories in the current directory (excluding hidden files/dirs)
        shopt -s extglob  # Enable extended globbing
        rm -rf !(.gitignore|clearconfig.sh|getconfig.sh)   # Remove all files and directories except gitignore (shouldn't be found anyway unless dotglob gets enabled)
        shopt -u extglob  # Disable extended globbing
        echo "Current directory cleared successfully! Use getconfig.sh to retrieve config files"
    else
        echo "Operation aborted. The current directory was not cleared."
    fi
}

clear_current_directory
