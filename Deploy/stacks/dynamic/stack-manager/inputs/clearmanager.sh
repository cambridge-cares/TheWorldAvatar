#!/bin/bash

shopt -s extglob # to allow extended globbing with !(pattern) to keep certain files


function clear_current_directory() {
    # Prompt for confirmation
    read -p "Are you sure you want to remove everything from data and config directories? [y/n] " choice

    # Convert the choice to lowercase (to handle uppercase input)
    choice="${choice,,}"

    # Check if the choice is 'y' or 'yes'
    if [[ $choice == 'y' || $choice == 'yes' ]]; then
        # Remove all files and directories in the current directory (excluding hidden files/dirs)

        rm -rf config/!(SERVICE_CONFIG_FILES_HAVE_MOVED.md|.gitignore|services)
        #rm -rf config/\!\(SERVICE_CONFIG_FILES_HAVE_MOVED.md\|services\)
        rm -rf data/!(.gitignore)
        rm -rf config/services/*
        #shopt -u extglob

        echo "Stack manager data and config files cleared successfully! Use getmanager.sh to retrieve stack manager files"
    else
        echo "Operation aborted. The current directory was not cleared."
    fi
}

clear_current_directory
