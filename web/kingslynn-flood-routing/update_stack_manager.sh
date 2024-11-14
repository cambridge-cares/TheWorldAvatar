#!/bin/bash

replace_string() {
    local directory="$1"
    local placeholder="$2"
    local new_string="$3"

    find "$directory" -type f \( -name "*.html" -o -name "*.json" \) | while read -r file; do
        echo "Processing file: $file"
        sed -i "s#$placeholder#$new_string#g" "$file"
    done
}

escape_url() {
local url="$1"
printf '%s\n' "$url" | sed -e 's/[\/&]/\\&/g'
}

print_help() {
echo "This script replaces the public URL for visualisation and automatically update the stack manager config files."
echo "Usage: $0 [option]"
echo "Options:"
echo "  -h    Display this help message."
echo "  <public_url> The public URL of this stack. Do not include / at the end."
}

## handle commandline input

if [ "$1" == "-h" ] || [ -z "$1" ]; then
print_help
exit 0
fi

arg="$1"

## replace URL for visualisation

dir_webspace="./stack-manager-inputs/data/webspace"
URL_PLACEHOLDER="<PUBLIC_URL>"
PUBLIC_URL=$(escape_url "$arg")

replace_string "$dir_webspace" "$URL_PLACEHOLDER" "$PUBLIC_URL"

## replace TWA directory in stack manager config files

# Assume TWA is two level up from here
dir_config="./stack-manager-inputs/config"
DIR_PLACEHOLDER="<REPLACE_WITH_YOUR_DIRECTORY>"
TWA_DIR=$(dirname "$(dirname "$(pwd)")")

replace_string "$dir_config" "$DIR_PLACEHOLDER" "$TWA_DIR"

## rename stack config file

STACK_NAME="routing"
mv "./stack-manager-inputs/config/routing.json" "./stack-manager-inputs/config/$STACK_NAME.json"

## send files to deployment

DEPLOY_DIR="$TWA_DIR/Deploy/stacks/dynamic"
rsync -av "./stack-manager-inputs/" "$DEPLOY_DIR/stack-manager/inputs/"