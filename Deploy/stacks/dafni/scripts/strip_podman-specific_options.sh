#!/bin/bash

script_dir="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cat "$script_dir/../docker-compose.yml" | grep -v "network_name" > "$script_dir/../docker-compose.deploy.yml"