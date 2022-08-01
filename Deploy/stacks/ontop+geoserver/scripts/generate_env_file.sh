#!/bin/bash

script_dir="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

default_project_id="the_world_avatar"

# Read user ID from input
read -p "Enter a project identifier [${default_project_id}]: " project_id

if [[ -z "${project_id}" ]]; then
  project_id="${default_project_id}"
else
  if [[ -n ${project_id//[0-9a-zA-z_-]/} ]]; then
    echo "Input contained characters that are not alphanumeric, '-' or '_', exiting..."
    exit 2
  fi
fi

default_port_prefix=231

# Read user ID from input
read -p "Enter three digits for port number prefix [${default_port_prefix}]: " port_prefix

if [[ -z "${port_prefix}" ]]; then
  port_prefix="${default_port_prefix}"
else
  if [ ${#port_prefix} -ne 3 ]; then
    echo "Input had length ${#port_prefix}; expected length 3, exiting..."
    exit 1
  fi

  if [[ -n ${port_prefix//[0-9]/} ]]; then
    echo "Input contained non-numeric characters, exiting..."
    exit 2
  fi
fi

if command -v podman-compose &> /dev/null
then
  container_host=$(hostname)
else
  container_host=host.docker.internal
fi

sed \
    -e "s/PROJECT_ID/${project_id}/" \
    -e "s/USER_PORT_BASE/${port_prefix}/" \
    -e "s/CONTAINER_HOST/${container_host}/" \
    < "$script_dir/../.env_template" \
    > "$script_dir/../.env"