#!/bin/bash

script_dir="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Read user ID from input
read -p "Enter the last two numbers in your dafni user ID, or '10' if you're running this elsewhere: " user_id

if [ ${#user_id} -ne 2 ]; then
  echo "Input had length ${#user_id}; expected length 2, exiting..."
  exit 1
fi

if [[ -n ${user_id//[0-9]/} ]]; then
  echo "Input contained non-numeric characters, exiting..."
  exit 2
fi

sed -e "s/USER_PORT_BASE/1$user_id/g" < "$script_dir/../.env_template" > "$script_dir/../.env"