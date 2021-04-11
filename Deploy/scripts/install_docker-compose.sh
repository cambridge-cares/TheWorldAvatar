#!/bin/bash

# Convenience script to install docker-compose

# Exit if not running as root
if [ $EUID -ne 0 ]; then
    echo "$0 needs to be run as root; aborting"
    exit 1
fi

# Suggest /usr/bin as install directory if it's on the path
install_dir=""
if echo "$PATH" | tr ':' '\n'|grep -xq "/usr/bin"; then
  read -p "/usr/bin is on the path, install there? ('Y' to accept, 'N' to choose a different location): " choice
  if [[ $choice == [yY] || $choice == [yY][eE][sS] ]]; then
    install_dir=/usr/bin
  fi
fi

# Allow a different location to be set if /usr/bin isn't on the path, or isn't suitable
if [ -z "$install_dir" ]; then
  read -p "Enter install directory:" install_dir
fi


# Check that install directory exists and doesn't already contain docker-compose before installing 
if [ -d "$install_dir" ]; then
  install_path="$install_dir/docker-compose"
  if [ -e "$install_path" ]; then
    echo "docker-compose already exists at [$install_path], aborting"
    exit 3
  fi
  echo "Installing docker-compose to $install_path"
  sudo curl -L "https://github.com/docker/compose/releases/download/1.28.2/docker-compose-$(uname -s)-$(uname -m)" -o "$install_path"
  sudo chmod +x "$install_path"
  echo Done
else
  echo "Installation dir [$install_dir] does not exist, aborting"
  exit 2
fi
