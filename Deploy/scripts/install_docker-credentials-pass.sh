#!/bin/bash

# Convenience script to install docker-credential-pass

exec_name="docker-credential-pass"

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


# Check that install directory exists and doesn't already contain docker-credential-pass before installing 
if [ -d "$install_dir" ]; then
  install_path="$install_dir/$exec_name"
  if [ -e "$install_path" ]; then
    echo "$exec_name already exists at [$install_path], aborting"
    exit 3
  fi
  echo "Installing $exec_name to $install_path"
  zipfile=docker-cred.tar.gz
  cd /tmp
  curl -s -L https://github.com/docker/docker-credential-helpers/releases/download/v0.6.3/docker-credential-pass-v0.6.3-amd64.tar.gz -o $zipfile
  tar xfvz $zipfile > /dev/null
  cp $exec_name "$install_path"
  cd - > /dev/null
  sudo chmod +x "$install_path"
  echo Done
else
  echo "Installation dir [$install_dir] does not exist, aborting"
  exit 2
fi
