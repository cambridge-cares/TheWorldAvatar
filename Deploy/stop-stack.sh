#!/bin/sh

# Wrapper script for docker-compose that brings down the stack for the requested mode.
#
# Run this script with no arguments for usage instructions.


# Bail out if the stack name and mode weren't supplied
if [ "$#" -ne 2 ]; then
  echo "============================================================================="
  echo " Usage:"
  echo "  $0 [stack_name] [mode]"
  echo ""
  echo " [stack_name] : the stack to stop (agent/db/web)"
  echo "       [mode] : configuration mode name (dev/test/prod)"
  echo "============================================================================="
  exit 1
fi

# Read stack and mode from the first two args
stack=$1
mode=$2

# Check that a valid stack name was supplied
case $stack in
  db) ;;
  agent) ;;
  web) ;;
  *)
    echo "[$stack] is not a recognised stack name; choose 'agent', 'db', or 'web'."
    exit 2
esac

# Check that a valid mode was supplied and set corresponding docker-compose files
case $mode in
  dev)
    ;;
  test)
    ;;
  prod)
    ;;
  *)
    echo "[$mode] is not a recognised mode - choose 'dev', 'test' or 'prod'."
    exit 3
    ;;
esac
compose_files="docker-compose.yml docker-compose.$mode.yml"

# Set args to docker-compose itself, including the file specifiers
compose_file_args=$(echo $compose_files |sed -e 's/ / -f /' -e 's/^/-f /')
compose_opts="$compose_file_args -p $mode-$stack"

printf "Stopping the $mode-$stack stack\n\n"

# Switch to stack dir to simplify finding config files
pushd $stack > /dev/null

if [ ! -e $env_filename ]; then
  echo "Warning: no env vars file at $stack/$env_filename, '$stack' stack may not have been started. Trying to stop it anyway..."
fi

# Run docker-compose
docker_compose_cmd="docker-compose $compose_opts down"
$docker_compose_cmd
compose_down_exit_code=$?
popd > /dev/null

if [ $compose_down_exit_code -eq 0 ]; then
  printf "\nDone\n"
else
  printf "\n'docker-compose down' failed with exit code $compose_down_exit_code\n"
fi
