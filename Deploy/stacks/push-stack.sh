#!/bin/sh

# Wrapper script for docker-compose that pushes images, where applicable.
#
# Run this script with no arguments for usage instructions.


# Bail out if the stack name wasn't supplied
if [ "$#" -ne 1 ]; then
  echo "============================================================================="
  echo " Usage:"
  echo "  $0 [stack_name]"
  echo ""
  echo " [stack_name] : the stack to stop (agent/db/web)"
  echo "============================================================================="
  exit 1
fi

# Read stack from the first arg
stack=$1

# Check that a valid stack name was supplied
case $stack in
  db) ;;
  agent) ;;
  web) ;;
  *)
    echo "[$stack] is not a recognised stack name; choose 'agent', 'db', or 'web'."
    exit 2
esac

compose_files="docker-compose.yml"

# Set args to docker-compose itself, including the file specifiers
compose_file_args=$(echo $compose_files |sed -e 's/ / -f /' -e 's/^/-f /')
env_filename="env.txt"
compose_opts="$compose_file_args --env-file $env_filename"

printf "Pushing all images in the $stack stack\n\n"

# Switch to stack dir to simplify finding config files
pushd $stack > /dev/null

if [ ! -e $env_filename ]; then
  echo "Warning: no env vars file at $stack/$env_filename, '$stack' stack may not have been started. Trying to stop it anyway..."
fi

# Run docker-compose
docker_compose_cmd="docker-compose $compose_opts push"
$docker_compose_cmd
compose_push_exit_code=$?
popd > /dev/null

if [ $compose_push_exit_code -eq 0 ]; then
  printf "\nDone\n"
else
  printf "\n'docker-compose push' failed with exit code $compose_push_exit_code\n"
fi
