#!/bin/sh

# Wrapper script for docker-compose that builds the requested stack in either 'dev' or 'prod' mode.
#
# Each stack directory contains a base configuration file and, optionally, mode-specific configuration files.
# That is:
#   docker-compose.build.yml
#   docker-compose.build.dev.yml (optional)
#   docker-compose.build.prod.yml (optional)


# Show a usage statement if too few arguments were supplied
#  (use 'lt' rather than 'ne' to allow additional arguments to docker-compose build)
if [ "$#" -lt 2 ]; then
  echo "============================================================================="
  echo " Usage:"
  echo "  $0 [stack_name] [mode] <--push> <additional_args>"
  echo ""
  echo "      stack_name : the stack to build (agent/db/web)"
  echo "            mode : configuration mode name (dev/prod)"
  echo "          --push : including this flag will push images to the registry once built"
  echo " additional_args : remaining arguments are passed on to 'docker-compose build'"
  echo ""
  echo "e.g. To build the web stack in dev mode, rebuilding each layer from scratch rather than using the cache:"
  echo "  $0 web dev --no-cache"
  echo "============================================================================="
  exit 1
fi

default_build_args=""

# Load common helper functions
if [ -e ./common_funcs.sh ]; then
  . ./common_funcs.sh
else
  echo "Unable to load bash helper functions, make sure you're running this script in Deploy/stacks/"
  exit 1
fi

# Assign input args to variables and pop/shift them from the arg array
process="build"
stack="$1"
mode="$2"
shift;shift

# Process remaining args. Avoiding using getopts here in a vain attempt to keep things shell-agnostic
additional_build_args=""
additional_push_args=""
push_enabled=$FALSE
while test $# -gt 0; do
  case "$1" in
    --push)
      push_enabled=$TRUE
      shift
      ;;
    -*)
      additional_build_args="$additional_build_args $1"
      shift
      ;;
    *)
      # Assume all remaining non-flag args are service name - pass on to 'build' and 'push'
      additional_push_args="$additional_push_args $1"
      additional_build_args="$additional_build_args $1"
      shift
      ;;
  esac
done

# Validate args
if ! $(is_valid_stack $stack); then echo "$0: '$stack' is not a valid stack" && exit 2; fi
if ! $(is_valid_mode $mode); then echo "$0: '$mode' is not a valid mode" && exit 3; fi

# Print preamble and cd to stack directory
init_stack_script $stack "Building the $stack stack in $mode mode\n\n"

# Get yml filenames
yml_fnames=$(get_yml_fnames $mode $process $FALSE)
if [ "$?" -ne 0 ]; then echo "$yml_fnames" ; exit "$?"; fi
yml_fname_args=$(echo $yml_fnames |sed -e 's/ / -f /g' -e 's/^/-f /')

# Write environment variables to file so that docker-compose can pick them up
env_filename="env.txt"
write_env_file $env_filename $stack $mode $FALSE

# Assemble arguments for docker-compose
compose_opts="$yml_fname_args --env-file $env_filename"

# Run docker-compose build, appending any additional args passed to this script
cmd="docker-compose $compose_opts build $default_build_args $additional_build_args"
echo "Running $cmd in ./$stack ..."
$cmd
build_exit_code=$?
exit_on_error $build_exit_code "\n'docker-compose build' failed"

# Run docker-compose push if requested
if [ $push_enabled -eq $TRUE ]; then
  echo "Pushing images to registry"
  cmd="docker-compose $compose_opts push $additional_push_args"
  echo "Running $cmd in ./$stack ..."
  $cmd
  push_exit_code=$?
  exit_on_error $push_exit_code "\n'docker-compose push' failed"
fi

# print success message and exit
exit_with_msg 0 "\nFinished building $stack stack in $mode mode"