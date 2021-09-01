#!/bin/sh

# Wrapper script for docker-compose that starts the requested stack in either 'dev' or 'prod' mode.
#
# Each stack directory contains a base configuration file and, optionally, mode-specific configuration files.
# That is:
#   docker-compose.deploy.yml
#   docker-compose.deploy.dev.yml (optional)
#   docker-compose.deploy.prod.yml (optional)


# Show a usage statement if too few arguments were supplied (use 'lt' rather than 'ne' to allow additional arguments to docker-compose up)
if [ "$#" -lt 2 ]; then
  echo "============================================================================="
  echo " Usage:"
  echo "  $0 [stack_name] [mode] <--force-pull service_list> <additional_args>"
  echo ""
  echo "                stack_name : the stack to start (agent/db/web)"
  echo "                      mode : configuration mode name (dev/prod)"
  echo " --force-pull service_list : pull images from the registry, even if they exist locally."
  echo "                             'service_list' is a comma-separated list of service names"
  echo "                    --test : modify service options (particularly port mappings) according to docker-compose.deploy.test.yml"
  echo "           additional_args : remaining arguments are passed on to 'docker-compose up'"
  echo ""
  echo "e.g. To start the web stack in dev mode:"
  echo "  $0 web dev"
  echo "============================================================================="
  exit 1
fi


# Default options for docker-compose up
default_up_args="-d --no-build"

# Assign first two input args to variables and pop/shift them from the arg array
process="deploy"
stack="$1"
mode="$2"
shift;shift

# Load common helper functions
if [ -e ./common_funcs.sh ]; then
  . ./common_funcs.sh
else
  echo "Unable to load bash helper functions, make sure you're running this script in Deploy/stacks/"
  exit 1
fi

# Process remaining args. Avoiding using getopts here in a vain attempt to keep things shell-agnostic
additional_up_args=""
services_to_force_pull=""
use_test_config=$FALSE
while test $# -gt 0; do
  case "$1" in
    --force-pull)
      shift
      services_to_force_pull=$1
      shift
      ;;
    --test)
      use_test_config=$TRUE
      shift
      ;;
    *)
      additional_up_args="$additional_up_args $1"
      shift
      ;;
  esac
done

# Validate args
if ! $(is_valid_stack $stack); then echo "$0: '$stack' is not a valid stack" && exit 2; fi
if ! $(is_valid_mode $mode); then echo "$0: '$mode' is not a valid mode" && exit 3; fi


# Print preamble and cd to stack directory
init_stack_script $stack "Deploying the $stack stack in $mode mode\n\n"

# Get yml filenames
yml_fnames=$(get_yml_fnames $mode $process $use_test_config)
if [ "$?" -ne 0 ]; then echo "$yml_fnames" ; exit "$?"; fi
yml_fname_args=$(echo $yml_fnames |sed -e 's/ / -f /' -e 's/^/-f /')

# Write environment variables to file so that docker-compose can pick them up
env_filename="env.txt"
write_env_file $env_filename $stack $mode $use_test_config

# Assemble arguments for docker-compose
project_name=$(get_project_name $stack $mode $use_test_config)
compose_opts="$yml_fname_args --env-file $env_filename -p $project_name"

# Examine secret files defined in the config files and set missing values where necessary
set_missing_secrets $yml_fnames

# For images that exist locally, but need to be updated from the registry, need to run docker pull explicitly
if [ ! -z "$services_to_force_pull" ]; then
  echo "Overriding local image(s) with registry version..."
  cmd="docker-compose $compose_opts pull $services_to_force_pull"
  echo "Running $cmd in ./$stack ..."
  $cmd
  pull_return_code=$?
  exit_on_error "$pull_return_code" "\n'docker-compose pull' failed"
  printf "Done\n\n"
fi

# Run docker-compose up, passing on any additional args that were supplied to this script
echo "Deploying stack"
cmd="docker-compose $compose_opts up $default_up_args $additional_up_args"
echo "Running $cmd in ./$stack ..."
$cmd
up_exit_code=$?
exit_on_error $up_exit_code "\n'docker-compose up' failed"

# print success message and exit
exit_with_msg 0 "\n$stack stack started in $mode mode"