#!/bin/sh

# Wrapper script for docker-compose that stops a stack running in the specified mode.

# Show a usage statement if the wrong number of arguments were supplied
if [ "$#" -ne 2 ]; then
  echo "============================================================================="
  echo " Usage:"
  echo "  $0 [stack_name] [mode]"
  echo ""
  echo "  stack_name : the stack to start (agent/db/web)"
  echo "        mode : configuration mode name (dev/test/prod)"
  echo ""
  echo "  e.g. To stop the agent stack, running in dev mode:"
  echo "   $0 agent dev"
  echo "============================================================================="
  exit 1
fi


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

# Validate args
if ! $(is_valid_stack $stack); then echo "$0: '$stack' is not a valid stack" && exit 2; fi
if ! $(is_valid_mode $mode); then echo "$0: '$mode' is not a valid mode" && exit 3; fi


# Print preamble and cd to stack directory
init_stack_script $stack "Stopping the $mode-$stack stack\n\n"

# Get yml filenames
yml_fnames=$(get_yml_fnames $mode $process $FALSE)
if [ "$?" -ne 0 ]; then echo "$yml_fnames" ; exit "$?"; fi
yml_fname_args=$(echo $yml_fnames |sed -e 's/ / -f /' -e 's/^/-f /')

# Write environment variables to file so that docker-compose can pick them up
env_filename="env.txt"
write_env_file $env_filename

# Assemble arguments for docker-compose
compose_opts="$yml_fname_args --env-file $env_filename"

# Run docker-compose down, passing on any additional args that were supplied to this script
cmd="docker-compose $compose_opts down"
echo "Running $cmd in ./$stack ..."
$cmd
down_exit_code=$?
exit_on_error $down_exit_code "\n'docker-compose down' failed"

# print success message and exit
exit_with_msg 0 "\n$mode-$stack stack stopped"