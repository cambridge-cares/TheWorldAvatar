#!/bin/sh

# Wrapper script for docker-compose that builds/starts the requested stack in one of three
# modes (dev/test/prod).
#
# Each stack has its own dev, test and prod configuration files. See the following files for
# more info:
# ./<stack_name>/
#   docker-compose.yml
#   docker-compose.dev.yml
#   docker-compose.test.yml
#   docker-compose.prod.yml
#
# Run this script with no arguments for usage instructions.


# Bail out if the stack name and mode weren't supplied
if [ "$#" -lt 2 ]; then
  echo "============================================================================="
  echo " Usage:"
  echo "  $0 [stack_name] [mode] <additional_args_for_docker_compose>"
  echo ""
  echo " [stack_name] : the stack to start (agent/db/web)"
  echo "       [mode] : configuration mode name (dev/test/prod)"
  echo "============================================================================="
  exit 1
fi

# Read stack and mode from the first two args
stack=$1
mode=$2
shift
shift

# Check that a valid stack name was supplied
case $stack in
  db) ;;
  agent) ;;
  web) ;;
  *)
    echo "[$stack] is not a recognised stack name; choose 'agent', 'db', or 'web'."
    exit 2
esac

# Check that a valid mode was supplied and set default options for 'docker-compose up'
up_default_opts="-d"
case $mode in
  dev)
    up_default_opts="$up_default_opts --force-recreate --build"
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

# Set compose files for this mode
compose_files="docker-compose.yml docker-compose.$mode.yml"

# Set args to docker-compose itself, including the file specifiers
compose_file_args=$(echo $compose_files |sed -e 's/ / -f /' -e 's/^/-f /')
env_filename="env.txt"
compose_opts="$compose_file_args -p $mode-$stack --env-file $env_filename"

# Set options for 'docker-compose up', including any additional args passed to this script
up_opts="$up_default_opts $*"

printf "\n==========================================================================================\n"
printf "Building the $stack stack in $mode mode\n\n"

# Build in stack dir
cd ./$stack

# Write some properties (e.g. the current git hash) to a temporary env file so that they can be used
# in the compose config files
# Note that this file shouldn't be deleted or down commands will fail
if [ -e "$env_filename" ]; then
  rm "$env_filename"
fi

echo "Generating environment variables file..."
hash="$(git rev-parse --short=6 HEAD)"
builder="$(git config user.name)"
echo "HASH=$hash" >> "$env_filename"
echo "MODE=$mode" >> "$env_filename"
echo "BUILDER=$builder" >> "$env_filename"
printf "Done\n\n"

# Loop over secret files listed in the compose files, ensuring that they all exist, and have exactly one word on one line
echo "Checking required Docker secrets..."
for compose_file in $compose_files; do
  secret_file_paths=$(grep -E "file:.*\/secrets\/.*" $compose_file|awk '{print $2}'|tr "\n" " ")
  for secret_file_path in $secret_file_paths; do
    secret_state=""
    while [ "$secret_state" != "valid" ]
    do
      # Check that the secret file is present and valid
      if [ -f $secret_file_path ]; then
        num_lines=$(wc -l $secret_file_path |awk '{print $1}')
        # Zero lines (having no newline char) is allowed; set num_lines=1 if that's the case
        num_lines=$(( num_lines==0 ? 1 : num_lines ))
        num_words=$(wc -w $secret_file_path |awk '{print $1}')
        if [ $num_lines -eq 1 ] && [ $num_words -eq 1 ]; then
          secret_state="valid"
        else
          secret_state="invalid"
          echo "  Secret file at $secret_file_path looks to be invalid (contains $num_words words on $num_lines lines; expected 1 word on 1 line)"
        fi
      else
        secret_state="missing"
        echo "  Expected secret file not found at $stack/$secret_file_path"
      fi

      # If secret is missing/invalid, let the user enter a new value
      if [ $secret_state != "valid" ]; then
          read -p "  Enter a value for $secret_file_path (or a blank string to abort): " new_secret_val
          if [ -z "$new_secret_val" ]; then
            echo "Aborting..."
            exit 1
          else
            if [ -f $secret_file_path ]; then
              \rm -f $secret_file_path
            fi
            touch $secret_file_path
            echo $new_secret_val >> $secret_file_path
            secret_state="valid"
            echo "  Value set"
          fi
      fi
    done
  done
done
printf "Done\n\n"

# Run docker-compose
docker_compose_cmd="docker-compose $compose_opts up $up_opts"
echo "Running $docker_compose_cmd in ./$stack ..."
$docker_compose_cmd
compose_up_exit_code=$?
echo Done


if [ $compose_up_exit_code -eq 0 ]; then
  printf "\n$stack stack started in $mode mode\n"
else
  printf "\n'docker-compose up' failed with exit code $compose_up_exit_code\n"
fi

printf "==========================================================================================\n"

exit $compose_up_exit_code
