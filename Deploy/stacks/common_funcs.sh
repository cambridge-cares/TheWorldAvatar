# This file contains constants and functions intended for use in the *-stack.sh scripts in Deploy/stacks/
# It should be sourced, rather than run directly, so is intentionally set as non-executable


#======================================= Useful constants =========================================
LOG_DIVIDER_STR="\n==========================================================================================\n"
TRUE=0; FALSE=1

#===================================== Standalone functions =======================================

# Print the supplied message, followed by LOG_DIVIDER_STR, then exit with the specified return code
exit_with_msg()
{
  local return_code="$1"
  local msg="$2"
  printf "$msg$LOG_DIVIDER_STR"
  exit $return_code
}

get_project_name()
{
  # Check number of args
  if [ "$#" -ne 3 ]; then
    echo "get_project_name: Expected 3 arguments, but $# were passed."
    exit 1
  fi

  local stack=$1
  local mode=$2  
  local use_test_config=$3

  # Default project name is <stack>-<mode> 
  local result="$stack-$mode"

  # Append a modifier if running in test configuration
  if [ $use_test_config -eq $TRUE ]; then
    result="$result-test"
  fi

  # Pass the result back via stdout
  echo "$result"
}

# Print the supplied message, then cd to the specified stack directory
init_stack_script()
{
  local stack="$1"
  local msg="$2"
  printf "$LOG_DIVIDER_STR$msg"
  cd "./$stack"
}

# Check whether the supplied string is a valid 'mode'
is_valid_mode()
{
  local mode=$1
  case "$mode" in
    "dev"|"prod")
      return $TRUE ;;
    *)
      return $FALSE ;;
  esac
}

# Check whether the supplied string is a valid 'process'
is_valid_process()
{
  local process=$1
  case "$process" in
    "build"|"deploy")
      return $TRUE ;;
    *)
      return $FALSE ;;
  esac
}

# Check whether the supplied string is a valid stack name
is_valid_stack()
{
  local stack=$1
  case "$stack" in
    "agent"|"dafni"|"db"|"web")
      return $TRUE ;;
    *)
      return $FALSE ;;
  esac
}

# Extract secrets from the supplied compose files and check that the corresponding file exists
# If it doesn't, give the user the option to create the file and supply a value
set_missing_secrets()
{
  local compose_files="$1"
  local mode="$2"

  for compose_file in $compose_files; do
    secret_file_paths=$(grep -E "^\s*file:.*\/secrets\/.*" $compose_file|awk '{print $2}'|tr "\n" " ")
    for raw_secret_file_path in $secret_file_paths; do

      # Handle secret paths containing the MODE variable
      secret_file_path=$(echo "$raw_secret_file_path"|sed -e "s/\${MODE}/$mode/" -e "s/\$MODE/$mode/")

      secret_state=""
      while [ "$secret_state" != "valid" ]
      do
        # Decide whether secret is a single word based on its name
        if [ -z "${secret_file_path##*password*}" ] || [ -z "${secret_file_path##*api_key*}" ]; then single_word_secret=$TRUE; else single_word_secret=$FALSE; fi

        # Check that the secret file is present and valid
        if [ -f $secret_file_path ]; then
          if [ $single_word_secret -eq $TRUE ]; then
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
            secret_state="valid"
          fi
        else
          secret_state="missing"
          echo "  Expected secret file not found at $stack/$secret_file_path"
        fi

        # If secret is missing/invalid, let the user enter a new value
        if [ $secret_state != "valid" ]; then
          if [ $single_word_secret -eq $TRUE ]; then
            read -p "  Enter a value for $secret_file_path (or a blank string to abort): " new_secret_val
            if [ -n "$new_secret_val" ]; then
              if [ -f $secret_file_path ]; then
                \rm -f $secret_file_path
              fi
              touch $secret_file_path
              echo $new_secret_val >> $secret_file_path
              secret_state="valid"
              echo "  Value set"
            fi
          fi
          # If secret still isn't valid, either it isn't a single word, or the user chose not to enter it; abort
          if [ $secret_state != "valid" ]; then
            echo " Populate the secret file and re-run this script to continue"
            exit 5
          fi
        fi
      done
    done
  done
}

# Write a file containing environment variables used when calling docker-compose at build and/or deploy time
write_env_file()
{
  # Check number of args
  if [ "$#" -ne 4 ]; then
    echo "write_env_file: Expected 4 arguments, but $# were passed."
    exit 1
  fi

  local env_filename=$1
  local stack=$2
  local mode=$3  
  local use_test_config=$4

  if [ -e "$env_filename" ]; then
    rm "$env_filename"
  fi

  # Determine network name
  local network_name="$stack-$mode"
  if [ $use_test_config -eq $TRUE ]; then
    network_name="$network_name-test"
  fi

  # Determine container name suffix
  local container_name_suffix="-$mode"
  if [ $use_test_config -eq $TRUE ]; then
    container_name_suffix="$container_name_suffix-test"
  fi
  
  echo "Generating environment variables file..."
  local hash="$(git rev-parse --short=6 HEAD)"
  local builder="$(git config user.name)"
  echo "HASH=$hash" >> "$env_filename"
  echo "MODE=$mode" >> "$env_filename"
  echo "BUILDER=$builder" >> "$env_filename"
  echo "CONTAINER_NAME_SUFFIX=$container_name_suffix" >> "$env_filename"
  echo "NETWORK_NAME=$network_name" >> "$env_filename"
  printf "Done\n\n"
}

#===================================== Dependent functions ========================================
# The following functions depend on one or more of the standalone functions, so need to be declared later

# Output an error message if the supplied string isn't a valid mode
check_mode()
{
  local mode=$1
  if ! $(is_valid_mode $mode); then echo "'$mode' is not a valid mode" && exit 11; fi
}

# If the supplied return code is not zero, print a message and exit
exit_on_error()
{
  local return_code="$1"
  local failure_msg="$2"
  if [ $return_code -ne 0 ]; then
    exit_with_msg "$return_code" "$failure_msg"
  fi
}

# Compile a list of docker-compose*.yml files depending on the 'mode' and 'process' being executed, and whether
# the stack is being run in a test configuration.
# Mode-specific and test configurations are silently ignored if the corresponding file doesn't exist.
get_yml_fnames()
{
  # Check number of args
  if [ "$#" -ne 3 ]; then
    echo "get_yml_fnames: Expected 3 arguments, but $# were passed."
    exit 1
  fi

  local mode=$1
  local process=$2
  local use_test_config=$3

  # Check mode arg is valid
  if ! $(is_valid_mode $mode); then echo "get_yml_fnames: '$mode' is not a valid mode" && exit 11; fi

  # Check process arg is valid
  if ! $(is_valid_process $process); then echo "get_yml_fnames: '$process' is not a valid process" && exit 11; fi

  # Include base yml file for this process
  local result="docker-compose.$process.yml"

  # Include mode-specific yml file if it exists
  local modespec_yml="docker-compose.$process.$mode.yml"
  if [ -f "$modespec_yml" ]; then
    result="$result $modespec_yml"
  fi

  # Add test options yml if requested and the file exists
  if [ $use_test_config -eq $TRUE ]; then
    local testmodifier_yml="docker-compose.$process.test.yml"
    if [ -f "$testmodifier_yml" ]; then
      result="$result $testmodifier_yml"
    fi
    local modespec_testmodifier_yml="docker-compose.$process.$mode.test.yml"
    if [ -f "$modespec_testmodifier_yml" ]; then
      result="$result $modespec_testmodifier_yml"
    fi
  else
    local livemodifier_yml="docker-compose.$process.live.yml"
    if [ -f "$livemodifier_yml" ]; then
      result="$result $livemodifier_yml"
    fi
    local modespec_livemodifier_yml="docker-compose.$process.$mode.live.yml"
    if [ -f "$modespec_livemodifier_yml" ]; then
      result="$result $modespec_livemodifier_yml"
    fi
  fi

  # Pass the result back via stdout
  echo "$result"
}