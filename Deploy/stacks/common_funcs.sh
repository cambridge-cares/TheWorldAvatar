# Useful constants
LOG_DIVIDER_STR="\n==========================================================================================\n"
TRUE=0; FALSE=1


# Standalone functions
exit_with_msg()
{
  local return_code="$1"
  local msg="$2"
  printf "$msg$LOG_DIVIDER_STR"
  exit $return_code
}

init_stack_script()
{
  local stack="$1"
  local msg="$2"
  printf "$LOG_DIVIDER_STR$msg"
  cd "./$stack"
}

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

is_valid_stack()
{
  local stack=$1
  case "$stack" in
    "agent"|"db"|"web")
      return $TRUE ;;
    *)
      return $FALSE ;;
  esac
}

set_missing_secrets()
{
  local compose_files=$1

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
            exit 5
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
}

write_env_file()
{
  local env_filename=$1
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
}


# Functions that depend on one or more of the standalone functions
check_mode()
{
  local mode=$1
  if ! $(is_valid_mode $mode); then echo "get_yml_fnames: '$mode' is not a valid mode" && exit 11; fi
}

exit_on_error()
{
  local return_code="$1"
  local failure_msg="$2"
  if [ $return_code -ne 0 ]; then
    exit_with_msg "$return_code" "$failure_msg"
  fi
}

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

  base_yml="docker-compose.$process.yml"
  modespec_yml="docker-compose.$process.$mode.yml"
  testmodifier_yml="docker-compose.$process.test.yml"

  # Compile result
  result="$base_yml"
  if [ -f "$modespec_yml" ]; then
    result="$result $modespec_yml"
  fi
  if [ $use_test_config -eq $TRUE ] && [ -f "$testmodifier_yml" ]; then
    result="$result $testmodifier_yml"
  fi
  # Pass the result back via stdout
  echo "$result"
}