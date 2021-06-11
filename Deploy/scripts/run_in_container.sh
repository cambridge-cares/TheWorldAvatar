#!/bin/sh

# Convenience script to find a container by name and run commands in it.
# The supplied pattern must match exactly one container in order for the command(s) to be run

# Bail out if the search string or command(s) weren't supplied
if [ "$#" -lt 1 ]; then
  echo "============================================================================="
  echo " Usage:"
  echo "  $0 [search_str] [cmd_str]"
  echo ""
  echo " [search_str] : regex pattern used to match to container name"
  echo "    [cmd_str] : the command to be run (spaces and quotes should be preserved)"
  echo "============================================================================="
  exit 1
fi


search_str=$1

container_id_str="$(sudo docker ps -qf name=$search_str)"
num_matches=$(echo $container_id_str|wc -w)

if [ $num_matches -eq 1 ]; then
    container_name="$(docker ps -f id=$container_id_str --format ‘{{.Names}}’)"
    printf "Running command: ["
    printf " %q" "${@:2}"
    printf " ] in container [$container_name]\n"
    docker exec -it "$container_id_str" "${@:2}"
else
    echo "Pattern must match exactly one container; matches were: "
    for container_id in $container_id_str; do
        echo "   $(docker ps -f id=$container_id --format ‘{{.Names}}’)"
    done
fi
