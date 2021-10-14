#!/bin/bash

# Convenience script to list localhost ports mapped by any service referenced in a docker-compose*yml file
if [ "$#" -gt 1 ]; then
  echo "============================================================================="
  echo " Usage:"
  echo "  $0 <--by_file>"

  echo "     --by_file : Report ports listed in each docker-compose*.yml file separately, rather than in a single list"
  echo ""
fi

# Process args
mode="flat_list"
while test $# -gt 0; do
  case "$1" in
    --by_file)
      shift
      mode="by_file"
      ;;
    *)
      echo "Ignoring unrecognised argument [$1]"
      shift
      ;;
  esac
done

REPO_ROOT="$(dirname $0)/../../"

# Loop over all docker-compose ymls in $REPO_ROOT (with a few exceptions), using sed to extract the mapped ports
port_list=""
echo "The following localhost ports are mapped to a service in a docker-compose file:"
for f in $(find "${REPO_ROOT}" -name "docker-compose*.yml" -type f -not -path "*Flask_deployment_docker_image/*" -not -path "*/JPS_LDF/dependencies*"  -not -path "*/Deploy/stacks/dafni*"); do
  ports_in_file=$(sed -n -e 's/.*-\ ["]\?\([0-9]*\):\([0-9]*\).*/\1/p' -e 's/\n/\ /' $f $EXCLUDE_PATTERNS)
  if [ -n "$ports_in_file" ]; then
    port_list="$port_list$ports_in_file "

    # Report ports for each file
    if [ "$mode" == "by_file" ]; then      
      echo " ${f/$REPO_ROOT//}: ${ports_in_file//$'\n'/,}"
    fi
  fi
done

# Report an ordered list of all ports'
if [ "$mode" == "flat_list" ]; then
  ordered_port_list=$(echo $port_list|tr " " "\n"| sort -g | tr "\n" ",")
  printf " $ordered_port_list\n"
fi