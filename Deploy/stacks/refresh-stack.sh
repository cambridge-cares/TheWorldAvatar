#!/bin/sh

# Wrapper script for docker-compose that brings down the stack for the requested mode,
# then brings it back up again.
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

./stop-stack.sh $1 $2
./start-stack.sh $1 $2