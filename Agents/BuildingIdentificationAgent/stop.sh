#!/bin/bash

#
# Stops the example stack and removes all volumes.
#
# Author: Michael Hillman (support<@>cmcl.io)
#

ROOT=$(git rev-parse --show-toplevel)
MANAGER_STACK="$ROOT/Deploy/stacks/dynamic/stack-manager/stack.sh"
$MANAGER_STACK remove heat