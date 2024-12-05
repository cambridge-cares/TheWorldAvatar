#!/bin/bash

# Create directory for stack_clients resource files (to be imported by py4jps)
mkdir tmp_stack

# Get paths to stack_clients and agent repos
AGENT_DIR="$(dirname -- "$(readlink -f "${BASH_SOURCE}")")"
STACK_CLIENTS_DIR="$(git rev-parse --show-toplevel)/Deploy/stacks/dynamic/stack-clients"

# Build latest STACK_CLIENTS .jar and copy resources files
cd $STACK_CLIENTS_DIR
mvn clean package -DskipTests
cp ./target/stack-clients*.jar $AGENT_DIR/tmp_stack/
cp -r ./target/lib $AGENT_DIR/tmp_stack/
