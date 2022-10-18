#!/bin/bash

# Create directory for stack_client resource files (to be imported by py4jps)
mkdir tmp_stack

# Clone TWA repository
git clone https://github.com/cambridge-cares/TheWorldAvatar.git TWA
cd TWA

# Build latest STACK_CLIENTS .jar
git checkout dev-MetOfficeAgent-withinStack
git pull
cd Deploy/stacks/dynamic/stack-clients
mvn clean package -DskipTests
cp ./target/stack-clients*.jar ../../../../../tmp_stack/
cp -r ./target/lib ../../../../../tmp_stack/
cd ../../../../..

# Remove TWA repository
rm -fr TWA