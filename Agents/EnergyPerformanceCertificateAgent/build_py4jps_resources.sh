#!/bin/bash

# Create directories for jps_base_lib and stack_client resource files
# (to be imported by py4jps)
mkdir tmp_base
mkdir tmp_stack

# Clone TWA repository
git clone https://github.com/cambridge-cares/TheWorldAvatar.git TWA
cd TWA

# Build latest JPS_BASE_LIB .jar
git checkout main
git pull
cd JPS_BASE_LIB
mvn clean package -DskipTests
# Copy required resource files
cp ./target/jps-base-lib*.jar ../../tmp_base/
cp -r ./target/lib ../../tmp_base/
cd ..

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
